context("test tidiers for ranger randomForest")

testdata_dir <- tempdir()
testdata_filename <- "airline_2013_10_tricky_v3_5k.csv" 
testdata_file_path <- paste0(testdata_dir, testdata_filename)

filepath <- if (!testdata_filename %in% list.files(testdata_dir)) {
  "https://www.dropbox.com/s/f47baw5f3v0xoll/airline_2013_10_tricky_v3.csv?dl=1"
} else {
  testdata_file_path
}

flight <- exploratory::read_delim_file(filepath, ",", quote = "\"", skip = 0 , col_names = TRUE , na = c("","NA") , locale=readr::locale(encoding = "UTF-8", decimal_mark = "."), trim_ws = FALSE , progress = FALSE) %>% exploratory::clean_data_frame()

filepath <- if (!testdata_filename %in% list.files(testdata_dir)) {
  flight <- flight %>% slice_sample(n=5000)
  write.csv(flight, testdata_file_path) # save sampled-down data for performance.
}

test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", NA), # test NA handling
      DISTANCE = c(NA, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, NA, 361, 507, 1020, 1092, 342, NA, 1184, 545),
      FNUMBER= c(21, NA, 6, 87, 23, 12, 3, 0, 13, 1, 85, 82, 31, 57, 20, 12, 42, 49, NA, 45)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE", "FNUMBER"))
test_that("test ranger with regression", {
  model_ret <- build_model(test_data,
                           model_func = rangerReg,
                           formula = FNUMBER ~ DISTANCE,
                           test_rate = 0.3)
  coef_ret <- model_coef(model_ret, pretty.name = TRUE)
  expect_equal(colnames(coef_ret), c("variable", "importance"))
  stats_ret <- suppressWarnings(model_stats(model_ret))
  expect_equal(colnames(stats_ret), c("r_squared", "root_mean_square_error", "n"))
  expected_colnames <- c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE",
                         "FNUMBER", "predicted_value")
  pred_train_ret <- suppressWarnings(prediction(model_ret, data = "training"))
  expect_equal(colnames(pred_train_ret), expected_colnames)

  pred_test_ret <- prediction(model_ret, data = "test")
  expect_equal(colnames(pred_test_ret), expected_colnames)

  pred_test_newdata_ret <- prediction(model_ret, data = "newdata", data_frame = test_data %>% select(-FNUMBER))
  expected_colnames <- c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE",
                         "predicted_value")
  expect_equal(colnames(pred_test_newdata_ret), expected_colnames)
})

test_that("test ranger with binary regression all predictor variables", {
  model_ret <- suppressWarnings({
    build_model(test_data,
                model_func = rangerReg,
                formula = FNUMBER ~ .,
                test_rate = 0.3)
  })
  coef_ret <- model_coef(model_ret, pretty.name = TRUE)
  expect_equal(colnames(coef_ret), c("variable", "importance"))
  stats_ret <- suppressWarnings(model_stats(model_ret))
  expect_equal(colnames(stats_ret), c("r_squared", "root_mean_square_error", "n"))
  expected_colnames <- c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE",
                         "FNUMBER", "predicted_value")
  pred_train_ret <- suppressWarnings(prediction(model_ret, data = "training"))
  expect_equal(colnames(pred_train_ret), expected_colnames)

  pred_test_newdata_ret <- prediction(model_ret, data = "newdata", data_frame = test_data %>% select(-FNUMBER))
  expected_colnames <- c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE",
                         "predicted_value")
  expect_equal(colnames(pred_test_newdata_ret), expected_colnames)
})
test_that("test ranger with binary classification with logical column", {
  test_data[["IS AA"]] <- factor(test_data$CARRIER == "AA") # test target column name with space
  test_data[1, "IS AA"] <- NA
  model_ret <- suppressWarnings({
      build_model(test_data,
        model_func = rangerBinary,
        formula = `IS AA` ~ DISTANCE,
        test_rate = 0.3)
  })
  coef_ret <- model_coef(model_ret)
  expect_equal(colnames(coef_ret), c("variable", "importance"))

  model_stats <- suppressWarnings(model_stats(model_ret, pretty.name = TRUE))
  expect_colnames <- c("AUC", "F Score", "Accuracy Rate", "Misclassification Rate",
                       "Precision", "Recall")
  expect_equal(colnames(model_stats), expect_colnames)

  pred_train_ret <- suppressWarnings(
    prediction_binary(model_ret, data = "training", threshold = 0.6)
  )
  pred_train_ret <- suppressWarnings(
    prediction_binary(model_ret, data = "training", threshold = "f_score")
  )
  expect_colnames <- c("CANCELLED", "Carrier Name", "CARRIER",
                       "DISTANCE", "FNUMBER", "IS AA", "predicted_probability", "predicted_label")
  expect_true(all(expect_colnames %in% colnames(pred_train_ret)))

  pred_train_ret2 <- suppressWarnings(
    prediction(model_ret, data = "training", threshold = "f_score")
  )
  expect_true(all(expect_colnames %in% colnames(pred_train_ret2)))

  pred_test_ret <- suppressWarnings(prediction_binary(model_ret, data = "test"))
  expect_true(all(expect_colnames %in% colnames(pred_test_ret)))

  pred_test_ret2 <- suppressWarnings(prediction(model_ret, data = "test"))
  expect_true(all(expect_colnames %in% colnames(pred_test_ret2)))

  expect_colnames <- c("CANCELLED", "Carrier Name", "CARRIER",
                       "DISTANCE", "FNUMBER", "predicted_probability", "predicted_label")
  pred_test_newdata_ret <- suppressWarnings(prediction_binary(model_ret, data = "newdata", data_frame = test_data %>% select(-`IS AA`)))
  expect_true(all(expect_colnames %in% colnames(pred_test_newdata_ret)))

  pred_test_newdata_ret2 <- suppressWarnings(prediction_binary(model_ret, data = "newdata", data_frame = test_data %>% select(-`IS AA`)))
  expect_true(all(expect_colnames %in% colnames(pred_test_newdata_ret2)))
})

test_that("test ranger with binary classification with factor", {
  test_data[["IS AA"]] <- if_else(test_data$CARRIER == "AA", "AA", "Not AA") # test target column name with space
  test_data[1, "IS AA"] <- NA
  model_ret <- suppressWarnings({
      build_model(test_data,
        model_func = rangerBinary,
        formula = `IS AA` ~ DISTANCE,
        test_rate = 0.3)
  })
  coef_ret <- model_coef(model_ret)
  expect_equal(colnames(coef_ret), c("variable", "importance"))

  model_stats <- suppressWarnings(model_stats(model_ret, pretty.name = TRUE))
  expect_colnames <- c("AUC", "F Score", "Accuracy Rate", "Misclassification Rate",
                       "Precision", "Recall")
  expect_equal(colnames(model_stats), expect_colnames)

  pred_train_ret <- suppressWarnings(
    prediction_binary(model_ret, data = "training", threshold = 0.6)
  )
  pred_train_ret <- suppressWarnings(
    prediction_binary(model_ret, data = "training", threshold = "f_score")
  )
  expect_colnames <- c("CANCELLED", "Carrier Name", "CARRIER",
                       "DISTANCE", "FNUMBER", "IS AA", "predicted_probability", "predicted_label")
  expect_true(all(expect_colnames %in% colnames(pred_train_ret)))

  pred_train_ret2 <- suppressWarnings(
    prediction(model_ret, data = "training", threshold = "f_score")
  )
  expect_true(all(expect_colnames %in% colnames(pred_train_ret2)))

  pred_test_ret <- suppressWarnings(prediction_binary(model_ret, data = "test"))
  expect_true(all(expect_colnames %in% colnames(pred_test_ret)))

  pred_test_ret2 <- suppressWarnings(prediction(model_ret, data = "test"))
  expect_true(all(expect_colnames %in% colnames(pred_test_ret2)))

  expect_colnames <- c("CANCELLED", "Carrier Name", "CARRIER",
                       "DISTANCE", "FNUMBER", "predicted_probability", "predicted_label")
  pred_test_newdata_ret <- suppressWarnings(prediction_binary(model_ret, data = "newdata", data_frame = test_data %>% select(-`IS AA`)))
  expect_true(all(expect_colnames %in% colnames(pred_test_newdata_ret)))

  pred_test_newdata_ret2 <- suppressWarnings(prediction_binary(model_ret, data = "newdata", data_frame = test_data %>% select(-`IS AA`)))
  expect_true(all(expect_colnames %in% colnames(pred_test_newdata_ret2)))
})

test_that("test ranger with binary classification (all predictor_varials)", {
  test_data[["IS AA"]] <- test_data$CARRIER == "AA" # test target column name with space
  test_data[1, "IS AA"] <- NA
  model_ret <- suppressWarnings({
      build_model(test_data,
        model_func = rangerBinary,
        formula = `IS AA` ~ .,
        test_rate = 0.3)
  })
  coef_ret <- model_coef(model_ret)
  expect_equal(colnames(coef_ret), c("variable", "importance"))

  model_stats <- suppressWarnings(model_stats(model_ret, pretty.name = TRUE))
  expect_colnames <- c("AUC", "F Score", "Accuracy Rate", "Misclassification Rate",
                       "Precision", "Recall")
  expect_equal(colnames(model_stats), expect_colnames)

  pred_train_ret <- suppressWarnings(
    prediction_binary(model_ret, data = "training", threshold = "f_score")
  )
  expect_colnames <- c("CANCELLED", "Carrier Name", "CARRIER",
                       "DISTANCE", "FNUMBER", "IS AA", "predicted_probability", "predicted_label")
  expect_true(all(expect_colnames %in% colnames(pred_train_ret)))

  pred_train_ret2 <- suppressWarnings(
    prediction(model_ret, data = "training", threshold = "f_score")
  )
  expect_true(all(expect_colnames %in% colnames(pred_train_ret2)))

  pred_test_ret <- suppressWarnings(prediction_binary(model_ret, data = "test"))
  expect_true(all(expect_colnames %in% colnames(pred_test_ret)))
  pred_test_ret2 <- suppressWarnings(prediction(model_ret, data = "test"))
  expect_true(all(expect_colnames %in% colnames(pred_test_ret2)))

  expect_colnames <- c("CANCELLED", "Carrier Name", "CARRIER",
                       "DISTANCE", "FNUMBER", "predicted_probability", "predicted_label")
  pred_test_newdata_ret <- suppressWarnings(prediction_binary(model_ret, data = "newdata", data_frame = test_data %>% select(-`IS AA`)))
  expect_true(all(expect_colnames %in% colnames(pred_test_newdata_ret)))

  pred_test_newdata_ret2 <- suppressWarnings(prediction(model_ret, data = "newdata", data_frame = test_data %>% select(-`IS AA`)))
  expect_true(all(expect_colnames %in% colnames(pred_test_newdata_ret2)))
})

test_that("test ranger with multinomial classification", {
  test_data[["IS_AA"]] <- as.factor(test_data$CARRIER == "AA")
  model_ret <- suppressWarnings({
    build_model(test_data,
      model_func = rangerMulti,
      formula = CARRIER ~ DISTANCE,
      test_rate = 0.2)
  })
  coef_ret <- model_coef(model_ret)
  expect_equal(colnames(coef_ret), c("variable", "importance"))
  model_stats <- suppressWarnings(model_stats(model_ret, pretty.name = TRUE))
  expect_colnames <- c("Micro-Averaged F Score", "Macro-Averaged F Score", "Accuracy Rate", "Misclassification Rate",
                       "Number of Rows")
  expect_equal(colnames(model_stats), expect_colnames)

  expect_colnames <- c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE", "FNUMBER",
                         "IS_AA", "predicted_probability_DL", "predicted_probability_AA",
                         "predicted_probability_MQ", "predicted_probability_EV", "predicted_probability_US",
                         "predicted_probability_9E", "predicted_probability", "predicted_label")

  pred_train_ret <- suppressWarnings(prediction(model_ret, data = "training"))
  expect_true(all(expect_colnames %in% colnames(pred_train_ret)))

  pred_test_ret <- suppressWarnings(prediction(model_ret, data = "test"))
  expect_true(all(expect_colnames %in% colnames(pred_test_ret)))

  expect_colnames <- c("CANCELLED", "Carrier Name", "DISTANCE", "FNUMBER",
                         "IS_AA", "predicted_probability_DL", "predicted_probability_AA",
                         "predicted_probability_MQ", "predicted_probability_EV", "predicted_probability_US",
                         "predicted_probability_9E", "predicted_probability", "predicted_label")
  pred_test_newdata_ret <- suppressWarnings(prediction(model_ret, data = "newdata", data_frame = test_data %>% select(-CARRIER)))
  expect_true(all(expect_colnames %in% colnames(pred_test_newdata_ret)))
})

test_that("test ranger with multinomial classification", {
  test_data[["IS_AA"]] <- as.factor(test_data$CARRIER == "AA")
  model_ret <- suppressWarnings({
    build_model(test_data,
      model_func = rangerMulti,
      formula = CARRIER ~ .,
      test_rate = 0.2)
  })
  coef_ret <- model_coef(model_ret)
  expect_equal(colnames(coef_ret), c("variable", "importance"))
  model_stats <- suppressWarnings(model_stats(model_ret, pretty.name = TRUE))
  expect_colnames <- c("Micro-Averaged F Score", "Macro-Averaged F Score", "Accuracy Rate", "Misclassification Rate",
                       "Number of Rows")
  expect_equal(colnames(model_stats), expect_colnames)

  expect_colnames <- c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE", "FNUMBER",
                         "IS_AA", "predicted_probability_DL", "predicted_probability_AA",
                         "predicted_probability_MQ", "predicted_probability_EV", "predicted_probability_US",
                         "predicted_probability", "predicted_label")

  pred_train_ret <- suppressWarnings(prediction(model_ret, data = "training"))
  expect_true(all(expect_colnames %in% colnames(pred_train_ret)))

  pred_test_ret <- suppressWarnings(prediction(model_ret, data = "test"))
  expect_true(all(expect_colnames %in% colnames(pred_test_ret)))

  expect_colnames <- c("CANCELLED", "Carrier Name", "DISTANCE", "FNUMBER",
                         "IS_AA", "predicted_probability_DL", "predicted_probability_AA",
                         "predicted_probability_MQ", "predicted_probability_EV", "predicted_probability_US",
                         "predicted_probability", "predicted_label")
  pred_test_newdata_ret <- suppressWarnings(prediction(model_ret, data = "newdata", data_frame = test_data %>% select(-CARRIER)))
  expect_true(all(expect_colnames %in% colnames(pred_test_newdata_ret)))
})


test_that("ranger.find_na", {
  df <- structure(
    list(x = 1:10, y = rep(TRUE, 10)),
    row.names = c(NA, 10L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("x", "y")
  )
  expected_na_at <- 1
  df[expected_na_at, 1] <- NA
  expect_equal(exploratory:::ranger.find_na("x", df), expected_na_at)

  expected_na_at <- c(1, 2, 5, 10)
  df[expected_na_at, 1] <- NA
  expect_equal(exploratory:::ranger.find_na("x", df), expected_na_at)
})


test_that("predict_value_from_prob", {
  df <- structure(
    list(x = 1:10, y = rep(TRUE, 10), z = c("A", "B", "C", "D", "D", "E", "F", "E", "A", "A")),
    row.names = c(NA, 10L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("x", "y", "z")
  )
  # reangerBinary
  m_b <- build_model(df, model_func = rangerBinary, formula = y ~ x)$model[[1]]
  expected_values <- rep(TRUE, 10)
  expect_equal(
    exploratory:::predict_value_from_prob(m_b$forest$levels, m_b$predictions, df[["y"]]),
    expected_values
  )

  # rangerMulti
  m_m <- build_model(df, model_func = rangerMulti, formula = z ~ x + y)$model[[1]]

  expected_values <- c("D", "A", "A", "A", "A", "A", "A", "A", "D", "A")
  res <- exploratory:::predict_value_from_prob(m_m$forest$levels, m_m$predictions, df[["z"]])
  expect_equal(typeof(res), typeof(df$z))
  expect_equal(length(res), length(df$z))
})

test_that("ranger.set_multi_predicted_values", {
  df <- structure(
    list(x = 1:10, y = rep(TRUE, 10), z = c("A", "B", "C", "D", "D", "E", "F", "E", "A", "A")),
    row.names = c(NA, 10L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("x", "y", "z")
  )
  df[1, "x"] <- NA
  m_m <- build_model(df, model_func = rangerMulti, formula = z ~ x + y)$model[[1]]
  predicted_value_nona <- exploratory:::predict_value_from_prob(m_m$forest$levels,
                                                         m_m$predictions,
                                                         df[["z"]])
  na_at <- exploratory:::ranger.find_na(c("x", "y"), df) 
  predicted_value <- exploratory:::restore_na(predicted_value_nona, na_at)
  predicted_prob_nona <- rep(0.5, length(predicted_value_nona)) # Dummy probability just to pass this test.
  predicted_prob <- exploratory:::restore_na(predicted_prob_nona, na_at)
  ret <- exploratory:::ranger.set_multi_predicted_values(df, m_m$predictions, predicted_value, predicted_prob, na_at)
  expected_colnames <-  c("x", "y", "z",
                          "predicted_label",
                          "predicted_probability",
                          "predicted_probability_A", "predicted_probability_D", "predicted_probability_E",
                          "predicted_probability_B", "predicted_probability_C", "predicted_probability_F"
                          )
  expect_equal(colnames(ret), expected_colnames)
})

test_that("in the case of a single target variable single_value ranger with binary classification", {
  test_data["IS_TRUE"] <- rep(TRUE, nrow(test_data))
  model_ret <- suppressWarnings({
      build_model(test_data,
        model_func = rangerBinary,
        formula = `IS_TRUE` ~ DISTANCE,
        test_rate = 0.3)
  })
  test_data[1, "IS_TRUE"] <- FALSE

  pred_train_ret <- suppressWarnings(prediction(model_ret, data = "newdata", data_frame = test_data %>% select(-IS_TRUE)))
  expect_true(is.na(pred_train_ret[1, "predicted_label"]$predicted_label))
 
})

test_that("in the case of a single target variable single_value ranger with multi classification", {
  test_data["CATEGORY"] <- rep("MA", nrow(test_data))
  model_ret <- suppressWarnings({
      build_model(test_data,
        model_func = rangerMulti,
        formula = `CATEGORY` ~ DISTANCE,
        test_rate = 0.3)
  })
  test_data[1, "CATEGORY"] <- "ME"

  pred_train_ret <- suppressWarnings(prediction(model_ret, data = "newdata", data_frame = test_data %>% select(-CATEGORY)))
  expect_equal(pred_train_ret[1, "predicted_label"]$predicted_label, NA_character_)
 
})

test_that("in the case of a unkown target variable of predictiton ranger with multi classification", {
  model_ret <- suppressWarnings({
      build_model(test_data,
        model_func = rangerMulti,
        formula = `CARRIER` ~ .,
        test_rate = 0.1)
  })
  test_data[1, "CARRIER"] <- "UNKOWN_CARRIER"

  pred_train_ret <- suppressWarnings(prediction(model_ret, data = "newdata", data_frame = test_data %>% select(-CARRIER)))
  expect_equal(pred_train_ret[1, "predicted_label"]$predicted_label, NA_character_)

})

test_that("calc imp negative test", { #TODO: What was this case for?
  model_df <- flight %>% slice_sample(n=4000) %>% calc_feature_imp(`ARR DELAY`, `YE AR`, `MON TH`, `DAY OF MONTH`, `FL DATE`, `TAIL NUM`, `FL NUM`, `ORI GIN`, `ORIGIN CITY NAME`, `ORIGIN STATE ABR`, `DE ST`, `DEST CITY NAME`, `DEST STATE ABR`, `DEP TIME`, `DEP DELAY`, `ARR TIME`, `CAN CELLED`, `CANCELLATION CODE`, `AIR TIME`, `DIS TANCE`, `WEATHER DELAY`, `delay ed`, `is UA`, `is delayed`, `end time`, `is UA or AA`, smote = FALSE)
  res_importance <- model_df %>% rf_importance()
  expect_equal(colnames(res_importance), c("variable", "importance"))
  res_partial_dependence <- model_df %>% rf_partial_dependence() %>% rename(`X-Axis`=x_value, `ARR DELAY`=y_value)
  expect_equal(colnames(res_partial_dependence), c("x_name", "X-Axis", "y_name", "ARR DELAY", "chart_type", "x_type"))
  res_evaluation <- model_df %>% rf_evaluation(pretty.name = TRUE)
  expect_equal(colnames(res_evaluation), c("R Squared", "RMSE", "Number of Rows"))
  res_tidy <- model_df %>% tidy_rowwise(model, type = "scatter") %>% rename(Actual=expected_value, Predicted=predicted_value) %>% mutate(`Perfect Fit`=Predicted)
  expect_equal(colnames(res_tidy), c("Actual", "Predicted", "Perfect Fit"))
})

test_that("calc imp - variables for edarf should correspond to variables decided to be Confirmed or Tentative by Boruta", {
  model_df <- flight %>% slice_sample(n=4000) %>% calc_feature_imp(`ARR DELAY`, `YE AR`, `MON TH`, `DAY OF MONTH`, `FL DATE`, `TAIL NUM`, `FL NUM`, `ORI GIN`, `ORIGIN CITY NAME`, `ORIGIN STATE ABR`, `DE ST`, `DEST CITY NAME`, `DEST STATE ABR`, `DEP TIME`, `DEP DELAY`, `ARR TIME`, `CAN CELLED`, `CANCELLATION CODE`, `AIR TIME`, `DIS TANCE`, `WEATHER DELAY`, `delay ed`, `is UA`, `is delayed`, `end time`, `is UA or AA`,
                                                                    smote = FALSE, with_boruta = TRUE)

  res_partial_dependence <- model_df %>% rf_partial_dependence()
  expect_equal(20, n_distinct(res_partial_dependence$x_name))
})
