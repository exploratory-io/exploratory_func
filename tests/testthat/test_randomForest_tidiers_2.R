context("test tidiers for ranger randomForest")
if (!exists("flight")) {
  # To skip repeated data loading, run the following outside of the context of the test,
  # so that it stays even after the test.
  flight <- exploratory::read_delim_file("https://www.dropbox.com/s/f47baw5f3v0xoll/airline_2013_10_tricky_v3.csv?dl=1", ",", quote = "\"", skip = 0 , col_names = TRUE , na = c("","NA") , locale=readr::locale(encoding = "UTF-8", decimal_mark = "."), trim_ws = FALSE , progress = FALSE) %>% exploratory::clean_data_frame()
  flight <- flight %>% sample_n(5000)
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
  expect_equal(colnames(stats_ret), c("root_mean_square_error", "r_squared"))
  expected_colnames <- c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE",
                         "FNUMBER", "predicted_value")
  pred_train_ret <- suppressWarnings(prediction(model_ret, data = "training"))
  expect_equal(colnames(pred_train_ret), expected_colnames)

  pred_test_ret <- prediction(model_ret, data = "test")
  expect_equal(colnames(pred_test_ret), expected_colnames)

  pred_test_newdata_ret <- prediction(model_ret, data = "newdata", data_frame = test_data)
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
  expect_equal(colnames(stats_ret), c("root_mean_square_error", "r_squared"))
  expected_colnames <- c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE",
                         "FNUMBER", "predicted_value")
  pred_train_ret <- suppressWarnings(prediction(model_ret, data = "training"))
  expect_equal(colnames(pred_train_ret), expected_colnames)

  pred_test_newdata_ret <- prediction(model_ret, data = "newdata", data_frame = test_data)
  expect_equal(colnames(pred_test_newdata_ret), expected_colnames)
})

test_that("test ranger with binary classification", {
  test_data[["IS AA"]] <- test_data$CARRIER == "AA" # test target column name with space
  test_data[1, "IS AA"] <- NA
  test_data[["IS AA"]]
  model_ret <- suppressWarnings({
      build_model(test_data,
        model_func = rangerBinary,
        formula = `IS AA` ~ DISTANCE,
        test_rate = 0.3)
  })
  coef_ret <- model_coef(model_ret)
  expect_equal(colnames(coef_ret), c("variable", "importance"))

  model_stats <- suppressWarnings(model_stats(model_ret, pretty.name = TRUE))
  expect_colnames <- c("F Score", "Accuracy Rate", "Misclassification Rate",
                       "Precision", "Recall")
  expect_equal(colnames(model_stats), expect_colnames)

  pred_train_ret <- suppressWarnings(
    prediction_binary(model_ret, data = "training", threshold = "f_score")
  )
  expect_colnames <- c("CANCELLED", "Carrier Name", "CARRIER",
                       "DISTANCE", "FNUMBER", "IS AA", "predicted_probability", "predicted_label")
  expect_equal(colnames(pred_train_ret), expect_colnames)

  pred_train_ret2 <- suppressWarnings(
    prediction(model_ret, data = "training", threshold = "f_score")
  )
  expect_equal(colnames(pred_train_ret2), expect_colnames)

  pred_test_ret <- suppressWarnings(prediction_binary(model_ret, data = "test"))
  expect_equal(colnames(pred_test_ret), expect_colnames)

  pred_test_ret2 <- suppressWarnings(prediction(model_ret, data = "test"))
  expect_equal(colnames(pred_test_ret2), expect_colnames)

  pred_test_newdata_ret <- suppressWarnings(prediction_binary(model_ret, data = "newdata", data_frame = test_data))
  expect_equal(colnames(pred_test_newdata_ret), expect_colnames)

  pred_test_newdata_ret2 <- suppressWarnings(prediction_binary(model_ret, data = "newdata", data_frame = test_data))
  expect_equal(colnames(pred_test_newdata_ret2), expect_colnames)
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
  expect_colnames <- c("F Score", "Accuracy Rate", "Misclassification Rate",
                       "Precision", "Recall")
  expect_equal(colnames(model_stats), expect_colnames)

  pred_train_ret <- suppressWarnings(
    prediction_binary(model_ret, data = "training", threshold = "f_score")
  )
  expect_colnames <- c("CANCELLED", "Carrier Name", "CARRIER",
                       "DISTANCE", "FNUMBER", "IS AA", "predicted_probability", "predicted_label")
  expect_equal(colnames(pred_train_ret), expect_colnames)

  pred_train_ret2 <- suppressWarnings(
    prediction(model_ret, data = "training", threshold = "f_score")
  )
  expect_equal(colnames(pred_train_ret2), expect_colnames)

  pred_test_ret <- suppressWarnings(prediction_binary(model_ret, data = "test"))
  expect_equal(colnames(pred_test_ret), expect_colnames)
  pred_test_ret2 <- suppressWarnings(prediction(model_ret, data = "test"))
  expect_equal(colnames(pred_test_ret2), expect_colnames)

  pred_test_newdata_ret <- suppressWarnings(prediction_binary(model_ret, data = "newdata", data_frame = test_data))
  expect_equal(colnames(pred_test_newdata_ret), expect_colnames)

  pred_test_newdata_ret2 <- suppressWarnings(prediction(model_ret, data = "newdata", data_frame = test_data))
  expect_equal(colnames(pred_test_newdata_ret2), expect_colnames)
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
  expect_colnames <- c("Class", "F Score", "Accuracy Rate", "Misclassification Rate",
                       "Precision", "Recall", "Data Size")
  expect_equal(colnames(model_stats), expect_colnames)

  expected_colnames <- c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE", "FNUMBER",
                         "IS_AA", "predicted_probability_DL", "predicted_probability_AA",
                         "predicted_probability_MQ", "predicted_probability_EV", "predicted_probability_US",
                         "predicted_probability_9E", "predicted_probability", "predicted_label")

  pred_train_ret <- suppressWarnings(prediction(model_ret, data = "training"))
  expect_equal(colnames(pred_train_ret), expected_colnames)

  pred_test_ret <- suppressWarnings(prediction(model_ret, data = "test"))
  expect_equal(colnames(pred_test_ret), expected_colnames)

  pred_test_newdata_ret <- suppressWarnings(prediction(model_ret, data = "newdata", data_frame = test_data))
  expect_equal(colnames(pred_test_newdata_ret), expected_colnames)
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
  expect_colnames <- c("Class", "F Score", "Accuracy Rate", "Misclassification Rate",
                       "Precision", "Recall", "Data Size")
  expect_equal(colnames(model_stats), expect_colnames)

  expected_colnames <- c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE", "FNUMBER",
                         "IS_AA", "predicted_probability_DL", "predicted_probability_AA",
                         "predicted_probability_MQ", "predicted_probability_EV", "predicted_probability_US",
                         "predicted_probability", "predicted_label")

  pred_train_ret <- suppressWarnings(prediction(model_ret, data = "training"))
  expect_equal(colnames(pred_train_ret), expected_colnames)

  pred_test_ret <- suppressWarnings(prediction(model_ret, data = "test"))
  expect_equal(colnames(pred_test_ret), expected_colnames)

  pred_test_newdata_ret <- suppressWarnings(prediction(model_ret, data = "newdata", data_frame = test_data))
  expect_equal(colnames(pred_test_newdata_ret), expected_colnames)
})


test_that("ranger.find_na", {
  df <- structure(
    list(x = 1:10, y = rep(TRUE, 10)),
    row.names = c(NA, 10L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("x", "y")
  )
  expected_na_at <- 1
  df[expected_na_at, 1] <- NA
  expect_equal(ranger.find_na("x", df), expected_na_at)

  expected_na_at <- c(1, 2, 5, 10)
  df[expected_na_at, 1] <- NA
  expect_equal(ranger.find_na("x", df), expected_na_at)
})


test_that("ranger.predict_value_from_prob", {
  df <- structure(
    list(x = 1:10, y = rep(TRUE, 10), z = c("A", "B", "C", "D", "D", "E", "F", "E", "A", "A")),
    row.names = c(NA, 10L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("x", "y", "z")
  )
  # reangerBinary
  m_b <- build_model(df, model_func = rangerBinary, formula = y ~ x)$model[[1]]
  expected_values <- rep(TRUE, 10)
  expect_equal(
    ranger.predict_value_from_prob(m_b$forest$levels, m_b$predictions, df[["y"]]),
    expected_values
  )

  # rangerMulti
  m_m <- build_model(df, model_func = rangerMulti, formula = z ~ x + y)$model[[1]]

  expected_values <- c("D", "A", "A", "A", "A", "A", "A", "A", "D", "A")
  res <- ranger.predict_value_from_prob(m_m$forest$levels, m_m$predictions, df[["z"]])
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
  predicted_value_nona <- ranger.predict_value_from_prob(m_m$forest$levels,
                                                         m_m$predictions,
                                                         df[["z"]])
  na_at <- ranger.find_na(c("x", "y"), df) 
  predicted_value <- ranger.add_narow(predicted_value_nona, nrow(df), na_at)
  ret <- ranger.set_multi_predicted_values(df, m_m, predicted_value, na_at)
  expected_colnames <-  c("x", "y", "z",
                          "predicted_probability_A", "predicted_probability_D", "predicted_probability_E",
                          "predicted_probability_B", "predicted_probability_C", "predicted_probability_F", "predicted_value")
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

  pred_train_ret <- suppressWarnings(prediction(model_ret, data = "newdata", data_frame = test_data))
  expect_equal(pred_train_ret[1, "predicted_label"]$predicted_label, NA)
 
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

  pred_train_ret <- suppressWarnings(prediction(model_ret, data = "newdata", data_frame = test_data))
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

  pred_train_ret <- suppressWarnings(prediction(model_ret, data = "newdata", data_frame = test_data))
  expect_equal(pred_train_ret[1, "predicted_label"]$predicted_label, NA_character_)
 
})

test_that("calc imp negative test", {
  model_df <- flight %>% dplyr::sample_n(4000) %>% calc_feature_imp(`ARR DELAY`, `YE AR`, `MON TH`, `DAY OF MONTH`, `FL DATE`, `TAIL NUM`, `FL NUM`, `ORI GIN`, `ORIGIN CITY NAME`, `ORIGIN STATE ABR`, `DE ST`, `DEST CITY NAME`, `DEST STATE ABR`, `DEP TIME`, `DEP DELAY`, `ARR TIME`, `CAN CELLED`, `CANCELLATION CODE`, `AIR TIME`, `DIS TANCE`, `WEATHER DELAY`, `delay ed`, `is UA`, `is delayed`, `end time`, `is UA or AA`, smote = FALSE)
  res_importance <- model_df %>% rf_importance()
  expect_equal(colnames(res_importance), c("variable", "importance"))
  res_partial_dependence <- model_df %>% rf_partial_dependence() %>% rename(`X-Axis`=x_value, `ARR DELAY`=y_value)
  expect_equal(colnames(res_partial_dependence), c("x_name", "X-Axis", "y_name", "ARR DELAY", "chart_type"))
  res_evaluation <- model_df %>% rf_evaluation(pretty.name = TRUE)
  expect_equal(colnames(res_evaluation), c("Root Mean Square Error", "R Squared"))
  res_tidy <- model_df %>% tidy(model, type = "scatter") %>% rename(Actual=expected_value, Predicted=predicted_value) %>% mutate(`Perfect Fit`=Predicted)
  expect_equal(colnames(res_tidy), c("Actual", "Predicted", "Perfect Fit"))
})
