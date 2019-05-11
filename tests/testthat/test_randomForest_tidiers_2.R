context("test tidiers for ranger randomForest")

test_that("test ranger with regression", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", NA), # test NA handling
      DISTANCE = c(NA, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, NA, 361, 507, 1020, 1092, 342, NA, 1184, 545),
      FNUMBER= c(21, NA, 6, 87, 23, 12, 3, 0, 13, 1, 85, 82, 31, 57, 20, 12, 42, 49, NA, 45)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE", "FNUMBER"))

  model_ret <- build_model(test_data,
                           model_func = rangerReg,
                           formula = FNUMBER ~ DISTANCE,
                           test_rate = 0.3)
  coef_ret <- model_coef(model_ret, pretty.name = TRUE)
  expect_equal(colnames(coef_ret), c("variable", "importance"))
  stats_ret <- model_stats(model_ret)
  expect_equal(colnames(stats_ret), c("root_mean_square_error", "r_squared"))
  expected_colnames <- c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE",
                         "FNUMBER", "predicted_value")
  pred_train_ret <- prediction(model_ret, data = "training")
  expect_equal(colnames(pred_train_ret), expected_colnames)

  pred_test_ret <- prediction(model_ret, data = "test")
  expect_equal(colnames(pred_test_ret), expected_colnames)

  pred_test_newdata_ret <- prediction(model_ret, data = "newdata", data_frame = test_data)
  expect_equal(colnames(pred_test_newdata_ret), expected_colnames)
})

test_that("test ranger with binary classification", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", NA), # test NA handling
      DISTANCE = c(1587, 173, NA, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))

  test_data[["IS AA"]] <- test_data$CARRIER == "AA" # test target column name with space
  test_data[1, "IS AA"] <- NA
  test_data[["IS AA"]] 
  model_ret <- build_model(test_data,
                           model_func = rangerBinary,
                           formula = `IS AA` ~ DISTANCE,
                           test_rate = 0.3)
  coef_ret <- model_coef(model_ret)
  expect_equal(colnames(coef_ret), c("variable", "importance"))

  model_stats <- model_stats(model_ret, pretty.name = TRUE)
  expect_colnames <- c("F Score", "Precision", "Misclassification Rate",
                       "Recall", "Accuracy")
  expect_equal(colnames(model_stats), expect_colnames)

  pred_train_ret <- prediction_binary(model_ret, data = "training", threshold = "f_score") # test f_score which had issue with target column name with space once.
  expect_colnames <- c("CANCELLED", "Carrier Name", "CARRIER",
                       "DISTANCE", "IS AA", "predicted_label", "predicted_probability")
  expect_equal(colnames(pred_train_ret), expect_colnames)

  pred_test_ret <- prediction_binary(model_ret, data = "test")
  expect_equal(colnames(pred_test_ret), expect_colnames)

  pred_test_newdata_ret <- prediction_binary(model_ret, data = "newdata", data_frame = test_data)
  expect_equal(colnames(pred_test_newdata_ret), expect_colnames)
})

test_that("test ranger with multinomial classification", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", NA, "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", NA), # test NA handling
      DISTANCE = c(NA, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))

  test_data[["IS_AA"]] <- as.factor(test_data$CARRIER == "AA")
  model_ret <- build_model(test_data,
                           model_func = rangerMulti,
                           formula = CARRIER ~ DISTANCE,
                           test_rate = 0.3)
  coef_ret <- model_coef(model_ret)
  expect_equal(colnames(coef_ret), c("variable", "importance"))
  model_stats <- model_stats(model_ret, pretty.name = TRUE)
  expect_colnames <- c("Label", "F Score", "Precision", "Misclassification Rate",
                       "Recall", "Accuracy")
  expect_equal(colnames(model_stats), expect_colnames)

  expected_colnames <- c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE",
                         "IS_AA", "predicted_probability_DL", "predicted_probability_AA",
                         "predicted_probability_EV", "predicted_probability_MQ", "predicted_probability_US",
                         "predicted_probability_9E", "predicted_probability_WN", "predicted_probability", "predicted_label")

  pred_train_ret <- prediction(model_ret, data = "training")
  expect_equal(colnames(pred_train_ret), expected_colnames)

  pred_test_ret <- prediction(model_ret, data = "test")
  expect_equal(colnames(pred_test_ret), expected_colnames)

  pred_test_newdata_ret <- prediction(model_ret, data = "newdata", data_frame = test_data)
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
  expect_equal(
    ranger.predict_value_from_prob(m_m$forest$levels, m_m$predictions, df[["z"]]),
    expected_values
  )
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

