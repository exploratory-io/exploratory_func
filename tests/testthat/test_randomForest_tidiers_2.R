context("test tidiers for ranger randomForest")

test_that("test ranger with regression", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", NA), # test NA handling
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))

  test_data[["IS_AA"]] <- as.integer(test_data$CARRIER == "AA")
  model_ret <- build_model(test_data,
                           model_func = rangerReg,
                           formula = IS_AA ~ DISTANCE,
                           test_rate = 0.3)
  coef_ret <- model_coef(model_ret, pretty.name = TRUE)
  expect_equal(colnames(coef_ret), c("variable", "importance"))
  stats_ret <- model_stats(model_ret)
  expect_equal(colnames(stats_ret), c("root_mean_square_error", "r_squared"))
  expected_colnames <- c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE",
                         "IS_AA", "predicted_value")
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
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))

  test_data[["IS AA"]] <- test_data$CARRIER == "AA" # test target column name with space
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
                       "DISTANCE", "IS AA", "predicted_probability", "predicted_label")
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
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", NA), # test NA handling
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
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
                         "predicted_probability_MQ", "predicted_probability_EV", "predicted_probability_US",
                         "predicted_probability_9E", "predicted_probability_WN", "predicted_probability", "predicted_label")

  pred_train_ret <- prediction(model_ret, data = "training")
  expect_equal(colnames(pred_train_ret), expected_colnames)

  pred_test_ret <- prediction(model_ret, data = "test")
  expect_equal(colnames(pred_test_ret), expected_colnames)

  pred_test_newdata_ret <- prediction(model_ret, data = "newdata", data_frame = test_data)
  expect_equal(colnames(pred_test_newdata_ret), expected_colnames)
})

