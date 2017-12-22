context("test build_xgboost")

test_that("test build_xgboost with na.omit", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", NA, "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))
  test_data[["w"]] <- c(seq(nrow(test_data)-1), NA)
  test_data[["isaa"]] <- test_data$CARRIER == "AA"

  model_ret <- build_model(test_data, model_func = xgboost_binary, formula = isaa ~ . - w - 1, nrounds = 5, weight = log(w), eval_metric = "auc", na.action = na.omit, sparse = FALSE)
  prediction_ret <- prediction_binary(model_ret)
  expect_true(any(prediction_ret$predicted_label))
  expect_true(any(!prediction_ret$predicted_label))
})

test_that("test build_xgboost prediction with optimized threshold", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", NA, "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))
  test_data[["w"]] <- c(seq(nrow(test_data)-1), NA)
  test_data[["isaa"]] <- test_data$CARRIER == "AA"

  model_ret <- build_model(
    test_data,
    model_func = xgboost_binary,
    formula = isaa ~ . - w - 1,
    nrounds = 5,
    weight = log(w),
    na.action = na.omit,
    sparse = FALSE,
    eval_metric = "auc"
  )

  prediction_ret <- prediction_binary(model_ret)
  expect_true(any(prediction_ret$predicted_label))
  expect_true(any(!prediction_ret$predicted_label))
})

test_that("test build_xgboost prediction with optimized threshold", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", NA, "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))
  test_data[["w"]] <- c(seq(nrow(test_data)-1), NA)
  test_data[["isaa"]] <- test_data$CARRIER == "AA"

  model_ret <- build_model(
    test_data,
    model_func = xgboost_binary,
    formula = isaa ~ . - w - 1,
    nrounds = 5,
    weight = log(w),
    eval_metric = "auc",
    na.action = na.omit,
    sparse = FALSE
  )
  prediction_ret <- prediction_binary(model_ret)
  expect_true(any(prediction_ret$predicted_label))
  expect_true(any(!prediction_ret$predicted_label))
})

test_that("test xgboost_reg with not clean names", {
  test_data <- data.frame(
    label = rep(seq(3) * 5, 100),
    num1 =  rep(seq(3), 100) + runif(100),
    num2 = rep(seq(3), 100) + runif(100)
  )
  colnames(test_data) <- c("label 1", "num-1", "Num 2")
  model_ret <- build_model(
    test_data,
    model_func = xgboost_reg,
    formula = `label 1` ~ .,
    nrounds = 5,
    na.action = na.omit
    )
  prediction_ret <- prediction(model_ret)
  expect_true(all(prediction_ret$predicted_label %in% c(5, 10, 15)))
})

test_that("test xgboost_reg with add_prediction", {
  train_data <- data.frame(
    label = rep(seq(3) * 5, 100),
    num1 =  rep(seq(3), 100) + runif(100),
    num2 = rep(seq(3), 100) + runif(100)
  )

  test_data <- data.frame(
    label = rep(seq(3) * 5, 100),
    num1 =  rep(seq(3), 100) + runif(100),
    num2 = rep(seq(3), 100) + runif(100)
  )
  colnames(train_data) <- c("label 1", "num-1", "Num 2")
  colnames(test_data) <- colnames(train_data)
  model_ret <- build_model(train_data, model_func = xgboost_reg, formula = `label 1` ~ ., nrounds = 5)
  prediction_ret <- add_prediction(test_data, model_df = model_ret)
  expect_true(all(prediction_ret$predicted_label %in% c(5, 10, 15)))
})

test_that("test xgboost_multi with numeric target", {
  test_data <- data.frame(
    label = rep(seq(3) * 5, 100),
    num1 =  rep(seq(3), 100) + runif(100),
    num2 = rep(seq(3), 100) + runif(100)
  )
  model_ret <- build_model(test_data, model_func = xgboost_multi, formula = label ~ ., nrounds = 5)
  prediction_ret <- prediction(model_ret)
  expect_true(all(prediction_ret$predicted_label %in% c(5, 10, 15)))
})

test_that("test build_xgboost", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))

  test_data[["weight"]] <- seq(nrow(test_data))
  test_data <- test_data %>% rename(`CAN CELLED`=CANCELLED, `DIS TANCE`=DISTANCE)
  model_ret <- build_model(
    test_data,
    model_func = xgboost_binary,
    formula = `CAN CELLED` ~ `DIS TANCE`,
    nrounds = 5,
    eval_metric = "auc",
    verbose = 0
    )
  coef_ret <- model_coef(model_ret)
  expect_equal(ncol(model_ret), 4)
})

test_that("test build_xgboost with weight", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))

  test_data[["weight"]] <- seq(nrow(test_data))
  model_ret <- build_model(
    test_data,
    model_func = xgboost_binary,
    formula = CANCELLED ~ DISTANCE,
    nrounds = 5,
    output_type = "logistic",
    eval_metric = "auc")
  coef_ret <- model_coef(model_ret)
  expect_equal(ncol(model_ret), 4)
})

test_that("test build_xgboost with weight", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))
  test_data[["weight"]] <- seq(nrow(test_data))
  test_data[["IS_AA"]] <- test_data$CARRIER == "AA"
  model_ret <- build_model(test_data, model_func = xgboost_binary, formula = IS_AA ~ DISTANCE, nrounds = 5, booster = "dart", eval_metric = "auc", output_type = "logitraw", weight = log(weight))
  coef_ret <- model_coef(model_ret)
  stats_ret <- model_stats(model_ret)
  prediction_ret <- prediction_binary(model_ret)
  expect_true(is.logical(prediction_ret$predicted_label))
  expect_equal(ncol(model_ret), 4)
})

test_that("test build_xgboost reg", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))
  test_data[["weight"]] <- c(seq(nrow(test_data) -1), NA)
  test_data[["IS_AA"]] <- test_data$CARRIER == "AA"
  model_ret <- build_model(
    test_data,
    model_func = xgboost_reg,
    formula = IS_AA ~ DISTANCE,
    nrounds = 5,
    weight = log(weight),
    verbose = 1,
    booster = "dart",
    sparse = FALSE
  )
  stats_ret <- model_stats(model_ret)
  expect_equal(ncol(model_ret), 4)
})

test_that("test build_xgboost with dot", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))
  test_data[["weight"]] <- c(seq(nrow(test_data)-1), NA)
  test_data[["IS_AA"]] <- test_data$CARRIER == "AA"
  model_ret <- build_model(
    test_data,
    model_func = xgboost_binary,
    formula = IS_AA ~ .,
    nrounds = 5,
    eval_metric = "auc",
    sparse = FALSE
    )
  prediction_ret <- prediction_binary(model_ret)
  expect_equal(ncol(prediction_ret), ncol(test_data) + 2)
})

test_that("test build_xgboost with multi char", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))
  test_data[["weight"]] <- c(seq(nrow(test_data)-1), NA)
  test_data[["IS_AA"]] <- test_data$CARRIER == "AA"
  model_ret <- build_model(
    test_data,
    model_func = xgboost_multi,
    formula = CARRIER ~ .,
    nrounds = 5,
    output_type = "softmax",
    verbose = 0,
    sparse = FALSE
    )
  prediction_ret <- prediction(model_ret)
  # TODO: This returns factor by now because of build_model behaviour but should return character
  # expect_true(is.character(prediction_ret$predicted_value))
})

test_that("test build_xgboost with multi", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))
  test_data[["weight"]] <- c(seq(nrow(test_data)-1), NA)
  test_data[["IS_AA"]] <- test_data$CARRIER == "AA"
  test_data[["CARRIER"]] <- as.factor(test_data[["CARRIER"]])
  model_ret <- build_model(
    test_data,
    model_func = xgboost_multi,
    formula = CARRIER ~ .,
    nrounds = 5,
    verbose = 0,
    sparse = FALSE
    )
  prediction_ret <- prediction(model_ret)
  # expect_true(is.factor(prediction_ret$predicted_value))
  expect_true(!any(is.na(prediction_ret$predicted_value)))
})

test_that("test build_xgboost with multi softprob", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))
  test_data[["weight"]] <- c(seq(nrow(test_data)-1), NA)
  test_data[["IS_AA"]] <- test_data$CARRIER == "AA"
  test_data[["CARRIER"]] <- as.factor(test_data[["CARRIER"]])
  model_ret <- build_model(
    test_data,
    model_func = xgboost_multi,
    formula = CARRIER ~ .,
    nrounds = 5,
    verbose = 0,
    sparse = FALSE
    )
  prediction_ret <- prediction(model_ret)
  prob <- prediction_ret$predicted_probability
  expect_true(all(prob[!is.na(prob)] > 0))
  expect_true(length(unique(prediction_ret$predicted_label)) > 1)
})

test_that("test build_xgboost with linear booster", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))
  test_data[["weight"]] <- c(seq(nrow(test_data)-1), NA)
  test_data[["IS_AA"]] <- test_data$CARRIER == "AA"
  model_ret <- build_model(test_data, model_func = xgboost_reg, formula = DISTANCE ~ CANCELLED, nrounds = 5, booster = "gblinear", eval_metric = "map")
  coef_ret <- model_coef(model_ret)
  stats_ret <- model_stats(model_ret)
  expect_equal(nrow(stats_ret), 5)
})

test_that("test build_xgboost with linear booster", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))
  expect_error({
    model_ret <- build_model(test_data, model_func = xgboost_binary, formula = CANCELLED ~ DISTANCE, nrounds = 5, booster = "gblinear")
  }, "The target only contains positive or negative values")
})

test_that("test build_xgboost prediction with optimized threshold", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))
  test_data[["weight"]] <- c(seq(nrow(test_data)-1), NA)
  test_data[["IS_AA"]] <- test_data$CARRIER == "AA"
  model_ret <- build_model(
    test_data,
    model_func = xgboost_binary,
    formula = IS_AA ~ .,
    nrounds = 5,
    eval_metric = "auc",
    sparse = FALSE
  )
  prediction_ret <- prediction_binary(model_ret, threshold = "f_score")
  expect_true(any(prediction_ret$predicted_label))
  expect_true(any(!prediction_ret$predicted_label))
})

test_that("new data prediction without response column", {
  train_data <- structure(list(age = c(66L, 44L, 21L, 78L, 28L, 40L, 61L, 60L,
                                       43L, 49L, 52L, 25L, 58L, 46L, 40L, 32L, 22L, 23L, 17L, 24L),
                               workclass = c("Local-gov", "Private", "Private", NA, "Private",
                                             "State-gov", "Private", "Private", "Local-gov", "Private",
                                             "Local-gov", "Private", "Private", "Self-emp-inc", "Private",
                                             "Self-emp-not-inc", "Private", "Private", "Private", "Private"),
                               education = c("7th-8th", "Masters", "Some-college", "Bachelors",
                                             "7th-8th", "Some-college", "HS-grad", "Some-college", "Some-college",
                                             "HS-grad", "Prof-school", "Masters", "HS-grad", "Some-college",
                                             "HS-grad", "Bachelors", "HS-grad", "Prof-school", "11th",
                                             "Some-college"),
                               `education-num` = c("4", "14", NA, "13",
                                                   "4", NA, "9", NA, NA, "9", NA, "14", "9", NA, "9", "13",
                                                   "9", NA, "7", NA),
                               `marital-status` = c("Widowed", "Divorced",
                                                    "Never-married", "Husband", "Divorced", "Never-married",
                                                    "Married-civ-spouse", "Widowed", "Married-civ-spouse", "Married-civ-spouse",
                                                    "Husband", "Never-married", "Married-civ-spouse",
                                                    "Married-civ-spouse", "Husband", "Divorced", "Husband",
                                                    "Never-married", "Husband", "Married-civ-spouse"),
                               occupation = c("Other-service", "Exec-managerial", "Sales",
                                              NA, "Other-service", "Adm-clerical", "Other-service", "Sales",
                                              "Transport-moving", "Adm-clerical", "Protective-serv", "Prof-specialty",
                                              "Craft-repair", "Exec-managerial", "Machine-op-inspct", "Exec-managerial",
                                              "Other-service", "Farming-fishing", "Other-service", "Craft-repair"),
                               relationship = c("Not-in-family", "Husband", "Not-in-family",
                                                "Unmarried", "Unmarried", "Not-in-family", "Husband", "Unmarried",
                                                "Unmarried", "Husband", "Husband", "Wife", "Not-in-family",
                                                "Husband", "Husband", "Unmarried", "Own-child", "Not-in-family",
                                                "Own-child", "Not-in-family"),
                               race = c("White", "White", "White",
                                        "Black", "White", "White", "Black", "White", "White", "White",
                                        "White", "White", "White", "White", "White", "Black", "White",
                                        "White", "White", "White"),
                               sex = c("Female", "Male", "Male",
                                       "Male", "Female", "Female", "Male", "Female", "Male", "Female",
                                       "Male", "Male", "Male", "Male", "Male", "Female", "Female",
                                       "Female", "Male", "Male"),
                               `capital-gain` = c(0, 10520, 0, 0, 0, 0, 0, 0, 0, 0, 3137, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                               `capital-loss` = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                               `hours-per-week` = c(20, 45, 40, 3, 50, 38, 40, 27, 17, 30, 42, 30, 40, 40, 40, 30, 35, 50, 12, 65),
                               `native-country` = c("United-States", "United-States", "United-States", "United-States", "United-States",
                                                    "United-States", "Mexico", "United-States", "United-States", "Mexico",
                                                    "United-States", "Mexico", "United-States", "United-States", "United-States",
                                                    "United-States", "United-States", "United-States", "United-States", "United-States"),
                               is_greater_than_50k = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                                                       FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  ),
  .Names = c("age", "workclass", "education", "education-num", "marital-status", "occupation", "relationship",
             "race", "sex", "capital-gain", "capital-loss", "hours-per-week", "native-country", "is_greater_than_50k"),
  row.names = c(NA, -20L), class = c("tbl_df", "tbl", "data.frame"))

  test_data <- structure(list(age = c(29L, 42L, 41L, 28L, 26L, 40L, 25L, 19L,
                                      40L, 29L),
                              workclass = c("Private", "Private", "Private", "Private",
                                            "Private", "Local-gov", "Private", "Private", "Private", "Private"
                              ),
                              education = c("HS-grad", "Bachelors", "HS-grad", "HS-grad",
                                            "Some-college", "HS-grad", "HS-grad", "HS-grad", "Prof-school",
                                            "HS-grad"),
                              `education-num` = c("9", "13", "9", "9", NA, "9", "9", "9", "15", "9"),
                              `marital-status` = c("Divorced", "Married-civ-spouse", "Married-civ-spouse",
                                                   "Husband", "Never-married", "Never-married",
                                                   "Never-married", "Never-married", "Married-civ-spouse",
                                                   "Married-civ-spouse"),
                              occupation = c("Adm-clerical", "Tech-support", "Machine-op-inspct", "Adm-clerical",
                                             "Exec-managerial", "Adm-clerical", "Adm-clerical", "Other-service",
                                             "Craft-repair", "Craft-repair"
                              ),
                              relationship = c("Unmarried", "Husband", "Husband", "Husband", "Not-in-family",
                                               "Own-child", "Not-in-family", "Own-child", "Husband", "Husband"
                              ),
                              race = c("White", "White", "White", "White", "White", "White", "Black", "White",
                                       "White", "White"
                              ),
                              sex = c("Female", "Male", "Male", "Male", "Male", "Female", "Male", "Female",
                                      "Male", "Male"),
                              `capital-gain` = c(0, 0, 0, 0, 0, 0, 0, 0, 5178, 0),
                              `capital-loss` = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                              `hours-per-week` = c(50, 45, 40, 40, 40, 40, 40, 40, 40, 60),
                              `native-country` = c("United-States", "United-States", "United-States", "United-States",
                                                   "United-States", "United-States", "United-States", "United-States",
                                                   "United-States", "United-States"),
                              is_greater_than_50k = c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)),
                         .Names = c("age", "workclass", "education", "education-num", "marital-status", "occupation", "relationship",
                                    "race", "sex", "capital-gain", "capital-loss", "hours-per-week", "native-country", "is_greater_than_50k"
                         ),
                         row.names = c(NA, -10L), class = c("tbl_df", "tbl", "data.frame"))

  # remove age (response variable) from new data
  test_data <- test_data %>%
    dplyr::mutate(`education-num` = as.numeric(`education-num`)) %>%
    dplyr::select(-is_greater_than_50k)

  model_ret <- train_data %>%
    dplyr::mutate(`education-num` = as.numeric(`education-num`)) %>%
    dplyr::select(age, is_greater_than_50k, `hours-per-week`, `capital-loss`, `capital-gain`, relationship, `education-num`) %>%
    build_model(
      model_func = xgboost_binary,
      formula = is_greater_than_50k ~ .,
      nrounds = 5,
      sparse = FALSE
    )

  prediction_ret <- model_ret %>%
    prediction_binary(data = "newdata", data_frame = test_data, threshold = 0.4)

  expect_true(all(dplyr::between(prediction_ret$predicted_probability,0,1)))
  expect_true(any(prediction_ret$predicted_label) && any(!prediction_ret$predicted_label))

  # there should be an error because no actual column
  # but this tries to optimize threshold,
  # which needs actual column
  expect_error(
    prediction_ret <- model_ret %>%
      prediction_binary(data = "newdata", data_frame = test_data, threshold = "f_score")
  )

  # test factor response column
  train_data2 <- train_data %>%
    mutate(is_greater_than_50k = as.factor(c("no", "yes")[as.integer(is_greater_than_50k)+1]))

  model_ret2 <- train_data2 %>%
    dplyr::mutate(`education-num` = as.numeric(`education-num`)) %>%
    dplyr::select(age, is_greater_than_50k, `hours-per-week`, `capital-loss`, `capital-gain`, relationship, `education-num`) %>%
    build_model(
      model_func = xgboost_binary,
      formula = is_greater_than_50k ~ .,
      nrounds = 5,
      sparse = FALSE
    )

  prediction_ret2 <- model_ret2 %>%
    prediction_binary(data = "newdata", data_frame = test_data, threshold = 0.4)
  expect_equal(as.integer(prediction_ret$predicted_label)+1, as.integer(prediction_ret2$predicted_label))
})
