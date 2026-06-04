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

test_that("test xgboost_binary with not clean names", {
  test_data <- data.frame(
    label = rep(c(T,F,F), 100),
    num1 =  rep(c(seq(2),NA), 100) + runif(100), # include NA for test.
    num2 = rep(seq(3), 100) + runif(100)
  )
  colnames(test_data) <- c("Label 1", "Num 1", "Num 2")
  model_ret <- build_model(
    test_data,
    model_func = xgboost_binary,
    formula = `Label 1` ~ `Num 1` + `Num 2`,
    nrounds = 5,
    na.action = na.omit
    )
  prediction_ret <- prediction(model_ret)
  expect_true(class(prediction_ret$predicted_probability) == "numeric")
  expect_true(all(between(prediction_ret$predicted_probability, 0,1), na.rm=TRUE))
})

if (Sys.info()["sysname"] != "Windows") {
  test_that("test xgboost_binary with not clean Japanese names", {
    test_data <- data.frame(
      label = rep(c(T,F,F), 100),
      num1 =  rep(c(seq(2),NA), 100) + runif(100), # include NA for test.
      num2 = rep(seq(3), 100) + runif(100)
    )
    colnames(test_data) <- c("ラベル 1", "数値 1", "数値 2")
    model_ret <- build_model(
      test_data,
      model_func = xgboost_binary,
      formula = `ラベル 1` ~ `数値 1` + `数値 2`,
      nrounds = 5,
      na.action = na.omit
      )
    prediction_ret <- prediction(model_ret)
    expect_true(class(prediction_ret$predicted_probability) == "numeric")
    expect_true(all(between(prediction_ret$predicted_probability, 0,1), na.rm=TRUE))
  })
}

test_that("test xgboost_reg with not clean names", {
  test_data <- data.frame(
    label = rep(seq(3) * 5, 100),
    num1 =  rep(c(seq(2),NA), 100) + runif(100), # include NA for test.
    num2 = rep(seq(3), 100) + runif(100)
  )
  colnames(test_data) <- c("Label 1", "Num 1", "Num 2")
  model_ret <- build_model(
    test_data,
    model_func = xgboost_reg,
    formula = `Label 1` ~ `Num 1` + `Num 2`,
    nrounds = 5,
    na.action = na.omit
    )
  prediction_ret <- prediction(model_ret)
  expect_true(class(prediction_ret$predicted_value) == "numeric")
})

if (Sys.info()["sysname"] != "Windows") {
  test_that("test xgboost_reg with not clean Japanese names", {
    test_data <- data.frame(
      label = rep(seq(3) * 5, 100),
      num1 =  rep(c(seq(2),NA), 100) + runif(100), # include NA for test.
      num2 = rep(seq(3), 100) + runif(100)
    )
    colnames(test_data) <- c("ラベル 1", "数値 1", "数値 2")
    model_ret <- build_model(
      test_data,
      model_func = xgboost_reg,
      formula = `ラベル 1` ~ `数値 1` + `数値 2`,
      nrounds = 5,
      na.action = na.omit
      )
    prediction_ret <- prediction(model_ret)
    expect_true(class(prediction_ret$predicted_value) == "numeric")
  })
}

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

test_that("test xgboost_multi with not clean names and NA", {
  test_data <- data.frame(
    label = rep(seq(3) * 5, 100),
    num1 =  rep(c(seq(2),NA), 100) + runif(100), # include NA for test.
    num2 = rep(seq(3), 100) + runif(100)
  )
  colnames(test_data) <- c("Label 1", "Num 1", "Num 2")
  model_ret <- build_model(
    test_data,
    model_func = xgboost_multi,
    formula = `Label 1` ~ `Num 1` + `Num 2`,
    nrounds = 5,
    na.action = na.omit
    )
  prediction_ret <- prediction(model_ret)
  expect_true(all(prediction_ret$predicted_label %in% c(5, 10, 15)))
})

if (Sys.info()["sysname"] != "Windows") {
  test_that("test xgboost_multi with not clean Japanese names and NA", {
    test_data <- data.frame(
      label = rep(seq(3) * 5, 100),
      num1 =  rep(c(seq(2),NA), 100) + runif(100), # include NA for test.
      num2 = rep(seq(3), 100) + runif(100)
    )
    colnames(test_data) <- c("ラベル 1", "数値 1", "数値 2")
    model_ret <- build_model(
      test_data,
      model_func = xgboost_multi,
      formula = `ラベル 1` ~ `数値 1` + `数値 2`,
      nrounds = 5,
      na.action = na.omit
      )
    prediction_ret <- prediction(model_ret)
    expect_true(all(prediction_ret$predicted_label %in% c(5, 10, 15)))
  })
}

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
  # test_data[["weight"]] <- c(seq(nrow(test_data) -1), NA) # It seems that NA in weight is not allowed anymore.
  # With xgboost 1.3.1.1, following error is retuned.
  # "amalgamation/../src/data/data.cc:365: Check failed: valid: Weights must be positive values."
  test_data[["weight"]] <- seq(nrow(test_data))
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
  expect_true(is.data.frame(prediction_ret))
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
  # xgboost 3.x: "map" eval_metric requires binary labels; use "rmse" for regression.
  model_ret <- build_model(test_data, model_func = xgboost_reg, formula = DISTANCE ~ CANCELLED, nrounds = 5, booster = "gblinear", eval_metric = "rmse")
  coef_ret <- model_coef(model_ret)
  stats_ret <- model_stats(model_ret)
  expect_equal(nrow(stats_ret), 5)
})

test_that("test xgboost_binary with linear booster and single-class target", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))
  # With 1.4.1.1, we are not seeing error from this following call, but the error messages we have seen from the call in the past are...
  # - "Check failed: !auc_error: AUC: the dataset only contains pos or neg samples"
  # - "The target only contains positive or negative values"
  model_ret <- build_model(test_data, model_func = xgboost_binary, formula = CANCELLED ~ DISTANCE, nrounds = 5, booster = "gblinear")
  coef_ret <- model_coef(model_ret)
  stats_ret <- model_stats(model_ret)
  expect_true(!is.null(coef_ret))
  expect_equal(nrow(stats_ret), 5)
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

  # xgboost 3.x auto-computes base_score from label mean (~0.15 for this data),
  # so predicted probabilities are lower than with the old default base_score=0.5.
  # Use threshold=0.2 to ensure both TRUE and FALSE predictions appear.
  prediction_ret <- model_ret %>%
    prediction_binary(data = "newdata", data_frame = test_data, threshold = 0.2)

  expect_true(all(dplyr::between(prediction_ret$predicted_probability,0,1), na.rm=TRUE))
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
    prediction_binary(data = "newdata", data_frame = test_data, threshold = 0.2)
  expect_equal(as.integer(prediction_ret$predicted_label)+1, as.integer(prediction_ret2$predicted_label))
})

# #36153 - XGBoost L1 (alpha) / L2 (lambda) regularization support.
# These params are forwarded through ... from exp_xgboost -> xgboost_*() -> fml_xgboost() ->
# xgboost::xgb.train(), exactly like `objective` (which is read back at model$params$objective).
xgb_l1l2_test_data <- structure(
  list(
    CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
    CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
    DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)),
  row.names = c(NA, -20L),
  class = c("tbl_df", "tbl", "data.frame"))

test_that("exp_xgboost forwards alpha (L1) and lambda (L2) to the booster", {
  set.seed(1)
  # Regression target (numeric) - non-default alpha/lambda must reach xgb.train params.
  model_df <- xgb_l1l2_test_data %>% exp_xgboost(DISTANCE, CARRIER, alpha = 0.5, lambda = 2, nrounds = 5)
  model <- model_df$model[[1]]
  expect_equal(as.numeric(model$params$alpha), 0.5)
  expect_equal(as.numeric(model$params$lambda), 2)

  # Binary target (logical) - same forwarding through the binary branch.
  bin_data <- xgb_l1l2_test_data %>% dplyr::mutate(is_cancelled = CANCELLED == 1)
  model_df_bin <- bin_data %>% exp_xgboost(is_cancelled, CARRIER, alpha = 0.3, lambda = 4, nrounds = 5)
  model_bin <- model_df_bin$model[[1]]
  expect_equal(as.numeric(model_bin$params$alpha), 0.3)
  expect_equal(as.numeric(model_bin$params$lambda), 4)
})

test_that("exp_xgboost uses XGBoost default alpha=0 / lambda=1 when not specified", {
  set.seed(1)
  model_df <- xgb_l1l2_test_data %>% exp_xgboost(DISTANCE, CARRIER, nrounds = 5)
  model <- model_df$model[[1]]
  expect_equal(as.numeric(model$params$alpha), 0)
  expect_equal(as.numeric(model$params$lambda), 1)
})

test_that("exp_xgboost with alpha and lambda produces valid prediction outputs", {
  set.seed(1)
  n <- 100
  # Binary: predicted probabilities must be in [0, 1]
  bin_data <- data.frame(
    target = rep(c(TRUE, FALSE), n / 2),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  model_df <- bin_data %>% exp_xgboost(target, x1, x2, alpha = 1, lambda = 2, nrounds = 5)
  probs <- model_df$model[[1]]$prediction_training
  expect_true(is.numeric(probs))
  expect_true(all(dplyr::between(probs, 0, 1), na.rm = TRUE))

  # Regression: predicted values must be finite numerics
  reg_data <- data.frame(y = rnorm(n), x1 = rnorm(n), x2 = rnorm(n))
  model_df_reg <- reg_data %>% exp_xgboost(y, x1, x2, alpha = 0.5, lambda = 3, nrounds = 5)
  preds <- model_df_reg$model[[1]]$prediction_training
  expect_true(is.numeric(preds))
  expect_true(all(is.finite(preds)))

  # Multiclass: probability matrix with values in [0, 1]
  multi_data <- data.frame(
    label = rep(c("A", "B", "C"), length.out = n),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  model_df_multi <- multi_data %>% exp_xgboost(label, x1, x2, alpha = 0.5, lambda = 2, nrounds = 5)
  prob_mat <- model_df_multi$model[[1]]$prediction_training
  expect_true(is.matrix(prob_mat))
  expect_true(all(dplyr::between(as.numeric(prob_mat), 0, 1), na.rm = TRUE))
})

test_that("exp_xgboost handles highly imbalanced binary target", {
  set.seed(1)
  n <- 100
  test_data <- data.frame(
    target = c(rep(TRUE, 5), rep(FALSE, 95)),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  model_df <- test_data %>% exp_xgboost(target, x1, x2, nrounds = 5)
  model <- model_df$model[[1]]
  expect_false("error" %in% class(model))
  expect_true(is.numeric(model$prediction_training))
  expect_true(all(dplyr::between(model$prediction_training, 0, 1), na.rm = TRUE))
})

test_that("exp_xgboost handles NA values in predictors", {
  set.seed(1)
  n <- 100
  x1 <- rnorm(n)
  x1[sample(n, 10)] <- NA
  test_data <- data.frame(
    target = rep(c(TRUE, FALSE), n / 2),
    x1 = x1,
    x2 = rnorm(n)
  )
  model_df <- test_data %>% exp_xgboost(target, x1, x2, nrounds = 5)
  model <- model_df$model[[1]]
  expect_false("error" %in% class(model))
  expect_true(is.numeric(model$prediction_training))
  expect_true(all(dplyr::between(model$prediction_training, 0, 1), na.rm = TRUE))
})

if (Sys.info()["sysname"] != "Windows") {
  test_that("exp_xgboost handles column names with Japanese and many special characters", {
    set.seed(1)
    n <- 100
    test_data <- data.frame(
      col1 = rep(c(TRUE, FALSE), n / 2),
      col2 = rep(c("DL", "MQ", "AA", "WN"), length.out = n),
      col3 = rnorm(n),
      stringsAsFactors = FALSE
    )
    colnames(test_data) <- c(
      "遅れ た !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表",
      "航空 会社 !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表",
      "飛行 時間 !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表"
    )
    model_df <- test_data %>%
      exp_xgboost(
        `遅れ た !"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表`,
        `航空 会社 !"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表`,
        `飛行 時間 !"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表`,
        nrounds = 5
      )
    model <- model_df$model[[1]]
    expect_false("error" %in% class(model))
    expect_true(is.numeric(model$prediction_training))
    expect_true(all(dplyr::between(model$prediction_training, 0, 1), na.rm = TRUE))
  })
}

test_that("exp_xgboost handles column names with ASCII special characters", {
  set.seed(1)
  n <- 100
  test_data <- data.frame(
    col1 = rep(c(TRUE, FALSE), n / 2),
    col2 = rep(c("DL", "MQ", "AA", "WN"), length.out = n),
    col3 = rnorm(n),
    stringsAsFactors = FALSE
  )
  colnames(test_data) <- c(
    "Is Delayed !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ x",
    "Airline !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ x",
    "Flight Time !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ x"
  )
  model_df <- test_data %>%
    exp_xgboost(
      `Is Delayed !"#$%&'()*+, -./:;<=>?@[]^_'{|}~ x`,
      `Airline !"#$%&'()*+, -./:;<=>?@[]^_'{|}~ x`,
      `Flight Time !"#$%&'()*+, -./:;<=>?@[]^_'{|}~ x`,
      nrounds = 5
    )
  model <- model_df$model[[1]]
  expect_false("error" %in% class(model))
  expect_true(is.numeric(model$prediction_training))
  expect_true(all(dplyr::between(model$prediction_training, 0, 1), na.rm = TRUE))
})

test_that("exp_xgboost works with grouped data (repeat-by)", {
  set.seed(1)
  test_data <- dplyr::tibble(
    group = rep(c("A", "B"), each = 50),
    target = c(rep(c(TRUE, FALSE), 25), rep(c(FALSE, TRUE), 25)),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  model_df <- test_data %>%
    dplyr::group_by(group) %>%
    exp_xgboost(target, x1, x2, nrounds = 5)
  expect_equal(nrow(model_df), 2)
  expect_true(all(sapply(model_df$model, function(m) !("error" %in% class(m)))))
})
