context("test build_lightgbm")

testthat::skip_if_not_installed("lightgbm")

test_that("test build_lightgbm with na.omit", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", NA, "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))
  test_data[["w"]] <- c(seq(nrow(test_data)-1), NA)
  test_data[["isaa"]] <- test_data$CARRIER == "AA"

  model_ret <- build_model(test_data, model_func = lightgbm_binary, formula = isaa ~ . - w - 1, nrounds = 5, weight = log(w), eval_metric = "auc", na.action = na.omit, sparse = FALSE)
  prediction_ret <- prediction_binary(model_ret)
  expect_true(any(prediction_ret$predicted_label))
  expect_true(any(!prediction_ret$predicted_label))
})

test_that("test build_lightgbm prediction with optimized threshold", {
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
    model_func = lightgbm_binary,
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

test_that("test lightgbm_binary with not clean names", {
  test_data <- data.frame(
    label = rep(c(T, F, F), 100),
    num1 = rep(c(seq(2), NA), 100) + runif(100),
    num2 = rep(seq(3), 100) + runif(100)
  )
  colnames(test_data) <- c("Label 1", "Num 1", "Num 2")
  model_ret <- build_model(
    test_data,
    model_func = lightgbm_binary,
    formula = `Label 1` ~ `Num 1` + `Num 2`,
    nrounds = 5,
    na.action = na.omit
  )
  prediction_ret <- prediction(model_ret)
  expect_true(class(prediction_ret$predicted_probability) == "numeric")
  expect_true(all(between(prediction_ret$predicted_probability, 0, 1), na.rm = TRUE))
})

if (Sys.info()["sysname"] != "Windows") {
  test_that("test lightgbm_binary with not clean Japanese names", {
    test_data <- data.frame(
      label = rep(c(T, F, F), 100),
      num1 = rep(c(seq(2), NA), 100) + runif(100),
      num2 = rep(seq(3), 100) + runif(100)
    )
    colnames(test_data) <- c("ラベル 1", "数値 1", "数値 2")
    model_ret <- build_model(
      test_data,
      model_func = lightgbm_binary,
      formula = `ラベル 1` ~ `数値 1` + `数値 2`,
      nrounds = 5,
      na.action = na.omit
    )
    prediction_ret <- prediction(model_ret)
    expect_true(class(prediction_ret$predicted_probability) == "numeric")
    expect_true(all(between(prediction_ret$predicted_probability, 0, 1), na.rm = TRUE))
  })
}

test_that("test lightgbm_reg with not clean names", {
  test_data <- data.frame(
    label = rep(seq(3) * 5, 100),
    num1 = rep(c(seq(2), NA), 100) + runif(100),
    num2 = rep(seq(3), 100) + runif(100)
  )
  colnames(test_data) <- c("Label 1", "Num 1", "Num 2")
  model_ret <- build_model(
    test_data,
    model_func = lightgbm_reg,
    formula = `Label 1` ~ `Num 1` + `Num 2`,
    nrounds = 5,
    na.action = na.omit
  )
  prediction_ret <- prediction(model_ret)
  expect_true(class(prediction_ret$predicted_value) == "numeric")
})

if (Sys.info()["sysname"] != "Windows") {
  test_that("test lightgbm_reg with not clean Japanese names", {
    test_data <- data.frame(
      label = rep(seq(3) * 5, 100),
      num1 = rep(c(seq(2), NA), 100) + runif(100),
      num2 = rep(seq(3), 100) + runif(100)
    )
    colnames(test_data) <- c("ラベル 1", "数値 1", "数値 2")
    model_ret <- build_model(
      test_data,
      model_func = lightgbm_reg,
      formula = `ラベル 1` ~ `数値 1` + `数値 2`,
      nrounds = 5,
      na.action = na.omit
    )
    prediction_ret <- prediction(model_ret)
    expect_true(class(prediction_ret$predicted_value) == "numeric")
  })
}

test_that("test lightgbm_reg with add_prediction", {
  train_data <- data.frame(
    label = rep(seq(3) * 5, 100),
    num1 = rep(seq(3), 100) + runif(100),
    num2 = rep(seq(3), 100) + runif(100)
  )

  test_data <- data.frame(
    label = rep(seq(3) * 5, 100),
    num1 = rep(seq(3), 100) + runif(100),
    num2 = rep(seq(3), 100) + runif(100)
  )
  colnames(train_data) <- c("label 1", "num-1", "Num 2")
  colnames(test_data) <- colnames(train_data)
  model_ret <- build_model(train_data, model_func = lightgbm_reg, formula = `label 1` ~ ., nrounds = 5)
  prediction_ret <- add_prediction(test_data, model_df = model_ret)
  expect_true(all(prediction_ret$predicted_label %in% c(5, 10, 15)))
})

test_that("test lightgbm_multi with numeric target", {
  test_data <- data.frame(
    label = rep(seq(3) * 5, 100),
    num1 = rep(seq(3), 100) + runif(100),
    num2 = rep(seq(3), 100) + runif(100)
  )
  model_ret <- build_model(test_data, model_func = lightgbm_multi, formula = label ~ ., nrounds = 5)
  prediction_ret <- prediction(model_ret)
  expect_true(all(prediction_ret$predicted_label %in% c(5, 10, 15)))
})

test_that("test lightgbm_multi with not clean names and NA", {
  test_data <- data.frame(
    label = rep(seq(3) * 5, 100),
    num1 = rep(c(seq(2), NA), 100) + runif(100),
    num2 = rep(seq(3), 100) + runif(100)
  )
  colnames(test_data) <- c("Label 1", "Num 1", "Num 2")
  model_ret <- build_model(
    test_data,
    model_func = lightgbm_multi,
    formula = `Label 1` ~ `Num 1` + `Num 2`,
    nrounds = 5,
    na.action = na.omit
  )
  prediction_ret <- prediction(model_ret)
  expect_true(all(prediction_ret$predicted_label %in% c(5, 10, 15)))
})

if (Sys.info()["sysname"] != "Windows") {
  test_that("test lightgbm_multi with not clean Japanese names and NA", {
    test_data <- data.frame(
      label = rep(seq(3) * 5, 100),
      num1 = rep(c(seq(2), NA), 100) + runif(100),
      num2 = rep(seq(3), 100) + runif(100)
    )
    colnames(test_data) <- c("ラベル 1", "数値 1", "数値 2")
    model_ret <- build_model(
      test_data,
      model_func = lightgbm_multi,
      formula = `ラベル 1` ~ `数値 1` + `数値 2`,
      nrounds = 5,
      na.action = na.omit
    )
    prediction_ret <- prediction(model_ret)
    expect_true(all(prediction_ret$predicted_label %in% c(5, 10, 15)))
  })
}
