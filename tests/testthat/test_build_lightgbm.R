context("test build_lightgbm")

testthat::skip_if_not_installed("lightgbm")
testthat::skip_if_not_installed("Matrix")

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

test_that("test build_lightgbm with sparse=TRUE and valid_data", {
  set.seed(1)
  df <- data.frame(
    y = sample(c(TRUE, FALSE), 120, replace = TRUE),
    x1 = rnorm(120),
    x2 = factor(sample(letters[1:3], 120, replace = TRUE))
  )

  train_df <- df[21:120, , drop = FALSE]
  valid_df <- df[1:20, , drop = FALSE]

  model_ret <- build_model(
    train_df,
    model_func = lightgbm_binary,
    formula = y ~ x1 + x2,
    nrounds = 5,
    eval_metric = "auc",
    na.action = na.pass,
    sparse = TRUE,
    valid_data = valid_df
  )

  # Assert the sparse flag is propagated into the fitted model wrapper.
  expect_true(isTRUE(model_ret$model[[1]]$is_sparse))

  # And that we can still generate predictions successfully.
  prediction_ret <- prediction_binary(model_ret)
  expect_true(is.numeric(prediction_ret$predicted_probability))
  expect_true(all(prediction_ret$predicted_probability >= 0 & prediction_ret$predicted_probability <= 1, na.rm = TRUE))
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

test_that("exp_lightgbm filters Inf values from predictors", {
  testthat::skip_if_not_installed("lightgbm")
  # Create test data with Inf values in predictors
  test_data <- data.frame(
    x = c(1, 2, Inf, 4, 5),
    y = c(1, 2, 3, 4, 5)
  )

  # Model should train successfully with Inf values filtered
  result <- test_data %>% exp_lightgbm(y, x, nrounds = 5)
  expect_false("error" %in% class(result$model[[1]]))

  # Verify that Inf values were filtered (model should have fewer rows than original)
  # The model should train on 4 rows (one Inf row removed)
  expect_true(nrow(result$source.data[[1]]) == 4)
})

test_that("glance.lightgbm_exp shows Inf removal message for regression", {
  testthat::skip_if_not_installed("lightgbm")
  # Create test data with Inf values in predictors
  test_data <- data.frame(
    x = c(1, 2, Inf, 4, 5),
    y = c(1, 2, 3, 4, 5)
  )

  result <- test_data %>% exp_lightgbm(y, x, nrounds = 5)
  glance_result <- glance(result$model[[1]])

  # Should have Note column with Inf removal message
  expect_true("Note" %in% colnames(glance_result))
  expect_true(stringr::str_detect(glance_result$Note, "Inf values"))
  expect_true(stringr::str_detect(glance_result$Note, "automatically removed"))
})

test_that("glance.lightgbm_exp shows no note when no Inf values", {
  testthat::skip_if_not_installed("lightgbm")
  # Create test data without Inf values
  test_data <- data.frame(
    x = c(1, 2, 3, 4, 5),
    y = c(1, 2, 3, 4, 5)
  )

  result <- test_data %>% exp_lightgbm(y, x, nrounds = 5)
  glance_result <- glance(result$model[[1]])

  # Should not have Note column or Note should be empty/NA
  if ("Note" %in% colnames(glance_result)) {
    expect_true(is.na(glance_result$Note) || glance_result$Note == "" || !stringr::str_detect(glance_result$Note, "Inf values"))
  }
})

test_that("tidy.lightgbm_exp shows Inf removal message for classification", {
  testthat::skip_if_not_installed("lightgbm")
  # Create test data with Inf values in predictors for binary classification
  test_data <- data.frame(
    x = c(1, 2, Inf, 4, 5, 6),
    y = c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE)
  )

  result <- test_data %>% exp_lightgbm(y, x, nrounds = 5)
  tidy_result <- tidy(result$model[[1]], type = "evaluation")

  # Should have Note column with Inf removal message
  expect_true("Note" %in% colnames(tidy_result))
  expect_true(stringr::str_detect(tidy_result$Note, "Inf values"))
  expect_true(stringr::str_detect(tidy_result$Note, "automatically removed"))
})

test_that("exp_lightgbm handles all rows removed due to Inf values", {
  testthat::skip_if_not_installed("lightgbm")
  # Create test data where all rows have Inf in predictors
  test_data <- data.frame(
    x = c(Inf, Inf, Inf),
    y = c(1, 2, 3)
  )

  # Should fail gracefully with clear error message
  expect_error(
    test_data %>% exp_lightgbm(y, x, nrounds = 5),
    "All rows were removed due to Inf values"
  )
})
