context("test build_lm")

test_that("test build_lm summary output ", {
  test_df = data.frame(
    num1 = seq(20) / 10.0,
    num2 = seq(20) - 10,
    weight = seq(20) + 100,
    category = rep(letters[1:4], 5),
    with_NA = rep(c(letters[5:6], NA, NA), 5)
  )
  trial <- test_df %>% build_lm(num1 ~ num2 + category + with_NA, weights = weight)

  expect_equal(colnames(trial), c("source.data", ".test_index", "model", ".model_metadata"))

  res <- capture.output(summary(trial$model[[1]]))
  expect_lt(length(res), 50) # the output of summary should be less than 50 lines
})

test_that("test build_lm with keep.source FALSE ", {
  test_df = data.frame(
    num1 = seq(20) / 10.0,
    num2 = seq(20) - 10,
    weight = seq(20) + 100,
    category = rep(letters[1:4], 5),
    with_NA = rep(c(letters[5:6], NA, NA), 5)
  )
  trial <- test_df %>% build_lm(num1 ~ num2 + category + with_NA, weights = weight, keep.source = FALSE)

  expect_equal(colnames(trial), c(".test_index", "model", ".model_metadata"))
})

test_that("test build_lm with grouped ", {
  test_df = data.frame(
    num1 = seq(20) / 10.0,
    num2 = seq(20) - 10,
    group1 = rep(letters[1:4], 5),
    group2 = rep(letters[1:2], each = 10)
  )
  trial <- test_df %>% build_lm(num1 ~ num2, group_cols = c("group1", "group2"))
  expect_equal(length(trial[["group2"]]), 8)
  expect_equal(length(trial[["group1"]]), 8)
  expect_error(build_lm(test_df, num1 ~ num2 + group1, group_cols = c("group1", "group2")), "group1 is a grouping column. Please remove it from variables.")
})

test_that("test build_lm with augment TRUE", {
  test_df = data.frame(
    num1 = seq(20) / 10.0,
    num2 = seq(20) - 10,
    group1 = rep(letters[1:4], 5),
    group2 = rep(letters[1:2], each = 10)
  )
  trial <- test_df %>% build_lm(num1 ~ num2, group_cols = c("group1", "group2"), augment = TRUE)
  expect_equal(length(trial[["group2"]]), 20)
  expect_equal(length(trial[["group1"]]), 20)
})

test_that("test name conflict avoid", {
  test_df = data.frame(
    num1 = seq(20) / 10.0,
    num2 = seq(20) - 11,
    estimate = rep(letters[1:4], 5),
    model = rep(letters[1:2], each = 10),
    model.group = rep(letters[1:2], each = 10)
  )

  lm_model <- test_df %>%
    build_lm(num1 ~ num2, group_cols = c("estimate", "model", "model.group"))

  expect_equal(colnames(lm_model), c("estimate.group", "model.group", "model.group1", "source.data", ".test_index", "model", ".model_metadata"))

  trial <- suppressWarnings({
    lm_model %>%
      broom::tidy(model)
  })

  expect_equal(colnames(trial), c("estimate.group", "model.group", "model.group1",
                                  "term", "estimate", "std.error", "statistic", "p.value"))
})

test_that("build_lm with evaluation", {
  test_df = data.frame(
    num1 = seq(20) / 10.0,
    num2 = seq(20) - 11,
    group = rep(letters[1:2], each = 10)
  )

  lm_model <- test_df %>%
    build_lm(num1 ~ num2, group_cols = c("group"), test_rate = 0.1)

  evaluated <- lm_model %>%
    prediction(data = "test")

  expect_equal(colnames(evaluated), c("group", "num1", "num2", "predicted_value", "standard_error", "conf_low", "conf_high"))

  test_eval <- lm_model %>%
    prediction(data = "training")

  expect_equal(colnames(test_eval), c("group", "num1", "num2",
                                      "predicted_value", "standard_error", "conf_low", "conf_high", "residuals",
                                      "hat", "residual_standard_deviation", "cooks_distance",
                                      "standardised_residuals"
                                      ))

})

test_that("prediction with categorical columns", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))

  # duplicate rows to make some predictable data
  # otherwise, the number of rows of the result of prediction becomes 0
  test_data <- dplyr::bind_rows(test_data, test_data)

  model_data <- build_lm(test_data, CANCELLED ~ `Carrier Name` + CARRIER + DISTANCE, test_rate = 0.6)

  ret <- prediction(model_data, data = "test", pretty.name = TRUE)
  expect_true(nrow(ret) > 0)
  expect_equal(colnames(ret), c("CANCELLED", "Carrier.Name", "CARRIER", "DISTANCE", "predicted_value", "standard_error", "conf_low", "conf_high"))

  grouped <- test_data %>%
    dplyr::group_by(CARRIER)

  expect_error(build_lm(grouped, CANCELLED ~ CARRIER), "CARRIER is a grouping column. Please remove it from variables.")
})

test_that("prediction with target column name with space by build_lm.fast", {
  test_data <- structure(
    list(
      `CANCELLED X` = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      # testing filtering of Inf, -Inf, NA here.
      DISTANCE = c(Inf, -Inf, NA, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED X", "Carrier Name", "CARRIER", "DISTANCE"))

  # duplicate rows to make some predictable data
  # otherwise, the number of rows of the result of prediction becomes 0
  test_data <- dplyr::bind_rows(test_data, test_data)

  model_data <- build_lm.fast(test_data, `CANCELLED X`, `Carrier Name`, CARRIER, DISTANCE)
  ret <- model_data %>% broom::augment(model)

  expect_true(nrow(ret) > 0)
  expect_equal(colnames(ret), c("CANCELLED.X", "Carrier.Name","CARRIER","DISTANCE",".fitted",".se.fit",".resid",".hat",".sigma",".cooksd",".std.resid"))
})

test_that("prediction with glm model with SMOTE by build_lm.fast", {
  test_data <- structure(
    list(
      `CANCELLED X` = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      # testing filtering of Inf, -Inf, NA here.
      DISTANCE = c(Inf, -Inf, NA, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED X", "Carrier Name", "CARRIER", "DISTANCE"))

  # duplicate rows to make some predictable data
  # otherwise, the number of rows of the result of prediction becomes 0
  test_data <- dplyr::bind_rows(test_data, test_data)

  model_data <- build_lm.fast(test_data, `CANCELLED X`, `Carrier Name`, CARRIER, DISTANCE, model_type = "glm", smote=TRUE)
  ret <- model_data %>% broom::tidy(model)

  expect_true(nrow(ret) > 0)
})
