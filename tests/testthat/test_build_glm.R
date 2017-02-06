context("test build_glm")

test_that("test build_glm summary output ", {
  test_df = data.frame(
    num1 = seq(20) / 10.0,
    num2 = seq(20) - 10,
    weights = seq(20) + 100,
    category = rep(letters[1:4], 5),
    with_NA = rep(c(letters[5:6], NA, NA), 5)
  )
  trial <- test_df %>% build_glm(num1 ~ num2 + category + with_NA, weights = weights)

  expect_equal(colnames(trial), c(".test_index", "source.data", "model"))

  res <- capture.output(summary(trial$model[[1]]))
  expect_lt(length(res), 50) # the output of summary should be less than 50 lines
})

test_that("test build_glm with keep.source FALSE ", {
  test_df = data.frame(
    num1 = seq(20) / 10.0,
    num2 = seq(20) - 10,
    weights = seq(20) + 100,
    category = rep(letters[1:4], 5),
    with_NA = rep(c(letters[5:6], NA, NA), 5)
  )
  trial <- test_df %>% build_glm(num1 ~ num2 + category + with_NA, weights = weights, keep.source = FALSE)

  expect_equal(colnames(trial), c(".test_index", "model"))
})

test_that("test build_glm with grouped ", {
  test_df = data.frame(
    num1 = seq(20) / 10.0,
    num2 = seq(20) - 10,
    group1 = rep(letters[1:4], 5),
    group2 = rep(letters[1:2], each = 10)
  )
  trial <- test_df %>% build_glm(num1 ~ num2, group_cols = c("group1", "group2"))
  expect_equal(length(trial[["group2"]]), 8)
  expect_equal(length(trial[["group1"]]), 8)

  expect_error(build_glm(test_df, num1 ~ num2 + group1, group_cols = c("group1", "group2")), "'group1' is a grouping column. Please remove it from variables.")
})

test_that("test build_glm with augment TRUE", {
  test_df = data.frame(
    num1 = seq(20) / 10.0,
    num2 = seq(20) - 10,
    group1 = rep(letters[1:4], 5),
    group2 = rep(letters[1:2], each = 10)
  )
  trial <- test_df %>% build_glm(num1 ~ num2, group_cols = c("group1", "group2"), augment = TRUE)
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

  glm_model <- test_df %>%
    build_glm(num1 ~ num2, group_cols = c("estimate", "model", "model.group"))

  expect_equal(colnames(glm_model), c("estimate.group", "model.group", "model.group1", "source.data", ".test_index", "model"))

  trial <- suppressWarnings({
    glm_model %>%
      broom::tidy(model)
  })

  expect_equal(colnames(trial), c("estimate.group", "model.group", "model.group1",
                                  "term", "estimate", "std.error", "statistic", "p.value"))
})

test_that("predict glm with new data", {
  loadNamespace("dplyr")
  fit_df <- data.frame(
    group = rep(paste("group", seq(2)), each=15),
    num1 = seq(30) %% 3 == 0,
    num2 = 10- (seq(30) %% 2)
  )

  model_data <- fit_df %>% build_glm(num1 ~ num2, family = binomial, group_cols = "group")

  coef_ret <- model_data %>% model_coef()
  expect_equal(colnames(coef_ret), c("group", "term", "estimate", "std_error", "t_ratio", "p_value", "odds_ratio"))

  stats_ret <- model_data %>% model_stats()
  expect_equal(colnames(stats_ret), c("group", "null_deviance", "df_for_null_model", "log_likelihood",
                                      "AIC", "BIC", "deviance", "residual_df"))

  anova_ret <- model_data %>% model_anova()
  expect_equal(colnames(anova_ret), c("group", "term", "df", "deviance", "residual_df",
                                      "residual_deviance"))

  confint_ret <- model_data %>% model_confint(level = 0.99)
  expect_equal(colnames(confint_ret), c("group", "Term", "Prob 0.5", "Prob 99.5"))
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

  model_data <- build_glm(test_data, family = "binomial", CANCELLED ~ `Carrier Name` + CARRIER + DISTANCE, test_rate = 0.6)

  ret <- prediction(model_data, data = "test", type.predict = "response")
  both_ret <- prediction(model_data, data = "test")
  train_ret <- prediction(model_data, data = "training")

  expect_true(nrow(ret) > 0)
  expect_true(all(ret["predicted_value"] >= 0 & ret["predicted_value"] <= 1))
  expect_equal(colnames(ret), c("CANCELLED", "Carrier.Name", "CARRIER", "DISTANCE", "predicted_value", "standard_error", "conf_low", "conf_high"))

  expect_true(all(both_ret["predicted_response"] >= 0 & both_ret["predicted_response"] <= 1))
  expect_equal(colnames(both_ret), c("CANCELLED", "Carrier.Name", "CARRIER", "DISTANCE", "predicted_value", "standard_error", "conf_low", "conf_high", "predicted_response"))

  expect_true(all(train_ret["predicted_response"] >= 0 & train_ret["predicted_response"] <= 1))
  expect_equal(colnames(train_ret), c("CANCELLED", "Carrier.Name", "CARRIER", "DISTANCE", "predicted_value",
                                      "standard_error", "conf_low", "conf_high", "residuals", "hat", "residual_standard_deviation",
                                      "cooks_distance", "standardised_residuals", "predicted_response"))

  add_prediction_ret <- test_data %>% add_prediction(model_data, type.predict = "response")
  expect_true(all(add_prediction_ret["predicted_value"] >= 0 & add_prediction_ret["predicted_value"] <= 1))
})

test_that("test prediction binary", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))

  # duplicate rows to make some predictable data
  # otherwise, the number of rows of the result of prediction becomes 0
  for (i in seq(5)){
    test_data <- dplyr::bind_rows(test_data, test_data)
  }

  test_data[["CANCELLED"]] <- as.character(test_data[["CANCELLED"]])

  model_data <- build_lr(test_data, CANCELLED ~ DISTANCE, test_rate = 0.2)

  coef_ret <- model_coef(model_data, conf_int = "default")

  expect_true(!is.null(coef_ret[["odds_ratio"]]))

  prediction_train_ret <- prediction_binary(model_data, data = "training")
  expect_true(any("predicted_label" == colnames(prediction_train_ret)))

})

