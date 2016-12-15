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

  expect_equal(colnames(glm_model), c("estimate.group", "model.group", "model.group1", ".test_index", "source.data", "model"))

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
  expect_equal(colnames(coef_ret), c("group", "Term", "Estimate", "Std Error", "t Ratio", "Prob > |t|"))

  stats_ret <- model_data %>% model_stats()
  expect_equal(colnames(stats_ret), c("group", "Null Deviance", "Degree of Freedom for Null Model", "Log Likelihood",
                                      "AIC", "BIC", "Deviance", "Residual Degree of Freedom"))

  anova_ret <- model_data %>% model_anova()
  expect_equal(colnames(anova_ret), c("group", "Term", "Degree of Freedom", "Deviance", "Residual Degree of Freedom",
                                      "Residual Deviance"))

  confint_ret <- model_data %>% model_confint(level = 0.99)
  expect_equal(colnames(confint_ret), c("group", "Term", "Prob 0.5", "Prob 99.5"))
})

