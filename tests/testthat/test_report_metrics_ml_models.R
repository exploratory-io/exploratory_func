# how to run this test:
# devtools::test(filter="report_metrics_ml_models")
#
# Training Summary for RF / XGBoost / LightGBM / CatBoost goes through tidy.*
# (not only rf_evaluation). report_metrics must be forwarded so Analytics Report
# gets ROC AUC / PR AUC like Decision Tree (#37256).

context("report_metrics for RF / GBDT tidy evaluation (#37256)")

make_binary_df <- function(n = 150, seed = 42) {
  set.seed(seed)
  dplyr::tibble(
    y = rbinom(n, 1, 0.35) == 1,
    x1 = rnorm(n),
    x2 = rnorm(n)
  ) %>%
    dplyr::mutate(x1 = x1 + ifelse(y, 1.2, 0))
}

expected_binary <- c("ROC AUC", "PR AUC", "Balanced Accuracy", "Specificity")

expect_report_metrics_binary <- function(model_df, label) {
  base <- rf_evaluation_training_and_test(model_df, pretty.name = TRUE)
  with_metrics <- rf_evaluation_training_and_test(model_df, pretty.name = TRUE,
                                                  report_metrics = TRUE)
  expect_true(all(expected_binary %in% colnames(with_metrics)), info = label)
  expect_false(any(expected_binary %in% colnames(base)), info = label)
  expect_false("AUC" %in% colnames(with_metrics), info = label)
  expect_false(any(is.na(with_metrics[, expected_binary, drop = FALSE])), info = label)
}

test_that("calc_feature_imp (ranger) report_metrics renames AUC to ROC AUC and adds PR AUC", {
  df <- make_binary_df()
  model_df <- df %>% calc_feature_imp(y, x1, x2, test_rate = 0)
  expect_report_metrics_binary(model_df, "ranger")
})

test_that("exp_xgboost report_metrics renames AUC to ROC AUC and adds PR AUC", {
  skip_if_not_installed("xgboost")
  df <- make_binary_df()
  model_df <- suppressWarnings(df %>% exp_xgboost(y, x1, x2, test_rate = 0, nrounds = 15))
  expect_report_metrics_binary(model_df, "xgboost")
})

test_that("exp_lightgbm report_metrics renames AUC to ROC AUC and adds PR AUC", {
  skip_if_not_installed("lightgbm")
  df <- make_binary_df()
  model_df <- suppressWarnings(df %>% exp_lightgbm(y, x1, x2, test_rate = 0, nrounds = 15))
  expect_report_metrics_binary(model_df, "lightgbm")
})

test_that("exp_catboost report_metrics renames AUC to ROC AUC and adds PR AUC", {
  skip_if_not_installed("catboost")
  df <- make_binary_df()
  model_df <- suppressWarnings(df %>% exp_catboost(y, x1, x2, test_rate = 0, iterations = 15))
  expect_report_metrics_binary(model_df, "catboost")
})
