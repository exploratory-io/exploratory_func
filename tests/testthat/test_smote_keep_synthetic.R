context("Test smote_keep_synthetic parameter")

testthat::skip_if_not_installed("xgboost")
testthat::skip_if_not_installed("lightgbm")
testthat::skip_if_not_installed("ranger")

# ── Shared fixtures ──────────────────────────────────────────────────────────
# All six models share the same data. Built once at file scope so each
# test_that block only needs to assert, not re-build.
set.seed(123)
n <- 1000
smote_test_data <- data.frame(
  feature1 = rnorm(n),
  feature2 = rnorm(n),
  feature3 = rnorm(n),
  target = c(rep(FALSE, floor(n * 0.85)), rep(TRUE, ceiling(n * 0.15)))
)

# smote_keep_synthetic = TRUE (default)
glm_keep_true <- smote_test_data %>%
  build_lm.fast(target, feature1, feature2, feature3,
                model_type = "glm", test_rate = 0.2, smote = TRUE,
                smote_target_minority_perc = 45, seed = 123)

xgb_keep_true <- smote_test_data %>%
  exp_xgboost(target, feature1, feature2, feature3,
              test_rate = 0.2, smote = TRUE,
              smote_target_minority_perc = 45, nrounds = 5, seed = 123)

lgbm_keep_true <- smote_test_data %>%
  exp_lightgbm(target, feature1, feature2, feature3,
               test_rate = 0.2, smote = TRUE,
               smote_target_minority_perc = 45, nrounds = 5, seed = 123)

rf_keep_true <- smote_test_data %>%
  calc_feature_imp(target, feature1, feature2, feature3,
                   test_rate = 0.2, smote = TRUE,
                   smote_target_minority_perc = 45, seed = 123)

# smote_keep_synthetic = FALSE
glm_keep_false <- smote_test_data %>%
  build_lm.fast(target, feature1, feature2, feature3,
                model_type = "glm", test_rate = 0.2, smote = TRUE,
                smote_keep_synthetic = FALSE, smote_target_minority_perc = 45,
                seed = 123)

xgb_keep_false <- smote_test_data %>%
  exp_xgboost(target, feature1, feature2, feature3,
              test_rate = 0.2, smote = TRUE,
              smote_keep_synthetic = FALSE, smote_target_minority_perc = 45,
              nrounds = 5, seed = 123)

# ── Tests: smote_keep_synthetic = TRUE ──────────────────────────────────────

test_that("build_lm.fast (GLM) with smote_keep_synthetic = TRUE includes synthesized column", {
  expect_true(!is.null(glm_keep_true))
  expect_true("model" %in% colnames(glm_keep_true))
  expect_true("source.data" %in% colnames(glm_keep_true))

  source_data <- glm_keep_true$source.data[[1]]
  expect_true("synthesized" %in% colnames(source_data), info = "SMOTE should have been applied")
  expect_true(nrow(source_data) > n)
  expect_type(source_data$synthesized, "logical")
  expect_true(sum(source_data$synthesized) > 0)
  expect_true(sum(!source_data$synthesized) > 0)
  test_index <- glm_keep_true$.test_index[[1]]
  expect_true(all(!source_data$synthesized[test_index]))
})

test_that("exp_xgboost with smote_keep_synthetic = TRUE includes synthesized column", {
  expect_true(!is.null(xgb_keep_true))
  expect_true("model" %in% colnames(xgb_keep_true))
  expect_true("source.data" %in% colnames(xgb_keep_true))

  source_data <- xgb_keep_true$source.data[[1]]
  expect_true("synthesized" %in% colnames(source_data), info = "SMOTE should have been applied")
  expect_true(nrow(source_data) > n)
  expect_type(source_data$synthesized, "logical")
  expect_true(sum(source_data$synthesized) > 0)
  test_index <- xgb_keep_true$.test_index[[1]]
  expect_true(all(!source_data$synthesized[test_index]))
})

test_that("exp_lightgbm with smote_keep_synthetic = TRUE includes synthesized column", {
  expect_true(!is.null(lgbm_keep_true))
  expect_true("model" %in% colnames(lgbm_keep_true))
  expect_true("source.data" %in% colnames(lgbm_keep_true))

  source_data <- lgbm_keep_true$source.data[[1]]
  expect_true("synthesized" %in% colnames(source_data), info = "SMOTE should have been applied")
  expect_true(nrow(source_data) > n)
  expect_type(source_data$synthesized, "logical")
  test_index <- lgbm_keep_true$.test_index[[1]]
  expect_true(all(!source_data$synthesized[test_index]))
})

test_that("calc_feature_imp (ranger) with smote_keep_synthetic = TRUE includes synthesized column", {
  expect_true(!is.null(rf_keep_true))
  expect_true("model" %in% colnames(rf_keep_true))
  expect_true("source.data" %in% colnames(rf_keep_true))

  source_data <- rf_keep_true$source.data[[1]]
  expect_true("synthesized" %in% colnames(source_data), info = "SMOTE should have been applied")
  expect_true(nrow(source_data) > n)
  expect_type(source_data$synthesized, "logical")
  test_index <- rf_keep_true$.test_index[[1]]
  expect_true(all(!source_data$synthesized[test_index]))
})

# ── Tests: smote_keep_synthetic = FALSE ─────────────────────────────────────

test_that("build_lm.fast (GLM) with smote_keep_synthetic = FALSE excludes synthesized column", {
  expect_true(!is.null(glm_keep_false))
  expect_true("model" %in% colnames(glm_keep_false))
  expect_true("source.data" %in% colnames(glm_keep_false))

  source_data <- glm_keep_false$source.data[[1]]
  expect_equal(nrow(source_data), n)
  expect_false("synthesized" %in% colnames(source_data))
})

test_that("exp_xgboost with smote_keep_synthetic = FALSE excludes synthesized column", {
  expect_true(!is.null(xgb_keep_false))
  expect_true("model" %in% colnames(xgb_keep_false))
  expect_true("source.data" %in% colnames(xgb_keep_false))

  source_data <- xgb_keep_false$source.data[[1]]
  expect_equal(nrow(source_data), n)
  expect_false("synthesized" %in% colnames(source_data))
})

# ── Tests: training evaluation with smote_keep_synthetic = FALSE (tam#39340) ──
# The training predictions are made on the original (pre-SMOTE) training rows, so the
# actual values used for evaluation must come from those rows too, not from the SMOTE-resampled rows.

smote_no_keep_data <- local({
  set.seed(1)
  n <- 1000
  x <- rnorm(n)
  z <- rnorm(n)
  y <- runif(n) < plogis(-3 + x) # Imbalanced logical target.
  data.frame(y = y, x = x, z = z)
})

# Expect evaluation and confusion matrix of the training data to work on exactly the original training rows.
expect_training_metrics_on_original_rows <- function(model_df) {
  fit <- model_df$model[[1]]
  n_original_train <- nrow(model_df$source.data[[1]]) - length(model_df$.test_index[[1]])
  expect_false("synthesized" %in% colnames(model_df$source.data[[1]]))

  evaluation <- tidy(fit, type = "evaluation")
  expect_true(is.data.frame(evaluation))
  expect_equal(nrow(evaluation), 1)

  conf_mat <- tidy(fit, type = "conf_mat")
  expect_equal(sum(conf_mat$count), n_original_train)

  # Training actual values must match the original training data.
  original_train <- model_df$source.data[[1]][-model_df$.test_index[[1]], ]
  expect_equal(sort(tapply(conf_mat$count, conf_mat$actual_value, sum)),
               sort(c(table(original_train$y))), ignore_attr = TRUE)
}

test_that("calc_feature_imp (ranger) with smote = TRUE and smote_keep_synthetic = FALSE evaluates on original training rows", {
  model_df <- smote_no_keep_data %>%
    calc_feature_imp(y, x, z, test_rate = 0.2, smote = TRUE, smote_keep_synthetic = FALSE, seed = 1)
  expect_training_metrics_on_original_rows(model_df)
})

test_that("exp_xgboost with smote = TRUE and smote_keep_synthetic = FALSE evaluates on original training rows", {
  model_df <- smote_no_keep_data %>%
    exp_xgboost(y, x, z, test_rate = 0.2, smote = TRUE, smote_keep_synthetic = FALSE, nrounds = 5, seed = 1)
  expect_training_metrics_on_original_rows(model_df)
})

test_that("exp_lightgbm with smote = TRUE and smote_keep_synthetic = FALSE evaluates on original training rows", {
  model_df <- smote_no_keep_data %>%
    exp_lightgbm(y, x, z, test_rate = 0.2, smote = TRUE, smote_keep_synthetic = FALSE, nrounds = 5, seed = 1)
  expect_training_metrics_on_original_rows(model_df)
})

test_that("exp_catboost with smote = TRUE and smote_keep_synthetic = FALSE evaluates on original training rows", {
  testthat::skip_if_not_installed("catboost")
  model_df <- smote_no_keep_data %>%
    exp_catboost(y, x, z, test_rate = 0.2, smote = TRUE, smote_keep_synthetic = FALSE, iterations = 5, seed = 1)
  expect_training_metrics_on_original_rows(model_df)
})

test_that("ranger evaluation of the training data is unchanged when smote_keep_synthetic = TRUE", {
  # The default (TRUE) path must keep working: evaluation is on the SMOTE-resampled training rows.
  model_df <- smote_no_keep_data %>%
    calc_feature_imp(y, x, z, test_rate = 0.2, smote = TRUE, smote_keep_synthetic = TRUE, seed = 1)
  fit <- model_df$model[[1]]
  source_data <- model_df$source.data[[1]]
  n_train <- nrow(source_data) - length(model_df$.test_index[[1]])
  expect_true("synthesized" %in% colnames(source_data))
  expect_equal(sum(tidy(fit, type = "conf_mat")$count), n_train)
  expect_equal(length(fit$y), n_train)
})
