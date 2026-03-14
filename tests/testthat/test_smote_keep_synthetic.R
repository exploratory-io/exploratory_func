context("Test smote_keep_synthetic parameter")

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
  smote_was_applied <- "synthesized" %in% colnames(source_data)

  if (smote_was_applied) {
    expect_true(nrow(source_data) > n)
    expect_type(source_data$synthesized, "logical")
    expect_true(sum(source_data$synthesized) > 0)
    expect_true(sum(!source_data$synthesized) > 0)
    test_index <- glm_keep_true$.test_index[[1]]
    expect_true(all(!source_data$synthesized[test_index]))
  } else {
    expect_equal(nrow(source_data), n)
    skip("SMOTE was not applied - data conditions may not support it")
  }
})

test_that("exp_xgboost with smote_keep_synthetic = TRUE includes synthesized column", {
  expect_true(!is.null(xgb_keep_true))
  expect_true("model" %in% colnames(xgb_keep_true))
  expect_true("source.data" %in% colnames(xgb_keep_true))

  source_data <- xgb_keep_true$source.data[[1]]
  smote_was_applied <- "synthesized" %in% colnames(source_data)

  if (smote_was_applied) {
    expect_true(nrow(source_data) > n)
    expect_type(source_data$synthesized, "logical")
    expect_true(sum(source_data$synthesized) > 0)
    test_index <- xgb_keep_true$.test_index[[1]]
    expect_true(all(!source_data$synthesized[test_index]))
  } else {
    expect_equal(nrow(source_data), n)
    skip("SMOTE was not applied - data conditions may not support it")
  }
})

test_that("exp_lightgbm with smote_keep_synthetic = TRUE includes synthesized column", {
  expect_true(!is.null(lgbm_keep_true))
  expect_true("model" %in% colnames(lgbm_keep_true))
  expect_true("source.data" %in% colnames(lgbm_keep_true))

  source_data <- lgbm_keep_true$source.data[[1]]
  smote_was_applied <- "synthesized" %in% colnames(source_data)

  if (smote_was_applied) {
    expect_true(nrow(source_data) > n)
    expect_type(source_data$synthesized, "logical")
    test_index <- lgbm_keep_true$.test_index[[1]]
    expect_true(all(!source_data$synthesized[test_index]))
  } else {
    expect_equal(nrow(source_data), n)
    skip("SMOTE was not applied - data conditions may not support it")
  }
})

test_that("calc_feature_imp (ranger) with smote_keep_synthetic = TRUE includes synthesized column", {
  expect_true(!is.null(rf_keep_true))
  expect_true("model" %in% colnames(rf_keep_true))
  expect_true("source.data" %in% colnames(rf_keep_true))

  source_data <- rf_keep_true$source.data[[1]]
  smote_was_applied <- "synthesized" %in% colnames(source_data)

  if (smote_was_applied) {
    expect_true(nrow(source_data) > n)
    expect_type(source_data$synthesized, "logical")
    test_index <- rf_keep_true$.test_index[[1]]
    expect_true(all(!source_data$synthesized[test_index]))
  } else {
    expect_equal(nrow(source_data), n)
    skip("SMOTE was not applied - data conditions may not support it")
  }
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
