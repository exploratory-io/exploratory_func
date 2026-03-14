# Test to verify that SMOTE is applied only to training data after train/test split
# This ensures test data remains pure and doesn't contain synthetic samples
# how to run this test:
# devtools::test(filter="smote_test_mode")

context("test SMOTE with test mode - verify test data purity")

# ── Shared fixtures for the main group (n=500, smote_keep_synthetic=FALSE, test_rate=0.3) ──
# All four models share the same data. Built once at file scope.
set.seed(123)
n <- 500
test_mode_data <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  target = c(rep(FALSE, 400), rep(TRUE, 100))
)

glm_test_mode <- test_mode_data %>%
  build_lm.fast(target, x1, x2,
                model_type = "glm", test_rate = 0.3, smote = TRUE,
                smote_keep_synthetic = FALSE, seed = 123)

xgb_test_mode <- test_mode_data %>%
  exp_xgboost(target, x1, x2,
              test_rate = 0.3, smote = TRUE,
              smote_keep_synthetic = FALSE, seed = 123)

lgbm_test_mode <- test_mode_data %>%
  exp_lightgbm(target, x1, x2,
               test_rate = 0.3, smote = TRUE,
               smote_keep_synthetic = FALSE, seed = 123)

rf_test_mode <- test_mode_data %>%
  calc_feature_imp(target, x1, x2,
                   test_rate = 0.3, smote = TRUE,
                   smote_keep_synthetic = FALSE, seed = 123)

# ── Tests: main group ────────────────────────────────────────────────────────

test_that("build_lm.fast GLM with SMOTE and test mode - test data should be pure", {
  source_data <- glm_test_mode$source.data[[1]]
  test_index <- glm_test_mode$.test_index[[1]]

  expect_equal(nrow(source_data), n)
  expect_false("synthesized" %in% colnames(source_data))

  test_data_actual <- source_data[test_index, ]
  expected_test_size <- round(n * 0.3)
  expect_gte(nrow(test_data_actual), expected_test_size - 20)
  expect_lte(nrow(test_data_actual), expected_test_size + 20)

  train_data_before_smote <- source_data[-test_index, ]
  expected_train_size <- n - nrow(test_data_actual)
  expect_gte(nrow(train_data_before_smote), expected_train_size - 20)
  expect_lte(nrow(train_data_before_smote), expected_train_size + 20)
})

test_that("exp_xgboost with SMOTE and test mode - test data should be pure", {
  source_data <- xgb_test_mode$source.data[[1]]
  test_index <- xgb_test_mode$.test_index[[1]]

  expect_equal(nrow(source_data), n)
  expect_false("synthesized" %in% colnames(source_data))

  test_data_actual <- source_data[test_index, ]
  expected_test_size <- round(n * 0.3)
  expect_gte(nrow(test_data_actual), expected_test_size - 20)
  expect_lte(nrow(test_data_actual), expected_test_size + 20)
})

test_that("exp_lightgbm with SMOTE and test mode - test data should be pure", {
  source_data <- lgbm_test_mode$source.data[[1]]
  test_index <- lgbm_test_mode$.test_index[[1]]

  expect_equal(nrow(source_data), n)
  expect_false("synthesized" %in% colnames(source_data))

  test_data_actual <- source_data[test_index, ]
  expected_test_size <- round(n * 0.3)
  expect_gte(nrow(test_data_actual), expected_test_size - 20)
  expect_lte(nrow(test_data_actual), expected_test_size + 20)
})

test_that("calc_feature_imp (Ranger) with SMOTE and test mode - test data should be pure", {
  source_data <- rf_test_mode$source.data[[1]]
  test_index <- rf_test_mode$.test_index[[1]]

  expect_equal(nrow(source_data), n)
  expect_false("synthesized" %in% colnames(source_data))

  test_data_actual <- source_data[test_index, ]
  expected_test_size <- round(n * 0.3)
  expect_gte(nrow(test_data_actual), expected_test_size - 20)
  expect_lte(nrow(test_data_actual), expected_test_size + 20)
})

# ── One-off tests (distinct configurations) ──────────────────────────────────

test_that("exp_rpart with SMOTE and test mode - test data should be pure", {
  set.seed(222)
  n_rpart <- 500
  rpart_data <- data.frame(
    x1 = rnorm(n_rpart),
    x2 = rnorm(n_rpart),
    target = as.factor(c(rep("A", 400), rep("B", 100)))
  )

  model_df <- rpart_data %>%
    exp_rpart(target, x1, x2,
              test_rate = 0.3,
              smote = TRUE,
              smote_keep_synthetic = FALSE,
              seed = 222)

  source_data <- model_df$source.data[[1]]
  test_index <- model_df$.test_index[[1]]

  expect_equal(nrow(source_data), n_rpart)
  expect_false("synthesized" %in% colnames(source_data))

  test_data_actual <- source_data[test_index, ]
  expected_test_size <- round(n_rpart * 0.3)
  expect_gte(nrow(test_data_actual), expected_test_size - 20)
  expect_lte(nrow(test_data_actual), expected_test_size + 20)
})

test_that("SMOTE with test_rate=0 should still apply SMOTE to all training data", {
  set.seed(333)
  n_zero <- 500
  test_data <- data.frame(
    x1 = rnorm(n_zero),
    x2 = rnorm(n_zero),
    target = c(rep(FALSE, 400), rep(TRUE, 100))
  )

  model_df <- test_data %>%
    build_lm.fast(target, x1, x2,
                  model_type = "glm",
                  test_rate = 0,
                  smote = TRUE,
                  smote_keep_synthetic = FALSE,
                  seed = 333)

  source_data <- model_df$source.data[[1]]
  expect_equal(nrow(source_data), n_zero)
  expect_false("synthesized" %in% colnames(source_data))
})

test_that("No SMOTE with test mode should not alter data sizes", {
  set.seed(444)
  n_nosmote <- 500
  test_data <- data.frame(
    x1 = rnorm(n_nosmote),
    x2 = rnorm(n_nosmote),
    target = c(rep(FALSE, 400), rep(TRUE, 100))
  )

  model_df <- test_data %>%
    build_lm.fast(target, x1, x2,
                  model_type = "glm",
                  test_rate = 0.3,
                  smote = FALSE,
                  seed = 444)

  source_data <- model_df$source.data[[1]]
  test_index <- model_df$.test_index[[1]]

  expect_equal(nrow(source_data), n_nosmote)
  expect_false("synthesized" %in% colnames(source_data))

  test_data_actual <- source_data[test_index, ]
  expected_test_size <- round(n_nosmote * 0.3)
  expect_gte(nrow(test_data_actual), expected_test_size - 20)
  expect_lte(nrow(test_data_actual), expected_test_size + 20)

  train_data <- source_data[-test_index, ]
  expected_train_size <- n_nosmote - nrow(test_data_actual)
  expect_gte(nrow(train_data), expected_train_size - 20)
  expect_lte(nrow(train_data), expected_train_size + 20)
})

test_that("Train/test split works correctly with very few rows (3 rows: 1 test, 2 training)", {
  set.seed(555)
  test_data <- data.frame(
    x1 = c(1, 2, 3),
    x2 = c(10, 20, 30),
    target = c(FALSE, TRUE, FALSE)
  )

  model_df <- test_data %>%
    build_lm.fast(target, x1, x2,
                  model_type = "glm",
                  test_rate = 0.34,
                  smote = FALSE,
                  seed = 555)

  source_data <- model_df$source.data[[1]]
  test_index <- model_df$.test_index[[1]]

  expect_equal(nrow(source_data), 3)
  expect_true(length(test_index) >= 1)

  test_data_actual <- source_data[test_index, , drop = FALSE]
  expect_equal(nrow(test_data_actual), 1)

  train_data <- source_data[-test_index, , drop = FALSE]
  expect_equal(nrow(train_data), 2)

  expect_equal(length(intersect(test_index, seq_len(3)[-test_index])), 0)
  expect_equal(nrow(test_data_actual) + nrow(train_data), 3)
  expect_true(all(test_index >= 1 & test_index <= 3))
  expect_true(length(test_index) == 1)
})

test_that("Train/test split works with very few rows using XGBoost", {
  set.seed(666)
  test_data <- data.frame(
    x1 = c(1, 2, 3),
    x2 = c(10, 20, 30),
    target = c(FALSE, TRUE, FALSE)
  )

  model_df <- test_data %>%
    exp_xgboost(target, x1, x2,
                test_rate = 0.34,
                smote = FALSE,
                seed = 666)

  source_data <- model_df$source.data[[1]]
  test_index <- model_df$.test_index[[1]]

  expect_equal(nrow(source_data), 3)
  expect_true(length(test_index) >= 1)

  test_data_actual <- source_data[test_index, , drop = FALSE]
  train_data <- source_data[-test_index, , drop = FALSE]

  expect_equal(nrow(test_data_actual), 1)
  expect_equal(nrow(train_data), 2)
  expect_equal(nrow(test_data_actual) + nrow(train_data), 3)
  expect_true(all(test_index >= 1 & test_index <= 3))
  expect_true(length(test_index) == 1)
})

test_that("Train/test split with very few rows and SMOTE (should handle gracefully)", {
  set.seed(777)
  test_data <- data.frame(
    x1 = c(1, 2, 3),
    x2 = c(10, 20, 30),
    target = c(FALSE, TRUE, FALSE)
  )

  model_df <- test_data %>%
    build_lm.fast(target, x1, x2,
                  model_type = "glm",
                  test_rate = 0.34,
                  smote = TRUE,
                  smote_keep_synthetic = FALSE,
                  seed = 777)

  source_data <- model_df$source.data[[1]]
  test_index <- model_df$.test_index[[1]]

  expect_equal(nrow(source_data), 3)
  expect_true(length(test_index) >= 1)

  test_data_actual <- source_data[test_index, , drop = FALSE]
  train_data <- source_data[-test_index, , drop = FALSE]

  expect_equal(nrow(test_data_actual), 1)
  expect_equal(nrow(train_data), 2)
  expect_false("synthesized" %in% colnames(source_data))
})
