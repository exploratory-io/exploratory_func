# Test to verify that SMOTE is applied only to training data after train/test split
# This ensures test data remains pure and doesn't contain synthetic samples

context("test SMOTE with test mode - verify test data purity")

testthat::skip_if_not_installed("ranger")

# ── Shared fixtures for the main group (n=500, smote_keep_synthetic=FALSE, test_rate=0.3) ──
set.seed(123)
n <- 500
test_mode_data <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  target = c(rep(FALSE, 400), rep(TRUE, 100))
)

rf_test_mode <- test_mode_data %>%
  calc_feature_imp(target, x1, x2,
                   test_rate = 0.3, smote = TRUE,
                   smote_keep_synthetic = FALSE, seed = 123)

# ── Tests ────────────────────────────────────────────────────────────────────

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
