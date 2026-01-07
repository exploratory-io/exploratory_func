# Test to verify that SMOTE is applied only to training data after train/test split
# This ensures test data remains pure and doesn't contain synthetic samples
# how to run this test:
# devtools::test(filter="smote_test_mode")

context("test SMOTE with test mode - verify test data purity")

test_that("build_lm.fast GLM with SMOTE and test mode - test data should be pure", {
  # Create a dataset with imbalanced binary target
  set.seed(123)
  n <- 500
  test_data <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    # Create imbalanced target: 80% FALSE, 20% TRUE
    target = c(rep(FALSE, 400), rep(TRUE, 100))
  )

  # Train model with SMOTE and test_rate
  # Set smote_keep_synthetic = FALSE to test that source.data contains original data only
  model_df <- test_data %>%
    build_lm.fast(target, x1, x2,
                  model_type = "glm",
                  test_rate = 0.3,
                  smote = TRUE,
                  smote_keep_synthetic = FALSE,
                  seed = 123)

  # Get source.data which should contain original data only (no synthetic samples)
  source_data <- model_df$source.data[[1]]
  test_index <- model_df$.test_index[[1]]

  # Verify source.data matches original data size (not inflated by SMOTE)
  expect_equal(nrow(source_data), n)

  # Verify no 'synthesized' column when smote_keep_synthetic = FALSE
  expect_false("synthesized" %in% colnames(source_data))

  # Get test data by extracting rows at test_index from source.data
  test_data_actual <- source_data[test_index, ]
  expected_test_size <- round(n * 0.3)

  # Verify test data size is approximately 30% of original (not inflated by SMOTE)
  expect_gte(nrow(test_data_actual), expected_test_size - 20)
  expect_lte(nrow(test_data_actual), expected_test_size + 20)

  # Get training data (rows NOT in test_index)
  train_data_before_smote <- source_data[-test_index, ]
  expected_train_size <- n - nrow(test_data_actual)

  # Verify training size is approximately 70% of original
  expect_gte(nrow(train_data_before_smote), expected_train_size - 20)
  expect_lte(nrow(train_data_before_smote), expected_train_size + 20)
})

test_that("exp_xgboost with SMOTE and test mode - test data should be pure", {
  set.seed(456)
  n <- 500
  test_data <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    # Create imbalanced target
    target = c(rep(FALSE, 400), rep(TRUE, 100))
  )

  model_df <- test_data %>%
    exp_xgboost(target, x1, x2,
                test_rate = 0.3,
                smote = TRUE,
                smote_keep_synthetic = FALSE,
                seed = 456)

  source_data <- model_df$source.data[[1]]
  test_index <- model_df$.test_index[[1]]

  # Verify source.data is original size (no synthetic samples)
  expect_equal(nrow(source_data), n)
  expect_false("synthesized" %in% colnames(source_data))

  # Verify test data size
  test_data_actual <- source_data[test_index, ]
  expected_test_size <- round(n * 0.3)
  expect_gte(nrow(test_data_actual), expected_test_size - 20)
  expect_lte(nrow(test_data_actual), expected_test_size + 20)
})

test_that("exp_lightgbm with SMOTE and test mode - test data should be pure", {
  set.seed(789)
  n <- 500
  test_data <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    # Create imbalanced target
    target = c(rep(FALSE, 400), rep(TRUE, 100))
  )

  model_df <- test_data %>%
    exp_lightgbm(target, x1, x2,
                 test_rate = 0.3,
                 smote = TRUE,
                 smote_keep_synthetic = FALSE,
                 seed = 789)

  source_data <- model_df$source.data[[1]]
  test_index <- model_df$.test_index[[1]]

  # Verify source.data is original size (no synthetic samples)
  expect_equal(nrow(source_data), n)
  expect_false("synthesized" %in% colnames(source_data))

  # Verify test data size
  test_data_actual <- source_data[test_index, ]
  expected_test_size <- round(n * 0.3)
  expect_gte(nrow(test_data_actual), expected_test_size - 20)
  expect_lte(nrow(test_data_actual), expected_test_size + 20)
})

test_that("calc_feature_imp (Ranger) with SMOTE and test mode - test data should be pure", {
  set.seed(111)
  n <- 500
  test_data <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    # Create imbalanced target
    target = c(rep(FALSE, 400), rep(TRUE, 100))
  )

  model_df <- test_data %>%
    calc_feature_imp(target, x1, x2,
                     test_rate = 0.3,
                     smote = TRUE,
                     smote_keep_synthetic = FALSE,
                     seed = 111)

  source_data <- model_df$source.data[[1]]
  test_index <- model_df$.test_index[[1]]

  # Verify source.data is original size (no synthetic samples)
  expect_equal(nrow(source_data), n)
  expect_false("synthesized" %in% colnames(source_data))

  # Verify test data size
  test_data_actual <- source_data[test_index, ]
  expected_test_size <- round(n * 0.3)
  expect_gte(nrow(test_data_actual), expected_test_size - 20)
  expect_lte(nrow(test_data_actual), expected_test_size + 20)
})

test_that("exp_rpart with SMOTE and test mode - test data should be pure", {
  set.seed(222)
  n <- 500
  test_data <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    # Create imbalanced target
    target = as.factor(c(rep("A", 400), rep("B", 100)))
  )

  model_df <- test_data %>%
    exp_rpart(target, x1, x2,
              test_rate = 0.3,
              smote = TRUE,
              smote_keep_synthetic = FALSE,
              seed = 222)

  source_data <- model_df$source.data[[1]]
  test_index <- model_df$.test_index[[1]]

  # Verify source.data is original size (no synthetic samples)
  expect_equal(nrow(source_data), n)
  expect_false("synthesized" %in% colnames(source_data))

  # Verify test data size
  test_data_actual <- source_data[test_index, ]
  expected_test_size <- round(n * 0.3)
  expect_gte(nrow(test_data_actual), expected_test_size - 20)
  expect_lte(nrow(test_data_actual), expected_test_size + 20)
})

test_that("SMOTE with test_rate=0 should still apply SMOTE to all training data", {
  # When test_rate is 0, SMOTE should be applied to all data (backward compatibility)
  set.seed(333)
  n <- 500
  test_data <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    target = c(rep(FALSE, 400), rep(TRUE, 100))
  )

  model_df <- test_data %>%
    build_lm.fast(target, x1, x2,
                  model_type = "glm",
                  test_rate = 0,  # No test split
                  smote = TRUE,
                  smote_keep_synthetic = FALSE,
                  seed = 333)

  source_data <- model_df$source.data[[1]]

  # With test_rate=0 and smote_keep_synthetic=FALSE, source.data should still be original size
  expect_equal(nrow(source_data), n)
  expect_false("synthesized" %in% colnames(source_data))
})

test_that("No SMOTE with test mode should not alter data sizes", {
  # When SMOTE is disabled, data sizes should match expectations exactly
  set.seed(444)
  n <- 500
  test_data <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    target = c(rep(FALSE, 400), rep(TRUE, 100))
  )

  model_df <- test_data %>%
    build_lm.fast(target, x1, x2,
                  model_type = "glm",
                  test_rate = 0.3,
                  smote = FALSE,  # No SMOTE
                  seed = 444)

  source_data <- model_df$source.data[[1]]
  test_index <- model_df$.test_index[[1]]

  # Verify source.data is original size
  expect_equal(nrow(source_data), n)
  expect_false("synthesized" %in% colnames(source_data))

  # Verify test data size is approximately 30%
  test_data_actual <- source_data[test_index, ]
  expected_test_size <- round(n * 0.3)
  expect_gte(nrow(test_data_actual), expected_test_size - 20)
  expect_lte(nrow(test_data_actual), expected_test_size + 20)

  # Verify training data size is approximately 70%
  train_data <- source_data[-test_index, ]
  expected_train_size <- n - nrow(test_data_actual)
  expect_gte(nrow(train_data), expected_train_size - 20)
  expect_lte(nrow(train_data), expected_train_size + 20)
})

test_that("Train/test split works correctly with very few rows (3 rows: 1 test, 2 training)", {
  # Test edge case: minimal dataset with only 3 rows
  # This verifies that the split logic works even with very small datasets
  # Using 3 rows with rate 0.34 ensures nrow(df) * rate = 1.02, which truncates to 1 test row
  # (sample() truncates fractional sizes, so 1.02 becomes 1)
  set.seed(555)
  n <- 3
  test_data <- data.frame(
    x1 = c(1, 2, 3),
    x2 = c(10, 20, 30),
    # Create binary target with both classes present
    target = c(FALSE, TRUE, FALSE)
  )

  # Test with SMOTE disabled first (SMOTE might not work with only 2 training rows)
  model_df <- test_data %>%
    build_lm.fast(target, x1, x2,
                  model_type = "glm",
                  test_rate = 0.34,  # Should result in 1 test row (3 * 0.34 = 1.02, truncates to 1), 2 training rows
                  smote = FALSE,  # Disable SMOTE for this minimal case
                  seed = 555)

  source_data <- model_df$source.data[[1]]
  test_index <- model_df$.test_index[[1]]

  # Verify source.data has all 3 rows
  expect_equal(nrow(source_data), n)

  # Verify test_index is not empty and has exactly 1 row
  expect_true(length(test_index) >= 1)

  # Verify test data has exactly 1 row
  test_data_actual <- source_data[test_index, , drop = FALSE]
  expect_equal(nrow(test_data_actual), 1)

  # Verify training data has exactly 2 rows
  train_data <- source_data[-test_index, , drop = FALSE]
  expect_equal(nrow(train_data), 2)

  # Verify no row overlap between test and training
  expect_equal(length(intersect(test_index, seq_len(n)[-test_index])), 0)

  # Verify all rows are accounted for
  expect_equal(nrow(test_data_actual) + nrow(train_data), n)

  # Verify test_index is valid (between 1 and n)
  expect_true(all(test_index >= 1 & test_index <= n))
  expect_true(length(test_index) == 1)  # Exactly 1 test row
})

test_that("Train/test split works with very few rows using XGBoost", {
  # Test the same minimal case with a different model function
  # Using 3 rows with rate 0.34 ensures 1 test row (3 * 0.34 = 1.02, truncates to 1)
  set.seed(666)
  n <- 3
  test_data <- data.frame(
    x1 = c(1, 2, 3),
    x2 = c(10, 20, 30),
    target = c(FALSE, TRUE, FALSE)
  )

  model_df <- test_data %>%
    exp_xgboost(target, x1, x2,
                test_rate = 0.34,  # 3 * 0.34 = 1.02, truncates to 1 test row
                smote = FALSE,  # Disable SMOTE for minimal case
                seed = 666)

  source_data <- model_df$source.data[[1]]
  test_index <- model_df$.test_index[[1]]

  # Verify split works correctly
  expect_equal(nrow(source_data), n)

  # Verify test_index is not empty
  expect_true(length(test_index) >= 1)

  test_data_actual <- source_data[test_index, , drop = FALSE]
  train_data <- source_data[-test_index, , drop = FALSE]

  # Verify exact split: 1 test, 2 training
  expect_equal(nrow(test_data_actual), 1)
  expect_equal(nrow(train_data), 2)
  expect_equal(nrow(test_data_actual) + nrow(train_data), n)

  # Verify test_index is valid
  expect_true(all(test_index >= 1 & test_index <= n))
  expect_true(length(test_index) == 1)
})

test_that("Train/test split with very few rows and SMOTE (should handle gracefully)", {
  # Test that SMOTE is handled gracefully when training data is too small
  # SMOTE might not apply with only 2 training rows, but split should still work
  # Using 3 rows with rate 0.34 ensures 1 test row (3 * 0.34 = 1.02, truncates to 1)
  set.seed(777)
  n <- 3
  test_data <- data.frame(
    x1 = c(1, 2, 3),
    x2 = c(10, 20, 30),
    target = c(FALSE, TRUE, FALSE)
  )

  # Try with SMOTE enabled - it might not apply due to small size, but should not error
  model_df <- test_data %>%
    build_lm.fast(target, x1, x2,
                  model_type = "glm",
                  test_rate = 0.34,  # 3 * 0.34 = 1.02, truncates to 1 test row
                  smote = TRUE,
                  smote_keep_synthetic = FALSE,
                  seed = 777)

  source_data <- model_df$source.data[[1]]
  test_index <- model_df$.test_index[[1]]

  # Verify split still works (SMOTE might not have applied due to small size)
  expect_equal(nrow(source_data), n)

  # Verify test_index is not empty
  expect_true(length(test_index) >= 1)

  test_data_actual <- source_data[test_index, , drop = FALSE]
  train_data <- source_data[-test_index, , drop = FALSE]

  # Verify exact split: 1 test, 2 training
  expect_equal(nrow(test_data_actual), 1)
  expect_equal(nrow(train_data), 2)

  # With smote_keep_synthetic = FALSE, no synthesized column should exist
  # (SMOTE likely didn't apply due to small training size)
  expect_false("synthesized" %in% colnames(source_data))
})

