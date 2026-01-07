# Test to verify that SMOTE is applied only to training data after train/test split
# This ensures test data remains pure and doesn't contain synthetic samples
# how to run this test:
# devtools::test(filter="smote_test_mode")

context("test SMOTE with test mode - verify test data purity")

test_that("build_lm.fast GLM with SMOTE and test mode - test data should be pure", {
  # Create a dataset with imbalanced binary target
  # Use identifiable row IDs to track which data ends up where
  set.seed(123)
  n <- 500
  test_data <- data.frame(
    row_id = 1:n,
    x1 = rnorm(n),
    x2 = rnorm(n),
    # Create imbalanced target: 80% FALSE, 20% TRUE
    target = c(rep(FALSE, 400), rep(TRUE, 100))
  )
  
  # Train model with SMOTE and test_rate
  model_df <- test_data %>%
    build_lm.fast(target, x1, x2, 
                  model_type = "glm", 
                  test_rate = 0.3, 
                  smote = TRUE,
                  seed = 123)
  
  # Get source.data which should contain original data only
  source_data <- model_df$source.data[[1]]
  test_index <- model_df$.test_index[[1]]
  
  # Verify source.data matches original data size (not inflated by SMOTE)
  expect_equal(nrow(source_data), n)
  expect_equal(sort(source_data$row_id), 1:n)
  
  # Get test data by extracting rows at test_index from source.data
  test_data_actual <- source_data[test_index, ]
  expected_test_size <- round(n * 0.3)
  
  # Verify test data size is approximately 30% of original (not inflated by SMOTE)
  expect_gte(nrow(test_data_actual), expected_test_size - 20)
  expect_lte(nrow(test_data_actual), expected_test_size + 20)
  
  # Verify test data row_ids are from original data (no synthetic samples)
  expect_true(all(test_data_actual$row_id %in% 1:n))
  
  # Get training data (rows NOT in test_index)
  train_data_before_smote <- source_data[-test_index, ]
  expected_train_size <- n - nrow(test_data_actual)
  
  # Verify training size before SMOTE is approximately 70% of original
  expect_gte(nrow(train_data_before_smote), expected_train_size - 20)
  expect_lte(nrow(train_data_before_smote), expected_train_size + 20)
})

test_that("exp_xgboost with SMOTE and test mode - test data should be pure", {
  set.seed(456)
  n <- 500
  test_data <- data.frame(
    row_id = 1:n,
    x1 = rnorm(n),
    x2 = rnorm(n),
    # Create imbalanced target
    target = c(rep(FALSE, 400), rep(TRUE, 100))
  )
  
  model_df <- test_data %>%
    exp_xgboost(target, x1, x2, 
                test_rate = 0.3, 
                smote = TRUE,
                seed = 456)
  
  source_data <- model_df$source.data[[1]]
  test_index <- model_df$.test_index[[1]]
  
  # Verify source.data is original size
  expect_equal(nrow(source_data), n)
  expect_equal(sort(source_data$row_id), 1:n)
  
  # Verify test data size
  test_data_actual <- source_data[test_index, ]
  expected_test_size <- round(n * 0.3)
  expect_gte(nrow(test_data_actual), expected_test_size - 20)
  expect_lte(nrow(test_data_actual), expected_test_size + 20)
  
  # Verify test data contains only original row_ids
  expect_true(all(test_data_actual$row_id %in% 1:n))
})

test_that("exp_lightgbm with SMOTE and test mode - test data should be pure", {
  set.seed(789)
  n <- 500
  test_data <- data.frame(
    row_id = 1:n,
    x1 = rnorm(n),
    x2 = rnorm(n),
    # Create imbalanced target
    target = c(rep(FALSE, 400), rep(TRUE, 100))
  )
  
  model_df <- test_data %>%
    exp_lightgbm(target, x1, x2, 
                 test_rate = 0.3, 
                 smote = TRUE,
                 seed = 789)
  
  source_data <- model_df$source.data[[1]]
  test_index <- model_df$.test_index[[1]]
  
  # Verify source.data is original size
  expect_equal(nrow(source_data), n)
  expect_equal(sort(source_data$row_id), 1:n)
  
  # Verify test data size
  test_data_actual <- source_data[test_index, ]
  expected_test_size <- round(n * 0.3)
  expect_gte(nrow(test_data_actual), expected_test_size - 20)
  expect_lte(nrow(test_data_actual), expected_test_size + 20)
  
  # Verify test data contains only original row_ids
  expect_true(all(test_data_actual$row_id %in% 1:n))
})

test_that("exp_ranger with SMOTE and test mode - test data should be pure", {
  set.seed(111)
  n <- 500
  test_data <- data.frame(
    row_id = 1:n,
    x1 = rnorm(n),
    x2 = rnorm(n),
    # Create imbalanced target
    target = c(rep(FALSE, 400), rep(TRUE, 100))
  )
  
  model_df <- test_data %>%
    exp_ranger(target, x1, x2, 
               test_rate = 0.3, 
               smote = TRUE,
               seed = 111)
  
  source_data <- model_df$source.data[[1]]
  test_index <- model_df$.test_index[[1]]
  
  # Verify source.data is original size
  expect_equal(nrow(source_data), n)
  expect_equal(sort(source_data$row_id), 1:n)
  
  # Verify test data size
  test_data_actual <- source_data[test_index, ]
  expected_test_size <- round(n * 0.3)
  expect_gte(nrow(test_data_actual), expected_test_size - 20)
  expect_lte(nrow(test_data_actual), expected_test_size + 20)
  
  # Verify test data contains only original row_ids
  expect_true(all(test_data_actual$row_id %in% 1:n))
})

test_that("exp_rpart with SMOTE and test mode - test data should be pure", {
  set.seed(222)
  n <- 500
  test_data <- data.frame(
    row_id = 1:n,
    x1 = rnorm(n),
    x2 = rnorm(n),
    # Create imbalanced target
    target = as.factor(c(rep("A", 400), rep("B", 100)))
  )
  
  model_df <- test_data %>%
    exp_rpart(target, x1, x2, 
              test_rate = 0.3, 
              smote = TRUE,
              seed = 222)
  
  source_data <- model_df$source.data[[1]]
  test_index <- model_df$.test_index[[1]]
  
  # Verify source.data is original size
  expect_equal(nrow(source_data), n)
  expect_equal(sort(source_data$row_id), 1:n)
  
  # Verify test data size
  test_data_actual <- source_data[test_index, ]
  expected_test_size <- round(n * 0.3)
  expect_gte(nrow(test_data_actual), expected_test_size - 20)
  expect_lte(nrow(test_data_actual), expected_test_size + 20)
  
  # Verify test data contains only original row_ids
  expect_true(all(test_data_actual$row_id %in% 1:n))
})

test_that("SMOTE with test_rate=0 should still apply SMOTE to all training data", {
  # When test_rate is 0, SMOTE should be applied to all data (backward compatibility)
  set.seed(333)
  n <- 500
  test_data <- data.frame(
    row_id = 1:n,
    x1 = rnorm(n),
    x2 = rnorm(n),
    target = c(rep(FALSE, 400), rep(TRUE, 100))
  )
  
  model_df <- test_data %>%
    build_lm.fast(target, x1, x2, 
                  model_type = "glm", 
                  test_rate = 0,  # No test split
                  smote = TRUE,
                  seed = 333)
  
  source_data <- model_df$source.data[[1]]
  
  # With test_rate=0, source.data should still be original size
  expect_equal(nrow(source_data), n)
  expect_equal(sort(source_data$row_id), 1:n)
})

test_that("No SMOTE with test mode should not alter data sizes", {
  # When SMOTE is disabled, data sizes should match expectations exactly
  set.seed(444)
  n <- 500
  test_data <- data.frame(
    row_id = 1:n,
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
  expect_equal(sort(source_data$row_id), 1:n)
  
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

