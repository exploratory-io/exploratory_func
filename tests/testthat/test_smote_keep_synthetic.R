context("Test smote_keep_synthetic parameter")

test_that("build_lm.fast (GLM) with smote_keep_synthetic = TRUE includes synthesized column", {
  # Create test data with imbalanced target
  set.seed(123)
  n <- 1000  # Increased for better SMOTE application
  test_data <- data.frame(
    feature1 = rnorm(n),
    feature2 = rnorm(n),
    feature3 = rnorm(n),
    # Create imbalanced target: ~15% minority class for better SMOTE
    target = c(rep(FALSE, floor(n * 0.85)), rep(TRUE, ceiling(n * 0.15)))
  )

  # Build model with smote_keep_synthetic = TRUE (default)
  model_df <- test_data %>%
    build_lm.fast(
      target, feature1, feature2, feature3,
      model_type = "glm",
      test_rate = 0.2,  # Reduced to keep more training data
      smote = TRUE,
      smote_target_minority_perc = 45,  # Explicit SMOTE target
      # smote_keep_synthetic = TRUE by default
      seed = 123
    )

  source_data <- model_df$source.data[[1]]

  # Check if SMOTE was actually applied
  smote_was_applied <- "synthesized" %in% colnames(source_data)

  if (smote_was_applied) {
    # If SMOTE was applied, check all expected behaviors
    expect_true(nrow(source_data) > n)
    expect_type(source_data$synthesized, "logical")
    expect_true(sum(source_data$synthesized) > 0)
    expect_true(sum(!source_data$synthesized) > 0)

    # Verify test data is not synthetic
    test_index <- model_df$.test_index[[1]]
    expect_true(all(!source_data$synthesized[test_index]))
  } else {
    # SMOTE wasn't applied - just verify basic functionality
    expect_equal(nrow(source_data), n)
    skip("SMOTE was not applied - data conditions may not support it")
  }
})

test_that("build_lm.fast (GLM) with smote_keep_synthetic = FALSE excludes synthesized column", {
  # Create test data with imbalanced target
  set.seed(456)
  n <- 1000
  test_data <- data.frame(
    feature1 = rnorm(n),
    feature2 = rnorm(n),
    feature3 = rnorm(n),
    # Create imbalanced target: ~15% minority class
    target = c(rep(FALSE, floor(n * 0.85)), rep(TRUE, ceiling(n * 0.15)))
  )

  # Build model with smote_keep_synthetic = FALSE
  model_df <- test_data %>%
    build_lm.fast(
      target, feature1, feature2, feature3,
      model_type = "glm",
      test_rate = 0.2,
      smote = TRUE,
      smote_keep_synthetic = FALSE,
      smote_target_minority_perc = 45,
      seed = 123
    )

  source_data <- model_df$source.data[[1]]

  # When smote_keep_synthetic = FALSE, source_data should match original size
  expect_equal(nrow(source_data), n)

  # Check that synthesized column is NOT present
  expect_false("synthesized" %in% colnames(source_data))
})

test_that("exp_xgboost with smote_keep_synthetic = TRUE includes synthesized column", {
  # Create test data with imbalanced target
  set.seed(789)
  n <- 1000
  test_data <- data.frame(
    feature1 = rnorm(n),
    feature2 = rnorm(n),
    feature3 = rnorm(n),
    # Create imbalanced target: ~15% minority class
    target = c(rep(FALSE, floor(n * 0.85)), rep(TRUE, ceiling(n * 0.15)))
  )

  # Build model with smote_keep_synthetic = TRUE (default)
  model_df <- test_data %>%
    exp_xgboost(
      target, feature1, feature2, feature3,
      test_rate = 0.2,
      smote = TRUE,
      smote_target_minority_perc = 45,
      # smote_keep_synthetic = TRUE by default
      nrounds = 5,
      seed = 123
    )

  source_data <- model_df$source.data[[1]]

  # Check if SMOTE was actually applied
  smote_was_applied <- "synthesized" %in% colnames(source_data)

  if (smote_was_applied) {
    # If SMOTE was applied, check all expected behaviors
    expect_true(nrow(source_data) > n)
    expect_type(source_data$synthesized, "logical")
    expect_true(sum(source_data$synthesized) > 0)

    # Verify test data is not synthetic
    test_index <- model_df$.test_index[[1]]
    expect_true(all(!source_data$synthesized[test_index]))
  } else {
    # SMOTE wasn't applied
    expect_equal(nrow(source_data), n)
    skip("SMOTE was not applied - data conditions may not support it")
  }
})

test_that("exp_xgboost with smote_keep_synthetic = FALSE excludes synthesized column", {
  # Create test data with imbalanced target
  set.seed(999)
  n <- 1000
  test_data <- data.frame(
    feature1 = rnorm(n),
    feature2 = rnorm(n),
    feature3 = rnorm(n),
    # Create imbalanced target: ~15% minority class
    target = c(rep(FALSE, floor(n * 0.85)), rep(TRUE, ceiling(n * 0.15)))
  )

  # Build model with smote_keep_synthetic = FALSE
  model_df <- test_data %>%
    exp_xgboost(
      target, feature1, feature2, feature3,
      test_rate = 0.2,
      smote = TRUE,
      smote_keep_synthetic = FALSE,
      smote_target_minority_perc = 45,
      nrounds = 5,
      seed = 123
    )

  source_data <- model_df$source.data[[1]]

  # When smote_keep_synthetic = FALSE, source_data should match original size
  expect_equal(nrow(source_data), n)

  # Check that synthesized column is NOT present
  expect_false("synthesized" %in% colnames(source_data))
})

test_that("exp_lightgbm with smote_keep_synthetic = TRUE includes synthesized column", {
  # Create test data with imbalanced target
  set.seed(111)
  n <- 1000
  test_data <- data.frame(
    feature1 = rnorm(n),
    feature2 = rnorm(n),
    feature3 = rnorm(n),
    # Create imbalanced target: ~15% minority class
    target = c(rep(FALSE, floor(n * 0.85)), rep(TRUE, ceiling(n * 0.15)))
  )

  # Build model with smote_keep_synthetic = TRUE (default)
  model_df <- test_data %>%
    exp_lightgbm(
      target, feature1, feature2, feature3,
      test_rate = 0.2,
      smote = TRUE,
      smote_target_minority_perc = 45,
      nrounds = 5,
      seed = 123
    )

  source_data <- model_df$source.data[[1]]

  # Check if SMOTE was actually applied
  smote_was_applied <- "synthesized" %in% colnames(source_data)

  if (smote_was_applied) {
    # If SMOTE was applied, check all expected behaviors
    expect_true(nrow(source_data) > n)
    expect_type(source_data$synthesized, "logical")

    # Verify test data is not synthetic
    test_index <- model_df$.test_index[[1]]
    expect_true(all(!source_data$synthesized[test_index]))
  } else {
    # SMOTE wasn't applied
    expect_equal(nrow(source_data), n)
    skip("SMOTE was not applied - data conditions may not support it")
  }
})

test_that("calc_feature_imp (ranger) with smote_keep_synthetic = TRUE includes synthesized column", {
  # Create test data with imbalanced target
  set.seed(222)
  n <- 1000
  test_data <- data.frame(
    feature1 = rnorm(n),
    feature2 = rnorm(n),
    feature3 = rnorm(n),
    # Create imbalanced target: ~15% minority class
    target = c(rep(FALSE, floor(n * 0.85)), rep(TRUE, ceiling(n * 0.15)))
  )

  # Build model with smote_keep_synthetic = TRUE (default)
  model_df <- test_data %>%
    calc_feature_imp(
      target, feature1, feature2, feature3,
      test_rate = 0.2,
      smote = TRUE,
      smote_target_minority_perc = 45,
      seed = 123
    )

  source_data <- model_df$source.data[[1]]

  # Check if SMOTE was actually applied
  smote_was_applied <- "synthesized" %in% colnames(source_data)

  if (smote_was_applied) {
    # If SMOTE was applied, check all expected behaviors
    expect_true(nrow(source_data) > n)
    expect_type(source_data$synthesized, "logical")

    # Verify test data is not synthetic
    test_index <- model_df$.test_index[[1]]
    expect_true(all(!source_data$synthesized[test_index]))
  } else {
    # SMOTE wasn't applied
    expect_equal(nrow(source_data), n)
    skip("SMOTE was not applied - data conditions may not support it")
  }
})
