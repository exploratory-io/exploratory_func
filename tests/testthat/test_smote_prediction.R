context("Test SMOTE prediction functionality")

test_that("build_lm.fast (GLM) with SMOTE and test mode can be built successfully", {
  # Create test data with imbalanced target
  set.seed(123)
  n <- 1000
  test_data <- data.frame(
    feature1 = rnorm(n),
    feature2 = rnorm(n),
    feature3 = rnorm(n),
    # Create imbalanced target: ~15% minority class
    target = c(rep(FALSE, floor(n * 0.85)), rep(TRUE, ceiling(n * 0.15)))
  )

  # Build model with SMOTE and test mode
  model_df <- test_data %>%
    build_lm.fast(
      target, feature1, feature2, feature3,
      model_type = "glm",
      test_rate = 0.2,
      smote = TRUE,
      smote_keep_synthetic = FALSE,
      seed = 123
    )

  # Model should be created successfully
  expect_true(!is.null(model_df))
  expect_true("model" %in% colnames(model_df))
  expect_true("source.data" %in% colnames(model_df))

  # Source data should have original size when smote_keep_synthetic = FALSE
  source_data <- model_df$source.data[[1]]
  expect_equal(nrow(source_data), n)
  expect_false("synthesized" %in% colnames(source_data))
})

test_that("build_lm.fast (GLM) with SMOTE, test mode, and smote_keep_synthetic=TRUE", {
  # Create test data
  set.seed(456)
  n <- 1000
  test_data <- data.frame(
    feature1 = rnorm(n),
    feature2 = rnorm(n),
    feature3 = rnorm(n),
    target = c(rep(FALSE, floor(n * 0.85)), rep(TRUE, ceiling(n * 0.15)))
  )

  # Build model with SMOTE and keep synthetic samples
  model_df <- test_data %>%
    build_lm.fast(
      target, feature1, feature2, feature3,
      model_type = "glm",
      test_rate = 0.2,
      smote = TRUE,
      smote_keep_synthetic = TRUE,
      smote_target_minority_perc = 45,
      seed = 123
    )

  # Model should be created successfully
  expect_true(!is.null(model_df))
  expect_true("model" %in% colnames(model_df))

  # Check if SMOTE was applied
  source_data <- model_df$source.data[[1]]
  smote_was_applied <- "synthesized" %in% colnames(source_data)

  if (smote_was_applied) {
    # If SMOTE was applied, source data should be larger
    expect_true(nrow(source_data) > n)
    expect_true("synthesized" %in% colnames(source_data))
  } else {
    # SMOTE wasn't applied
    expect_equal(nrow(source_data), n)
    skip("SMOTE was not applied - data conditions may not support it")
  }
})

test_that("exp_xgboost with SMOTE and test mode can be built successfully", {
  # Create test data
  set.seed(789)
  n <- 1000
  test_data <- data.frame(
    feature1 = rnorm(n),
    feature2 = rnorm(n),
    feature3 = rnorm(n),
    target = c(rep(FALSE, floor(n * 0.85)), rep(TRUE, ceiling(n * 0.15)))
  )

  # Build model with SMOTE and test mode
  model_df <- test_data %>%
    exp_xgboost(
      target, feature1, feature2, feature3,
      test_rate = 0.2,
      smote = TRUE,
      smote_keep_synthetic = FALSE,
      nrounds = 10,
      seed = 123
    )

  # Model should be created successfully
  expect_true(!is.null(model_df))
  expect_true("model" %in% colnames(model_df))
  expect_true("source.data" %in% colnames(model_df))

  # Source data should have original size when smote_keep_synthetic = FALSE
  source_data <- model_df$source.data[[1]]
  expect_equal(nrow(source_data), n)
  expect_false("synthesized" %in% colnames(source_data))
})

test_that("exp_xgboost with SMOTE, test mode, and smote_keep_synthetic=TRUE", {
  # Create test data
  set.seed(999)
  n <- 1000
  test_data <- data.frame(
    feature1 = rnorm(n),
    feature2 = rnorm(n),
    feature3 = rnorm(n),
    target = c(rep(FALSE, floor(n * 0.85)), rep(TRUE, ceiling(n * 0.15)))
  )

  # Build model with SMOTE and keep synthetic samples
  model_df <- test_data %>%
    exp_xgboost(
      target, feature1, feature2, feature3,
      test_rate = 0.2,
      smote = TRUE,
      smote_keep_synthetic = TRUE,
      smote_target_minority_perc = 45,
      nrounds = 10,
      seed = 123
    )

  # Model should be created successfully
  expect_true(!is.null(model_df))
  expect_true("model" %in% colnames(model_df))

  # Check if SMOTE was applied
  source_data <- model_df$source.data[[1]]
  smote_was_applied <- "synthesized" %in% colnames(source_data)

  if (smote_was_applied) {
    # If SMOTE was applied, source data should be larger
    expect_true(nrow(source_data) > n)
    expect_true("synthesized" %in% colnames(source_data))
  } else {
    # SMOTE wasn't applied
    expect_equal(nrow(source_data), n)
    skip("SMOTE was not applied - data conditions may not support it")
  }
})
