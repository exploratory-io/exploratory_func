context("Test synthesized column name preservation")

test_that("exp_xgboost: synthesized column name is preserved with special character columns", {
  # Create test data with columns that require name mapping (special characters)
  n <- 500
  set.seed(456)
  test_data <- data.frame(
    `special col!` = rnorm(n),
    `another-col` = rnorm(n),
    `col with spaces` = rnorm(n),
    target = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.2, 0.8)),
    stringsAsFactors = FALSE,
    check.names = FALSE  # Preserve special characters
  )

  # Build model with SMOTE and test mode
  model_df <- test_data %>%
    exp_xgboost(target, `special col!`, `another-col`, `col with spaces`,
                test_rate = 0.3, smote = TRUE, smote_keep_synthetic = TRUE,
                seed = 123, nrounds = 10)

  source_data <- model_df$source.data[[1]]

  # Check that synthesized column exists and has correct name
  expect_true("synthesized" %in% colnames(source_data))
  expect_false(any(is.na(colnames(source_data))))

  # Check that original column names are preserved
  expect_true("special col!" %in% colnames(source_data))
  expect_true("another-col" %in% colnames(source_data))
  expect_true("col with spaces" %in% colnames(source_data))
  expect_true("target" %in% colnames(source_data))

  # Check that synthesized column has correct values
  expect_type(source_data$synthesized, "logical")
  test_index <- model_df$.test_index[[1]]
  train_data <- source_data[-test_index, ]
  test_data_actual <- source_data[test_index, ]

  # Test data should all be FALSE (not synthesized)
  expect_true(all(test_data_actual$synthesized == FALSE))

  # Training data should have some TRUE (synthesized samples)
  expect_true(any(train_data$synthesized == TRUE))
})

test_that("exp_lightgbm: synthesized column name is preserved with special character columns", {
  # Create test data with columns that require name mapping
  n <- 500
  set.seed(789)
  test_data <- data.frame(
    `x 1` = rnorm(n),
    `x-2` = rnorm(n),
    `x!3` = rnorm(n),
    target = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.2, 0.8)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Build model with SMOTE and test mode
  model_df <- test_data %>%
    exp_lightgbm(target, `x 1`, `x-2`, `x!3`,
                 test_rate = 0.3, smote = TRUE, smote_keep_synthetic = TRUE,
                 seed = 456, nrounds = 10)

  source_data <- model_df$source.data[[1]]

  # Check that synthesized column exists and has correct name
  expect_true("synthesized" %in% colnames(source_data))
  expect_false(any(is.na(colnames(source_data))))

  # Check that original column names are preserved
  expect_true("x 1" %in% colnames(source_data))
  expect_true("x-2" %in% colnames(source_data))
  expect_true("x!3" %in% colnames(source_data))
  expect_true("target" %in% colnames(source_data))

  # Check that synthesized column has correct values
  expect_type(source_data$synthesized, "logical")
})

test_that("calc_feature_imp (ranger): synthesized column name is preserved with special character columns", {
  # Create test data with columns that require name mapping
  n <- 500
  set.seed(111)
  test_data <- data.frame(
    `pred@1` = rnorm(n),
    `pred#2` = rnorm(n),
    `pred$3` = rnorm(n),
    target = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.2, 0.8)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Build model with SMOTE and test mode
  model_df <- test_data %>%
    calc_feature_imp(target, `pred@1`, `pred#2`, `pred$3`,
                     test_rate = 0.3, smote = TRUE, smote_keep_synthetic = TRUE,
                     seed = 222)

  source_data <- model_df$source.data[[1]]

  # Check that synthesized column exists and has correct name
  expect_true("synthesized" %in% colnames(source_data))
  expect_false(any(is.na(colnames(source_data))))

  # Check that original column names are preserved
  expect_true("pred@1" %in% colnames(source_data))
  expect_true("pred#2" %in% colnames(source_data))
  expect_true("pred$3" %in% colnames(source_data))
  expect_true("target" %in% colnames(source_data))

  # Check that synthesized column has correct values
  expect_type(source_data$synthesized, "logical")
})

test_that("exp_xgboost: no NA column names when smote_keep_synthetic = FALSE", {
  # Create test data with special character columns
  n <- 500
  set.seed(999)
  test_data <- data.frame(
    `col-1` = rnorm(n),
    `col 2` = rnorm(n),
    target = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.2, 0.8)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Build model with SMOTE but keep_synthetic = FALSE
  model_df <- test_data %>%
    exp_xgboost(target, `col-1`, `col 2`,
                test_rate = 0.3, smote = TRUE, smote_keep_synthetic = FALSE,
                seed = 888, nrounds = 10)

  source_data <- model_df$source.data[[1]]

  # Check that no column names are NA
  expect_false(any(is.na(colnames(source_data))))

  # Check that original column names are preserved
  expect_true("col-1" %in% colnames(source_data))
  expect_true("col 2" %in% colnames(source_data))
  expect_true("target" %in% colnames(source_data))

  # synthesized column should NOT exist when smote_keep_synthetic = FALSE
  expect_false("synthesized" %in% colnames(source_data))
})

test_that("exp_xgboost: column names are correct when no SMOTE", {
  # Create test data with special character columns
  n <- 500
  set.seed(777)
  test_data <- data.frame(
    `var!1` = rnorm(n),
    `var@2` = rnorm(n),
    target = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.5, 0.5)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Build model without SMOTE
  model_df <- test_data %>%
    exp_xgboost(target, `var!1`, `var@2`,
                test_rate = 0.3, smote = FALSE,
                seed = 666, nrounds = 10)

  source_data <- model_df$source.data[[1]]

  # Check that no column names are NA
  expect_false(any(is.na(colnames(source_data))))

  # Check that original column names are preserved
  expect_true("var!1" %in% colnames(source_data))
  expect_true("var@2" %in% colnames(source_data))
  expect_true("target" %in% colnames(source_data))

  # synthesized column should NOT exist when smote = FALSE
  expect_false("synthesized" %in% colnames(source_data))
})

test_that("Multiple special characters in one column name are handled correctly", {
  # Create test data with very complex column names
  n <- 500
  set.seed(555)
  test_data <- data.frame(
    `col with spaces & special!@#` = rnorm(n),
    `another-complex_name (test)` = rnorm(n),
    `simple` = rnorm(n),
    target = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.2, 0.8)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Build model with SMOTE
  model_df <- test_data %>%
    exp_xgboost(target, `col with spaces & special!@#`,
                `another-complex_name (test)`, simple,
                test_rate = 0.3, smote = TRUE, smote_keep_synthetic = TRUE,
                seed = 444, nrounds = 10)

  source_data <- model_df$source.data[[1]]

  # Check that no column names are NA
  expect_false(any(is.na(colnames(source_data))))

  # Check that all original column names are preserved exactly
  expect_true("col with spaces & special!@#" %in% colnames(source_data))
  expect_true("another-complex_name (test)" %in% colnames(source_data))
  expect_true("simple" %in% colnames(source_data))
  expect_true("target" %in% colnames(source_data))
  expect_true("synthesized" %in% colnames(source_data))

  # Verify we have exactly the expected columns
  expected_cols <- c("col with spaces & special!@#",
                     "another-complex_name (test)",
                     "simple",
                     "target",
                     "synthesized")
  expect_setequal(colnames(source_data), expected_cols)
})

