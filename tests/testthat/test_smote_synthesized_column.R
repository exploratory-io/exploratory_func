context("Test synthesized column name preservation")



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




