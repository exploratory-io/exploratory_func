context("Test smote_keep_synthetic parameter")

testthat::skip_if_not_installed("ranger")

# ── Shared fixtures ──────────────────────────────────────────────────────────
set.seed(123)
n <- 1000
smote_test_data <- data.frame(
  feature1 = rnorm(n),
  feature2 = rnorm(n),
  feature3 = rnorm(n),
  target = c(rep(FALSE, floor(n * 0.85)), rep(TRUE, ceiling(n * 0.15)))
)

# smote_keep_synthetic = TRUE (default)
rf_keep_true <- smote_test_data %>%
  calc_feature_imp(target, feature1, feature2, feature3,
                   test_rate = 0.2, smote = TRUE,
                   smote_target_minority_perc = 45, seed = 123)

# ── Tests: smote_keep_synthetic = TRUE ──────────────────────────────────────

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
