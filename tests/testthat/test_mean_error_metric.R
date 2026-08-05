# how to run this test:
# devtools::test(filter="mean_error_metric")
#
# Mean Error (ME) = mean(actual - predicted). Unlike MAE / RMSE it keeps the sign,
# so a positive value means the model under-predicts on average and a negative value
# means it over-predicts. The numeric Analytics Report templates document it, so the
# regression Summary tables have to actually produce a "Mean Error" column
# (tam#37510).
#
# It rides on the existing report_metrics opt-in, so the invariants under test are:
#   1. the value is correct (hand-computed on a fixture),
#   2. it is present and non-NA on BOTH the training and the test row (those come
#      from two different code paths -- glance.<model>() vs the model-agnostic test
#      branch in rf_evaluation_training_and_test() -- so a one-sided change silently
#      yields NA on the other row instead of an error),
#   3. the default output (report_metrics = FALSE) is completely unchanged.

context("Mean Error metric for regression models (tam#37510)")

make_regression_df <- function(n = 200, seed = 42) {
  set.seed(seed)
  dplyr::tibble(
    x1 = rnorm(n),
    x2 = rnorm(n)
  ) %>%
    dplyr::mutate(y = 3 * x1 - 2 * x2 + rnorm(n, sd = 0.5))
}

# The columns report_metrics adds on top of the default regression Summary table.
expected_added_regression <- c("MAE", "Mean Error")

# Runs the shared Summary-table entry point with and without report_metrics and
# asserts all three invariants above for a single model.
# expected_rows is 1 when there is no test data and 2 (training + test) otherwise.
expect_mean_error_metric <- function(model_df, label, expected_rows) {
  base <- rf_evaluation_training_and_test(model_df, pretty.name = TRUE)
  with_metrics <- rf_evaluation_training_and_test(model_df, pretty.name = TRUE,
                                                  report_metrics = TRUE)

  # (1)+(2) Mean Error exists and is a real number on EVERY row, including the test
  # row, which is produced by a different code path than the training row.
  expect_true("Mean Error" %in% colnames(with_metrics), info = label)
  expect_true(is.numeric(with_metrics$`Mean Error`), info = label)
  expect_equal(nrow(with_metrics), expected_rows, info = label)
  expect_false(any(is.na(with_metrics$`Mean Error`)), info = label)

  # (3) The default output is untouched -- neither a new column nor a changed value.
  expect_false(any(expected_added_regression %in% colnames(base)), info = label)
  kept <- intersect(colnames(base), colnames(with_metrics))
  expect_equal(as.data.frame(base)[, kept, drop = FALSE],
               as.data.frame(with_metrics)[, kept, drop = FALSE], info = label)
}

test_that("mean_error() returns mean(actual - predicted) with the sign preserved", {
  # Hand-computed: actual - predicted = c(1, -3, 2, 0) -> sum 0 -> mean 0.
  expect_equal(mean_error(c(10, 20, 30, 40), c(9, 23, 28, 40)), 0)

  # Under-prediction: every prediction is 2 too low -> ME = +2.
  expect_equal(mean_error(c(10, 20, 30), c(8, 18, 28)), 2)

  # Over-prediction: every prediction is 5 too high -> ME = -5.
  expect_equal(mean_error(c(10, 20, 30), c(15, 25, 35)), -5)

  # Errors cancel out even though the magnitudes are large. This is what makes
  # Mean Error different from MAE, and is the whole reason it is reported.
  expect_equal(mean_error(c(0, 100), c(-50, 150)), 0)
  expect_equal(mae(c(0, 100), c(-50, 150)), 50)

  # NA handling matches mae() / rmse(): NAs are dropped.
  # actual - predicted = c(1, NA, 3) -> mean of c(1, 3) = 2.
  expect_equal(mean_error(c(1, 2, 5), c(0, NA, 2)), 2)

  # is_test_data subsets both vectors, mirroring mae() / rmse().
  # Only the last two rows are kept: (30-25) and (40-38) -> mean 3.5.
  expect_equal(mean_error(c(10, 20, 30, 40), c(0, 0, 25, 38),
                          is_test_data = c(FALSE, FALSE, TRUE, TRUE)), 3.5)
})

test_that("exp_rpart Summary reports Mean Error on both training and test rows", {
  df <- make_regression_df()

  for (test_rate in c(0, 0.3)) {
    model_df <- df %>% exp_rpart(y, x1, x2, test_rate = test_rate)
    label <- paste0("rpart test_rate=", test_rate)

    base <- rf_evaluation_training_and_test(model_df, pretty.name = TRUE)
    with_metrics <- rf_evaluation_training_and_test(model_df, pretty.name = TRUE,
                                                    report_metrics = TRUE)

    expect_true(all(expected_added_regression %in% colnames(with_metrics)), info = label)
    # The training row and the test row are built by two different code paths, so
    # assert on BOTH rather than only on the first one.
    expect_equal(nrow(with_metrics), if (test_rate > 0) 2 else 1, info = label)
    expect_false(any(is.na(with_metrics$`Mean Error`)), info = label)

    # Default output unchanged.
    expect_false(any(expected_added_regression %in% colnames(base)), info = label)
    kept <- intersect(colnames(base), colnames(with_metrics))
    expect_equal(as.data.frame(base)[, kept, drop = FALSE],
                 as.data.frame(with_metrics)[, kept, drop = FALSE], info = label)
  }
})

test_that("glance.rpart Mean Error equals mean(actual - predicted) on the training data", {
  df <- make_regression_df()
  model_df <- df %>% exp_rpart(y, x1, x2, test_rate = 0)
  model <- model_df$model[[1]]

  ret <- glance(model, pretty.name = TRUE, report_metrics = TRUE)
  expect_equal(ret$`Mean Error`, mean(model$y - predict(model)))

  # Non-pretty output uses the snake_case name, like mean_absolute_error.
  raw <- glance(model, pretty.name = FALSE, report_metrics = TRUE)
  expect_true("mean_error" %in% colnames(raw))
  expect_equal(raw$mean_error, mean(model$y - predict(model)))
})

test_that("calc_feature_imp (ranger) Summary reports Mean Error", {
  df <- make_regression_df()
  for (test_rate in c(0, 0.3)) {
    model_df <- df %>% calc_feature_imp(y, x1, x2, test_rate = test_rate)
    expect_mean_error_metric(model_df, paste0("ranger test_rate=", test_rate), if (test_rate > 0) 2 else 1)
  }
})

test_that("exp_xgboost Summary reports Mean Error", {
  skip_if_not_installed("xgboost")
  df <- make_regression_df()
  for (test_rate in c(0, 0.3)) {
    model_df <- suppressWarnings(df %>% exp_xgboost(y, x1, x2, test_rate = test_rate, nrounds = 15))
    expect_mean_error_metric(model_df, paste0("xgboost test_rate=", test_rate), if (test_rate > 0) 2 else 1)
  }
})

test_that("exp_lightgbm Summary reports Mean Error", {
  skip_if_not_installed("lightgbm")
  df <- make_regression_df()
  for (test_rate in c(0, 0.3)) {
    model_df <- suppressWarnings(df %>% exp_lightgbm(y, x1, x2, test_rate = test_rate, nrounds = 15))
    expect_mean_error_metric(model_df, paste0("lightgbm test_rate=", test_rate), if (test_rate > 0) 2 else 1)
  }
})

test_that("exp_catboost Summary reports Mean Error", {
  skip_if_not_installed("catboost")
  df <- make_regression_df()
  for (test_rate in c(0, 0.3)) {
    model_df <- suppressWarnings(df %>% exp_catboost(y, x1, x2, test_rate = test_rate, iterations = 15))
    expect_mean_error_metric(model_df, paste0("catboost test_rate=", test_rate), if (test_rate > 0) 2 else 1)
  }
})
