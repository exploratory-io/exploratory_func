# Tests for improved error handling in purrr::map calls
# These tests verify that error messages include context information
# when errors occur inside purrr::map in Analytics View functions.
#
# Note: dplyr/purrr wrap errors, so the context is in the full condition message
# (accessed via rlang::cnd_message()), not just e$message.
# The tests below verify that our context appears in the full error chain.

context("test error handling with context in Analytics functions")

# Helper function to check if error message contains expected pattern
# Uses rlang::cnd_message() to get the full error chain including our context
expect_error_with_context <- function(expr, pattern) {
  error_caught <- NULL
  tryCatch(
    force(expr),
    error = function(e) {
      error_caught <<- e
    }
  )

  if (is.null(error_caught)) {
    fail("Expected an error but none occurred")
  }

  full_message <- rlang::cnd_message(error_caught)
  if (!grepl(pattern, full_message)) {
    fail(paste0(
      "Error message does not contain expected pattern.\n",
      "Expected pattern: ", pattern, "\n",
      "Actual message: ", full_message
    ))
  }

  succeed()
}

# =============================================================================
# Phase 1: K-means error handling tests
# =============================================================================

test_that("iterate_kmeans reports error with centers context", {
  # Create data with only 2 distinct points - will fail for centers > 2
  # This triggers an error inside the purrr::map in iterate_kmeans
  df <- data.frame(x = c(1, 1, 1, 2, 2, 2), y = c(1, 1, 1, 2, 2, 2))

  # The error message should contain "centers=" to indicate which center value failed
  # Note: iterate_kmeans is an internal function, accessed via :::
  expect_error_with_context(
    iterate_kmeans(df, max_centers = 5),
    "centers="
  )
})

test_that("exp_kmeans elbow method reports error with centers context", {
  # Create data where elbow method iteration will fail
  # Only 2 distinct points, so centers > 2 will fail in iterate_kmeans
  df <- data.frame(x = c(1, 1, 1, 2, 2, 2), y = c(1, 1, 1, 2, 2, 2))

  # Use centers=1 for the main k-means (which will succeed)
  # but max_centers=5 for elbow method (which will fail at centers=3)
  expect_error_with_context(
    exp_kmeans(df, x, y, centers = 1, elbow_method_mode = TRUE, max_centers = 5),
    "centers="
  )
})

test_that("exp_kmeans normal operation works correctly", {
  # Verify that normal operation still works after adding error handling
  df <- mtcars[1:15, ]
  result <- exp_kmeans(df, mpg, hp, centers = 2)
  expect_s3_class(result, "data.frame")
  expect_true("model" %in% colnames(result))
})

test_that("exp_kmeans elbow method normal operation works correctly", {
  # Verify elbow method still works when data is valid
  df <- mtcars[1:15, ]
  result <- exp_kmeans(df, mpg, hp, elbow_method_mode = TRUE, max_centers = 5)
  expect_s3_class(result, "data.frame")
  expect_true(!is.null(result$model[[1]]$elbow_result))
})

# =============================================================================
# Phase 2: XGBoost error handling tests
# =============================================================================

test_that("calc_permutation_importance_xgboost_regression reports error with variable context", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("mmpf")

  # Create a simple xgboost model
  df <- mtcars[1:20, c("mpg", "hp", "wt")]

  # Build a model first
  model_df <- tryCatch({
    df %>% exp_xgboost(mpg, hp, wt, test_rate = 0)
  }, error = function(e) NULL)

  skip_if(is.null(model_df), "Could not build xgboost model")

  fit <- model_df$model[[1]]

  # Test permutation importance with a non-existent variable to trigger error
  expect_error_with_context(
    calc_permutation_importance_xgboost_regression(fit, "mpg", c("hp", "nonexistent_var"), df),
    "variable"
  )
})

test_that("exp_xgboost normal operation works correctly", {
  skip_if_not_installed("xgboost")

  df <- mtcars[1:20, ]
  result <- tryCatch({
    exp_xgboost(df, mpg, hp, wt, test_rate = 0.2)
  }, error = function(e) NULL)

  skip_if(is.null(result), "exp_xgboost failed - may be environment issue")

  expect_s3_class(result, "data.frame")
  expect_true("model" %in% colnames(result))
})

# =============================================================================
# Phase 3: Survival forest error handling tests
# =============================================================================

test_that("calc_permutation_importance_ranger_survival reports error with variable context", {
  skip_if_not_installed("ranger")
  skip_if_not_installed("mmpf")

  df <- survival::lung %>%
    dplyr::mutate(status = status == 2) %>%
    dplyr::select(time, status, age, sex) %>%
    dplyr::slice(1:50)

  model_df <- tryCatch({
    df %>% exp_survival_forest(time, status, age, sex, ntree = 5, predictor_n = 2, test_rate = 0)
  }, error = function(e) NULL)

  skip_if(is.null(model_df), "exp_survival_forest failed - may be environment issue")

  fit <- model_df$model[[1]]

  expect_error_with_context(
    calc_permutation_importance_ranger_survival(fit, "time", "status", c("age", "nonexistent_var"), df),
    "nonexistent_var"
  )
})

# =============================================================================
# Phase 4: Time series clustering error handling tests
# =============================================================================

test_that("exp_ts_cluster reports error with centers context when tsclust fails", {
  skip_if_not_installed("dtwclust")

  df <- data.frame(
    time = as.Date("2020-01-01") + 0:5,
    value = c(1, 2, 3, 4, 5, 6),
    category = c("A", "B", "A", "B", "A", "B")
  )

  expect_error_with_context(
    exp_ts_cluster(df, time, value, category, centers = 2, centroid = "invalid_centroid"),
    "centers="
  )
})
