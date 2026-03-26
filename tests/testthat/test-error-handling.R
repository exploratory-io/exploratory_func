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
    exploratory:::iterate_kmeans(df, max_centers = 5),
    "centers="
  )
})




# =============================================================================
# Phase 2: XGBoost error handling tests
# =============================================================================





# =============================================================================
# Phase 3: LightGBM error handling tests
# =============================================================================




# =============================================================================
# Phase 4: Rpart error handling tests
# =============================================================================

test_that("calc_permutation_importance_rpart_regression reports error with variable context", {
  skip_if_not_installed("mmpf")

  df <- mtcars[1:30, c("mpg", "hp", "wt")]
  fit <- rpart::rpart(mpg ~ hp + wt, df)

  expect_error_with_context(
    exploratory:::calc_permutation_importance_rpart_regression(fit, "mpg", "nonexistent_var", df),
    "nonexistent_var"
  )
})

test_that("calc_permutation_importance_rpart_binary reports error with variable context", {
  skip_if_not_installed("mmpf")

  df <- mtcars[1:30, ]
  df$am <- as.logical(df$am)
  fit <- rpart::rpart(am ~ mpg + hp + wt, df)

  expect_error_with_context(
    exploratory:::calc_permutation_importance_rpart_binary(fit, "am", "nonexistent_var", df),
    "nonexistent_var"
  )
})

test_that("calc_permutation_importance_rpart_multiclass reports error with variable context", {
  skip_if_not_installed("mmpf")

  df <- iris
  fit <- rpart::rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, df)

  expect_error_with_context(
    exploratory:::calc_permutation_importance_rpart_multiclass(fit, "Species", "nonexistent_var", df),
    "nonexistent_var"
  )
})

# =============================================================================
# Phase 5: Survival forest error handling tests
# =============================================================================


# =============================================================================
# Phase 6: Time series clustering error handling tests
# =============================================================================

