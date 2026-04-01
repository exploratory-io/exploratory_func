context("Reproduction of issue #34541: build_lr fails with factor that has unused levels")

# Root cause:
# build_lr uses levels() to check the number of unique values in the target variable.
# levels() returns ALL factor levels, including unused ones (i.e., levels that were
# defined but have no corresponding rows after filtering).
# This causes a false "outcome column has to have 2 unique values" error even when
# the data truly has only 2 unique values.
#
# build_lm.fast correctly uses unique() instead, which only counts values actually present.

test_that("build_lr works when target factor has unused levels (3 defined, 2 present)", {
  # Simulate a common real-world scenario:
  # A dataset is loaded with a 3-class outcome, then filtered to 2 classes.
  # After filtering, the factor column still retains all 3 original levels.

  set.seed(42)
  n <- 60

  df_full <- data.frame(
    outcome = factor(
      sample(c("ClassA", "ClassB", "ClassC"), n, replace = TRUE),
      levels = c("ClassA", "ClassB", "ClassC")
    ),
    predictor = rnorm(n)
  )

  # Filter to only 2 classes. The factor column still has 3 levels.
  df_filtered <- df_full[df_full$outcome %in% c("ClassA", "ClassB"), ]

  # Confirm the setup: 3 defined levels, but only 2 actually present
  expect_equal(length(levels(df_filtered$outcome)), 3)
  expect_equal(length(unique(df_filtered$outcome)), 2)

  # This should NOT throw an error, because there are only 2 unique values.
  # BUG: build_lr incorrectly checks levels() instead of unique(), so it
  # throws "outcome column has to have 2 unique values" even though 2 values exist.
  model <- build_lr(df_filtered, outcome ~ predictor)
  expect_true(!is.null(model))
})

test_that("build_lr works when target factor has unused levels (4 defined, 2 present)", {
  # Another variant: starting from 4 classes, filtering to 2.
  set.seed(123)
  n <- 80

  df_full <- data.frame(
    status = factor(
      sample(c("Active", "Inactive", "Pending", "Cancelled"), n, replace = TRUE),
      levels = c("Active", "Inactive", "Pending", "Cancelled")
    ),
    score = runif(n, 0, 100)
  )

  # Keep only Active/Inactive rows for binary classification
  df_filtered <- df_full[df_full$status %in% c("Active", "Inactive"), ]

  # 4 levels defined, 2 actually present
  expect_equal(length(levels(df_filtered$status)), 4)
  expect_equal(length(unique(df_filtered$status)), 2)

  # BUG: this incorrectly errors with "outcome column has to have 2 unique values"
  model <- build_lr(df_filtered, status ~ score)
  expect_true(!is.null(model))
})
