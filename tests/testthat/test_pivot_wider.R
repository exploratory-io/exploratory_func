context("test pivot_wider")

test_that("pivot_wider creates one-hot encoding when values_from is NULL", {
  df <- data.frame(
    id = c(1, 1, 2, 3),
    category = c("A", "B", "A", "C")
  )
  result <- df %>% pivot_wider(names_from = category)

  expect_equal(nrow(result), 3)  # 3 unique ids
  expect_true(all(c("A", "B", "C") %in% names(result)))
  # Values should be numeric 1 and 0
  expect_equal(result$A, c(1, 1, 0))
  expect_equal(result$B, c(1, 0, 0))
  expect_equal(result$C, c(0, 0, 1))
  # Verify values are numeric, not integer
  expect_true(is.numeric(result$A))
})

test_that("pivot_wider works normally when values_from is provided", {
  df <- data.frame(
    id = c(1, 1),
    key = c("x", "y"),
    value = c(10, 20)
  )
  result <- df %>% pivot_wider(names_from = key, values_from = value)

  expect_equal(nrow(result), 1)
  expect_equal(result$x, 10)
  expect_equal(result$y, 20)
})

test_that("pivot_wider errors when multiple names_from columns without values_from", {
  df <- data.frame(a = 1, b = 2, c = 3)
  expect_error(
    df %>% pivot_wider(names_from = c(a, b)),
    "requires exactly one column"
  )
})

test_that("pivot_wider one-hot encoding handles multiple rows per id correctly", {
  df <- data.frame(
    id = c(1, 1, 1, 2, 2),
    category = c("A", "A", "B", "B", "C")
  )
  result <- df %>% pivot_wider(names_from = category)

  expect_equal(nrow(result), 2)  # 2 unique ids
  # id=1 has A (twice) and B
  expect_equal(result$A[result$id == 1], 1)
  expect_equal(result$B[result$id == 1], 1)
  expect_equal(result$C[result$id == 1], 0)
  # id=2 has B and C
  expect_equal(result$A[result$id == 2], 0)
  expect_equal(result$B[result$id == 2], 1)
  expect_equal(result$C[result$id == 2], 1)
})

test_that("pivot_wider passes additional arguments to tidyr::pivot_wider", {
  df <- data.frame(
    id = c(1, 2),
    category = c("A", "B")
  )
  result <- df %>% pivot_wider(names_from = category, names_prefix = "cat_")

  expect_true("cat_A" %in% names(result))
  expect_true("cat_B" %in% names(result))
})

test_that("pivot_wider normal mode passes additional arguments correctly", {
  df <- data.frame(
    id = c(1, 2),
    key = c("x", "y"),
    value = c(10, 20)
  )
  result <- df %>% pivot_wider(names_from = key, values_from = value, names_prefix = "val_")

  expect_true("val_x" %in% names(result))
  expect_true("val_y" %in% names(result))
})

test_that("pivot_wider one-hot mode converts empty strings to NA", {
  df <- data.frame(
    id = c(1, 2, 3, 4),
    category = c("A", "", "B", "A")
  )
  # Should not error - empty strings converted to NA
  result <- df %>% pivot_wider(names_from = category)

  expect_equal(nrow(result), 4)  # 4 unique ids
  expect_true("A" %in% names(result))
  expect_true("B" %in% names(result))
  # NA column should be created (tidyr creates NA_ column or similar)
  # The key is that it doesn't error
  expect_equal(result$A, c(1, 0, 0, 1))
  expect_equal(result$B, c(0, 0, 1, 0))
  # Verify the row with empty string (id=2) has 0 for all category columns
  expect_equal(result$A[result$id == 2], 0)
  expect_equal(result$B[result$id == 2], 0)
})

test_that("pivot_wider normal mode converts empty strings to NA", {
  df <- data.frame(
    id = c(1, 2, 3),
    key = c("x", "", "y"),
    value = c(10, 20, 30)
  )
  # Should not error - empty strings converted to NA
  result <- df %>% pivot_wider(names_from = key, values_from = value)

  expect_equal(nrow(result), 3)
  expect_equal(result$x, c(10, NA, NA))
  expect_equal(result$y, c(NA, NA, 30))
})

test_that("pivot_wider handles column with only empty strings", {
  df <- data.frame(
    id = c(1, 2),
    category = c("", "")
  )
  # All empty strings become NA - should work without error
  result <- df %>% pivot_wider(names_from = category)

  expect_equal(nrow(result), 2)
  # Verify column structure - should have id and NA-related column
  expect_true("id" %in% names(result))
  expect_equal(ncol(result), 2)  # id + one NA column
})

test_that("pivot_wider handles mixed empty strings and NA values", {
  df <- data.frame(
    id = c(1, 2, 3, 4),
    category = c("A", "", NA, "B"),
    stringsAsFactors = FALSE
  )
  result <- df %>% pivot_wider(names_from = category)

  expect_equal(nrow(result), 4)
  expect_true("A" %in% names(result))
  expect_true("B" %in% names(result))
  expect_equal(result$A, c(1, 0, 0, 0))
  expect_equal(result$B, c(0, 0, 0, 1))
})
