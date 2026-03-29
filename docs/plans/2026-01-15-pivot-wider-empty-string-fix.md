# pivot_wider Empty String to NA Conversion Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix the pivot_wider wrapper to automatically convert empty strings to NA in the `names_from` column(s) before calling tidyr::pivot_wider, preventing the "Subscript can't contain the empty string" error.

**Architecture:** Add a preprocessing step in the pivot_wider wrapper function that identifies and converts empty strings ("") to NA in the specified `names_from` column(s). This conversion happens before the data is passed to tidyr::pivot_wider, ensuring compatibility with tidyr's requirements.

**Tech Stack:** R, dplyr, tidyr, rlang, testthat

---

## Background

**Issue:** [GitHub Issue #33756](https://github.com/exploratory-io/tam/issues/33756)

When pivot_wider is called with a `names_from` column containing empty strings, tidyr throws:
```
Error in values[spec$.name] : Can't subset columns with `spec$.name`.
✖ Subscript `spec$.name` can't contain the empty string.
✖ It has an empty string at location 1.
```

**Solution:** Convert empty strings to NA before calling tidyr::pivot_wider. This is transparent to users and prevents the error.

---

## Task 1: Add Tests for Empty String Handling

**Files:**
- Modify: `tests/testthat/test_pivot_wider.R`

**Step 1: Write the failing test for one-hot mode with empty strings**

Add this test at the end of the file:

```r
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
})
```

**Step 2: Write the failing test for normal mode with empty strings**

Add this test:

```r
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
```

**Step 3: Run tests to verify they fail**

Run: `Rscript -e "testthat::test_file('tests/testthat/test_pivot_wider.R')"`

Expected: Tests fail with "Subscript can't contain the empty string" error

---

## Task 2: Implement Empty String to NA Conversion

**Files:**
- Modify: `R/util.R` (lines 3664-3700)

**Step 1: Add the preprocessing logic after capturing quosures**

Modify the `pivot_wider` function. After line 3672 (`names_from_cols <- tidyselect::eval_select(names_from_quo, data)`), add the preprocessing logic:

```r
pivot_wider <- function(data, names_from, values_from = NULL, ...) {
  # Capture arguments as quosures immediately
  names_from_quo <- rlang::enquo(names_from)
  values_from_quo <- rlang::enquo(values_from)

  # Check if values_from is NULL (one-hot encoding mode)
  is_onehot_mode <- rlang::quo_is_null(values_from_quo)

  names_from_cols <- tidyselect::eval_select(names_from_quo, data)

  # Convert empty strings to NA in names_from columns
  # This prevents tidyr error: "Subscript can't contain the empty string"
  for (col_name in names(names_from_cols)) {
    if (is.character(data[[col_name]])) {
      data[[col_name]] <- ifelse(data[[col_name]] == "", NA_character_, data[[col_name]])
    }
  }

  if (is_onehot_mode) {
    # One-hot encoding mode

    # Validate: only single column allowed in one-hot encoding mode
    if (length(names_from_cols) > 1) {
      stop("One-hot encoding mode (values_from not specified) requires exactly one column in names_from") # nolint
    }

    # Use names_from as values_from, with values_fn returning 1 (numeric)
    tidyr::pivot_wider(
      data,
      names_from = !!names_from_quo,
      values_from = !!names_from_quo,
      values_fn = function(x) 1,
      values_fill = 0,
      ...
    )
  } else {
    # Normal mode - pass through to tidyr::pivot_wider
    tidyr::pivot_wider(
      data,
      names_from = !!names_from_quo,
      values_from = !!values_from_quo,
      ...
    )
  }
}
```

**Step 2: Run tests to verify they pass**

Run: `Rscript -e "testthat::test_file('tests/testthat/test_pivot_wider.R')"`

Expected: All tests pass

---

## Task 3: Add Edge Case Tests

**Files:**
- Modify: `tests/testthat/test_pivot_wider.R`

**Step 1: Add test for all empty strings**

```r
test_that("pivot_wider handles column with only empty strings", {
  df <- data.frame(
    id = c(1, 2),
    category = c("", "")
  )
  # All empty strings become NA - should work without error
  result <- df %>% pivot_wider(names_from = category)

  expect_equal(nrow(result), 2)
  # Only NA column should exist (no category columns)
})
```

**Step 2: Add test for mixed empty strings and NA**

```r
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
```

**Step 3: Run all tests**

Run: `Rscript -e "testthat::test_file('tests/testthat/test_pivot_wider.R')"`

Expected: All tests pass

**Step 4: Commit the changes**

```bash
git add R/util.R tests/testthat/test_pivot_wider.R
git commit -m "$(cat <<'EOF'
Fix pivot_wider to convert empty strings to NA in names_from column

When names_from column contains empty strings, tidyr::pivot_wider fails with
"Subscript can't contain the empty string" error. This fix automatically
converts empty strings to NA before calling tidyr::pivot_wider.

Fixes: exploratory-io/tam#33756

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
EOF
)"
```

---

## Task 4: Run Full Test Suite

**Step 1: Run all pivot_wider tests**

Run: `Rscript -e "testthat::test_file('tests/testthat/test_pivot_wider.R')"`

Expected: All tests pass

**Step 2: Run related one_hot tests (if they exist)**

Run: `Rscript -e "testthat::test_file('tests/testthat/test_one_hot.R')"` (if file exists)

Expected: All tests pass

---

## Summary of Changes

| File | Change |
|------|--------|
| `R/util.R` | Add preprocessing loop to convert empty strings to NA in names_from columns |
| `tests/testthat/test_pivot_wider.R` | Add 4 new tests for empty string handling |

## Test Coverage

| Scenario | Test |
|----------|------|
| One-hot mode with empty strings | `pivot_wider one-hot mode converts empty strings to NA` |
| Normal mode with empty strings | `pivot_wider normal mode converts empty strings to NA` |
| All empty strings | `pivot_wider handles column with only empty strings` |
| Mixed empty strings and NA | `pivot_wider handles mixed empty strings and NA values` |
