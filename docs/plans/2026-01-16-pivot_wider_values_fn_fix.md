# Fix pivot_wider values_fn Conflict in One-Hot Mode

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix the error when users pass `values_fn` to `pivot_wider` without specifying `values_from` (one-hot encoding mode).

**Architecture:** In one-hot mode, the function internally sets `values_fn = ~ 1` and `values_fill = 0`. When users also pass these arguments via `...`, it causes a duplicate argument error. The fix will strip these arguments from `...` in one-hot mode and warn the user.

**Tech Stack:** R, rlang, tidyr, tidyselect

---

### Task 1: Write Failing Test for values_fn Conflict

**Files:**
- Modify: `tests/testthat/test_pivot_wider.R`

**Step 1: Write the failing test**

Add the following test at the end of the file:

```r
test_that("pivot_wider one-hot mode ignores values_fn and values_fill from user with warning", {
  df <- data.frame(
    id = c(1, 1, 2, 2),
    category = c("A", "B", "A", "C")
  )

  # Should warn but not error when values_fn is passed without values_from
  expect_warning(
    result <- df %>% pivot_wider(names_from = category, values_fn = mean),
    "values_fn.*ignored.*one-hot"
  )

  # Result should still be correct one-hot encoding
  expect_equal(nrow(result), 2)
  expect_equal(result$A, c(1, 1))
  expect_equal(result$B, c(1, 0))
  expect_equal(result$C, c(0, 1))
})

test_that("pivot_wider one-hot mode ignores values_fill from user with warning", {
  df <- data.frame(
    id = c(1, 2),
    category = c("A", "B")
  )

  # Should warn when values_fill is passed without values_from
  expect_warning(
    result <- df %>% pivot_wider(names_from = category, values_fill = 999),
    "values_fill.*ignored.*one-hot"
  )

  # Result should still use 0 for missing values (not 999)
  expect_equal(result$A, c(1, 0))
  expect_equal(result$B, c(0, 1))
})
```

**Step 2: Run test to verify it fails**

Run: `Rscript -e '.libPaths("~/.exploratory/R/4.4_ARM"); library(testthat); test_file("tests/testthat/test_pivot_wider.R", filter = "ignores values_fn")'`

Expected: FAIL with "formal argument matched by multiple actual arguments"

---

### Task 2: Implement Fix to Strip Conflicting Arguments

**Files:**
- Modify: `R/util.R:3682-3698` (the one-hot mode block)

**Step 1: Add helper to strip arguments from dots**

Add the following code inside the `pivot_wider` function, just before the `if (is_onehot_mode)` check (around line 3681):

```r
  # Helper function to strip specified arguments from ... and warn
  strip_onehot_conflicting_args <- function(dots, arg_names) {
    stripped <- list()
    for (name in arg_names) {
      if (name %in% names(dots)) {
        stripped[[name]] <- dots[[name]]
        dots[[name]] <- NULL
      }
    }
    list(dots = dots, stripped = stripped)
  }
```

**Step 2: Modify one-hot mode block to strip and warn**

Replace the one-hot mode block (lines 3682-3698) with:

```r
  if (is_onehot_mode) {
    # One-hot encoding mode

    # Validate: only single column allowed in one-hot encoding mode
    if (length(names_from_cols) > 1) {
      stop("One-hot encoding mode (values_from not specified) requires exactly one column in names_from") # nolint
    }

    # Capture ... arguments and strip conflicting ones
    dots <- rlang::list2(...)
    conflicting_args <- c("values_fn", "values_fill")
    result <- strip_onehot_conflicting_args(dots, conflicting_args)
    dots <- result$dots
    stripped <- result$stripped

    # Warn if user passed conflicting arguments
    if (length(stripped) > 0) {
      stripped_names <- paste(names(stripped), collapse = ", ")
      warning(paste0(
        stripped_names,
        " argument(s) ignored in one-hot encoding mode (when values_from is not specified). ",
        "These are automatically set to values_fn = ~ 1 and values_fill = 0."
      ))
    }

    # Use names_from as values_from, with values_fn returning 1 (numeric)
    rlang::exec(
      tidyr::pivot_wider,
      data,
      names_from = !!names_from_quo,
      values_from = !!names_from_quo,
      values_fn = ~ 1,
      values_fill = 0,
      !!!dots
    )
  }
```

**Step 3: Run test to verify it passes**

Run: `Rscript -e '.libPaths("~/.exploratory/R/4.4_ARM"); library(testthat); test_file("tests/testthat/test_pivot_wider.R", filter = "ignores")'`

Expected: PASS

**Step 4: Run all pivot_wider tests to ensure no regressions**

Run: `Rscript -e '.libPaths("~/.exploratory/R/4.4_ARM"); library(testthat); test_file("tests/testthat/test_pivot_wider.R")'`

Expected: All tests PASS

---

### Task 3: Add Test for User's Original Error Case

**Files:**
- Modify: `tests/testthat/test_pivot_wider.R`

**Step 1: Write test for Japanese column name case**

```r
test_that("pivot_wider handles Japanese column names with values_fn", {
  df <- data.frame(
    id = c(1, 1, 2),
    `サービスの改善点` = c("速度", "価格", "速度"),
    check.names = FALSE
  )

  # Should work with warning (values_fn ignored in one-hot mode)
  expect_warning(
    result <- df %>% pivot_wider(names_from = `サービスの改善点`, values_fn = mean),
    "values_fn.*ignored"
  )

  expect_equal(nrow(result), 2)
  expect_true("速度" %in% names(result))
  expect_true("価格" %in% names(result))
})
```

**Step 2: Run test to verify it passes**

Run: `Rscript -e '.libPaths("~/.exploratory/R/4.4_ARM"); library(testthat); test_file("tests/testthat/test_pivot_wider.R", filter = "Japanese")'`

Expected: PASS

---

### Task 4: Commit Changes

**Step 1: Run full test suite for pivot_wider**

Run: `Rscript -e '.libPaths("~/.exploratory/R/4.4_ARM"); library(testthat); test_file("tests/testthat/test_pivot_wider.R")'`

Expected: All tests PASS

**Step 2: Commit**

```bash
git add R/util.R tests/testthat/test_pivot_wider.R
git commit -m "$(cat <<'EOF'
fix: handle conflicting values_fn/values_fill in pivot_wider one-hot mode

When users pass values_fn or values_fill without specifying values_from,
pivot_wider enters one-hot encoding mode where these arguments are set
internally. Previously this caused a "formal argument matched by multiple
actual arguments" error.

Now the function strips these conflicting arguments from ... and warns
the user that they are ignored in one-hot mode.

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>
EOF
)"
```

---

## Summary

| Task | Description | Files |
|------|-------------|-------|
| 1 | Write failing test | tests/testthat/test_pivot_wider.R |
| 2 | Implement fix | R/util.R |
| 3 | Add Japanese column test | tests/testthat/test_pivot_wider.R |
| 4 | Commit | - |
