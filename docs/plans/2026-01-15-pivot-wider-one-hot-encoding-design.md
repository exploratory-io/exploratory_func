# Design: pivot_wider One-Hot Encoding Support

**Issue:** https://github.com/exploratory-io/tam/issues/33757
**Date:** 2026-01-15
**Status:** Approved

## Problem

Users want to perform one-hot encoding using `pivot_wider`. Currently this requires:

```r
pivot_wider(
  names_from = `column`,
  values_from = `column`,  # same column
  values_fn = function(x) 1,
  values_fill = 0
)
```

The UI doesn't expose `values_fn`, making one-hot encoding difficult.

## Solution

Create a wrapper function `pivot_wider` in the exploratory package that:
- When `values_from` is omitted: enables one-hot encoding mode
- When `values_from` is provided: passes through to `tidyr::pivot_wider`

## Design Decisions

| Aspect | Decision |
|--------|----------|
| Function name | `pivot_wider` (masks `tidyr::pivot_wider`) |
| Location | `R/util.R` |
| One-hot trigger | `values_from` is NULL or missing |
| Output values | Numeric 1.0/0.0 |
| values_fill in one-hot | Always 0 (not overridable) |
| Multi-column names_from | Error in one-hot mode, allowed in normal mode |
| Tests | New file `tests/testthat/test_pivot_wider.R` |

## Implementation

### Function Signature

```r
#' Wrapper for tidyr::pivot_wider with one-hot encoding support
#'
#' @param data A data frame
#' @param names_from Column(s) to get the name of the output columns
#' @param values_from Column to get the values from. If NULL or missing,
#'        enables one-hot encoding mode using names_from column with 1/0 values.
#' @param ... Additional arguments passed to tidyr::pivot_wider
#' @export
pivot_wider <- function(data, names_from, values_from = NULL, ...)
```

### Core Logic

```r
pivot_wider <- function(data, names_from, values_from = NULL, ...) {
  # Capture names_from for validation
  names_from_cols <- tidyselect::eval_select(rlang::enquo(names_from), data)

  if (is.null(values_from) || missing(values_from)) {
    # One-hot encoding mode

    # Validate: only single column allowed
    if (length(names_from_cols) > 1) {
      stop("One-hot encoding mode (values_from not specified) requires exactly one column in names_from")
    }

    # Use names_from as values_from, with values_fn returning 1
    tidyr::pivot_wider(
      data,
      names_from = {{ names_from }},
      values_from = {{ names_from }},
      values_fn = function(x) 1,
      values_fill = 0,
      ...
    )
  } else {
    # Normal mode - pass through to tidyr::pivot_wider
    tidyr::pivot_wider(
      data,
      names_from = {{ names_from }},
      values_from = {{ values_from }},
      ...
    )
  }
}
```

### Namespace Handling

- Package exports `pivot_wider`, masking `tidyr::pivot_wider` when loaded
- Existing internal code uses `tidyr::pivot_wider` explicitly, so unaffected

## Tests

File: `tests/testthat/test_pivot_wider.R`

```r
# Test 1: One-hot encoding mode (no values_from)
test_that("pivot_wider creates one-hot encoding when values_from is NULL", {
  df <- data.frame(
    id = c(1, 1, 2, 3),
    category = c("A", "B", "A", "C")
  )
  result <- df %>% pivot_wider(names_from = category)

  expect_equal(nrow(result), 3)  # 3 unique ids
  expect_true(all(c("A", "B", "C") %in% names(result)))
  expect_equal(result$A, c(1, 1, 0))  # numeric, not integer
  expect_equal(result$B, c(1, 0, 0))
  expect_equal(result$C, c(0, 0, 1))
})

# Test 2: Normal mode (values_from provided)
test_that("pivot_wider works normally when values_from is provided", {
  df <- data.frame(
    id = c(1, 2),
    key = c("x", "y"),
    value = c(10, 20)
  )
  result <- df %>% pivot_wider(names_from = key, values_from = value)

  expect_equal(result$x, 10)
  expect_equal(result$y, 20)
})

# Test 3: Error on multiple columns in one-hot mode
test_that("pivot_wider errors when multiple names_from without values_from", {
  df <- data.frame(a = 1, b = 2, c = 3)
  expect_error(
    df %>% pivot_wider(names_from = c(a, b)),
    "requires exactly one column"
  )
})
```

## Usage Example

Before (workaround):
```r
df %>% pivot_wider(
  names_from = category,
  values_from = category,
  values_fn = function(x) 1,
  values_fill = 0
)
```

After (with this enhancement):
```r
df %>% pivot_wider(names_from = category)
```
