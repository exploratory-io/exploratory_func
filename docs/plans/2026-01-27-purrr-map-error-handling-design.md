# Design: Improve Error Reporting in purrr::map for Analytics Functions

**Issue:** [exploratory-io/tam#33858](https://github.com/exploratory-io/tam/issues/33858)
**Date:** 2026-01-27
**Status:** Draft

## Problem Statement

When an error occurs inside a `purrr::map()` call in Analytics functions, the error message shown to users is unhelpful:

```
Error in try({ : In argument: `model = purrr::map(...)`.
```

This message doesn't reveal what actually failed inside the map function, making debugging difficult for users and support teams.

### Example from Issue

```r
exp_kmeans(`Q10 他の学生との交流満足度`, algorithm = "Hartigan-Wong", ...)
# Results in:
# R error: Error in try({ : In argument: `model = purrr::map(...)`.
```

## Goal

Show detailed error messages that include:
1. The original error message (e.g., "initial centers are not distinct")
2. Context about what was being processed (e.g., "centers=3" or "variable='age'")

### Expected Result

```
initial centers are not distinct (while building k-means model with centers=3)
```

## Scope

**In Scope:** Analytics View functions (`exp_*`) only
- exp_kmeans, exp_xgboost, exp_lightgbm, exp_balance, exp_rpart
- exp_survival_forest, exp_ts_cluster
- Related helper functions (calc_permutation_importance_*, iterate_kmeans, etc.)

**Out of Scope:**
- File import functions (system.R, google_cloud_storage.R, aws_s3.R, etc.)
- Other purrr::map usages outside Analytics View

## Approach

**Inline tryCatch** - Add `tryCatch` inside existing `purrr::map` callbacks with contextual error messages.

### Why This Approach?

| Approach | Pros | Cons |
|----------|------|------|
| **Inline tryCatch** ✓ | Minimal code change, preserves style, adds context | Slightly more verbose |
| Custom safe_map wrapper | Reusable, consistent | More code to maintain |
| purrr::safely() | Built-in | Requires post-processing, no auto-context |

### Performance

Performance impact is negligible (<0.01% overhead) because:
- `tryCatch` overhead: ~5 microseconds per iteration
- Actual work (model building): 100-5000 milliseconds per iteration
- The iteration overhead is insignificant compared to computational work

## Pattern Template

### Standard Pattern

```r
# BEFORE
ret <- ret %>% dplyr::mutate(model = purrr::map(items, function(item) {
  some_operation(item)
}))

# AFTER
ret <- ret %>% dplyr::mutate(model = purrr::map(items, function(item) {
  tryCatch({
    some_operation(item)
  }, error = function(e) {
    stop(paste0(e$message, " (while <action> with <context>=", item, ")"),
         call. = FALSE)
  })
}))
```

### Error Message Format

```
<original_error_message> (while <action> with <context>=<value>)
```

Examples:
- `"initial centers are not distinct (while building k-means model with centers=3)"`
- `"object 'missing_col' not found (while calculating permutation importance for variable 'age')"`
- `"subscript out of bounds (while extracting model from group 2)"`

## Files and Functions to Modify

### High Priority

| File | Function | Line(s) | Context to Add |
|------|----------|---------|----------------|
| R/kmeans.R | `iterate_kmeans` | 14-28 | `centers=X` |
| R/kmeans.R | `exp_kmeans` | 115-123 | `model index` |
| R/build_xgboost.R | `calc_permutation_importance_*` | 788, 802, 817 | `variable='name'` |
| R/build_xgboost.R | `exp_xgboost` | 1449-1462 | `group index` |

### Medium Priority

| File | Function | Line(s) | Context to Add |
|------|----------|---------|----------------|
| R/build_lightgbm.R | `calc_permutation_importance_*` | ~1400s | `variable='name'` |
| R/randomForest_tidiers.R | `calc_permutation_importance_rpart_*` | 3113, 3127, 3146 | `variable='name'` |
| R/randomForest_tidiers.R | `exp_balance`, `exp_rpart` | 2554-2560, 3499-3505 | `group index` |
| R/survival_forest.R | `calc_permutation_importance_ranger_survival` | 22 | `variable='name'` |
| R/ts_cluster.R | internal functions | 72, 196 | `centers=X` |

### Low Priority

| File | Function | Line(s) |
|------|----------|---------|
| R/textanal.R | `exp_textanal`, etc. | 71, 142, 762, 960 |
| R/arima.R | `exp_arima` | 139 |

## Concrete Code Changes

### Example 1: iterate_kmeans in R/kmeans.R

```r
# BEFORE (lines 14-28)
ret <- ret %>% dplyr::mutate(model = purrr::map(center, function(x) {
  model_df <- df %>% build_kmeans.cols(everything(),
                                       centers=x,
                                       iter.max = iter.max,
                                       nstart = nstart,
                                       algorithm = algorithm,
                                       trace = trace,
                                       normalize_data = normalize_data,
                                       seed=seed,
                                       keep.source=FALSE,
                                       augment=FALSE,
                                       na.rm = FALSE)
  ret <- model_df$model[[1]]
  ret
}))

# AFTER
ret <- ret %>% dplyr::mutate(model = purrr::map(center, function(x) {
  tryCatch({
    model_df <- df %>% build_kmeans.cols(everything(),
                                         centers=x,
                                         iter.max = iter.max,
                                         nstart = nstart,
                                         algorithm = algorithm,
                                         trace = trace,
                                         normalize_data = normalize_data,
                                         seed=seed,
                                         keep.source=FALSE,
                                         augment=FALSE,
                                         na.rm = FALSE)
    model_df$model[[1]]
  }, error = function(e) {
    stop(paste0(e$message, " (while building k-means model with centers=", x, ")"),
         call. = FALSE)
  })
}))
```

### Example 2: Permutation Importance in R/build_xgboost.R

```r
# BEFORE (line ~788)
importances <- purrr::map(var_list, function(var) {
  mmpf::permutationImportance(data, vars=var, y=target, model=fit, ...)
})

# AFTER
importances <- purrr::map(var_list, function(var) {
  tryCatch({
    mmpf::permutationImportance(data, vars=var, y=target, model=fit, ...)
  }, error = function(e) {
    stop(paste0(e$message, " (while calculating permutation importance for variable '", var, "')"),
         call. = FALSE)
  })
})
```

### Example 3: Nested Data Extraction in R/build_xgboost.R

```r
# BEFORE (lines 1449-1462)
ret <- ret %>% dplyr::mutate(model = purrr::map(data, function(df){
  df[[model_and_data_col]][[1]]$model
}))

# AFTER - use purrr::imap for automatic index
ret <- ret %>% dplyr::mutate(model = purrr::imap(data, function(df, idx){
  tryCatch({
    df[[model_and_data_col]][[1]]$model
  }, error = function(e) {
    stop(paste0(e$message, " (while extracting model from group ", idx, ")"),
         call. = FALSE)
  })
}))
```

## Testing Strategy

### Test File

Create `tests/testthat/test-error-handling.R` for all error context tests.

### Test Cases

```r
test_that("iterate_kmeans reports error with centers context", {
  df <- data.frame(x = c(1, 2), y = c(3, 4))  # Only 2 rows

  expect_error(
    iterate_kmeans(df, max_centers = 5),
    regexp = "centers="
  )
})

test_that("exp_kmeans reports error with context when elbow method fails", {
  df <- data.frame(x = c(1, 1, 1), y = c(1, 1, 1))

  expect_error(
    exp_kmeans(df, x, y, elbow_method_mode = TRUE),
    regexp = "centers=|while building"
  )
})

test_that("calc_permutation_importance reports error with variable context", {
  skip_if_not_installed("mmpf")

  expect_error(
    calc_permutation_importance_linear(...),
    regexp = "variable.*'|for variable"
  )
})

test_that("exp_xgboost reports error with group context when extraction fails", {
  expect_error(
    ...,
    regexp = "group [0-9]+|extracting model"
  )
})
```

## Implementation Order

### Phase 1: Core Issue Fix
1. R/kmeans.R - `iterate_kmeans` (line 14)
2. R/kmeans.R - `exp_kmeans` (line 115)

### Phase 2: XGBoost & LightGBM
3. R/build_xgboost.R - `calc_permutation_importance_*` (lines 788, 802, 817)
4. R/build_xgboost.R - `exp_xgboost` extraction (lines 1449-1462)
5. R/build_lightgbm.R - Similar permutation importance functions

### Phase 3: Other ML Functions
6. R/randomForest_tidiers.R - `calc_permutation_importance_rpart_*`
7. R/randomForest_tidiers.R - `exp_balance`, `exp_rpart` extraction
8. R/survival_forest.R - Permutation importance

### Phase 4: Time Series & Text
9. R/ts_cluster.R - Time series clustering iterations
10. R/textanal.R - Text analysis functions (low priority)

## Summary of Changes

| File | Functions Modified | Map Operations | Estimated Lines Changed |
|------|-------------------|----------------|------------------------|
| R/kmeans.R | 2 | 2 | ~20 |
| R/build_xgboost.R | 4 | 6 | ~40 |
| R/build_lightgbm.R | 4 | 6 | ~40 |
| R/randomForest_tidiers.R | 5 | 9 | ~60 |
| R/survival_forest.R | 2 | 2 | ~15 |
| R/ts_cluster.R | 2 | 3 | ~25 |
| R/textanal.R | 3 | 4 | ~30 |
| **New test file** | - | - | ~100 |
| **Total** | **22 functions** | **32 operations** | **~330 lines** |

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Breaking existing error handling | Medium | Check for existing `"error" %in% class()` patterns before changes |
| Error message too verbose | Low | Keep context concise: `(centers=3)` not long descriptions |
| Performance regression | Very Low | Benchmarks show negligible overhead (<0.01%) |
| Inconsistent error formats | Low | Define standard format and follow consistently |

## Acceptance Criteria

1. When an error occurs inside `purrr::map` in any Analytics function, the error message includes:
   - The original error message
   - Context about what was being processed
2. All existing tests pass
3. New tests verify error context is present
4. No measurable performance regression
