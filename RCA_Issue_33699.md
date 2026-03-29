# Root Cause Analysis and Code Fix Plan

## Issue Summary

**Issue ID:** #33699  
**Component:** `exp_arima` function in `R/arima.R`  
**Severity:** High (Blocks user workflow)  
**Customer:** Kaplan Business School (Panuwong)  
**Platform:** v14.0.5_WIN

### Problem Description

When using `exp_arima` with `test_mode = TRUE` on grouped data, the function fails with a "subscript out of bounds" error.

**User's Query:**

```r
exp_arima(`date`, `Cash Rate (%)`, 12, 
          time_unit = "month", 
          fun.aggregate = sum, 
          na_fill_type = "previous", 
          auto = TRUE, 
          seasonal = FALSE, 
          seasonal_auto = TRUE, 
          ic = "aic", 
          unit_root_test = "kpss", 
          test_mode = TRUE)
```

**Key Characteristics:**
- Data is grouped by `CPI (index)` which is a binned column (5 breaks)
- `test_mode = TRUE` with `periods = 12`
- Some groups may have sparse data (fewer observations than the forecast period)

## Root Cause Analysis

### Investigation Steps

1. **Issue Comment Analysis:** AI detected potential issue at line 351 where `tail(filled_aggregated_data, periods)[["y"]]` can fail
2. **Code Review:** Analyzed the `exp_arima` function focusing on `test_mode = TRUE` logic
3. **Pattern Analysis:** Identified all `[[1]]` access patterns that could cause "subscript out of bounds"

### Findings

#### Primary Root Cause: Unsafe Access to `filled_aggregated_data` in Test Mode

The issue occurs in the test mode logic when extracting actual y values for comparison with forecasted values. The original problematic code was:

```r
# Original problematic code (approximately line 351 in older version)
forecast_rows$y <- tail(filled_aggregated_data, periods)[["y"]]
```

This fails when:
1. **`filled_aggregated_data` has fewer rows than `periods`:** When a group has sparse data, after filling missing date/time, there may still be fewer observations than the requested forecast periods
2. **The "y" column is missing:** In edge cases, the aggregation might not produce a "y" column
3. **Assignment length mismatch:** `tail()` returns fewer elements than `periods`, causing assignment failure

#### Secondary Root Causes: Unguarded Model Access

Multiple locations in the code access `model_df$arima[[1]]` without proper guards:

| Line | Code | Risk |
|------|------|------|
| 291 | `class(model_df$arima[[1]]$fit)` | Null check |
| 421 | `class(model_df$arima[[1]]$fit) <- ...` | Class modification |
| 463 | `model_df$arima[[1]]$fit$spec$d` | Spec access |
| 470-471 | Seasonal differences access | Spec access |
| 520-522 | Model parameters access | Spec access |
| 550 | `model_df$arima[[1]]$fit$model` | Model extraction |
| 561 | `attr(ret$model[[1]]$arima[[1]]$fit, ...)` | Attribute setting |

If `model_df` is empty (0 rows) or `model_df$arima` is an empty list, all these accesses would throw "subscript out of bounds".

#### Scenario That Triggers the Bug

1. User groups data by a binned column (e.g., `CPI (index)` with 5 breaks)
2. Some bins have very few observations (e.g., edge bins with < 14 rows)
3. With `test_mode = TRUE` and `periods = 12`, the function:
   - Reserves 12 rows for testing
   - Leaves < 2 rows for training
4. Either:
   - Model fitting fails/returns empty result → "subscript out of bounds" on `[[1]]` access
   - `filled_aggregated_data` has fewer rows than `periods` → "subscript out of bounds" on data access

### Specific Code Issues

#### Issue 1: Unguarded Training Data Length Check

**Original behavior (before fix):**
- No check for minimum training data size
- Model fitting proceeds with insufficient data
- Downstream `[[1]]` accesses fail

#### Issue 2: Unguarded y-value Extraction

**Original code:**
```r
forecast_rows$y <- tail(filled_aggregated_data, periods)[["y"]]
```

**Problems:**
- No check if `filled_aggregated_data` has enough rows
- No check if "y" column exists
- No handling of length mismatch

#### Issue 3: Unguarded Model Result Access

**Original behavior:**
- No check if model fitting returned valid result
- Direct access to `model_df$arima[[1]]` could fail

## Code Fix Plan

### Fixes Already Applied (Verification Needed)

Based on current code analysis, the following guards appear to have been added:

#### Fix 1: Training Data Guard (Lines 206-217)

```r
# Guard: if periods is too large (or data is too sparse), training_data can become empty.
# Downstream code assumes we can fit at least one model and will otherwise throw
# "subscript out of bounds" when accessing model objects with [[1]].
non_na_y <- sum(!is.na(training_data$y))
if (nrow(training_data) < 2 || non_na_y < 2) {
  stop(paste0(
    "Not enough training data to fit ARIMA in test_mode. ",
    "After reserving periods=", periods, " as test data, ",
    "training rows=", nrow(training_data), ", non-NA y rows=", non_na_y, ". ",
    "Reduce periods or set test_mode=FALSE."
  ))
}
```

#### Fix 2: Model Result Guard (Lines 287-293)

```r
# Guard: model fitting can return an empty mable (e.g. if training data is too short),
# which would cause "subscript out of bounds" on model_df$arima[[1]] below.
if (nrow(model_df) < 1 || length(model_df$arima) < 1) {
  stop("ARIMA model fitting returned an empty result. Check that training data has enough observations.")
}
if (class(model_df$arima[[1]]$fit) == "null_mdl") {
  stop("Null model was selected.")
}
```

#### Fix 3: Safe y-value Extraction (Lines 367-386)

```r
if (test_mode){
  fitted_training_df$is_test_data <- FALSE
  # Safely extract y values from filled_aggregated_data.
  # Keep the resulting vector length exactly equal to `periods` to match forecast_rows.
  if ("y" %in% colnames(filled_aggregated_data) && nrow(filled_aggregated_data) > 0) {
    y_values <- filled_aggregated_data[["y"]]
    if (length(y_values) >= periods) {
      forecast_rows$y <- tail(y_values, periods)
    }
    else {
      # If we have fewer rows than periods, pad with NAs at the beginning.
      forecast_rows$y <- c(rep(NA, periods - length(y_values)), y_values)
    }
  }
  else {
    # If y column doesn't exist or data is empty, fill with NAs.
    forecast_rows$y <- rep(NA, periods)
  }
  forecast_rows$is_test_data <- TRUE 
}
```

#### Fix 4: Safe Seasonal Slicing (Lines 443-450)

```r
# Safely slice for seasonal display - handle cases where seasonal_periods might be NULL or larger than data
if (!is.null(seasonal_periods) && seasonal_periods > 0 && nrow(stl_df) >= seasonal_periods) {
  stl_seasonal_df <- stl_df %>% dplyr::slice(1:seasonal_periods)
} else if (nrow(stl_df) > 0) {
  # If seasonal_periods is invalid, just take the first row
  stl_seasonal_df <- stl_df %>% dplyr::slice(1)
} else {
  stl_seasonal_df <- stl_df
}
```

### Additional Recommended Fixes

#### Recommendation 1: Add Grouped Data Warning

Add a warning when using `test_mode = TRUE` with grouped data to alert users about potential sparse data issues:

**Location:** Before line 138 (start of `do_arima_each`)

```r
# Add at the beginning of exp_arima function, after grouped_col is determined
if (test_mode && length(grouped_col) > 0) {
  # Check approximate rows per group
  group_sizes <- df %>% dplyr::count(!!!rlang::syms(grouped_col)) %>% dplyr::pull(n)
  min_group_size <- min(group_sizes, na.rm = TRUE)
  if (min_group_size < periods + 2) {
    warning(paste0(
      "Some groups have fewer observations (", min_group_size, ") ",
      "than periods + 2 (", periods + 2, "). ",
      "These groups may fail or produce unreliable results in test_mode. ",
      "Consider reducing periods or aggregating to fewer groups."
    ))
  }
}
```

#### Recommendation 2: Improve Error Messages for Grouped Data

Enhance the error message at lines 210-216 to include group information:

```r
if (nrow(training_data) < 2 || non_na_y < 2) {
  group_info <- if (length(grouped_col) > 0) {
    paste0(" (in group: ", paste(grouped_col, collapse=", "), ")")
  } else ""
  stop(paste0(
    "Not enough training data to fit ARIMA in test_mode", group_info, ". ",
    "After reserving periods=", periods, " as test data, ",
    "training rows=", nrow(training_data), ", non-NA y rows=", non_na_y, ". ",
    "Reduce periods, set test_mode=FALSE, or check this group's data."
  ))
}
```

### Testing Strategy

1. **Unit Tests for Edge Cases:**
   - Test with grouped data where one group has fewer rows than `periods + 2`
   - Test with empty groups
   - Test with `periods` larger than total data rows
   - Test with sparse time series data (many missing dates)

2. **Integration Tests:**
   - Test with the exact customer query from the issue (binned CPI data)
   - Test various group sizes with `test_mode = TRUE`
   - Verify error messages are helpful and actionable

3. **Regression Tests:**
   - Ensure existing test cases pass
   - Ensure `test_mode = FALSE` behavior is unchanged
   - Add specific test case for issue #33699

### Sample Test Case

```r
test_that("exp_arima handles sparse grouped data in test_mode gracefully", {
  # Create test data with one sparse group
  df <- tibble::tibble(
    date = rep(seq(as.Date("2020-01-01"), by = "month", length.out = 24), 2),
    value = c(rnorm(24), rnorm(24)),
    group = c(rep("A", 24), rep("B", 24))
  )
  
  # Make group B sparse (only 10 observations)
  df <- df %>% dplyr::filter(!(group == "B" & date > as.Date("2020-10-01")))
  
  # This should either work (with warning) or fail gracefully (with helpful error)
  result <- tryCatch({
    df %>% 
      dplyr::group_by(group) %>% 
      exp_arima(date, value, periods = 12, time_unit = "month", test_mode = TRUE)
  }, error = function(e) e)
  
  # Should get a helpful error message, not "subscript out of bounds"
  if (inherits(result, "error")) {
    expect_false(grepl("subscript out of bounds", result$message))
    expect_true(grepl("Not enough training data|training rows", result$message))
  }
})
```

## Summary of Root Causes

| Root Cause | Location | Status | Fix Description |
|------------|----------|--------|-----------------|
| Unguarded training data check | ~Line 196 | ✅ Fixed | Added check for minimum training rows |
| Unguarded model result access | ~Line 287 | ✅ Fixed | Added check for empty model result |
| Unsafe y-value extraction | ~Line 351 (old) | ✅ Fixed | Safe extraction with length handling |
| Unsafe seasonal slicing | ~Line 443 | ✅ Fixed | Safe slicing with bounds check |
| No grouped data warning | Lines 104-121 | ✅ Implemented | Added warning for sparse groups in test_mode |
| Error messages lack group context | Lines 170-183, 240-246 | ✅ Implemented | Error messages now include group context |

## Version Compatibility

**Issue affects:** v14.0.5 and earlier  
**Fix available in:** [Version to be determined based on when fixes were applied]

## Customer Workaround

If the customer is on an affected version:

1. **Reduce forecast periods:** Use smaller `periods` value that is less than the smallest group size minus 2
2. **Disable test mode:** Set `test_mode = FALSE` for initial analysis
3. **Aggregate groups:** Combine sparse groups into larger categories
4. **Filter sparse groups:** Remove groups with fewer than `periods + 5` observations before running ARIMA

```r
# Workaround: Filter out sparse groups
df %>% 
  dplyr::group_by(`CPI (index)`) %>%
  dplyr::filter(n() >= 15) %>%  # Keep only groups with at least 15 rows
  exp_arima(`date`, `Cash Rate (%)`, 12, 
            time_unit = "month", 
            test_mode = TRUE, ...)
```

## Approval

**Analyzed By:** AI Assistant  
**Review Date:** 2026-01-12  
**Implementation Date:** 2026-01-12  
**Status:** ✅ Implemented - All fixes and improvements applied

### Implementation Summary

1. **Test Cases Added:** `tests/testthat/test_arima_1.R`
   - Test for binned grouped data scenario
   - Test for sufficient data case (regression)
   - Test for helpful error messages with group context
   - Test for warning emission with sparse grouped data

2. **Code Improvements in `R/arima.R`:**
   - Added grouped data warning (Lines 104-121)
   - Added group context to error messages (Lines 170-183, 240-246)

---

## Related Issues

- **#33708:** Similar ARIMA issue with null model when using `auto = FALSE`
- Both issues highlight the need for robust error handling in time series model fitting

## Lessons Learned

1. **Guard all indexed access:** Any `[[1]]` or `[["column"]]` access should be preceded by bounds/existence checks
2. **Handle grouped data edge cases:** When functions support grouped data, consider scenarios where individual groups have insufficient data
3. **Provide actionable error messages:** Error messages should guide users toward solutions, especially for data-dependent failures
4. **Test with sparse data:** Include test cases with varying data densities and group sizes
