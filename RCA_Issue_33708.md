# Root Cause Analysis and Code Fix Plan

## Issue Summary

**Issue ID:** #33708  
**Component:** `exp_arima` function in `R/arima.R`  
**Severity:** High (Blocks user workflow)  
**Customer:** Kaplan Business School (Ronish)

### Problem Description

When using `exp_arima` with `auto = FALSE` and manually specified ARIMA parameters (e.g., `p = 2, d = 0, q = 2`), the function fails with the following error:

```
Null model was selected.
It looks like you're trying to fully specify your ARIMA model but have not said if a constant should be included.
You can include a constant using `ARIMA(y~1)` to the formula or exclude it by adding `ARIMA(y~0)`.
1 error encountered for arima
[1] Could not find an appropriate ARIMA model.
This is likely because automatic selection does not select models with characteristic roots that may be numerically unstable.
```

**User's Query:**
```r
exp_arima(`Date`, , 12, time_unit = "day", na_fill_type = "previous", 
          auto = FALSE, p = 2, d = 0, q = 2, seasonal = FALSE, 
          seasonal_auto = TRUE, ic = "aic", unit_root_test = "kpss", 
          test_mode = FALSE)
```

## Root Cause Analysis

### Investigation Steps

1. **Code Review:** Analyzed the `exp_arima` function in `R/arima.R`, specifically the formula construction logic (lines 253-278)
2. **Error Message Analysis:** Examined the fable::ARIMA error message requirements
3. **Test Case Review:** Examined existing test cases in `tests/testthat/test_arima_1.R`

### Findings

#### Missing Constant Term Specification

When `auto = FALSE` and manual ARIMA parameters are specified, the code constructs formulas without explicitly specifying whether to include a constant (intercept) term. The fable::ARIMA package requires explicit specification when manually specifying model parameters.

**Current Code (Lines 253-278):**

```r
if (seasonal && !is.null(seasonal_periods)) {
  if (auto && seasonal_auto) {
    formula_str <- paste0("y ~ PDQ(period=", seasonal_periods, ")")
  }
  else if (seasonal_auto) {
    formula_str <- paste0("y ~ pdq(", p, ",", d, ",", q, ") + PDQ(period=", seasonal_periods, ")")
  }
  else if (auto) {
    formula_str <- paste0("y ~ PDQ(", P, ",", D, ",", Q, ", period=", seasonal_periods, ")")
  }
  else {
    formula_str <- paste0("y ~ pdq(", p, ",", d, ",", q, ") + PDQ(", P, ",", D, ",", Q, ", period=", seasonal_periods, ")")
  }
}
else {
  if (auto) {
    formula_str <- "y ~ PDQ(0,0,0)"
  }
  else {
    formula_str <- paste0("y ~ pdq(", p, ",", d, ",", q, ") + PDQ(0,0,0)")
  }
}
```

#### Root Cause

1. **Missing Constant Term:** When `auto = FALSE`, the formula doesn't specify whether to include a constant term (`1` or `0`). The fable::ARIMA package requires this when manually specifying models.

2. **No Parameter for Constant:** The function doesn't expose a parameter to control the constant term (like `allowmean` or `allowdrift` which are in the function signature but not used in the formula construction).

3. **Error Handling:** While there's a check for null model at line 291-292, it doesn't provide guidance on how to fix the issue.

4. **Numerical Instability:** The error message also indicates that ARIMA(2,0,2) may have numerically unstable characteristic roots, which is a separate but related issue.

### Specific Code Issues

1. **Lines 253-278:** Formula construction doesn't include constant term specification when `auto = FALSE`
2. **Line 19-64:** Function parameters include `allowdrift` and `allowmean` but they're not used in formula construction
3. **Line 291-292:** Error message for null model doesn't guide users on how to fix the issue
4. **Missing Validation:** No validation that manually specified parameters (p, d, q) result in a valid, stable model

## Code Fix Plan

### Proposed Solution

1. Add constant term specification to formulas when `auto = FALSE`
2. Use existing `allowmean` parameter to control constant term inclusion
3. Improve error messages to guide users
4. Add fallback logic for numerically unstable models

### Implementation Steps

#### Step 1: Add Constant Term to Manual ARIMA Formulas

**Location:** Lines 253-278, modify formula construction

**For non-seasonal models (lines 271-277):**

```r
else {
  if (auto) {
    formula_str <- "y ~ PDQ(0,0,0)"
  }
  else {
    # When manually specifying, include constant term based on allowmean
    constant_term <- if (allowmean) "1" else "0"
    formula_str <- paste0("y ~ ", constant_term, " + pdq(", p, ",", d, ",", q, ") + PDQ(0,0,0)")
  }
}
```

**For seasonal models (lines 253-269):**

```r
if (seasonal && !is.null(seasonal_periods)) {
  if (auto && seasonal_auto) {
    formula_str <- paste0("y ~ PDQ(period=", seasonal_periods, ")")
  }
  else if (seasonal_auto) {
    # p, d, q are set manually. For P, D, Q, automatically search them.
    constant_term <- if (allowmean) "1" else "0"
    formula_str <- paste0("y ~ ", constant_term, " + pdq(", p, ",", d, ",", q, ") + PDQ(period=", seasonal_periods, ")")
  }
  else if (auto) {
    # p, d, q are automatically searched, while P, D, Q are manually specified.
    formula_str <- paste0("y ~ PDQ(", P, ",", D, ",", Q, ", period=", seasonal_periods, ")")
  }
  else {
    # p, d, q, P, D, Q are all set manually.
    constant_term <- if (allowmean) "1" else "0"
    formula_str <- paste0("y ~ ", constant_term, " + pdq(", p, ",", d, ",", q, ") + PDQ(", P, ",", D, ",", Q, ", period=", seasonal_periods, ")")
  }
}
```

#### Step 2: Improve Error Handling for Null Model

**Location:** Lines 291-293, enhance error message

```r
if (class(model_df$arima[[1]]$fit) == "null_mdl") {
  # Provide helpful guidance based on whether auto was used
  if (!auto) {
    stop(paste0(
      "Null model was selected. The manually specified ARIMA(", p, ",", d, ",", q, 
      ") model may have numerically unstable characteristic roots.\n",
      "Try one of the following:\n",
      "1. Set auto=TRUE to let ARIMA automatically select a stable model\n",
      "2. Try simpler parameters (e.g., p=1, d=1, q=1)\n",
      "3. Change allowmean parameter to control constant term inclusion\n",
      "4. Check if your data needs differencing (try d=1 instead of d=0)"
    ))
  } else {
    stop("Null model was selected. This may indicate insufficient data or numerical issues. Try adjusting parameters or using a different time series model.")
  }
}
```

#### Step 3: Add Validation for Manual Parameters

**Location:** After line 102, add validation before model fitting

```r
# Validate manual parameters when auto = FALSE
if (!auto) {
  # Check if parameters are reasonable
  if (p < 0 || q < 0 || d < 0) {
    stop("ARIMA parameters (p, d, q) must be non-negative integers.")
  }
  if (p > 5 || q > 5) {
    warning(paste0("High order ARIMA models (p=", p, ", q=", q, 
                   ") may be numerically unstable. Consider using auto=TRUE or simpler parameters."))
  }
  if (seasonal && !is.null(seasonal_periods)) {
    if (P < 0 || Q < 0 || D < 0) {
      stop("Seasonal ARIMA parameters (P, D, Q) must be non-negative integers.")
    }
  }
}
```

#### Step 4: Add Fallback Logic for Numerically Unstable Models

**Location:** After line 294, add try-catch with fallback

```r
# Try to forecast, with fallback for unstable models
forecasted_df <- tryCatch({
  model_df %>% fabletools::forecast(h=periods)
}, error = function(e) {
  if (grepl("numerically unstable|characteristic roots", e$message, ignore.case = TRUE)) {
    if (!auto) {
      stop(paste0(
        "The manually specified ARIMA(", p, ",", d, ",", q, 
        ") model has numerically unstable characteristic roots.\n",
        "Recommendation: Set auto=TRUE to find a stable model automatically, ",
        "or try simpler parameters (e.g., reduce p and q values)."
      ))
    } else {
      stop(paste0("Model fitting failed: ", e$message))
    }
  } else {
    stop(e)
  }
})
```

### Testing Strategy

1. **Unit Tests:**
   - Test `auto = FALSE` with various parameter combinations (p=2,d=0,q=2, p=1,d=1,q=1, etc.)
   - Test with `allowmean = TRUE` and `allowmean = FALSE`
   - Test seasonal models with manual parameters
   - Test edge cases (p=0, q=0, high-order models)

2. **Integration Tests:**
   - Test with the exact customer query from the issue
   - Test with grouped data and manual parameters
   - Test error messages are helpful and actionable

3. **Regression Tests:**
   - Ensure existing test cases in `test_arima_1.R` still pass
   - Ensure `auto = TRUE` behavior is unchanged
   - Add specific test case for issue #33708

### Risk Assessment

**Low Risk Changes:**
- Adding constant term to formulas (Step 1) - follows fable::ARIMA requirements
- Improving error messages (Step 2) - only affects error output

**Medium Risk Changes:**
- Parameter validation (Step 3) - may change behavior for edge cases
- Fallback logic (Step 4) - adds new error handling path

**Mitigation:**
- Changes are backward compatible for `auto = TRUE` case
- New behavior only affects `auto = FALSE` which is already failing
- Error messages guide users to solutions
- Validation warnings don't break existing workflows

### Rollback Plan

If issues arise:
1. Revert constant term addition (Step 1) - can be done independently
2. Revert error message improvements (Step 2) - cosmetic only
3. Keep parameter validation (Step 3) - helps prevent issues
4. Fallback logic (Step 4) - can be disabled if problematic

### Documentation Updates

1. **Function Documentation:** Update `@param` documentation:
   - Clarify that `allowmean` controls constant term when `auto = FALSE`
   - Add guidance on choosing manual parameters
   - Add examples of common parameter combinations

2. **Code Comments:** Add comments explaining:
   - Why constant term is required for manual specifications
   - The relationship between `allowmean` and formula construction
   - When to use `auto = TRUE` vs manual parameters

## Timeline

**Estimated Timeframe:**

- **Investigation:** 2 hours (completed)
- **Development:** 6-8 hours
  - Implementation: 3-4 hours
  - Testing: 3-4 hours
- **Code Review:** 2 hours
- **Deployment:** 1 hour

**Total:** 11-13 hours

## Approval

**Reviewed By:** [To be filled]  
**Approved By:** [To be filled]  
**Date:** [To be filled]

---

## Additional Notes

### Customer Workaround (From Issue Comments)

The issue comments suggest these workarounds:
1. Use `auto = TRUE` to let ARIMA automatically select the model
2. Try simpler models (e.g., ARIMA(1,0,1) or ARIMA(1,0,0))
3. Consider differencing (set `d = 1` instead of `d = 0`)

### Related Issues

- The code already has guards for empty model results (lines 288-293)
- Function parameters `allowdrift` and `allowmean` exist but aren't fully utilized
- Similar issues may exist for other time series models in the codebase

### Code Quality Improvements

This fix also improves:
- Error message clarity and actionability
- Parameter validation
- User guidance for model selection
- Alignment with fable::ARIMA package requirements

### Future Considerations

1. Consider adding a `constant` parameter explicitly (in addition to `allowmean`)
2. Add model diagnostics to help users choose appropriate parameters
3. Consider adding automatic fallback to simpler models when manual specification fails
4. Document common ARIMA parameter choices for different data characteristics
