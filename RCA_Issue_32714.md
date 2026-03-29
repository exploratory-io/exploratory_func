# Root Cause Analysis: Issue #32714

## Issue Summary

**Issue ID:** #32714  
**Component:** `exp_xgboost` function in `R/build_xgboost.R`  
**Severity:** High (Blocks user workflow)  
**Customer:** 理想科学工業 (Riso Kogyo) - 庄司-san  
**Intercom:** https://app.intercom.com/a/inbox/b6uma1h1/inbox/shared/all/conversation/215470156576656

### Problem Description

When using XGBoost (and Random Forest) with explanatory variables containing `Inf` values, the model execution fails with an error. The error message is not user-friendly and doesn't indicate that `Inf` values are the root cause.

**XGBoost Error:**
```
[19:30:19] src/data/data.cc:1104: Check failed: valid: Input data contains `inf` or `nan`
Stack trace:
[bt] (0) 1 xgboost.so 0x0000000134065c3c dmlc::LogMessageFatal::~LogMessageFatal() + 124
[bt] (1) 2 xgboost.so 0x00000001340f311c unsigned long long xgboost::SparsePage::Push<xgboost::data::DenseAdapterBatch>(...) + 988
...
```

**Key Characteristics:**
- Only affects Random Forest and XGBoost models
- Decision trees, linear regression, and logistic regression work fine with `Inf` values
- NaN values do NOT cause errors (only `Inf` values)
- Error message is cryptic and doesn't help users understand the issue

## Root Cause Analysis

### Investigation Steps

1. **Issue Analysis:** Reviewed customer report and error message
2. **Code Review:** Analyzed `exp_xgboost` function and data preprocessing pipeline
3. **Comparison:** Compared with other model implementations (lm, glm, rpart) that handle `Inf` values
4. **Data Flow Analysis:** Traced data preprocessing from input to XGBoost DMatrix creation

### Findings

#### Primary Root Cause: Missing `Inf` Filtering in XGBoost Data Preprocessing

The issue occurs in the data preprocessing pipeline before XGBoost model creation. The root cause is in `R/build_xgboost.R` at line 1143:

```r
clean_df_ret <- cleanup_df_per_group(df, clean_target_col, sample_size, clean_cols, name_map, predictor_n, filter_numeric_na=FALSE, convert_logical=FALSE)
```

**Problem Chain:**

1. **`filter_numeric_na=FALSE` is passed** to `cleanup_df_per_group`
2. **`cleanup_df_per_group` calls `preprocess_regression_data_before_sample`** (line 2038 in `randomForest_tidiers.R`)
3. **`preprocess_regression_data_before_sample`** (in `R/build_lm.R`) has conditional logic:
   - When `filter_predictor_numeric_na=TRUE`: Filters both `NA` and `Inf` from numeric predictors (line 459)
   - When `filter_predictor_numeric_na=FALSE`: Only filters `NA` from numeric predictors, **NOT `Inf`** (line 467)

4. **`Inf` values remain in the data** and are passed to XGBoost
5. **XGBoost's DMatrix creation fails** when it encounters `Inf` values in the input matrix

#### Code Location Analysis

**File:** `R/build_xgboost.R`

| Line | Code | Issue |
|------|------|-------|
| 1143 | `filter_numeric_na=FALSE` | Disables NA filtering, but also prevents Inf filtering |
| 1207-1208 | Formula creation | Uses data that may contain Inf values |
| 1211-1257 | Model building | Calls `xgboost_binary`, `xgboost_reg`, or `xgboost_multi` |
| 34-64 | `fml_xgboost` | Creates model matrix that may contain Inf values |
| 73-78 | `xgb.DMatrix` / `xgboost` | XGBoost library fails here when Inf values are present |

**File:** `R/build_lm.R`

| Line | Code | Issue |
|------|------|-------|
| 458-468 | Conditional filtering logic | When `filter_predictor_numeric_na=FALSE`, only filters NA, not Inf |
| 467 | `dplyr::filter(!is.na(!!rlang::sym(col)))` | Missing `!is.infinite()` check |

#### Comparison with Other Models

**Models that handle Inf correctly:**
- **Linear Regression (`exp_lm`)**: Filters `Inf` from both target and predictors (line 429, 459 in `build_lm.R`)
- **Logistic Regression (`exp_glm`)**: Uses same preprocessing pipeline that filters `Inf`
- **Decision Trees (`exp_rpart`)**: Filters `Inf` values (line 3212 in `randomForest_tidiers.R` uses `filter_numeric_na=TRUE`)

**Models that fail:**
- **XGBoost (`exp_xgboost`)**: Uses `filter_numeric_na=FALSE`, so `Inf` values pass through
- **Random Forest (`exp_ranger`)**: Similar issue - uses conditional filtering that may not catch all `Inf` cases

#### Why NaN Doesn't Cause Errors

The customer confirmed that NaN values do NOT cause errors. This is because:
- XGBoost's error check specifically mentions "inf or nan" but the actual validation may be more lenient with NaN
- NaN values might be handled differently in the matrix conversion process
- The error message is misleading - it says "inf or nan" but only `Inf` actually causes the failure

### Specific Code Issues

#### Issue 1: Incomplete Filtering Logic in `preprocess_regression_data_before_sample`

**Location:** `R/build_lm.R`, lines 458-468

**Problem:**
When `filter_predictor_numeric_na=FALSE`, the code only filters `NA` values but not `Inf` values:

```r
if (filter_predictor_numeric_na) {
  df <- df %>% dplyr::filter(!is.na(!!rlang::sym(col)) & !is.infinite(!!rlang::sym(col)))
} else {
  # For ranger, removing numeric NA is not necessary.
  # But even for ranger, filter Inf/-Inf to avoid following error from ranger.
  # Error in seq.default(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = length.out) : 'from' must be a finite number
  # TODO: In exp_rpart and calc_feature_imp, we have logic to remember and restore NA rows, but they are probably not made use of
  # if we filter NA rows here.
  df <- df %>% dplyr::filter(!is.na(!!rlang::sym(col)))  # <-- Missing !is.infinite() check
}
```

**Note:** The comment mentions filtering `Inf` for ranger, but the code doesn't actually do it!

#### Issue 2: XGBoost Doesn't Filter Inf Values

**Location:** `R/build_xgboost.R`, line 1143

**Problem:**
XGBoost explicitly disables numeric NA filtering, which also disables `Inf` filtering:

```r
# XGBoost can work with NAs in numeric predictors. TODO: verify it.
# Also, no need to convert logical to factor unlike ranger.
clean_df_ret <- cleanup_df_per_group(df, clean_target_col, sample_size, clean_cols, name_map, predictor_n, filter_numeric_na=FALSE, convert_logical=FALSE)
```

**Impact:**
- `Inf` values pass through to XGBoost
- XGBoost's DMatrix creation fails with cryptic error

#### Issue 3: No User-Friendly Error Message

**Location:** `R/build_xgboost.R`, `fml_xgboost` function

**Problem:**
When XGBoost fails, the error message is cryptic and doesn't help users understand that `Inf` values are the issue:

```
[19:30:19] src/data/data.cc:1104: Check failed: valid: Input data contains `inf` or `nan`
```

**Impact:**
- Users don't know what to fix
- Users have to manually investigate and remove `Inf` values
- Poor user experience

### Scenario That Triggers the Bug

1. User has data with `Inf` values in predictor columns (e.g., division by zero, log(0), etc.)
2. User runs `exp_xgboost` with those predictors
3. Data preprocessing filters `NA` values but not `Inf` values (because `filter_numeric_na=FALSE`)
4. `Inf` values are included in the model matrix
5. XGBoost's `xgb.DMatrix` or `xgboost()` function fails during DMatrix creation
6. User sees cryptic error message and doesn't know how to fix it

## Impact Assessment

### User Impact
- **High:** Blocks workflow completely - users cannot run XGBoost models with `Inf` values
- **Poor UX:** Error message doesn't explain the issue or suggest a solution
- **Workaround exists:** Users can manually remove `Inf` values, but this is not discoverable

### Technical Impact
- **Low:** Fix is straightforward - add `Inf` filtering
- **No breaking changes:** Filtering `Inf` values is safe and expected behavior
- **Consistency:** Aligns XGBoost behavior with other models (lm, glm, rpart)

## Related Issues

- Similar issue exists in Random Forest (`exp_ranger`) - see error handling at line 42, 255 in `randomForest_tidiers.R`
- Other models (lm, glm, rpart) already filter `Inf` values correctly

## Summary of Root Causes

| Root Cause | Location | Status | Fix Description |
|------------|----------|--------|-----------------|
| Missing Inf filtering when filter_numeric_na=FALSE | `R/build_lm.R:467` | 🔴 Unfixed | Add `!is.infinite()` check even when `filter_predictor_numeric_na=FALSE` |
| XGBoost doesn't filter Inf values | `R/build_xgboost.R:1143` | 🔴 Unfixed | Ensure Inf values are filtered before XGBoost model creation |
| No user-friendly error message | `R/build_xgboost.R:fml_xgboost` | 🔴 Unfixed | Catch XGBoost errors and provide helpful message about Inf values |
| No notification when Inf values are removed | `R/build_xgboost.R:exp_xgboost` | 🔴 Unfixed | Track and report when Inf values are automatically removed |

## Version Compatibility

**Issue affects:** All versions that use `exp_xgboost`  
**Fix available in:** [To be determined after fix implementation]

## Customer Workaround

If the customer is on an affected version:

1. **Manually remove Inf values** before running XGBoost:
   ```r
   df <- df %>% 
     dplyr::filter(across(where(is.numeric), ~ !is.infinite(.x)))
   ```
2. **Or replace Inf with NA** and let XGBoost handle NA:
   ```r
   df <- df %>% 
     dplyr::mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x), NA, .x)))
   ```

However, the preferred solution is to fix the code to automatically handle `Inf` values and notify users.
