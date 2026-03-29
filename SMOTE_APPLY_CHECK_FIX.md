# SMOTE Application Check Fix

## Problem

Test failures occurred because the code assumed SMOTE was applied whenever `smote = TRUE`, but `exp_balance()` can sometimes return the original dataframe without applying SMOTE under certain conditions:

1. **No rows left after filtering NAs** - Returns original df
2. **Target has less than 2 distinct values after filtering** - Returns original df  
3. **SMOTE undersamples too much** (leaves only 1 unique value) - Returns original df

When SMOTE isn't applied, `exp_balance()` doesn't add the `synthesized` column. Our code was trying to access this column unconditionally, causing errors.

## Root Cause

The code flow was:
1. Call `exp_balance()` assuming it will apply SMOTE
2. Assume `synthesized` column exists
3. Try to manipulate/remove the `synthesized` column
4. **FAIL** if column doesn't exist (SMOTE wasn't actually applied)

## Solution

Added explicit check for whether SMOTE was actually applied by verifying the presence of the `synthesized` column after calling `exp_balance()`:

```r
# Before (problematic):
if (smote && length(unique_val[!is.na(unique_val)]) == 2) {
  df <- df %>% exp_balance(...)
  
  if (smote_keep_synthetic) {
    # Assume synthesized column exists
    source_data <- bind_rows(df, df_test)
  } else {
    # Try to remove column that might not exist
    df <- df %>% dplyr::select(-synthesized)
  }
  smote_applied <- TRUE  # Not necessarily true!
}

# After (fixed):
if (smote && length(unique_val[!is.na(unique_val)]) == 2) {
  df <- df %>% exp_balance(...)
  
  # Check if SMOTE was actually applied
  smote_applied <- "synthesized" %in% colnames(df)
  
  if (smote_keep_synthetic && smote_applied) {
    # Only update if SMOTE was actually applied
    source_data <- bind_rows(df, df_test)
  } else if (smote_applied) {
    # Only remove if column exists
    df <- df %>% dplyr::select(-synthesized)
  }
}
```

## Changes Made

### Files Updated

1. **R/build_lm.R**
   - Added `smote_applied` check after `exp_balance()` call
   - Only update `source_data` if SMOTE was actually applied
   - Only remove `synthesized` column if it exists

2. **R/build_xgboost.R**
   - Same fix applied

3. **R/build_lightgbm.R**
   - Same fix applied

4. **R/randomForest_tidiers.R**
   - Same fix applied to both `calc_feature_imp` and `exp_rpart`

### Logic Flow

```
1. Try to apply SMOTE via exp_balance()
2. Check if synthesized column exists
3. If column exists:
   - SMOTE was applied successfully
   - If smote_keep_synthetic = TRUE: include synthetic samples in source.data
   - If smote_keep_synthetic = FALSE: remove synthesized column
4. If column doesn't exist:
   - SMOTE was not applied (returned original data)
   - Don't update source.data
   - Don't try to remove non-existent column
```

## Impact

- ✅ **Robustness**: Code now handles cases where SMOTE can't be applied
- ✅ **No false positives**: `smote_applied` flag accurately reflects whether SMOTE was used
- ✅ **Prevents errors**: No attempt to access/remove non-existent `synthesized` column
- ✅ **Correct behavior**: `source.data` only includes synthetic samples when they actually exist
- ✅ **Test compatibility**: Tests can now detect when SMOTE wasn't applied

## Test Expectations

After this fix, tests should:
1. **Check if SMOTE was applied** before asserting synthetic samples exist
2. **Handle both cases**: SMOTE applied vs. SMOTE not applied
3. **Verify correct behavior** in both scenarios

## When SMOTE May Not Apply

SMOTE may not be applied when:
- Training data has too few samples after filtering NAs
- Training data has too few minority class samples (< k+1, where k is typically 5)
- Training data has only one class after filtering
- Predictor columns have too many NAs
- SMOTE's undersampling removes too many majority samples

In all these cases, the code now gracefully handles the situation and returns the original data without synthetic samples.


