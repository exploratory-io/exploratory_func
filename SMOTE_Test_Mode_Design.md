# SMOTE and Test Mode Behavior Change - Design Document

## Executive Summary

This document describes the design for changing the order of operations when both SMOTE (Synthetic Minority Over-sampling Technique) and Test Mode are enabled. The change ensures that SMOTE is applied only to training data after the train/test split, preventing synthetic data from contaminating the test set.

## Current Behavior

### Problem Statement

Currently, when both SMOTE and Test Mode are enabled, the operations occur in this order:

1. **Data preprocessing** (cleaning, filtering, etc.)
2. **Apply SMOTE** to the entire dataset → Creates synthetic samples
3. **Split data** into training and test sets → Test set may contain synthetic samples
4. **Train model** on training data
5. **Evaluate** on test data (which may include synthetic samples)

**Issue**: Test data may contain synthetic (fake) samples created by SMOTE, leading to:
- Misleading performance metrics
- Overly optimistic model evaluation
- Test set not representing real-world data distribution
- Invalid assessment of model generalization capability

### Affected Files

The following files implement SMOTE and are affected by this issue:

1. **R/build_lightgbm.R** (lines 1637-1644)
   - Function: `exp_lightgbm()`
   
2. **R/build_lm.R** (lines 900-931)
   - Function: `build_lm.fast()` for GLM models
   
3. **R/build_xgboost.R** (lines 1156-1164)
   - Function: `exp_xgboost()`
   
4. **R/randomForest_tidiers.R**
   - Lines 2222-2230: Function `exp_ranger()`
   - Lines 3146-3162: Function `exp_rpart()`

## Proposed New Behavior

### Solution Overview

Change the order of operations so that SMOTE is applied only to training data:

1. **Data preprocessing** (cleaning, filtering, etc.)
2. **Split data** into training and test sets → Test set is pure, unmodified data
3. **Apply SMOTE** only to training data → Creates synthetic samples for training only
4. **Train model** on SMOTE-enhanced training data
5. **Evaluate** on original, untouched test data

**Benefits**:
- Test data remains pure and represents real-world distribution
- More accurate and reliable performance metrics
- Better assessment of model's ability to generalize
- Industry best practice compliance

### Key Principle

> **SMOTE should be considered a training augmentation technique, not a data preprocessing step.**

## Implementation Details

### General Pattern

For each affected function, the code structure will change from:

```r
# CURRENT (INCORRECT) ORDER
# 1. Apply SMOTE if conditions are met
if (smote && length(unique_val[!is.na(unique_val)]) == 2) {
  df <- df %>% exp_balance(...)
  df <- df %>% dplyr::select(-synthesized)
}

# 2. Split into train/test
source_data <- df  # source_data now contains synthetic samples
test_index <- sample_df_index(source_data, rate = test_rate, ...)
df <- safe_slice(source_data, test_index, remove = TRUE)
if (test_rate > 0) {
  df_test <- safe_slice(source_data, test_index, remove = FALSE)
}
```

To:

```r
# NEW (CORRECT) ORDER
# 1. Split into train/test FIRST
source_data <- df  # source_data contains only original data
test_index <- sample_df_index(source_data, rate = test_rate, ...)
df <- safe_slice(source_data, test_index, remove = TRUE)
if (test_rate > 0) {
  df_test <- safe_slice(source_data, test_index, remove = FALSE)
}

# 2. Apply SMOTE only to training data
if (smote && length(unique_val[!is.na(unique_val)]) == 2) {
  df <- df %>% exp_balance(...)
  df <- df %>% dplyr::select(-synthesized)
}
```

### Special Considerations

#### 1. Sample Size Logic (build_lightgbm.R and others)

**Current behavior**: When SMOTE is enabled, `sample_size` is set to `NULL` before the main cleanup step:

```r
if (smote && length(unique_val[!is.na(unique_val)]) == 2) {
  sample_size <- NULL  # Don't downsample; let exp_balance handle it
} else {
  sample_size <- max_nrow
}
```

**New behavior**: This logic remains the same, but now it affects only the pre-split dataset size, not the post-SMOTE training size.

#### 2. GLM Marginal Effects (build_lm.R)

**Current behavior** (lines 900-926): When `with_marginal_effects` is enabled, a copy of pre-SMOTE data is kept:

```r
if (smote) {
  if (with_marginal_effects) {
    df_before_smote <- df  # Keep pre-SMOTE data for marginal effects
  }
  df <- df %>% exp_balance(...)
  # ... clean up factors
  if (with_marginal_effects) {
    df_before_smote <- df_before_smote %>% sample_rows(max_nrow)
  }
}
```

**New behavior**: This logic remains similar but needs adjustment:
- The pre-SMOTE data for marginal effects should be the training data before SMOTE
- Split happens first, then SMOTE, then the marginal effects data is derived from training-only

```r
# Split first
source_data <- df
test_index <- sample_df_index(source_data, rate = test_rate, ...)
df <- safe_slice(source_data, test_index, remove = TRUE)
if (test_rate > 0) {
  df_test <- safe_slice(source_data, test_index, remove = FALSE)
}

# Then SMOTE on training only
if (smote) {
  if (with_marginal_effects) {
    df_before_smote <- df  # This is now training data only
  }
  df <- df %>% exp_balance(...)
  # ... rest of SMOTE logic
}
```

#### 3. Factor Level Cleanup (build_lm.R)

The factor level cleanup that happens after SMOTE (lines 910-920) must still be applied, but now it only affects training data:

```r
for(col in names(df)){
  if(is.factor(df[[col]])) {
    df[[col]] <- forcats::fct_drop(df[[col]])
    if (with_marginal_effects) {
      df_before_smote <- df_before_smote %>% 
        dplyr::filter(!!rlang::sym(col) %in% levels(df[[col]]))
      df_before_smote[[col]] <- forcats::fct_drop(df_before_smote[[col]])
    }
  }
}
```

This remains the same and continues to work on training data only.

#### 4. LightGBM Validation Data (build_lightgbm.R)

Lines 1646-1661 handle validation data for LightGBM. The test data used for validation should remain clean:

```r
if (test_rate > 0) {
  df_test <- safe_slice(source_data, test_index, remove = FALSE)
  df_test_clean <- cleanup_df_for_test(df_test, df, c_cols)
  # ... use as validation data
}
```

This logic is correct and doesn't need changes - the validation data is already from the test split.

## Detailed Changes by File

### 1. R/build_lightgbm.R

**Location**: Lines 1636-1644 in `exp_lightgbm()` function

**Current Code**:
```r
unique_val <- unique(df[[clean_target_col]])
if (smote && length(unique_val[!is.na(unique_val)]) == 2) {
  df <- df %>% exp_balance(clean_target_col, target_size = max_nrow, 
                          target_minority_perc = smote_target_minority_perc, 
                          max_synth_perc = smote_max_synth_perc, k = smote_k)
  df <- df %>% dplyr::select(-synthesized)
}

source_data <- df
test_index <- sample_df_index(source_data, rate = test_rate, ordered = (test_split_type == "ordered"))
df <- safe_slice(source_data, test_index, remove = TRUE)
```

**New Code**:
```r
# Split FIRST - before SMOTE
source_data <- df
test_index <- sample_df_index(source_data, rate = test_rate, ordered = (test_split_type == "ordered"))
df <- safe_slice(source_data, test_index, remove = TRUE)

# Apply SMOTE only to training data
unique_val <- unique(df[[clean_target_col]])
if (smote && length(unique_val[!is.na(unique_val)]) == 2) {
  df <- df %>% exp_balance(clean_target_col, target_size = max_nrow, 
                          target_minority_perc = smote_target_minority_perc, 
                          max_synth_perc = smote_max_synth_perc, k = smote_k)
  df <- df %>% dplyr::select(-synthesized)
}
```

### 2. R/build_lm.R

**Location**: Lines 900-935 in `build_lm.fast()` function

**Current Code**: (Complex due to marginal effects handling)

**New Code**: Move the train/test split before SMOTE, adjust `df_before_smote` logic:
```r
# Split FIRST - before SMOTE
source_data <- df
test_index <- sample_df_index(source_data, rate = test_rate, ordered = (test_split_type == "ordered"))
df <- safe_slice(source_data, test_index, remove = TRUE)
if (test_rate > 0) {
  df_test <- safe_slice(source_data, test_index, remove = FALSE)
}

# Apply SMOTE only to training data
if (smote) {
  if (with_marginal_effects) {
    # Keep the version of training data before SMOTE,
    # since we want to know average marginal effect on a data that has
    # close distribution to the original data.
    df_before_smote <- df
  }
  # Note: If there is weight column, we synthesize weight column as well.
  df <- df %>% exp_balance(clean_target_col, target_size = max_nrow, 
                          target_minority_perc = smote_target_minority_perc, 
                          max_synth_perc = smote_max_synth_perc, k = smote_k)
  df <- df %>% dplyr::select(-synthesized)
  
  for(col in names(df)){
    if(is.factor(df[[col]])) {
      df[[col]] <- forcats::fct_drop(df[[col]])
      if (with_marginal_effects) {
        df_before_smote <- df_before_smote %>% 
          dplyr::filter(!!rlang::sym(col) %in% levels(df[[col]]))
        df_before_smote[[col]] <- forcats::fct_drop(df_before_smote[[col]])
      }
    }
  }
  
  if (with_marginal_effects) {
    df_before_smote <- df_before_smote %>% sample_rows(max_nrow)
  }
}
```

### 3. R/build_xgboost.R

**Location**: Lines 1155-1167 in `exp_xgboost()` function

**Current Code**:
```r
unique_val <- unique(df[[clean_target_col]])
if (smote && length(unique_val[!is.na(unique_val)]) == 2) {
  df <- df %>% exp_balance(clean_target_col, target_size = max_nrow, 
                          target_minority_perc = smote_target_minority_perc, 
                          max_synth_perc = smote_max_synth_perc, k = smote_k)
  df <- df %>% dplyr::select(-synthesized)
}

source_data <- df
test_index <- sample_df_index(source_data, rate = test_rate, ordered = (test_split_type == "ordered"))
df <- safe_slice(source_data, test_index, remove = TRUE)
if (test_rate > 0) {
  df_test <- safe_slice(source_data, test_index, remove = FALSE)
}
```

**New Code**:
```r
# Split FIRST - before SMOTE
source_data <- df
test_index <- sample_df_index(source_data, rate = test_rate, ordered = (test_split_type == "ordered"))
df <- safe_slice(source_data, test_index, remove = TRUE)
if (test_rate > 0) {
  df_test <- safe_slice(source_data, test_index, remove = FALSE)
}

# Apply SMOTE only to training data
unique_val <- unique(df[[clean_target_col]])
if (smote && length(unique_val[!is.na(unique_val)]) == 2) {
  df <- df %>% exp_balance(clean_target_col, target_size = max_nrow, 
                          target_minority_perc = smote_target_minority_perc, 
                          max_synth_perc = smote_max_synth_perc, k = smote_k)
  df <- df %>% dplyr::select(-synthesized)
}
```

### 4. R/randomForest_tidiers.R

#### 4a. exp_ranger() function

**Location**: Lines 2221-2233

**Changes**: Same pattern as XGBoost - move split before SMOTE

#### 4b. exp_rpart() function

**Location**: Lines 3145-3165

**Changes**: Same pattern as XGBoost - move split before SMOTE

## Edge Cases and Validation

### Edge Cases to Consider

1. **SMOTE disabled, Test Mode enabled**: No change in behavior
2. **SMOTE enabled, Test Mode disabled** (test_rate = 0): SMOTE applies to all training data (no change)
3. **Both SMOTE and Test Mode enabled**: New behavior applies
4. **Very small training set after split**: SMOTE may not have enough samples to work with
   - The `exp_balance()` function should handle this gracefully
5. **All minority class ends up in test set**: Training data would have single class
   - Existing validation should catch this

### Validation Checks

After implementation, verify:

1. **Test set purity**: Test data contains no synthetic samples
2. **Training set size**: Training set may be larger after SMOTE (expected)
3. **Test metrics**: Metrics should be more conservative (possibly lower) than before
4. **Factor levels**: Factor levels in test data are still handled correctly by model
5. **Source data**: `source_data` column in output should contain original, un-SMOTE'd data

## Testing Strategy

### Unit Tests to Add/Modify

1. **Test SMOTE order with test mode**:
   ```r
   test_that("SMOTE is applied after train/test split", {
     # Create imbalanced dataset with identifiable rows
     # Enable both SMOTE and test_rate
     # Verify test set contains only original rows
     # Verify training set size increased (due to SMOTE)
   })
   ```

2. **Test source_data integrity**:
   ```r
   test_that("source.data contains original data without SMOTE samples", {
     # Train model with SMOTE and test_rate
     # Extract source.data from result
     # Verify it matches original data size and content
   })
   ```

3. **Test marginal effects with SMOTE** (for build_lm.R):
   ```r
   test_that("marginal effects use training-only pre-SMOTE data", {
     # Train GLM with SMOTE, test_rate, and with_marginal_effects
     # Verify marginal effects are calculated correctly
   })
   ```

### Integration Tests

1. Compare metrics before/after change on known datasets
2. Verify all model types (LightGBM, XGBoost, Random Forest, RPart, GLM)
3. Test with various test_rate values (0.1, 0.2, 0.3, 0.5)
4. Test with various SMOTE parameters

## Impact Assessment

### Benefits

1. **Correctness**: Test metrics now reflect true model performance on unseen data
2. **Best Practices**: Aligns with ML community standards
3. **Reliability**: More trustworthy model evaluation
4. **Debugging**: Easier to debug model issues when test set is pure

### Potential Concerns

1. **Metric Changes**: Existing models may show different (likely lower) test metrics
   - This is expected and correct - previous metrics were inflated
2. **User Communication**: Users need to understand why metrics may change
3. **Documentation**: Need to update documentation about SMOTE behavior

### Breaking Changes

**Behavior Change**: Yes, this changes observable behavior when both SMOTE and test mode are enabled.

**User Impact**:
- Users with existing models may see different test metrics upon retraining
- Test set metrics will be more conservative (realistic)
- No impact on non-test mode or non-SMOTE workflows

**Recommendation**: Clearly document this as a bug fix in release notes.

## Documentation Updates

### Release Notes

```markdown
### Bug Fix: SMOTE Now Applied Only to Training Data

**Fixed**: When using SMOTE (Synthetic Minority Over-sampling) with Test Mode enabled,
SMOTE is now correctly applied only to training data after the train/test split.
Previously, SMOTE was applied before splitting, which could result in synthetic
samples appearing in the test set.

**Impact**: Test metrics may be more conservative (lower) than before, as they now
reflect performance on real, unseen data rather than potentially including synthetic
samples. This change aligns with ML best practices and provides more accurate model
evaluation.

**Affected Functions**:
- `exp_lightgbm()`
- `exp_xgboost()`
- `exp_ranger()`
- `exp_rpart()`
- `build_lm.fast()` (GLM models)
```

### User Documentation

Update documentation to clarify:
1. SMOTE is a training augmentation technique
2. Test data always remains unmodified
3. `source.data` contains original data without synthetic samples
4. Training data size may exceed `max_nrow` when SMOTE is applied

## Implementation Checklist

- [ ] Update `R/build_lightgbm.R`
- [ ] Update `R/build_lm.R`
- [ ] Update `R/build_xgboost.R`
- [ ] Update `R/randomForest_tidiers.R` (exp_ranger)
- [ ] Update `R/randomForest_tidiers.R` (exp_rpart)
- [ ] Add/update unit tests for each affected function
- [ ] Run full test suite
- [ ] Update release notes
- [ ] Update user documentation
- [ ] Code review
- [ ] QA testing with various scenarios

## Timeline

**Estimated Effort**: 1-2 days
- Code changes: 4 hours
- Testing: 4 hours
- Documentation: 2 hours
- Review and QA: 2 hours

## Conclusion

This change fixes a fundamental issue in the SMOTE implementation that could lead to inflated and misleading model performance metrics. While it represents a behavioral change, it is a necessary bug fix that aligns with ML best practices and will provide users with more accurate and reliable model evaluations.

The implementation is straightforward - moving the SMOTE operation after the train/test split - but requires careful attention to special cases like marginal effects calculation in GLM models.

