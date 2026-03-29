# SMOTE Keep Synthetic - Default Value Change

## Summary

The default value of `smote_keep_synthetic` parameter has been changed from `FALSE` to `TRUE` across all model functions.

## Rationale

Changing the default to `TRUE` provides:

1. **Transparency** - Users can see exactly what data was used for training, including synthetic samples
2. **Traceability** - The `synthesized` column clearly marks which samples are real vs. synthetic
3. **Better Understanding** - Users can inspect and verify SMOTE behavior on their specific dataset
4. **Educational Value** - New users learn about SMOTE by seeing the synthetic samples

## Impact

### Before (smote_keep_synthetic = FALSE by default):
- Users had to explicitly set `smote_keep_synthetic = TRUE` to see synthetic samples
- Output contained only original data
- Synthetic samples were "hidden" from view

### After (smote_keep_synthetic = TRUE by default):
- Users see SMOTE-enhanced data with synthetic samples by default
- A `synthesized` column marks real (FALSE) vs. synthetic (TRUE) samples
- Output has more rows than input when SMOTE is applied
- Users must explicitly set `smote_keep_synthetic = FALSE` to hide synthetic samples

## Affected Functions

All model functions with SMOTE support now default to `smote_keep_synthetic = TRUE`:

- `build_lm.fast()` - Linear/Logistic Regression
- `exp_xgboost()` - XGBoost
- `exp_lightgbm()` - LightGBM
- `calc_feature_imp()` - Ranger (Random Forest)
- `exp_rpart()` - Decision Trees

## Migration Guide

If you want the old behavior (hiding synthetic samples), explicitly set the parameter:

```r
# Old behavior (now requires explicit setting)
model_df <- data %>%
  build_lm.fast(
    target, x1, x2,
    smote = TRUE,
    smote_keep_synthetic = FALSE  # Explicitly hide synthetic samples
  )
```

## Example Usage

### Default Behavior (Shows Synthetic Samples)

```r
# SMOTE with default settings - synthetic samples included
model_df <- data %>%
  build_lm.fast(
    target, x1, x2,
    smote = TRUE
  )

source_data <- model_df$source.data[[1]]

# Check composition
cat("Total rows:", nrow(source_data), "\n")
cat("Real samples:", sum(!source_data$synthesized), "\n")
cat("Synthetic samples:", sum(source_data$synthesized), "\n")
```

### Opt-Out to Hide Synthetic Samples

```r
# Explicitly hide synthetic samples
model_df <- data %>%
  build_lm.fast(
    target, x1, x2,
    smote = TRUE,
    smote_keep_synthetic = FALSE
  )

source_data <- model_df$source.data[[1]]
# Now source_data contains only original data
# No 'synthesized' column
```

## Key Points

1. **Test Data Purity**: Test data NEVER contains synthetic samples, regardless of this setting
2. **Backward Compatibility**: Existing code that doesn't specify `smote_keep_synthetic` will now see synthetic samples
3. **Easy Filtering**: If you get synthetic samples but don't want them, filter with `filter(!synthesized)`
4. **No Impact on Training**: The model training process is identical in both cases; only the output differs

## Benefits of the New Default

### For New Users:
- Immediately see what SMOTE is doing
- Learn about synthetic sample generation
- Can verify SMOTE behavior visually

### For Data Scientists:
- Inspect synthetic samples for quality control
- Verify class balancing worked as expected
- Debug SMOTE parameter tuning more easily

### For Production:
- Greater transparency in model training
- Better audit trail of training data composition
- Can document exact data used for model training

## Considerations

### When to Keep Default (TRUE):
- ✅ Exploring data and SMOTE behavior
- ✅ Debugging class imbalance issues
- ✅ Documenting model training process
- ✅ Quality control and validation
- ✅ Learning and education

### When to Set FALSE:
- ✅ Need clean output matching input row count
- ✅ Downstream processing expects original data only
- ✅ Don't want to deal with `synthesized` column
- ✅ Simpler data export and reporting
- ✅ Integration with existing pipelines

## Updated Documentation

The following files have been updated to reflect the new default:

- `SMOTE_KEEP_SYNTHETIC_FEATURE.md` - Complete feature documentation
- `test_smote_keep_synthetic.R` - Test script
- All function signatures in:
  - `R/build_lm.R`
  - `R/build_xgboost.R`
  - `R/build_lightgbm.R`
  - `R/randomForest_tidiers.R`

## Questions?

If you have questions about this change or need help adapting your code, please refer to:
- `SMOTE_KEEP_SYNTHETIC_FEATURE.md` for detailed feature documentation
- `test_smote_keep_synthetic.R` for usage examples


