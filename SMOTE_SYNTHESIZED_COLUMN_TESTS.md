# Synthesized Column Name Preservation Tests

## Overview

This document describes the test cases added to verify that the `synthesized` column name is properly preserved when `name_map` is used to restore original column names in the SMOTE implementation.

## Problem Being Tested

When `smote_keep_synthetic = TRUE`, the `synthesized` column is added to `source_data` after `name_map` is created. During column name restoration, columns not in `name_map` could potentially get `NA` names. The fix ensures that such columns (like `synthesized`) retain their original names.

## Test File

**Location**: `tests/testthat/test_smote_synthesized_column.R`

## Test Cases

### 1. `exp_xgboost: synthesized column name is preserved with special character columns`

**Purpose**: Verify that the `synthesized` column retains its name when columns with special characters are renamed through `name_map`.

**Test Data**: Columns with special characters (`special col!`, `another-col`, `col with spaces`)

**Assertions**:
- ✅ `synthesized` column exists
- ✅ No `NA` column names
- ✅ All original column names are preserved
- ✅ `synthesized` column has correct logical type
- ✅ Test data has `synthesized = FALSE`
- ✅ Training data has some `synthesized = TRUE`

### 2. `exp_lightgbm: synthesized column name is preserved with special character columns`

**Purpose**: Same as test 1, but for LightGBM models.

**Test Data**: Columns with special characters (`x 1`, `x-2`, `x!3`)

**Assertions**:
- ✅ `synthesized` column exists
- ✅ No `NA` column names
- ✅ All original column names are preserved
- ✅ `synthesized` column has correct logical type

### 3. `calc_feature_imp (ranger): synthesized column name is preserved with special character columns`

**Purpose**: Same as test 1, but for Ranger (Random Forest) models.

**Test Data**: Columns with special characters (`pred@1`, `pred#2`, `pred$3`)

**Assertions**:
- ✅ `synthesized` column exists
- ✅ No `NA` column names
- ✅ All original column names are preserved
- ✅ `synthesized` column has correct logical type

### 4. `exp_xgboost: no NA column names when smote_keep_synthetic = FALSE`

**Purpose**: Verify that column names are preserved correctly even when `smote_keep_synthetic = FALSE` (no `synthesized` column should exist).

**Test Data**: Columns with special characters (`col-1`, `col 2`)

**Assertions**:
- ✅ No `NA` column names
- ✅ All original column names are preserved
- ✅ `synthesized` column does NOT exist

### 5. `exp_xgboost: column names are correct when no SMOTE`

**Purpose**: Verify that column names are preserved correctly when SMOTE is disabled.

**Test Data**: Columns with special characters (`var!1`, `var@2`)

**Assertions**:
- ✅ No `NA` column names
- ✅ All original column names are preserved
- ✅ `synthesized` column does NOT exist

### 6. `Multiple special characters in one column name are handled correctly`

**Purpose**: Test extreme cases with very complex column names containing multiple types of special characters.

**Test Data**: Columns with multiple special characters:
- `col with spaces & special!@#`
- `another-complex_name (test)`
- `simple`

**Assertions**:
- ✅ No `NA` column names
- ✅ All original complex column names are preserved exactly
- ✅ `synthesized` column exists
- ✅ Exact column set matches expectations

## Implementation Details

The fix uses the following logic when restoring column names:

```r
# Preserve column names that don't have mappings (like 'synthesized')
new_names <- rev_name_map[colnames(source_data)]
colnames(source_data) <- ifelse(is.na(new_names), colnames(source_data), new_names)
```

This ensures:
1. Columns in `name_map` get their original names restored
2. Columns NOT in `name_map` (like `synthesized`) keep their current names
3. No columns end up with `NA` names

## Files Modified

The fix was applied to:
- `R/build_xgboost.R`
- `R/build_lightgbm.R`
- `R/randomForest_tidiers.R` (for `calc_feature_imp`)

Note: `R/build_lm.R` and `exp_rpart` don't use the `rev_name_map` pattern, so they didn't require this fix.

## Running the Tests

Run the tests using your standard test workflow:

```r
# Run all SMOTE-related tests
devtools::test(filter = "smote")

# Run only synthesized column tests
devtools::test(filter = "smote_synthesized_column")

# Run specific test file
testthat::test_file("tests/testthat/test_smote_synthesized_column.R")
```

## Expected Results

All tests should pass, confirming that:
1. The `synthesized` column name is never lost or converted to `NA`
2. Original column names with special characters are preserved correctly
3. The fix works across all model types (XGBoost, LightGBM, Ranger)
4. The fix doesn't break behavior when `smote_keep_synthetic = FALSE` or when SMOTE is disabled


