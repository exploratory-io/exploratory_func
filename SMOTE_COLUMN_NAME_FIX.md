# SMOTE Synthesized Column Name Preservation Fix

## Issue

GitHub Copilot identified a potential bug: When `smote_keep_synthetic = TRUE`, the `synthesized` column is added to `source_data` after `name_map` is created. When column names are later restored using `rev_name_map[colnames(source_data)]`, any column not in the mapping (like `synthesized`) would get an `NA` name.

## Root Cause

The code flow was:
1. Create `name_map` for cleaning column names
2. Perform operations on cleaned names
3. Add `synthesized` column (not in `name_map`)
4. Restore original names using: `colnames(source_data) <- rev_name_map[colnames(source_data)]`

In step 4, the `synthesized` column lookup would return `NA` because it wasn't in `rev_name_map`.

## Solution

Modified the name restoration logic to preserve column names that don't have mappings:

```r
# Before (problematic):
colnames(source_data) <- rev_name_map[colnames(source_data)]

# After (fixed):
new_names <- rev_name_map[colnames(source_data)]
colnames(source_data) <- ifelse(is.na(new_names), colnames(source_data), new_names)
```

This ensures:
- Columns in `name_map` → get original names restored
- Columns NOT in `name_map` → keep current names (like `synthesized`)
- No columns end up with `NA` names

## Files Modified

### 1. `R/build_xgboost.R` (line ~1199)
```r
# Restore source_data column name to original column name
rev_name_map <- names(name_map)
names(rev_name_map) <- name_map
# Preserve column names that don't have mappings (like 'synthesized')
new_names <- rev_name_map[colnames(source_data)]
colnames(source_data) <- ifelse(is.na(new_names), colnames(source_data), new_names)
```

### 2. `R/build_lightgbm.R` (line ~1702)
Same fix applied to LightGBM model function.

### 3. `R/randomForest_tidiers.R` (line ~2280)
Same fix applied to `calc_feature_imp` (Ranger) function.

### Note
`R/build_lm.R` and `exp_rpart` don't use the `rev_name_map` pattern, so they didn't require this fix.

## Test Coverage

Created comprehensive test suite: `tests/testthat/test_smote_synthesized_column.R`

### Test Cases

1. **XGBoost with special character columns**
   - Verifies `synthesized` column name is preserved
   - Tests with columns like: `special col!`, `another-col`, `col with spaces`
   - Confirms no `NA` column names

2. **LightGBM with special character columns**
   - Same verification for LightGBM models
   - Tests with columns like: `x 1`, `x-2`, `x!3`

3. **Ranger with special character columns**
   - Same verification for Ranger models
   - Tests with columns like: `pred@1`, `pred#2`, `pred$3`

4. **XGBoost with `smote_keep_synthetic = FALSE`**
   - Ensures no `NA` names even without `synthesized` column
   - Confirms `synthesized` column doesn't exist when not keeping synthetic samples

5. **XGBoost with `smote = FALSE`**
   - Ensures no `NA` names when SMOTE is disabled
   - Confirms `synthesized` column doesn't exist

6. **Multiple special characters test**
   - Extreme test with very complex column names:
     - `col with spaces & special!@#`
     - `another-complex_name (test)`
   - Verifies all names are preserved exactly

## Testing Instructions

Run the tests using your standard test workflow:

```bash
# Run all SMOTE-related tests
Rscript -e "devtools::test(filter = 'smote')"

# Run only synthesized column tests
Rscript -e "devtools::test(filter = 'smote_synthesized_column')"
```

## Impact

- ✅ Fixes potential bug where `synthesized` column could get `NA` name
- ✅ Works correctly with columns containing special characters
- ✅ Maintains backward compatibility
- ✅ No impact on existing functionality
- ✅ Comprehensive test coverage added

## Related Files

- `SMOTE_SYNTHESIZED_COLUMN_TESTS.md` - Detailed test documentation
- `tests/testthat/test_smote_synthesized_column.R` - Test implementation


