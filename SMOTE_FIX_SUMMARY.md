# SMOTE with Test Mode - Fix Summary

## Problem

After implementing the SMOTE behavior change (where SMOTE is applied only to training data after the train/test split), users encountered an error when calling `prediction_binary`:

```
Error in dplyr::mutate(., source.data.training = purrr::map2(model, source.data.training, ...)):
! Assigned data `predict(x, newdata, type = type.predict) %>% unname()`
  must be compatible with existing data.
✖ Existing data has 1029 rows.
✖ Assigned data has 1245 rows.
```

## Root Cause

The issue occurred because:

1. **After the train/test split**, SMOTE was applied to the training data, increasing its size (e.g., from 720 rows to 1245 rows)
2. The model was trained on this SMOTE-enhanced training data (1245 rows)
3. However, `source.data` stored in the model data frame contained the **original data** (1029 rows total)
4. When `prediction_binary` (or `prediction`) was called with `data='training_and_test'`:
   - It would slice `source.data` to get training data (original ~720 rows)
   - It would call the model's `augment` function to add predictions
   - The `augment` function expected to return predictions matching the provided data size
   - But the model's internal training data was SMOTE-enhanced (1245 rows), causing a mismatch

## Solution

The fix ensures that when SMOTE is applied, the model generates and stores predictions on the **original training data (before SMOTE)** rather than on the SMOTE-enhanced data. This way, predictions match the size of `source.data`.

### Changes Made

#### 1. **GLM (build_lm.R)**
- Store original training data before SMOTE: `df_train_original <- df`
- After model training, generate predictions on original data when SMOTE was used:
  ```r
  if (smote) {
    model$prediction_training <- predict(model, df_train_original, se.fit = TRUE)
    model$smote_applied <- TRUE
  }
  ```
- Update `augment.glm_exploratory` to use stored predictions when SMOTE was applied:
  ```r
  if (!is.null(x$smote_applied) && x$smote_applied && !is.null(x$prediction_training)) {
    data$.fitted <- x$prediction_training$fit
    data$.se.fit <- x$prediction_training$se.fit
    data
  }
  ```

#### 2. **XGBoost (build_xgboost.R)**
- Store original training data before SMOTE: `df_train_original <- df`
- Track whether SMOTE was applied: `smote_applied <- TRUE` 
- Generate predictions on original data when SMOTE was used:
  ```r
  if (smote_applied) {
    model$prediction_training <- predict_xgboost(model, df_train_original)
  } else {
    model$prediction_training <- predict_xgboost(model, df)
  }
  ```

#### 3. **LightGBM (build_lightgbm.R)**
- Store original training data before SMOTE: `df_train_original <- df`
- Track whether SMOTE was applied: `smote_applied <- TRUE`
- Generate predictions on original data when SMOTE was used:
  ```r
  if (smote_applied) {
    model$prediction_training <- predict_lightgbm(model, df_train_original)
  } else {
    model$prediction_training <- predict_lightgbm(model, df)
  }
  ```

#### 4. **Ranger (randomForest_tidiers.R)**
- Store original training data before SMOTE: `df_train_original <- df`
- Create `model_df_original` from original training data when SMOTE was applied
- Generate predictions on original data when SMOTE was used:
  ```r
  if (smote_applied) {
    model$prediction_training <- predict(model, model_df_original)
  } else {
    model$prediction_training <- predict(model, model_df)
  }
  ```

#### 5. **RPart (randomForest_tidiers.R)**
- Store original training data before SMOTE: `df_train_original <- df`
- Track whether SMOTE was applied: `smote_applied <- TRUE`
- Generate and store predicted classes and probabilities on original data:
  ```r
  if (smote_applied) {
    pred_original <- predict(model, df_train_original)
    model$predicted_class_original <- ... # compute from pred_original
    model$predicted_prob_original <- pred_original
    model$smote_applied <- TRUE
  }
  ```
- Update `augment.rpart.classification` to use stored predictions when SMOTE was applied

## Key Principle

**The model is trained on SMOTE-enhanced data to benefit from class balancing, but predictions are generated and stored on the original training data to maintain consistency with `source.data`.**

This ensures:
- Training benefits from SMOTE's synthetic samples for better class balance
- Test data remains pure (no synthetic samples)
- Predictions align with the original data dimensions stored in `source.data`
- `prediction_binary` and related functions work correctly

## Testing

To verify the fix works, you can run:

```r
source("test_smote_prediction.R")
```

Or run the test suite:

```r
devtools::test(filter = "smote_test_mode")
```

## Impact

This fix ensures that SMOTE with test mode now works correctly for all model types:
- GLM/Logistic Regression
- XGBoost
- LightGBM
- Ranger (Random Forest)
- RPart (Decision Trees)

The prediction functions (`prediction`, `prediction_binary`, `prediction_training_and_test`) will now work correctly when SMOTE and test mode are both enabled.


