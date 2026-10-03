# Design: Level / row-set mismatch fixes — Random Forest, ANOVA, SMOTE

- Issues: exploratory-io/tam#39338 (Random Forest), exploratory-io/tam#39339 (ANOVA), exploratory-io/tam#39340 (SMOTE)
- Origin: audit for the bug class behind tam#39280 / exploratory_func#1689 (LCA)
- Date: 2026-10-02

## 1. Bug class

Something derived from **row set A** (category levels, actual values) is paired **by position** with
model output built from **row set B** (training rows only, post-NA-filter rows, pre-SMOTE rows).
When A and B disagree, the result either crashes on a length mismatch or, worse, is silently
mislabeled.

| Issue | Analytics (UI name) | A | B | Failure |
|---|---|---|---|---|
| #39338 | Random Forest (`calc_feature_imp`) | `forest$levels`: all target levels | ranger probability-matrix columns: only classes seen in training | **Silent mislabel** of predicted labels and confusion matrix |
| #39339 | ANOVA (`exp_anova`), one-way, "Assume Equal Variances" = TRUE | `nlevels()` and `tapply()` over all factor levels | rows remaining after NA and outlier filtering | Wrong df, NA sums of squares and mean squares |
| #39340 | Random Forest / XGBoost / LightGBM / CatBoost with `smote_keep_synthetic = FALSE` | actual values from the SMOTE-resampled training rows | predictions on the original (pre-SMOTE) training rows | Crash: length mismatch |

Design rule for every fix: **derive both sides from the same object**. Look up by name, or build
the levels and actual values from the exact rows the model output describes. Do not add
post-hoc padding or truncation.

## 2. Fix 1: Random Forest multiclass label mapping (tam#39338)

**Location:** `predict_value_from_prob()` in `R/randomForest_tidiers.R` (~L1336), multiclass branch:

```r
to_same_type(levels_var[apply(pred, 1, which.max)], y_value)
```

`pred` is ranger's probability matrix, and its **columns are named** with the classes it saw in
training. `levels_var` (`x$forest$levels`) holds every level of the target factor. When a class
exists only in test rows and is not the last level, the two are offset and every label shifts.

**Change:** pick the label by column name, and fall back to `levels_var` only when `pred` has no
column names:

```r
labels <- colnames(pred)
if (is.null(labels)) labels <- levels_var
to_same_type(labels[max.col(pred, ties.method = "first")], y_value)
```

- `max.col(ties.method = "first")` matches the current `which.max` tie-breaking (first maximum).
- This also covers the rpart callers (`attr(x, "ylevels")`, `exp_rpart`, and others). rpart's
  `predict(type = "prob")` returns every ylevel as a named column, so the result is unchanged
  there. The tests must confirm this.
- `to_same_type()` already converts the character labels to the type of `y_value`. Check that
  the result for factor and logical targets matches the current behavior.
- Leave the binary branch alone. It already matches `"TRUE"` / `"FALSE"` by column name.
  Possible follow-up, not in this PR: the binary-probability call sites `predictions[, 1]`
  (~L888, L940, L947) take the column by position. That only matters if the training split
  contains a single class, which was not reproduced.

**Downstream:** the callers (`augment.ranger.classification`, `tidy.ranger` conf_mat and
evaluation, `glance.ranger.classification`, `get_test_predicted_labels`) need no change. The
`predicted_probability_<class>` columns are already built by name, so they will now agree with
`predicted_label`.

## 3. Fix 2: One-way ANOVA with equal variances (tam#39339)

**Location:** `exp_anova()` → `anova_each()` in `R/test_wrapper.R`.

The group column is converted with `factor()` (~L1852) **before** grouping, target-NA removal
(~L1895) and outlier removal (~L1944). A level can therefore end up with zero rows inside
`anova_each`. The `var.equal = TRUE` branch (~L2021–2045) then uses:

- `nlevels(df[[var2_col]])`, which counts the empty level, so the df is off;
- `tapply(..., length)` and `tapply(..., mean)`, which return NA for the empty level, so SS
  and MS become NA.

**Change:** in `anova_each`, right after the existing `n_distinct(...) < 2` check (~L1951), drop
the unused levels of the explanatory variable(s) on the rows actually analyzed:

```r
# tam#39339: levels were fixed before NA/outlier filtering; drop the ones with no rows left
for (col in var2_col) {
  if (is.factor(df[[col]])) df[[col]] <- droplevels(df[[col]])
}
```

- Apply this only to the **one-way, no covariates, no repeated measures** path:
  `is.null(covariates) && length(var2_col) == 1 && !with_repeated_measures`. That keeps
  two-way, ANCOVA and repeated-measures behavior unchanged. Note in a comment that two-way has
  the same exposure; a follow-up can extend it after its own verification.
- `oneway.test()` already ignores empty levels, so the F and P values do not change. With this
  fix, the equal-variance df, SS and MS will match `summary(aov(y ~ g))`.
- Check that the post-hoc output (from `model$lm.model`) and the display order
  (`common_var2_order`, computed before `anova_each` across all groups) still render when one
  level is gone. The missing level should simply be absent.

## 4. Fix 3: SMOTE with `smote_keep_synthetic = FALSE` (tam#39340)

**Locations:**

| Engine | Training prediction on original rows |
|---|---|
| ranger | `R/randomForest_tidiers.R` ~L2560–2570 (`predict(model, model_df_original)`) |
| xgboost | `R/build_xgboost.R` ~L1399–1410 (`predict_xgboost(model, df_train_original)`) |
| lightgbm | `R/build_lightgbm.R` ~L1852–1862 |
| catboost | `R/build_catboost.R` ~L1211–1215 |

When SMOTE was applied and synthetic rows are not kept, `prediction_training` is computed on
the **original** training rows (correct, since `source.data` holds the original rows). The
**actual** values that the tidy, glance and augment methods compare against still come from
the SMOTE-resampled data the model was fitted on: ranger's `model$y` (from `model_df`), and the
data or label field each boosting engine stores. Evaluation then fails with
`"Assertion: actual and predicted have different length in evaluate_classification."` or
`"arguments imply differing number of rows: 480, 1000"`.

**Change:** in each engine's `smote_applied && !smote_keep_synthetic` branch, make the stored
actual values describe the same rows as `prediction_training`:

- **ranger:** after predicting on `model_df_original`, also set the actual values that
  evaluation reads (`model$y`, or whichever field `tidy.ranger` and `glance` use; trace it) to
  `model_df_original`'s target column. Keep `na.action` consistent: `model_df_original` uses
  `na.roughfix`, so no rows are dropped.
- **xgboost / lightgbm / catboost:** trace which field the evaluation and augment code reads the
  training actual values from (for example `x$df[[target]]` or a stored label vector). Set it
  from `df_train_original` in the same branch. Do not change what the model was trained on.
- Model fitting, importance and the test predictions must not change.

This path cannot be reached from the current tam UI: `smote_keep_synthetic` defaults to `TRUE`
and the templates never pass it. That is why existing tests did not catch it. Fix it anyway so
that the R API option works.

## 5. Tests (testthat; add red → green)

| File | Test |
|---|---|
| `tests/testthat/test_randomForest_tidiers_*.R` (pick one, or add a new `test_randomForest_level_mismatch.R`) | Fixture from tam#39338: `y` is "b"/"c" by `x < 0.5`, rows 271–300 = "a", `test_rate = 0.1`, `test_split_type = "ordered"`. Assert `tidy(fit, "conf_mat")` puts all training rows on the diagonal (b→b, c→c), and that `augment(..., data_type = "training")` has `predicted_label` equal to the argmax of the `predicted_probability_*` columns. |
| same | Regression: the normal multiclass case (all classes in training) gives labels identical to the current output. |
| an existing rpart test file | Regression: `exp_rpart` multiclass predicted labels are unchanged. |
| `tests/testthat/test_test_oneway_anova.R` | Fixture from tam#39339 (`g` A/B/C, all of C's `y` NA, `var.equal = TRUE`). Assert df = 1 / 38 and SS and MS equal `summary(aov(...))` (tolerance 1e-8). Add a regression check that `var.equal = FALSE` output is unchanged. |
| `tests/testthat/test_smote_keep_synthetic.R` | Fixture from tam#39340 (n = 1000, imbalanced logical `y`). For ranger and xgboost (plus lightgbm and catboost if installed; use `skip_if_not_installed`): with `smote = TRUE, smote_keep_synthetic = FALSE`, `tidy(fit, "evaluation")` and `tidy(fit, "conf_mat")` run without error, and the confusion-matrix total equals the number of original training rows. |

Run every touched test file plus `test_smote*.R`, `test_randomForest_tidiers_*.R`,
`test_build_xgboost.R`, `test_build_lightgbm.R`, `test_build_catboost.R`,
`test_test_oneway_anova.R` and `test_test_wrapper_*.R`. Report pre-existing failures separately,
by running the same files on `origin/master`.

## 6. Out of scope (possible follow-ups)

- Two-way ANOVA / ANCOVA with an empty level (same exposure, different code path).
- `model_stats` base-level label (`R/broom_wrapper.R` ~L1435) and `fill_between` relabeling
  (`R/na_util.R` ~L117). These are not analytics views; file separately if wanted.
- ranger binary `predictions[, 1]` by position (see §2).
- `R/textanal.R:879`: LDA `theta[...]` indexed as a vector.

## 7. Risk

- Fix 1 changes output only when the probability columns and `forest$levels` disagree, which is
  exactly the buggy case. The fallback keeps rpart and other unnamed-matrix callers on the old
  path.
- Fix 2 only affects one-way ANOVA with an empty level. Previously that output was wrong or NA.
- Fix 3 only affects `smote_keep_synthetic = FALSE`. Previously that path crashed.
