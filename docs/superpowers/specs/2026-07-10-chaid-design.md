# CHAID Classification Tree Design

**Goal:** Add a self-contained, non-weighted classification CHAID engine to the `exploratory` R package without adding a third-party CHAID dependency or changing existing CART/rpart behavior.

## Scope

This change targets the R package layer only. It includes fitting, prediction, category merging, numeric binning, model metadata, report tables, tree data, model-independent permutation importance, and unit tests. Exploratory UI integration, weighted fitting, exhaustive CHAID, pruning, regression CHAID, survival CHAID, and advanced ordinal-target logic are out of scope.

## Architecture

The implementation will live in a new `R/chaid.R` file and expose a small public API:

- `chaid_fit()`
- `chaid_predict()`
- `predict.exploratory_chaid()`
- `chaid_node_summary()`
- `chaid_rule_table()`
- `chaid_category_merge_table()`
- `chaid_split_summary()`
- `chaid_tree_data()`

The Analytics integration layer additionally exposes permutation importance through
`tidy.exploratory_chaid(type = "importance")` after `exp_chaid()` has prepared the
evaluation data.

The engine will use base R and packages already imported by `exploratory`, primarily `stats` and `tibble`. It will not depend on an external CHAID implementation. Existing rpart and model-builder behavior will remain unchanged.

## Data preparation

- The target must be a character or factor column and will be represented by stable training factor levels.
- Character and factor predictors are categorical. Ordered factors merge only adjacent categories.
- Numeric predictors are converted to categorical bins during fitting. Supported methods are `quantile`, `equal_width`, and `none`; training breaks and labels are saved for prediction.
- `missing = "as_category"` represents missing values as the `"Missing"` category.
- `missing = "exclude"` removes incomplete training rows. Missing values in prediction data are treated as unknown traversal values.
- Weight columns are not accepted in this version.

## Tree-growth algorithm

At each node, the engine will:

1. Check purity, depth, minimum split size, and predictor availability.
2. Merge statistically similar predictor categories using pairwise chi-square tests. Nominal predictors may merge any pair; ordered predictors may merge adjacent pairs only.
3. Apply Bonferroni correction to category-comparison p-values and predictor-selection p-values.
4. Evaluate the merged predictor-by-target contingency table with Pearson or likelihood-ratio chi-square.
5. Select the predictor with the smallest adjusted p-value.
6. Create a multiway child for each merged category group only when the adjusted p-value meets `alpha_split` and every child satisfies `min_bucket` and `min_node_proportion`.

Predictor levels unseen during prediction stop traversal at the current node. The current node’s target distribution is used for the prediction.

Predictors with more than `max_categories` levels are skipped with a warning. The model records raw and adjusted split p-values and the category merge history.

## Model representation

`chaid_fit()` returns an S3 object with class `c("exploratory_chaid", "list")`. It stores:

- normalized node and edge tables;
- internal split-group metadata used for traversal;
- category merge history;
- numeric binning metadata;
- target levels, target/predictor names, parameters, and training metadata.

Node tables include node ID, parent, depth, terminal state, row counts, predicted class, target distribution, split variable, raw/adjusted p-values, and rule. Edge tables include parent/child IDs, split variable, child label, and original categories.

## Public outputs

- `chaid_predict(type = "class")` returns predicted classes.
- `chaid_predict(type = "prob")` returns one probability column per target class.
- `chaid_predict(type = "node")` returns terminal node IDs.
- `chaid_predict(type = "all")` returns node ID, predicted class, rule, and class probabilities.
- Report helpers return stable data frames for node summaries, terminal rules, category merges, and split summaries.
- `chaid_tree_data()` returns `nodes` and `edges` data frames suitable for a tree renderer.

## Model-independent variable importance (V2)

### Definition

CHAID does not expose a model-native variable-importance vector. The supported
importance measure is permutation importance: shuffle one predictor at a time,
re-score the same fitted model, and report the increase in multiclass log loss.
For predictor `j` and repeat `r`:

```text
importance(j, r) = permuted_log_loss(j, r) - baseline_log_loss
```

The reported `importance` is the mean over repeats. Positive values mean that
permuting the predictor worsened predictions. Negative values are retained rather
than clipped because a permutation can improve the score by chance.

### Evaluation data and reproducibility

- When Test Mode is enabled, calculate importance on the held-out test rows.
- When Test Mode is disabled, calculate it on the training rows but label the
  result `Training`; this is an optimistic diagnostic, not an out-of-sample
  estimate.
- Use the same fitted model, preprocessing, target levels, and classification
  probability contract as ordinary prediction.
- Use a fixed repeat count of 10 in V2 and derive each permutation from the
  model seed so repeated runs are deterministic.
- Preserve factor, ordered-factor, logical, numeric, and binned-numeric column
  types while shuffling values.

### Data contract

`tidy.exploratory_chaid(type = "importance")` returns one row per predictor with
the following columns:

| Column | Meaning |
|---|---|
| `variable` | Predictor name after `terms_mapping` is applied |
| `importance` | Mean increase in log loss |
| `std_error` | Standard error across permutation repeats |
| `rank` | Descending importance rank |
| `metric` | `log_loss` |
| `evaluation_data` | `Test` or `Training` |
| `repeats` | Number of permutations used |

The `exp_chaid()` wrapper computes and stores only this result table on the model
object (`model$importance`); it does not retain a second copy of the evaluation
data. A missing or unusable evaluation set returns an empty data frame with the
stable columns above.

### Interpretation limits

Permutation importance is marginal: correlated predictors can share or mask one
another's importance, and a negative value is not evidence that a predictor is
causal or harmful. CHAID split p-values, child counts, and Cramér-style effect
sizes remain available as node-level split evidence, but must not be presented as
model-independent variable importance.

Conditional permutation importance, FIRM, SHAP, local explanations, and a user
selectable importance method are deferred beyond V2.

## Testing strategy

`tests/testthat/test_chaid.R` will test behavior rather than private implementation details:

- input validation and single-class targets;
- nominal versus ordered category merging and alpha controls;
- significant/non-significant splits and stopping conditions;
- numeric binning and reuse of training boundaries;
- missing values and unknown prediction categories;
- class, probability, node, and all prediction modes;
- report and tree-data schemas;
- probability normalization and stable target levels.
- permutation importance uses held-out rows in Test Mode and training rows
  otherwise, returns the stable schema, is reproducible with a fixed seed, and
  preserves negative/zero importance values;
- constant predictors and predictors with unusable values return zero or `NA`
  without failing the whole model;
- `tidy.exploratory_chaid(type = "importance")` remains backward-compatible for
  fitted models that have no stored importance table by returning an empty table.

## Self-review

- No placeholders or deferred implementation steps are required inside this design.
- The tree-growth algorithm, model representation, and public outputs are consistent with one another.
- Scope is limited to one independently testable package-layer feature.
- Weighted fitting and UI integration are explicitly excluded rather than left ambiguous.
