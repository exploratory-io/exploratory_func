# CHAID Classification Tree Design

**Goal:** Add a self-contained, non-weighted classification CHAID engine to the `exploratory` R package without adding a third-party CHAID dependency or changing existing CART/rpart behavior.

## Scope

This change targets the R package layer only. It includes fitting, prediction, category merging, numeric binning, model metadata, report tables, tree data, and unit tests. Exploratory UI integration, weighted fitting, exhaustive CHAID, pruning, regression CHAID, survival CHAID, and advanced ordinal-target logic are out of scope.

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

## Self-review

- No placeholders or deferred implementation steps are required inside this design.
- The tree-growth algorithm, model representation, and public outputs are consistent with one another.
- Scope is limited to one independently testable package-layer feature.
- Weighted fitting and UI integration are explicitly excluded rather than left ambiguous.
