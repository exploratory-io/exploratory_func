# CHAID Classification Tree Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Implement a self-contained, non-weighted classification CHAID engine in the `exploratory` R package with fitting, prediction, category merging, numeric binning, report tables, tree data, and tests.

**Architecture:** Add one focused `R/chaid.R` module containing public CHAID functions and private helpers. The fitter prepares stable categorical training data, recursively grows a multiway tree using chi-square tests and Bonferroni-adjusted p-values, and stores normalized node/edge tables plus traversal metadata. A dedicated testthat file drives each behavior before implementation.

**Tech Stack:** Base R, `stats::chisq.test`, existing package imports, testthat, roxygen2-generated NAMESPACE.

## Global Constraints

- Classification CHAID only; the target must be character or factor.
- No third-party CHAID dependency.
- No weighted fitting in this version.
- Numeric predictors support `quantile`, `equal_width`, and `none` binning.
- Missing values support `as_category` and `exclude`.
- Unknown prediction categories stop traversal at the current node.
- Existing rpart and model-builder behavior must not change.
- Every production change must follow a failing test first.

---

### Task 1: Add the public contract and validation tests

**Files:**
- Create: `tests/testthat/test_chaid.R`
- Create: `R/chaid.R`

**Interfaces:**
- Consumes: no new code.
- Produces: failing tests that define `chaid_fit()` validation, stable target levels, and the root-node model shape.

- [ ] **Step 1: Write the failing tests**

```r
test_that('chaid_fit rejects invalid targets and predictors', {
  data <- data.frame(target = c('yes', 'no'), x = c('a', 'b'))

  expect_error(chaid_fit(data, target = 'missing'), 'target')
  expect_error(chaid_fit(data, target = 'target', predictors = 'missing'), 'predictor')
  expect_error(chaid_fit(data.frame(target = 1:2, x = 1:2), target = 'target'), 'character or factor')
  expect_error(chaid_fit(data, target = 'target', min_split = 1, min_bucket = 2), 'min_split')
})

test_that('chaid_fit returns a terminal root for a single-class target', {
  data <- data.frame(target = factor(rep('yes', 4)), x = c('a', 'b', 'a', 'b'))
  model <- chaid_fit(data, target = 'target', min_split = 2, min_bucket = 1)

  expect_s3_class(model, 'exploratory_chaid')
  expect_true(model$nodes$is_terminal[model$nodes$node_id == 1])
  expect_equal(model$class_levels, 'yes')
  expect_equal(model$nodes$n[model$nodes$node_id == 1], 4)
})
```

- [ ] **Step 2: Run the focused test to verify it fails**

Run: `Rscript -e "testthat::test_file('tests/testthat/test_chaid.R')"`

Expected: FAIL because `chaid_fit()` is not defined.

- [ ] **Step 3: Add the minimal public skeleton and validation helper**

Create `R/chaid.R` with a documented `chaid_fit()` signature, validation for column names, target type, predictor names, parameter ranges, and a terminal-root return containing `nodes`, `edges`, `class_levels`, `target`, `predictors`, and `parameters`.

- [ ] **Step 4: Run the focused test to verify it passes**

Run: `Rscript -e "testthat::test_file('tests/testthat/test_chaid.R')"`

Expected: PASS for the validation and single-class tests.

- [ ] **Step 5: Commit the task**

```bash
git add tests/testthat/test_chaid.R R/chaid.R
git commit -m "test: define CHAID fit contract"
```

### Task 2: Implement preprocessing and numeric binning

**Files:**
- Modify: `tests/testthat/test_chaid.R`
- Modify: `R/chaid.R`

**Interfaces:**
- Consumes: `chaid_fit()` validation from Task 1.
- Produces: private preprocessing helpers that return prepared data and saved `numeric_binning_map` metadata.

- [ ] **Step 1: Write the failing tests**

```r
test_that('numeric quantile and equal-width binning are saved and reused', {
  data <- data.frame(target = rep(c('yes', 'no'), 5), x = 1:10)
  quantile_model <- chaid_fit(data, target = 'target', numeric_binning = 'quantile',
                              numeric_bins = 3, min_split = 2, min_bucket = 1)
  equal_model <- chaid_fit(data, target = 'target', numeric_binning = 'equal_width',
                           numeric_bins = 3, min_split = 2, min_bucket = 1)

  expect_equal(quantile_model$numeric_binning_map$x$method, 'quantile')
  expect_equal(equal_model$numeric_binning_map$x$method, 'equal_width')
  expect_length(quantile_model$numeric_binning_map$x$breaks, 4)
  out_of_range_nodes <- chaid_predict(quantile_model, data.frame(x = c(-10, 100)), type = 'node')
  expect_length(out_of_range_nodes, 2)
  expect_true(all(!is.na(out_of_range_nodes)))
})

test_that('missing values can be represented as a category or excluded', {
  data <- data.frame(target = c('yes', 'no', 'yes'), x = c('a', NA, 'b'))
  as_category <- chaid_fit(data, target = 'target', missing = 'as_category',
                           min_split = 2, min_bucket = 1)
  excluded <- chaid_fit(data, target = 'target', missing = 'exclude',
                        min_split = 2, min_bucket = 1)

  expect_true('Missing' %in% as_category$prepared_levels$x)
  expect_equal(excluded$training_metadata$n_rows, 2)
})
```

- [ ] **Step 2: Run the focused test and verify the expected failure**

Run: `Rscript -e "testthat::test_file('tests/testthat/test_chaid.R', filter = 'numeric quantile')"`

Expected: FAIL because preprocessing metadata is not implemented.

- [ ] **Step 3: Implement minimal preprocessing**

Add helpers that validate `numeric_binning`, `numeric_bins`, and `missing`; convert character/factor predictors to character categories; preserve ordered-factor status; create deterministic quantile/equal-width breaks; use `cut()` with saved labels; and apply the same transformations to new data. `numeric_binning = 'none'` excludes numeric predictors.

- [ ] **Step 4: Run focused preprocessing tests**

Run: `Rscript -e "testthat::test_file('tests/testthat/test_chaid.R', filter = 'numeric')"` and `Rscript -e "testthat::test_file('tests/testthat/test_chaid.R', filter = 'missing')"`

Expected: PASS.

- [ ] **Step 5: Commit the task**

```bash
git add R/chaid.R tests/testthat/test_chaid.R
git commit -m "feat: prepare CHAID categorical predictors"
```

### Task 3: Implement chi-square evaluation and category merging

**Files:**
- Modify: `tests/testthat/test_chaid.R`
- Modify: `R/chaid.R`

**Interfaces:**
- Consumes: prepared categorical data from Task 2.
- Produces: private `compute_chisq_test()`, `merge_categories()`, and predictor-evaluation helpers returning merged groups, raw p-values, adjusted p-values, and merge history.

- [ ] **Step 1: Write the failing tests**

```r
test_that('CHAID merges similar nominal categories', {
  data <- data.frame(
    target = rep(c('yes', 'no'), c(40, 40)),
    segment = rep(c('a', 'b', 'c'), c(20, 20, 40))
  )
  model <- chaid_fit(data, target = 'target', predictors = 'segment',
                     alpha_merge = 0.05, min_split = 2, min_bucket = 1)

  merge_table <- chaid_category_merge_table(model)
  expect_true(nrow(merge_table) >= 1)
  expect_true(any(grepl('a|b', merge_table$original_categories)))
})

test_that('ordered predictors only merge adjacent categories', {
  data <- data.frame(
    target = rep(c('yes', 'no'), 30),
    ordered_x = ordered(rep(c('low', 'medium', 'high'), 20),
                        levels = c('low', 'medium', 'high'))
  )
  model <- chaid_fit(data, target = 'target', predictors = 'ordered_x',
                     min_split = 2, min_bucket = 1)
  merges <- chaid_category_merge_table(model)

  expect_false(any(grepl('low.*high|high.*low', merges$original_categories)))
})

test_that('alpha_merge controls category merging', {
  data <- data.frame(target = rep(c('yes', 'no'), 30), x = rep(c('a', 'b', 'c'), 20))
  conservative <- chaid_fit(data, target = 'target', alpha_merge = 0.001,
                             min_split = 2, min_bucket = 1)
  permissive <- chaid_fit(data, target = 'target', alpha_merge = 0.99,
                          min_split = 2, min_bucket = 1)

  expect_lte(nrow(chaid_category_merge_table(permissive)),
             nrow(chaid_category_merge_table(conservative)))
})
```

- [ ] **Step 2: Run the merging tests and verify they fail**

Run: `Rscript -e "testthat::test_file('tests/testthat/test_chaid.R', filter = 'merge')"`

Expected: FAIL because category merging and report extraction are not implemented.

- [ ] **Step 3: Implement chi-square and merging helpers**

Use `stats::chisq.test()` on contingency tables, return `NA` for degenerate tables, compare all nominal pairs or adjacent ordered pairs, merge the highest-p pair when it exceeds `alpha_merge`, and repeat until no candidate qualifies. Apply Bonferroni correction to the candidate comparisons and store each merge with node ID, variable, merged group, original categories, and merge p-value.

- [ ] **Step 4: Run the merging tests**

Run: `Rscript -e "testthat::test_file('tests/testthat/test_chaid.R', filter = 'merge')"`

Expected: PASS.

- [ ] **Step 5: Commit the task**

```bash
git add R/chaid.R tests/testthat/test_chaid.R
git commit -m "feat: add CHAID category merging"
```

### Task 4: Implement recursive multiway tree growth

**Files:**
- Modify: `tests/testthat/test_chaid.R`
- Modify: `R/chaid.R`

**Interfaces:**
- Consumes: preprocessing and predictor evaluation from Tasks 2–3.
- Produces: fitted multiway tree with normalized node/edge tables, split metadata, rules, and stopping-condition metadata.

- [ ] **Step 1: Write the failing tests**

```r
test_that('CHAID chooses a significant predictor and creates multiple children', {
  data <- data.frame(
    target = rep(c('yes', 'no'), each = 60),
    strong = rep(c('a', 'b', 'c'), each = 40),
    weak = rep(c('x', 'y'), 60)
  )
  model <- chaid_fit(data, target = 'target', predictors = c('strong', 'weak'),
                     alpha_split = 0.05, max_depth = 2, min_split = 10, min_bucket = 5)

  root <- model$nodes[model$nodes$node_id == 1, ]
  expect_false(root$is_terminal)
  expect_equal(root$split_variable, 'strong')
  expect_gte(sum(model$edges$parent_id == 1), 2)
  expect_true(all(model$nodes$n[model$nodes$parent_id == 1] >= 5))
})

test_that('CHAID stops on non-significant data and max depth', {
  data <- data.frame(target = rep(c('yes', 'no'), 50), x = rep(c('a', 'b'), 50))
  root_model <- chaid_fit(data, target = 'target', alpha_split = 0.001,
                          min_split = 10, min_bucket = 5)
  depth_model <- chaid_fit(data.frame(target = rep(c('yes', 'no'), 100),
                                      x = rep(c('a', 'b'), 100)),
                           target = 'target', max_depth = 0,
                           min_split = 10, min_bucket = 5)

  expect_true(root_model$nodes$is_terminal[1])
  expect_true(depth_model$nodes$is_terminal[1])
})
```

- [ ] **Step 2: Run the growth tests and verify failure**

Run: `Rscript -e "testthat::test_file('tests/testthat/test_chaid.R', filter = 'significant')"`

Expected: FAIL because recursive growth is not implemented.

- [ ] **Step 3: Implement tree growth**

Add a recursive `grow_chaid_node()` that computes node distributions, evaluates every eligible predictor, adjusts selection p-values by the number of evaluated predictors, rejects invalid child sizes, assigns sequential node IDs, builds human-readable rules, and stores split groups for traversal. Preserve terminal nodes when no valid split exists.

- [ ] **Step 4: Run growth and regression tests**

Run: `Rscript -e "testthat::test_file('tests/testthat/test_chaid.R', filter = 'significant')"` and `Rscript -e "testthat::test_file('tests/testthat/test_chaid.R')"`

Expected: PASS for all tests added so far.

- [ ] **Step 5: Commit the task**

```bash
git add R/chaid.R tests/testthat/test_chaid.R
git commit -m "feat: grow recursive CHAID trees"
```

### Task 5: Implement prediction and unknown-category handling

**Files:**
- Modify: `tests/testthat/test_chaid.R`
- Modify: `R/chaid.R`

**Interfaces:**
- Consumes: fitted model traversal metadata from Task 4.
- Produces: `chaid_predict()` and `predict.exploratory_chaid()` with `class`, `prob`, `node`, and `all` output modes.

- [ ] **Step 1: Write the failing tests**

```r
test_that('CHAID prediction returns class, probabilities, nodes, and rules', {
  data <- data.frame(target = rep(c('yes', 'no'), each = 30),
                     x = rep(c('a', 'b', 'c'), each = 20))
  model <- chaid_fit(data, target = 'target', min_split = 5, min_bucket = 2)
  new_data <- data.frame(x = c('a', 'b', 'unknown'))

  classes <- chaid_predict(model, new_data, type = 'class')
  probabilities <- chaid_predict(model, new_data, type = 'prob')
  nodes <- chaid_predict(model, new_data, type = 'node')
  all_predictions <- chaid_predict(model, new_data, type = 'all')

  expect_length(classes, 3)
  expect_equal(nrow(probabilities), 3)
  expect_equal(rowSums(probabilities), rep(1, 3))
  expect_equal(length(nodes), 3)
  expect_true(all(c('.chaid_node_id', '.pred_class', '.chaid_rule') %in%
                  names(all_predictions)))
  expect_equal(nodes[3], 1L)
  expect_equal(predict(model, new_data, type = 'class'), classes)
})
```

- [ ] **Step 2: Run the prediction test and verify failure**

Run: `Rscript -e "testthat::test_file('tests/testthat/test_chaid.R', filter = 'prediction')"`

Expected: FAIL because `chaid_predict()` is not implemented.

- [ ] **Step 3: Implement traversal**

Transform new data with the model’s saved binning/missing metadata, follow known split groups recursively, stop on unknown values, and calculate predictions from the terminal/current node’s stored class distribution. Ensure probability columns are named `.pred_prob_<class>` and rows sum to one even when a class has zero training count at a node.

- [ ] **Step 4: Run prediction tests**

Run: `Rscript -e "testthat::test_file('tests/testthat/test_chaid.R', filter = 'prediction')"`

Expected: PASS.

- [ ] **Step 5: Commit the task**

```bash
git add R/chaid.R tests/testthat/test_chaid.R
git commit -m "feat: add CHAID prediction"
```

### Task 6: Implement report tables and tree data

**Files:**
- Modify: `tests/testthat/test_chaid.R`
- Modify: `R/chaid.R`

**Interfaces:**
- Consumes: normalized tree and merge metadata from Task 4.
- Produces: all report helpers and `chaid_tree_data()` with stable columns.

- [ ] **Step 1: Write the failing tests**

```r
test_that('CHAID report helpers return stable schemas', {
  data <- data.frame(target = rep(c('yes', 'no'), each = 30),
                     x = rep(c('a', 'b', 'c'), each = 20))
  model <- chaid_fit(data, target = 'target', min_split = 5, min_bucket = 2)

  node_summary <- chaid_node_summary(model)
  rules <- chaid_rule_table(model)
  merges <- chaid_category_merge_table(model)
  splits <- chaid_split_summary(model)
  tree <- chaid_tree_data(model)

  expect_true(all(c('Node', 'Rule', 'Rows', 'Predicted Class', 'Split Variable') %in%
                  names(node_summary)))
  expect_true(all(c('Node', 'Rule', 'Prediction', 'Rows') %in% names(rules)))
  expect_true(all(c('Node', 'Variable', 'Merged Category', 'Original Categories') %in%
                  names(merges)))
  expect_true(all(c('Depth', 'Node', 'Split Variable', 'p-value', 'Adjusted p-value') %in%
                  names(splits)))
  expect_true(all(c('node_id', 'label', 'depth', 'n', 'is_terminal') %in%
                  names(tree$nodes)))
  expect_true(all(c('parent_id', 'child_id', 'label', 'split_variable') %in%
                  names(tree$edges)))
})
```

- [ ] **Step 2: Run the report test and verify failure**

Run: `Rscript -e "testthat::test_file('tests/testthat/test_chaid.R', filter = 'report helpers')"`

Expected: FAIL because the report helpers are not implemented.

- [ ] **Step 3: Implement report helpers**

Build the requested human-readable columns from model metadata, return zero-row data frames with the same schemas when a model has no merges or splits, and return `list(nodes = ..., edges = ...)` from `chaid_tree_data()`.

- [ ] **Step 4: Run the report test**

Run: `Rscript -e "testthat::test_file('tests/testthat/test_chaid.R', filter = 'report helpers')"`

Expected: PASS.

- [ ] **Step 5: Commit the task**

```bash
git add R/chaid.R tests/testthat/test_chaid.R
git commit -m "feat: expose CHAID report tables"
```

### Task 7: Export public APIs and verify the package

**Files:**
- Modify: `R/chaid.R`
- Modify: `NAMESPACE` through roxygen2
- Modify: `tests/testthat/test_chaid.R`

**Interfaces:**
- Consumes: complete CHAID implementation from Tasks 1–6.
- Produces: installable package API and full focused test coverage.

- [ ] **Step 1: Add export tags and an API smoke test**

Add roxygen `@export` tags to each public function and add:

```r
test_that('CHAID public functions are exported', {
  expect_true('chaid_fit' %in% getNamespaceExports('exploratory'))
  expect_true('chaid_predict' %in% getNamespaceExports('exploratory'))
  expect_true('chaid_tree_data' %in% getNamespaceExports('exploratory'))
})
```

- [ ] **Step 2: Regenerate NAMESPACE and run package checks**

Run: `Rscript -e "devtools::document(roclets = 'namespace')"`

Then run: `Rscript -e "testthat::test_file('tests/testthat/test_chaid.R')"`

Expected: exported functions resolve and all CHAID tests pass.

- [ ] **Step 3: Run broader verification**

Run: `Rscript tests/testthat.R`

Expected: the package test suite passes, or any unrelated pre-existing failures are recorded without changing unrelated files.

- [ ] **Step 4: Review generated and changed files**

Run: `git diff --check` and `git status --short`.

Confirm only `R/chaid.R`, `tests/testthat/test_chaid.R`, `NAMESPACE`, and the approved design/plan documents are changed or committed; do not stage the repository’s unrelated untracked files.

- [ ] **Step 5: Commit the completed feature**

```bash
git add R/chaid.R tests/testthat/test_chaid.R NAMESPACE
git commit -m "feat: implement classification CHAID engine"
```
