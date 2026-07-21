test_that('chaid_fit rejects invalid targets and predictors', {
  data <- data.frame(target = c('yes', 'no'), x = c('a', 'b'))

  expect_error(chaid_fit(data, target = 'missing'), 'target')
  expect_error(chaid_fit(data, target = 'target', predictors = 'missing'), 'predictor')
  expect_error(chaid_fit(data.frame(target = 1:2, x = 1:2), target = 'target'), 'character, factor, or logical')
  expect_error(chaid_fit(data, target = 'target', min_split = 1, min_bucket = 2), 'min_split')
  expect_error(chaid_fit(data.frame(target = c(NA_character_, NA_character_), x = c('a', 'b')),
                         target = 'target'), 'non-missing')
})

test_that('chaid_fit returns a terminal root for a single-class target', {
  data <- data.frame(target = factor(rep('yes', 4)), x = c('a', 'b', 'a', 'b'))
  model <- chaid_fit(data, target = 'target', min_split = 2, min_bucket = 1)

  expect_s3_class(model, 'exploratory_chaid')
  expect_true(model$nodes$is_terminal[model$nodes$node_id == 1])
  expect_equal(model$class_levels, 'yes')
  expect_equal(model$nodes$n[model$nodes$node_id == 1], 4)
})

test_that('numeric quantile and equal-width binning are saved and reused', {
  data <- data.frame(target = rep(c('yes', 'no'), 5), x = 1:10)
  quantile_model <- chaid_fit(data, target = 'target', numeric_binning = 'quantile',
                              numeric_bins = 3, min_split = 2, min_bucket = 1)
  equal_model <- chaid_fit(data, target = 'target', numeric_binning = 'equal_width',
                           numeric_bins = 3, min_split = 2, min_bucket = 1)

  expect_equal(quantile_model$numeric_binning_map$x$method, 'quantile')
  expect_equal(equal_model$numeric_binning_map$x$method, 'equal_width')
  expect_length(quantile_model$numeric_binning_map$x$breaks, 4)
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

test_that('CHAID merges similar nominal categories', {
  values <- rep(c('a', 'b', 'c'), each = 40)
  target <- c(
    rep(c('yes', 'no'), 20),
    rep(c('yes', 'no'), 20),
    rep('yes', 40)
  )
  result <- exploratory:::merge_categories(
    values = values,
    target = target,
    ordered = FALSE,
    alpha_merge = 0.05,
    bonferroni = TRUE,
    variable = 'segment',
    node_id = 1L
  )

  expect_true(length(result$groups) < 3)
  expect_true(any(vapply(result$merge_history, function(x) {
    all(c('a', 'b') %in% x$original_categories)
  }, logical(1))))
})

test_that('ordered predictors only merge adjacent categories', {
  values <- rep(c('low', 'medium', 'high'), each = 40)
  target <- c(
    rep(c('yes', 'no'), 20),
    rep(c('yes', 'no'), 20),
    rep('yes', 40)
  )
  result <- exploratory:::merge_categories(
    values = values,
    target = target,
    ordered = TRUE,
    ordered_levels = c('low', 'medium', 'high'),
    alpha_merge = 0.05,
    bonferroni = TRUE,
    variable = 'ordered_x',
    node_id = 1L
  )

  expect_false(any(vapply(result$merge_history, function(x) {
    all(c('low', 'high') %in% x$original_categories)
  }, logical(1))))
})

test_that('alpha_merge controls category merging', {
  values <- rep(c('a', 'b', 'c'), each = 40)
  target <- c(rep(c('yes', 'no'), 20), rep(c('yes', 'no'), 20), rep('yes', 40))
  conservative <- exploratory:::merge_categories(values, target, ordered = FALSE,
                                                 alpha_merge = 0.001, bonferroni = TRUE,
                                                 variable = 'x', node_id = 1L)
  permissive <- exploratory:::merge_categories(values, target, ordered = FALSE,
                                                alpha_merge = 0.99, bonferroni = TRUE,
                                                variable = 'x', node_id = 1L)

  expect_lte(length(permissive$groups), length(conservative$groups))
})

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
  child.rows <- !is.na(model$nodes$parent_id) & model$nodes$parent_id == 1
  expect_true(all(model$nodes$n[child.rows] >= 5))
})

test_that('CHAID stops on non-significant data and max depth', {
  data <- data.frame(target = rep(c('yes', 'no'), 50), x = rep(c('a', 'b'), each = 50))
  root_model <- chaid_fit(data, target = 'target', alpha_split = 0.001,
                          min_split = 10, min_bucket = 5)
  depth_model <- chaid_fit(data.frame(target = rep(c('yes', 'no'), 100),
                                      x = rep(c('a', 'b'), 100)),
                           target = 'target', max_depth = 0,
                           min_split = 10, min_bucket = 5)

  expect_true(root_model$nodes$is_terminal[1])
  expect_true(depth_model$nodes$is_terminal[1])
})

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
  expect_equal(unname(rowSums(probabilities)), rep(1, 3))
  expect_equal(length(nodes), 3)
  expect_true(all(c('.chaid_node_id', '.pred_class', '.chaid_rule') %in%
                  names(all_predictions)))
  expect_equal(nodes[3], 1L)
  expect_equal(predict(model, new_data, type = 'class'), classes)
})

test_that('CHAID prediction reuses numeric binning for out-of-range values', {
  data <- data.frame(target = rep(c('yes', 'no'), 5), x = 1:10)
  model <- chaid_fit(data, target = 'target', numeric_binning = 'quantile',
                     numeric_bins = 3, min_split = 2, min_bucket = 1)

  nodes <- chaid_predict(model, data.frame(x = c(-10, 100)), type = 'node')
  expect_length(nodes, 2)
  expect_true(all(!is.na(nodes)))
})

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

test_that('CHAID public functions are exported', {
  expect_true('chaid_fit' %in% getNamespaceExports('exploratory'))
  expect_true('chaid_predict' %in% getNamespaceExports('exploratory'))
  expect_true('chaid_tree_data' %in% getNamespaceExports('exploratory'))
})

test_that('numeric bins use human-readable range labels, not BinN codes', {
  data <- data.frame(target = rep(c('yes', 'no'), each = 30), x = rep(1:6, each = 10))
  model <- suppressWarnings(
    chaid_fit(data, target = 'target', numeric_binning = 'equal_width',
              numeric_bins = 3, min_split = 5, min_bucket = 2)
  )

  labels <- model$numeric_binning_map$x$labels
  expect_false(any(grepl('^Bin[0-9]+$', labels)))
  # First / last bins are open-ended; middle bins are (lo, hi].
  expect_true(any(grepl('^<= ', labels)))
  expect_true(any(grepl('^> ', labels)))
  # No BinN leaks into rules, edges, or the node summary.
  expect_false(any(grepl('Bin[0-9]', model$nodes$rule)))
  expect_false(any(grepl('Bin[0-9]', model$edges$label)))
})

test_that('numeric bin labels are reused at prediction time', {
  data <- data.frame(target = rep(c('yes', 'no'), 10), x = 1:20)
  model <- suppressWarnings(
    chaid_fit(data, target = 'target', numeric_binning = 'quantile',
              numeric_bins = 4, min_split = 2, min_bucket = 1)
  )
  labels <- model$numeric_binning_map$x$labels
  binned <- chaid_predict(model, data.frame(x = c(-100, 100)), type = 'node')
  # Out-of-range values map into the open-ended first/last bins without error.
  expect_length(binned, 2)
  expect_true(all(!is.na(binned)))
})

test_that('binned numeric predictors merge only adjacent (contiguous) bins', {
  # Ordered numeric bins may only merge neighbours, so no merge groups the
  # lowest and highest bins together.
  data <- data.frame(
    target = c(rep('yes', 60), rep('no', 60)),
    x = c(rep(1, 30), rep(2, 30), rep(3, 30), rep(4, 30))
  )
  model <- suppressWarnings(
    chaid_fit(data, target = 'target', numeric_binning = 'equal_width',
              numeric_bins = 4, min_split = 10, min_bucket = 5)
  )
  info <- model$predictor_info$x
  expect_true(isTRUE(info$ordered))
})

test_that('chaid_fit accepts a logical target with TRUE-first levels', {
  data <- data.frame(target = rep(c(TRUE, FALSE), each = 30),
                     x = rep(c('a', 'b', 'c'), each = 20))
  model <- suppressWarnings(chaid_fit(data, target = 'target', min_split = 5, min_bucket = 2))

  expect_s3_class(model, 'exploratory_chaid')
  expect_equal(model$class_levels, c('TRUE', 'FALSE'))
  expect_equal(model$target_type, 'logical')
  classes <- chaid_predict(model, data.frame(x = c('a', 'b')), type = 'class')
  expect_true(all(classes %in% c('TRUE', 'FALSE')))
})

test_that('chaid_fit emits spec warnings once per fit', {
  # Numeric binning warning.
  binned <- data.frame(target = rep(c('yes', 'no'), 10), x = 1:20)
  expect_warning(chaid_fit(binned, target = 'target', numeric_bins = 3,
                           min_split = 2, min_bucket = 1),
                 'were binned')

  # No-significant-split warning (root-only tree despite two classes).
  no_split <- data.frame(target = rep(c('yes', 'no'), 50),
                         x = rep(c('a', 'b'), each = 50))
  expect_warning(chaid_fit(no_split, target = 'target', alpha_split = 0.001,
                           min_split = 10, min_bucket = 5),
                 'only the root node')
})

test_that('counts-based chi-square matches the raw-vector path', {
  set.seed(1)
  values <- sample(c('a', 'b', 'c'), 200, replace = TRUE)
  target <- sample(c('yes', 'no'), 200, replace = TRUE)
  observed <- unclass(table(target, values))

  raw <- exploratory:::compute_chisq_test(values, target, method = 'pearson')
  counts <- exploratory:::compute_chisq_from_counts(observed, method = 'pearson')
  expect_equal(raw$statistic, counts$statistic)
  expect_equal(raw$p_value, counts$p_value)

  raw_lr <- exploratory:::compute_chisq_test(values, target, method = 'likelihood_ratio')
  counts_lr <- exploratory:::compute_chisq_from_counts(observed, method = 'likelihood_ratio')
  expect_equal(raw_lr$statistic, counts_lr$statistic)
})

test_that('vectorized node assignment matches the row-at-a-time traversal', {
  # chaid_assign_nodes() replaced a per-row traverse_chaid_tree() loop for
  # performance. Guard the equivalence across the paths that make a row stop
  # early: numeric binning, NA handling, unseen categories, root-only trees.
  set.seed(7)
  n <- 400
  df <- data.frame(
    target = sample(c('yes', 'no'), n, replace = TRUE),
    num = c(rnorm(n - 10), rep(NA_real_, 10)),
    fac = sample(letters[1:4], n, replace = TRUE),
    other = sample(letters[1:3], n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  df$target <- ifelse(df$fac %in% c('a', 'b') & runif(n) < 0.85, 'yes', df$target)
  df$fac[sample(n, 20)] <- NA

  reference <- function(model, data) {
    prepared <- prepare_chaid_new_data(data, model)
    if (nrow(prepared) == 0) {
      return(integer())
    }
    vapply(seq_len(nrow(prepared)), function(index) {
      traverse_chaid_tree(prepared[index, , drop = FALSE], model)
    }, integer(1))
  }

  for (missing_mode in c('as_category', 'exclude')) {
    model <- suppressWarnings(chaid_fit(df, target = 'target',
                                        predictors = c('num', 'fac', 'other'),
                                        missing = missing_mode))
    prepared <- prepare_chaid_new_data(df, model)
    expect_identical(chaid_assign_nodes(prepared, model), reference(model, df))

    unseen <- df
    unseen$fac <- 'never-seen'
    unseen$num <- unseen$num * 1000
    expect_identical(
      chaid_assign_nodes(prepare_chaid_new_data(unseen, model), model),
      reference(model, unseen)
    )

    # single row and empty input
    one <- df[1, , drop = FALSE]
    expect_identical(chaid_assign_nodes(prepare_chaid_new_data(one, model), model),
                     reference(model, one))
    expect_equal(nrow(chaid_predict(model, df[0, , drop = FALSE], type = 'all')), 0)
  }

  # root-only tree: every row stops at node 1
  flat <- data.frame(target = sample(c('yes', 'no'), n, replace = TRUE),
                     g = sample(letters[1:3], n, replace = TRUE),
                     stringsAsFactors = FALSE)
  root_only <- suppressWarnings(chaid_fit(flat, target = 'target', predictors = 'g'))
  expect_identical(chaid_assign_nodes(prepare_chaid_new_data(flat, root_only), root_only),
                   reference(root_only, flat))
})

test_that('permutation importance is exactly zero for predictors the tree never splits on', {
  # Those predictors are skipped without running a prediction; the drop is 0 by
  # construction, so the reported numbers must stay exactly 0 (not merely small).
  set.seed(13)
  n <- 500
  df <- data.frame(
    target = sample(c('yes', 'no'), n, replace = TRUE),
    driver = sample(letters[1:3], n, replace = TRUE),
    noise1 = sample(letters[1:4], n, replace = TRUE),
    noise2 = sample(letters[1:5], n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  df$target <- ifelse(df$driver == 'a' & runif(n) < 0.9, 'yes', df$target)
  predictors <- c('driver', 'noise1', 'noise2')
  # max_depth = 1 splits at the root only, so at most one predictor can be used
  # and the rest are guaranteed to be untouched by the tree.
  model <- suppressWarnings(chaid_fit(df, target = 'target', predictors = predictors,
                                      max_depth = 1))
  used <- unique(unlist(lapply(model$.node_metadata, function(m) m$split_variable),
                        use.names = FALSE))
  expect_true(length(setdiff(predictors, used)) > 0)

  importance <- chaid_permutation_importance(model, df, 'target', predictors)
  unused <- setdiff(predictors, used)
  unused_rows <- importance[importance$variable %in% unused, , drop = FALSE]
  expect_true(nrow(unused_rows) > 0)
  expect_true(all(unused_rows$importance == 0))
  expect_true(all(unused_rows$std_error == 0))
})
