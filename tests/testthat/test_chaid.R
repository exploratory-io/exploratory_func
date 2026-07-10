test_that('chaid_fit rejects invalid targets and predictors', {
  data <- data.frame(target = c('yes', 'no'), x = c('a', 'b'))

  expect_error(chaid_fit(data, target = 'missing'), 'target')
  expect_error(chaid_fit(data, target = 'target', predictors = 'missing'), 'predictor')
  expect_error(chaid_fit(data.frame(target = 1:2, x = 1:2), target = 'target'), 'character or factor')
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
