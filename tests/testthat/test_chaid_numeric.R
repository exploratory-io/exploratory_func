# tam #38166: CHAID numeric (One-way ANOVA F-test) target support.
#
# These tests exercise the numeric-target statistical engine directly
# (compute_anova_from_stats / merge_categories / chaid_fit), per the spec's
# section 38 test list. Where possible the expected result is computed by an
# INDEPENDENT base-R function (stats::aov(), stats::t.test()) rather than a
# hand-derived literal, so the test does not just re-encode this PR's own
# arithmetic.

test_that('compute_anova_from_stats matches stats::aov() on known data', {
  # 3 groups, well separated, non-zero within-group spread -- classic
  # textbook shape (F = 27, df = (2, 6)).
  y <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
  g <- factor(rep(c('A', 'B', 'C'), each = 3))

  fit <- stats::aov(y ~ g)
  summary_fit <- summary(fit)[[1]]
  expected_f <- summary_fit[['F value']][1]
  expected_p <- summary_fit[['Pr(>F)']][1]
  expected_df1 <- summary_fit[['Df']][1]
  expected_df2 <- summary_fit[['Df']][2]

  stats_mat <- vapply(split(y, g), function(v) {
    c(n = length(v), sum = sum(v), sumsq = sum(v^2))
  }, numeric(3))

  result <- exploratory:::compute_anova_from_stats(stats_mat)

  expect_equal(result$statistic, unname(expected_f), tolerance = 1e-8)
  expect_equal(result$p_value, unname(expected_p), tolerance = 1e-8)
  expect_equal(result$df1, unname(expected_df1))
  expect_equal(result$df2, unname(expected_df2))
  # Hand-verified expectation for this specific fixture (independent of aov()).
  expect_equal(result$statistic, 27, tolerance = 1e-8)
})

test_that('for two groups, the ANOVA F statistic equals t^2 from an equal-variance t-test', {
  group_a <- c(2, 4, 6)
  group_b <- c(8, 10, 12)

  t_result <- stats::t.test(group_b, group_a, var.equal = TRUE)
  expected_f <- unname(t_result$statistic)^2

  stats_mat <- cbind(
    a = c(n = length(group_a), sum = sum(group_a), sumsq = sum(group_a^2)),
    b = c(n = length(group_b), sum = sum(group_b), sumsq = sum(group_b^2))
  )
  result <- exploratory:::compute_anova_from_stats(stats_mat)

  expect_equal(result$statistic, expected_f, tolerance = 1e-8)
  expect_equal(result$df1, 1)
  expect_equal(result$df2, length(group_a) + length(group_b) - 2)
  # Hand-verified expectation for this specific fixture.
  expect_equal(result$statistic, 13.5, tolerance = 1e-8)
})

test_that('compute_anova_from_stats handles the section-36 special cases without erroring', {
  # SS_total == 0: every value, in every group, identical -> no evidence of a
  # difference. Must not error; must resolve as "never significant" (p = 1).
  constant_mat <- cbind(a = c(n = 3, sum = 15, sumsq = 75), b = c(n = 3, sum = 15, sumsq = 75))
  constant_result <- exploratory:::compute_anova_from_stats(constant_mat)
  expect_true(is.na(constant_result$statistic))
  expect_equal(constant_result$p_value, 1)

  # Zero within-group spread, non-zero between-group difference -> maximally
  # significant (F = Inf, p = 0), not NaN/error.
  perfect_sep_mat <- cbind(a = c(n = 3, sum = 15, sumsq = 75), b = c(n = 3, sum = 30, sumsq = 300))
  perfect_sep_result <- exploratory:::compute_anova_from_stats(perfect_sep_mat)
  expect_equal(perfect_sep_result$statistic, Inf)
  expect_equal(perfect_sep_result$p_value, 0)

  # Not enough rows to spare a residual degree of freedom (N <= k): the test
  # cannot be run -> NA, not an error, not a crash.
  insufficient_mat <- cbind(a = c(n = 1, sum = 5, sumsq = 25), b = c(n = 1, sum = 10, sumsq = 100))
  insufficient_result <- exploratory:::compute_anova_from_stats(insufficient_mat)
  expect_true(is.na(insufficient_result$statistic))
  expect_true(is.na(insufficient_result$p_value))

  # Fewer than 2 non-empty groups.
  single_group_mat <- cbind(a = c(n = 3, sum = 15, sumsq = 75), b = c(n = 0, sum = 0, sumsq = 0))
  single_result <- exploratory:::compute_anova_from_stats(single_group_mat)
  expect_true(is.na(single_result$statistic))
  expect_true(is.na(single_result$p_value))
})

test_that('merge_categories merges the closest-mean nominal categories first (numeric target)', {
  predictor <- rep(c('X', 'Y', 'Z'), each = 10)
  # X and Y share the exact same distribution (p = 1 for their pairwise
  # test); Z is a distant constant, so X/Y must be the FIRST (and only) merge.
  x_values <- c(9, 10, 11, 9, 10, 11, 9, 10, 11, 10)
  target <- c(x_values, x_values, rep(100, 10))

  result <- exploratory:::merge_categories(
    values = predictor, target = target, ordered = FALSE,
    alpha_merge = 0.05, bonferroni = TRUE, variable = 'seg', node_id = 1L,
    numeric_target = TRUE
  )

  expect_length(result$groups, 2)
  expect_length(result$merge_history, 1)
  expect_setequal(result$merge_history[[1]]$original_categories, c('X', 'Y'))
  expect_equal(result$merge_history[[1]]$merge_p_value, 1)
})

test_that('an ordered predictor only offers ADJACENT categories as merge candidates', {
  # Low and High have identical distributions, but are NOT adjacent in the
  # declared order (Low, Mid, High) -- they must never be compared, so no
  # merge can happen even though Low/High are mergeable.
  predictor <- rep(c('Low', 'Mid', 'High'), each = 10)
  low_values <- c(9, 10, 11, 9, 10, 11, 9, 10, 11, 10)
  high_values <- low_values
  mid_values <- rep(1000, 10)
  target <- c(low_values, mid_values, high_values)

  ordered_result <- exploratory:::merge_categories(
    values = predictor, target = target, ordered = TRUE,
    ordered_levels = c('Low', 'Mid', 'High'),
    alpha_merge = 0.05, bonferroni = TRUE, variable = 'seg', node_id = 1L,
    numeric_target = TRUE
  )
  # Mid is hugely different from both neighbors, and Low/High are never
  # directly compared -> no candidate pair clears alpha_merge -> no merge.
  expect_length(ordered_result$groups, 3)
  expect_length(ordered_result$merge_history, 0)

  # The SAME data treated as a NOMINAL predictor DOES compare Low and High
  # directly, and they merge. The ordered run above proves this pair is only
  # reachable when adjacency is not enforced.
  nominal_result <- exploratory:::merge_categories(
    values = predictor, target = target, ordered = FALSE,
    alpha_merge = 0.05, bonferroni = TRUE, variable = 'seg', node_id = 1L,
    numeric_target = TRUE
  )
  expect_length(nominal_result$groups, 2)
  expect_true(any(vapply(nominal_result$merge_history, function(m) {
    setequal(m$original_categories, c('Low', 'High'))
  }, logical(1))))
})

test_that('a well-separated 3-category predictor produces a multi-way (3-child) split', {
  x <- rep(c('A', 'B', 'C'), each = 30)
  y <- c(
    rep(c(10, 11, 9, 10), length.out = 30),
    rep(c(50, 51, 49, 50), length.out = 30),
    rep(c(90, 91, 89, 90), length.out = 30)
  )
  data <- data.frame(target = y, x = x, stringsAsFactors = FALSE)

  model <- chaid_fit(data, target = 'target', predictors = 'x',
                     min_split = 2, min_bucket = 1)

  expect_equal(model$target_type, 'numeric')
  expect_false(model$nodes$is_terminal[model$nodes$node_id == 1])
  expect_equal(model$nodes$split_variable[model$nodes$node_id == 1], 'x')
  root_children <- model$edges$child_id[model$edges$parent_id == 1]
  expect_length(root_children, 3)
  expect_true(all(model$nodes$is_terminal[model$nodes$node_id %in% root_children]))
  child_means <- sort(model$nodes$node_mean[model$nodes$node_id %in% root_children])
  expect_equal(child_means, c(10, 50, 90), tolerance = 1)
})

test_that('a target with no real group difference produces no split (root-only tree)', {
  x <- rep(c('A', 'B', 'C'), each = 20)
  # Identical value pattern in every group -> SS_between == 0.
  y <- rep(1:20, 3)
  data <- data.frame(target = y, x = x, stringsAsFactors = FALSE)

  model <- chaid_fit(data, target = 'target', predictors = 'x',
                     min_split = 2, min_bucket = 1)

  expect_equal(model$target_type, 'numeric')
  expect_true(model$nodes$is_terminal[model$nodes$node_id == 1])
  expect_equal(nrow(model$edges), 0)
  expect_equal(model$nodes$node_mean[model$nodes$node_id == 1], mean(y))
})

test_that('the predictor with a real target difference is chosen over an unrelated one', {
  x1 <- rep(c('A', 'B'), each = 30)              # perfectly separates the target
  x2 <- rep(c('P', 'Q'), times = 30)             # uncorrelated with the target
  y <- ifelse(x1 == 'A', 10, 50)
  data <- data.frame(target = y, x1 = x1, x2 = x2, stringsAsFactors = FALSE)

  model <- chaid_fit(data, target = 'target', predictors = c('x1', 'x2'),
                     min_split = 2, min_bucket = 1)

  expect_equal(model$target_type, 'numeric')
  expect_equal(model$nodes$split_variable[model$nodes$node_id == 1], 'x1')
  root_children <- model$edges$child_id[model$edges$parent_id == 1]
  expect_length(root_children, 2)
  child_means <- sort(model$nodes$node_mean[model$nodes$node_id %in% root_children])
  expect_equal(child_means, c(10, 50))
})

test_that('a completely constant numeric target fits as a root-only tree without erroring', {
  data <- data.frame(target = rep(5, 40), x = rep(c('A', 'B'), each = 20),
                     stringsAsFactors = FALSE)

  model <- chaid_fit(data, target = 'target', predictors = 'x',
                     min_split = 2, min_bucket = 1)

  expect_equal(model$target_type, 'numeric')
  expect_true(model$nodes$is_terminal[model$nodes$node_id == 1])
  expect_equal(model$nodes$node_mean[model$nodes$node_id == 1], 5)
  expect_equal(model$nodes$n[model$nodes$node_id == 1], 40)
})

test_that('non-finite target values (Inf/-Inf) are excluded from fitting, per r-integration finite-guard convention', {
  x <- rep(c('A', 'B'), each = 15)
  y <- c(rep(10, 14), Inf, rep(50, 14), -Inf)
  data <- data.frame(target = y, x = x, stringsAsFactors = FALSE)

  model <- chaid_fit(data, target = 'target', predictors = 'x',
                     min_split = 2, min_bucket = 1)

  expect_equal(model$target_type, 'numeric')
  # 2 rows (one +Inf, one -Inf) dropped out of 30.
  expect_equal(model$training_metadata$n_rows, 28)
  expect_true(is.finite(model$nodes$node_mean[model$nodes$node_id == 1]))
})

test_that('numeric CHAID predictions return the node mean and node id', {
  x <- rep(c('A', 'B'), each = 20)
  y <- ifelse(x == 'A', 10, 50)
  data <- data.frame(target = y, x = x, stringsAsFactors = FALSE)
  model <- chaid_fit(data, target = 'target', predictors = 'x',
                     min_split = 2, min_bucket = 1)

  values <- chaid_predict(model, data.frame(x = c('A', 'B')), type = 'value')
  expect_equal(values, c(10, 50))

  all_pred <- chaid_predict(model, data.frame(x = c('A', 'B')), type = 'all')
  expect_true('.pred_value' %in% names(all_pred))
  expect_true('.chaid_node_id' %in% names(all_pred))
  expect_false(any(grepl('^\\.pred_prob_', names(all_pred))))
})

test_that('chaid_node_summary / chaid_rule_table / chaid_split_summary report Mean / F Statistic for a numeric target', {
  x <- rep(c('A', 'B'), each = 30)
  y <- ifelse(x == 'A', 10, 50)
  data <- data.frame(target = y, x = x, stringsAsFactors = FALSE)
  model <- chaid_fit(data, target = 'target', predictors = 'x',
                     min_split = 2, min_bucket = 1)

  node_summary <- chaid_node_summary(model)
  expect_true('Mean' %in% names(node_summary))
  expect_true('Std. Dev.' %in% names(node_summary))
  expect_false('Predicted Class' %in% names(node_summary))

  rule_table <- chaid_rule_table(model)
  expect_true('Mean' %in% names(rule_table))
  expect_false('Probability' %in% names(rule_table))

  split_summary <- chaid_split_summary(model)
  expect_true('F Statistic' %in% names(split_summary))
  expect_true('df1' %in% names(split_summary))
  expect_true('df2' %in% names(split_summary))
})

test_that('existing categorical/logical CHAID behavior is unchanged (regression guard)', {
  # Same fixture shape as test_chaid.R's own merge test, run through the
  # updated merge_categories()/chaid_fit() with numeric_target defaulting to
  # FALSE -- must produce byte-identical results to before this PR.
  values <- rep(c('a', 'b', 'c'), each = 40)
  target <- c(
    rep(c('yes', 'no'), 20),
    rep(c('yes', 'no'), 20),
    rep('yes', 40)
  )
  result <- exploratory:::merge_categories(
    values = values, target = target, ordered = FALSE, alpha_merge = 0.05,
    bonferroni = TRUE, variable = 'segment', node_id = 1L
  )
  expect_true(length(result$groups) < 3)
  expect_true(any(vapply(result$merge_history, function(x) {
    all(c('a', 'b') %in% x$original_categories)
  }, logical(1))))

  character_data <- data.frame(target = c('yes', 'no', 'yes', 'no'), x = c('a', 'a', 'b', 'b'))
  model <- chaid_fit(character_data, target = 'target', min_split = 2, min_bucket = 1)
  expect_equal(model$target_type, 'character')
  expect_true(!is.null(model$class_levels))

  logical_data <- data.frame(target = c(TRUE, FALSE, TRUE, FALSE), x = c('a', 'a', 'b', 'b'))
  logical_model <- chaid_fit(logical_data, target = 'target', min_split = 2, min_bucket = 1)
  expect_equal(logical_model$target_type, 'logical')
  expect_true(!is.null(logical_model$class_levels))
})
