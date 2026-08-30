# Integration tests for exp_chaid() and the exploratory_chaid S3 layer.

make_binary_df <- function(n = 400, seed = 1) {
  set.seed(seed)
  data.frame(
    is_churn = c(rep(TRUE, n * 0.4), rep(FALSE, n * 0.6)),
    plan = sample(c("A", "B", "C"), n, replace = TRUE),
    region = sample(c("east", "west"), n, replace = TRUE),
    tenure = rnorm(n, 12, 4),
    stringsAsFactors = FALSE
  )
}

make_multi_df <- function(n = 450, seed = 2) {
  set.seed(seed)
  data.frame(
    segment = sample(c("gold", "silver", "bronze"), n, replace = TRUE),
    channel = rep(c("web", "store", "phone"), length.out = n),
    age_group = sample(c("young", "mid", "senior"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

test_that("exp_chaid returns a model data frame with the expected columns", {
  df <- make_multi_df()
  model_df <- suppressWarnings(exp_chaid(df, segment, channel, age_group,
                                         min_split = 20, min_bucket = 5))
  expect_true(all(c("model", ".test_index", "source.data") %in% colnames(model_df)))
  expect_true(".model.chaid" %in% class(model_df$model))
  expect_s3_class(model_df$model[[1]], "exploratory_chaid")
})

test_that("exp_chaid supports group_by with per-group models", {
  df <- make_multi_df()
  df$org <- rep(c("g1", "g2"), length.out = nrow(df))
  model_df <- suppressWarnings(
    df %>% dplyr::group_by(org) %>%
      exp_chaid(segment, channel, age_group, min_split = 20, min_bucket = 5)
  )
  expect_equal(nrow(model_df), 2)
  expect_true("org" %in% colnames(model_df))
})

test_that("exp_chaid test mode populates a test index", {
  df <- make_binary_df()
  model_df <- suppressWarnings(exp_chaid(df, is_churn, plan, region, tenure,
                                         test_rate = 0.3, min_split = 20, min_bucket = 5))
  expect_gt(length(model_df$.test_index[[1]]), 0)
})

test_that("tree_nodes tidy output matches the renderer schema", {
  df <- make_multi_df()
  model_df <- suppressWarnings(exp_chaid(df, segment, channel, age_group,
                                         min_split = 20, min_bucket = 5))
  nodes <- model_df %>% tidy_rowwise(model, type = "tree_nodes")

  expected_cols <- c("node_id", "parent_id", "depth", "is_leaf", "edge_label",
                     "predicted", "n", "pct", "class_json", "cond_column",
                     "cond_operator", "cond_value")
  expect_true(all(expected_cols %in% colnames(nodes)))
  root <- nodes[is.na(nodes$parent_id), ]
  expect_equal(root$pct, 1)
  expect_true(is.na(root$parent_id))
  # class_json parses to per-class label/n/pct.
  parsed <- jsonlite::fromJSON(nodes$class_json[[1]])
  expect_true(all(c("label", "n", "pct") %in% names(parsed)))
})

test_that("tree_nodes carries CHAID split stats on split nodes, NA on leaves", {
  # Build a target that genuinely depends on `plan` so CHAID makes a split.
  set.seed(3)
  n <- 400
  plan <- sample(c("A", "B"), n, replace = TRUE)
  is_churn <- ifelse(plan == "A", runif(n) < 0.8, runif(n) < 0.2)
  df <- data.frame(is_churn = is_churn, plan = plan,
                   region = sample(c("east", "west"), n, replace = TRUE),
                   stringsAsFactors = FALSE)
  model_df <- suppressWarnings(exp_chaid(df, is_churn, plan, region,
                                         min_split = 20, min_bucket = 5))
  nodes <- model_df %>% tidy_rowwise(model, type = "tree_nodes")

  stat_cols <- c("p_value", "adjusted_p_value", "split_statistic", "split_df")
  expect_true(all(stat_cols %in% colnames(nodes)))

  split_nodes <- nodes[!nodes$is_leaf, ]
  leaf_nodes <- nodes[nodes$is_leaf, ]
  expect_gt(nrow(split_nodes), 0)
  # Every splitting node reports a finite chi-square, df, and adjusted p-value.
  expect_true(all(is.finite(split_nodes$split_statistic)))
  expect_true(all(is.finite(split_nodes$split_df)))
  expect_true(all(is.finite(split_nodes$adjusted_p_value)))
  expect_true(all(split_nodes$adjusted_p_value >= 0 & split_nodes$adjusted_p_value <= 1))
  # Leaves never carry a split test.
  expect_true(all(is.na(leaf_nodes$split_statistic)))
  expect_true(all(is.na(leaf_nodes$split_df)))
})

test_that("binary tree_nodes lists the positive (TRUE) class first", {
  df <- make_binary_df()
  model_df <- suppressWarnings(exp_chaid(df, is_churn, plan, region,
                                         min_split = 20, min_bucket = 5))
  nodes <- model_df %>% tidy_rowwise(model, type = "tree_nodes")
  parsed <- jsonlite::fromJSON(nodes$class_json[[1]])
  expect_equal(parsed$label[[1]], "TRUE")
})

test_that("rf_evaluation_training_and_test produces training and test metrics", {
  df <- make_binary_df()
  model_df <- suppressWarnings(exp_chaid(df, is_churn, plan, region, tenure,
                                         test_rate = 0.3, min_split = 20, min_bucket = 5))
  summary <- model_df %>% rf_evaluation_training_and_test(pretty.name = TRUE)
  expect_gt(nrow(summary), 0)
  expect_true("is_test_data" %in% colnames(summary) || any(grepl("Test", colnames(summary))) || nrow(summary) >= 2)
})

test_that("numeric CHAID produces finite held-out regression metrics", {
  set.seed(4)
  n <- 200
  x <- sample(c("A", "B"), n, replace = TRUE)
  y <- ifelse(x == "A", 10, 50) + rnorm(n)
  df <- data.frame(y = y, x = x, stringsAsFactors = FALSE)

  model_df <- suppressWarnings(exp_chaid(
    df, y, x, test_rate = 0.3, min_split = 10, min_bucket = 5,
    max_depth = 1
  ))
  summary <- model_df %>% rf_evaluation_training_and_test(pretty.name = TRUE)

  expect_equal(nrow(summary), 2)
  expect_true(all(is.finite(summary$`R Squared`)))
  expect_true(all(is.finite(summary$RMSE)))
})

test_that("exp_chaid report_metrics adds ROC AUC / PR AUC like CART", {
  expected_binary <- c("ROC AUC", "PR AUC", "Balanced Accuracy", "Specificity")
  expected_multi <- c("Balanced Accuracy", "Macro ROC AUC", "Macro PR AUC")

  for (test_rate in c(0, 0.3)) {
    df <- make_binary_df(n = 500, seed = 21)
    model_df <- suppressWarnings(exp_chaid(df, is_churn, plan, region, tenure,
                                           test_rate = test_rate, min_split = 20, min_bucket = 5))
    base <- rf_evaluation_training_and_test(model_df, pretty.name = TRUE)
    with_metrics <- rf_evaluation_training_and_test(model_df, pretty.name = TRUE,
                                                    report_metrics = TRUE)
    label <- paste0("binary test_rate=", test_rate)
    expect_true(all(expected_binary %in% colnames(with_metrics)), info = label)
    expect_false(any(expected_binary %in% colnames(base)), info = label)
    expect_false("AUC" %in% colnames(with_metrics), info = label)
    expect_equal(nrow(with_metrics), if (test_rate > 0) 2 else 1, info = label)
    expect_false(any(is.na(with_metrics[, expected_binary, drop = FALSE])), info = label)
    expect_equal(
      intersect(c("ROC AUC", "PR AUC", "F1 Score", "Balanced Accuracy", "Accuracy Rate",
                  "Misclass. Rate", "Precision", "Recall", "Specificity"),
                colnames(with_metrics)),
      c("ROC AUC", "PR AUC", "F1 Score", "Balanced Accuracy", "Accuracy Rate",
        "Misclass. Rate", "Precision", "Recall", "Specificity"),
      info = label
    )
  }

  df <- make_multi_df(n = 500, seed = 22)
  model_df <- suppressWarnings(exp_chaid(df, segment, channel, age_group,
                                         min_split = 20, min_bucket = 5))
  with_metrics <- rf_evaluation_training_and_test(model_df, pretty.name = TRUE,
                                                  report_metrics = TRUE)
  expect_true(all(expected_multi %in% colnames(with_metrics)))
  by_class <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class",
                                              pretty.name = TRUE, report_metrics = TRUE)
  expect_true(all(c("Balanced Accuracy", "ROC AUC", "PR AUC", "Overall Share") %in% colnames(by_class)))
})

test_that("confusion matrix tidy returns actual/predicted/count", {
  df <- make_binary_df()
  model_df <- suppressWarnings(exp_chaid(df, is_churn, plan, region,
                                         min_split = 20, min_bucket = 5))
  cm <- model_df %>% tidy_rowwise(model, type = "conf_mat")
  expect_true(all(c("actual_value", "predicted_value") %in% colnames(cm)))
})

test_that("prediction(training_and_test) restores columns and adds predictions", {
  df <- make_binary_df()
  model_df <- suppressWarnings(exp_chaid(df, is_churn, plan, region, tenure,
                                         test_rate = 0.3, min_split = 20, min_bucket = 5))
  pred <- model_df %>% prediction(data = "training_and_test")
  expect_true("predicted_probability" %in% colnames(pred))
  expect_true("predicted_label" %in% colnames(pred))
  expect_true("is_test_data" %in% colnames(pred))
})

test_that("binary classification threshold shifts predicted labels", {
  df <- make_binary_df()
  model_df <- suppressWarnings(exp_chaid(df, is_churn, plan, region, tenure,
                                         min_split = 20, min_bucket = 5))
  low <- model_df %>% prediction(data = "training", binary_classification_threshold = 0.2)
  high <- model_df %>% prediction(data = "training", binary_classification_threshold = 0.8)
  n_true_low <- sum(low$predicted_label == "TRUE", na.rm = TRUE)
  n_true_high <- sum(high$predicted_label == "TRUE", na.rm = TRUE)
  expect_gte(n_true_low, n_true_high)
})

test_that("report tidy types return stable schemas and permutation importance", {
  df <- make_multi_df()
  model_df <- suppressWarnings(exp_chaid(df, segment, channel, age_group,
                                         min_split = 20, min_bucket = 5))
  ns <- model_df %>% tidy_rowwise(model, type = "node_summary")
  rules <- model_df %>% tidy_rowwise(model, type = "rules")
  splits <- model_df %>% tidy_rowwise(model, type = "split_summary")
  expect_true("Node" %in% colnames(ns))
  expect_true("Rule" %in% colnames(rules))
  expect_true("Node" %in% colnames(splits))
  importance <- model_df %>% tidy_rowwise(model, type = "importance")
  expect_true(all(c("variable", "importance", "std_error", "rank", "metric",
                    "evaluation_data", "repeats") %in% colnames(importance)))
  expect_equal(nrow(importance), 2)
  expect_true(all(importance$metric == "log_loss"))
  expect_true(all(importance$evaluation_data == "Training"))
  expect_true(all(importance$repeats == 10))
})

make_ordered_df <- function(n = 600, seed = 1) {
  set.seed(seed)
  satisfaction <- factor(sample(c("Low", "Mid", "High"), n, replace = TRUE),
                         levels = c("Low", "Mid", "High"), ordered = TRUE)
  overtime <- sample(c("Yes", "No"), n, replace = TRUE)
  score <- ifelse(overtime == "Yes", 1, 0) + as.integer(satisfaction) / 3 +
    rnorm(n, 0, 0.4)
  grade <- cut(score, breaks = quantile(score, c(0, 1 / 3, 2 / 3, 1)),
               labels = c("A", "B", "C"), include.lowest = TRUE)
  data.frame(
    grade = factor(as.character(grade), levels = c("A", "B", "C"),
                   ordered = TRUE),
    satisfaction = satisfaction,
    overtime = overtime,
    stringsAsFactors = FALSE
  )
}

test_that("category_error_distribution reports ordinal distance for an ordered target", {
  df <- make_ordered_df()
  model_df <- suppressWarnings(exp_chaid(df, grade, satisfaction, overtime,
                                         min_split = 20, min_bucket = 10,
                                         max_depth = 3))
  expect_true(isTRUE(model_df$model[[1]]$is_target_ordered))
  expect_equal(model_df$model[[1]]$ordered_levels, c("A", "B", "C"))

  dist <- model_df %>% tidy_rowwise(model, type = "category_error_distribution")
  expect_equal(colnames(dist), c("Category Distance", "Rows", "Percentage"))
  expect_true(nrow(dist) >= 1)
  # A perfect-distance-0 row must be present, distances are contiguous integers,
  # and rows account for every scored observation.
  expect_true(0 %in% dist[["Category Distance"]])
  expect_equal(dist[["Category Distance"]],
               seq(min(dist[["Category Distance"]]),
                   max(dist[["Category Distance"]])))
  expect_equal(sum(dist$Rows), length(model_df$model[[1]]$y))
  expect_equal(sum(dist$Percentage), 100, tolerance = 1e-6)
})

test_that("numeric_intervals reports initial binning + final intervals per numeric split", {
  set.seed(3); n <- 800
  df <- data.frame(
    grade = sample(c("A", "B", "C"), n, replace = TRUE),
    age = round(rnorm(n, 40, 12)),
    salary = round(rnorm(n, 500, 150)),
    overtime = sample(c("Yes", "No"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  model_df <- suppressWarnings(exp_chaid(df, grade, age, salary, overtime,
                                         min_split = 30, min_bucket = 15,
                                         max_depth = 3))
  ni <- model_df %>% tidy_rowwise(model, type = "numeric_intervals")
  # tam #37177: "Initial Binning" was split into method + bin count.
  expect_equal(colnames(ni),
               c("Node", "Variable", "Binning Method", "Initial Bins", "Final Intervals"))
  # Only numeric predictors appear.
  expect_true(all(ni$Variable %in% c("age", "salary")))
  expect_true(all(ni[["Binning Method"]] %in% c("quantile", "equal_width")))
  expect_true(all(ni[["Initial Bins"]] > 0))
  expect_true(all(nchar(ni[["Final Intervals"]]) > 0))
})

test_that("numeric_intervals is empty when no numeric predictor is binned", {
  set.seed(4); n <- 400
  df <- data.frame(
    grade = sample(c("A", "B", "C"), n, replace = TRUE),
    overtime = sample(c("Yes", "No"), n, replace = TRUE),
    dept = sample(c("X", "Y", "Z"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  model_df <- suppressWarnings(exp_chaid(df, grade, overtime, dept,
                                         min_split = 30, min_bucket = 15))
  ni <- model_df %>% tidy_rowwise(model, type = "numeric_intervals")
  expect_equal(nrow(ni), 0)
  expect_equal(colnames(ni),
               c("Node", "Variable", "Binning Method", "Initial Bins", "Final Intervals"))
})

test_that("Final Intervals shows 'N <' for display while cond_value keeps '> N' (tam #37691)", {
  # Same fixture as "tree_nodes edge labels collapse contiguous numeric bins"
  # below -- guarantees an unbounded-above final bin (salary > ~5045).
  set.seed(11); n <- 700
  df <- data.frame(
    salary = round(runif(n, 1000, 15000)),
    dept = sample(c("sales", "rnd", "hr"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  df$churn <- df$salary < 4000 | runif(n) < 0.1
  model_df <- suppressWarnings(exp_chaid(df, churn, salary, dept,
                                         min_split = 40, min_bucket = 20,
                                         max_depth = 2))
  ni <- model_df %>% tidy_rowwise(model, type = "numeric_intervals")
  expect_true(any(grepl("[0-9] <($| /)", ni[["Final Intervals"]])),
              "the report-display column must show the flipped 'N <' shape")
  nodes <- model_df %>% tidy_rowwise(model, type = "tree_nodes")
  salary_edges <- nodes[!is.na(nodes$cond_value) & nodes$cond_column == "salary", ]
  expect_true(nrow(salary_edges) > 0)
  values <- unlist(lapply(salary_edges$cond_value, jsonlite::fromJSON))
  # cond_value feeds DTreeGenerator.parseNumericBinLabel (tam) for Show Detail
  # drill-down -- it must stay the RAW "> N" shape, never the display "N <".
  expect_true(any(grepl("^>", values)),
              "cond_value must keep the raw '> N' shape unbounded-above bins use")
  expect_false(any(grepl("<$", values)),
               "cond_value must never carry the report-display 'N <' shape")
})

test_that("category_error_distribution is empty for a non-ordered target", {
  df <- make_ordered_df()
  df$grade <- as.character(df$grade) # drop the ordered attribute
  model_df <- suppressWarnings(exp_chaid(df, grade, satisfaction, overtime,
                                         min_split = 20, min_bucket = 10,
                                         max_depth = 3))
  expect_false(isTRUE(model_df$model[[1]]$is_target_ordered))
  dist <- model_df %>% tidy_rowwise(model, type = "category_error_distribution")
  expect_equal(nrow(dist), 0)
  expect_equal(colnames(dist), c("Category Distance", "Rows", "Percentage"))
})

test_that("permutation importance uses held-out rows in test mode", {
  set.seed(11)
  n <- 240
  plan <- sample(c("A", "B"), n, replace = TRUE)
  is_churn <- plan == "A"
  df <- data.frame(
    is_churn = is_churn,
    plan = plan,
    noise = sample(c("x", "y", "z"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  model_df <- suppressWarnings(exp_chaid(
    df, is_churn, plan, noise, test_rate = 0.3,
    min_split = 10, min_bucket = 3, seed = 17
  ))
  importance <- model_df %>% tidy_rowwise(model, type = "importance")

  expect_true(all(importance$evaluation_data == "Test"))
  expect_true(all(importance$repeats == 10))
  expect_gt(importance$importance[importance$variable == "plan"], 0)
})

test_that("permutation importance is reproducible with a fixed seed", {
  df <- make_binary_df(n = 240)
  m1 <- suppressWarnings(exp_chaid(df, is_churn, plan, region, tenure,
                                   min_split = 20, min_bucket = 5, seed = 23))
  m2 <- suppressWarnings(exp_chaid(df, is_churn, plan, region, tenure,
                                   min_split = 20, min_bucket = 5, seed = 23))
  i1 <- m1 %>% tidy_rowwise(model, type = "importance")
  i2 <- m2 %>% tidy_rowwise(model, type = "importance")
  expect_equal(i1, i2)
})

test_that("prediction on new data handles unseen categories without error", {
  df <- make_multi_df()
  model_df <- suppressWarnings(exp_chaid(df, segment, channel, age_group,
                                         min_split = 20, min_bucket = 5))
  new_df <- data.frame(channel = c("web", "unseen_channel"),
                       age_group = c("young", "senior"),
                       stringsAsFactors = FALSE)
  pred <- model_df %>% prediction(data = "newdata", data_frame = new_df)
  expect_equal(nrow(pred), 2)
  expect_true("predicted_label" %in% colnames(pred))
})

test_that("exp_chaid is reproducible with a fixed seed", {
  df <- make_binary_df(n = 2000)
  m1 <- suppressWarnings(exp_chaid(df, is_churn, plan, region, tenure,
                                   max_nrow = 500, seed = 42, min_split = 20, min_bucket = 5))
  m2 <- suppressWarnings(exp_chaid(df, is_churn, plan, region, tenure,
                                   max_nrow = 500, seed = 42, min_split = 20, min_bucket = 5))
  expect_equal(m1$model[[1]]$nodes$rule, m2$model[[1]]$nodes$rule)
})

test_that("exp_chaid supports a numeric target", {
  df <- data.frame(y = 1:10, x = rep(c("a", "b"), 5))
  model_df <- suppressWarnings(exp_chaid(df, y, x, min_split = 2, min_bucket = 1))
  expect_s3_class(model_df$model[[1]], "exploratory_chaid")
  expect_equal(model_df$model[[1]]$target_type, "numeric")
  expect_equal(model_df$model[[1]]$classification_type, "regression")
})

test_that("exp_chaid validates test split arguments", {
  df <- make_multi_df(n = 60)
  expect_error(exp_chaid(df, segment, channel, age_group, test_split_type = "bad"),
               "arg.*should be one of")
  expect_error(exp_chaid(df, segment, channel, age_group, test_rate = NA_real_),
               "test_rate must be between")
})

# ---------------------------------------------------------------------------
# CHAID stage 3: allow_resplit (#23772)
# ---------------------------------------------------------------------------

# Expand per-category target counts into the raw value/target vectors
# merge_categories() consumes. Deterministic -- no RNG in the fixtures.
chaid_resplit_fixture <- function(counts, levels = c("c1", "c2", "c3")) {
  values <- character(0)
  target <- character(0)
  for (nm in names(counts)) {
    values <- c(values, rep(nm, sum(counts[[nm]])))
    target <- c(target, rep(levels, counts[[nm]]))
  }
  list(values = values, target = target)
}

chaid_resplit_actions <- function(result) {
  vapply(result$merge_history, function(h) {
    if (is.null(h$action)) NA_character_ else h$action
  }, character(1))
}

test_that("chaid_resplit_partitions keeps ordered predictors contiguous", {
  ordered_parts <- chaid_resplit_partitions(c(1L, 2L, 3L, 4L), ordered = TRUE)
  expect_equal(length(ordered_parts), 3)
  for (part in ordered_parts) {
    expect_equal(part$a, sort(part$a))
    expect_equal(max(part$a) + 1L, min(part$b))   # one contiguous cut
  }
  # Nominal enumerates every partition: 2^(k-1) - 1.
  expect_equal(length(chaid_resplit_partitions(c(1L, 2L, 3L), ordered = FALSE)), 3)
  # Too small to split, and the combinatorial cap for wide nominal groups.
  expect_equal(length(chaid_resplit_partitions(c(1L, 2L))), 0)
  expect_equal(length(chaid_resplit_partitions(1:13, ordered = FALSE)), 0)
})

test_that("chaid_best_resplit splits a separable compound and leaves a uniform one alone", {
  separable <- matrix(c(180, 20, 20, 180, 25, 175), nrow = 2,
                      dimnames = list(c("c1", "c2"), c("A", "B", "C")))
  col_of <- function(g) if (length(g) == 1L) separable[, g] else rowSums(separable[, g, drop = FALSE])
  best <- chaid_best_resplit(c(1L, 2L, 3L), col_of, ordered = FALSE,
                             alpha_merge = 0.05, bonferroni = FALSE)
  expect_false(is.null(best))
  expect_equal(sort(c(length(best$a), length(best$b))), c(1, 2))
  expect_lt(best$adjusted_p_value, 0.05)

  uniform <- matrix(c(100, 100, 101, 99, 99, 101), nrow = 2,
                    dimnames = list(c("c1", "c2"), c("A", "B", "C")))
  col_of_uniform <- function(g) if (length(g) == 1L) uniform[, g] else rowSums(uniform[, g, drop = FALSE])
  expect_null(chaid_best_resplit(c(1L, 2L, 3L), col_of_uniform, ordered = FALSE,
                                 alpha_merge = 0.05, bonferroni = FALSE))
})

test_that("allow_resplit = FALSE leaves the greedy merge byte-identical", {
  fixture <- chaid_resplit_fixture(list(
    A = c(12, 29, 19), B = c(17, 19, 24), C = c(12, 22, 25), D = c(22, 15, 23),
    E = c(21, 32, 6), F = c(8, 17, 35), G = c(15, 21, 24)
  ))
  off <- merge_categories(fixture$values, fixture$target, alpha_merge = 0.05,
                          bonferroni = FALSE, chi_square = "pearson",
                          allow_resplit = FALSE)
  # Default argument must behave the same as an explicit FALSE.
  default <- merge_categories(fixture$values, fixture$target, alpha_merge = 0.05,
                              bonferroni = FALSE, chi_square = "pearson")
  expect_equal(off$group_labels, default$group_labels)
  expect_false(any(chaid_resplit_actions(off) %in% "resplit"))
  expect_true(all(chaid_resplit_actions(off) == "merge"))
})

test_that("allow_resplit = TRUE breaks apart an over-merged compound", {
  fixture <- chaid_resplit_fixture(list(
    A = c(12, 29, 19), B = c(17, 19, 24), C = c(12, 22, 25), D = c(22, 15, 23),
    E = c(21, 32, 6), F = c(8, 17, 35), G = c(15, 21, 24)
  ))
  args <- list(values = fixture$values, target = fixture$target, alpha_merge = 0.05,
               bonferroni = FALSE, chi_square = "pearson")
  off <- do.call(merge_categories, c(args, list(allow_resplit = FALSE)))
  on <- do.call(merge_categories, c(args, list(allow_resplit = TRUE)))

  # The greedy merge fuses five categories into one group; re-splitting repairs it.
  expect_true(any(chaid_resplit_actions(on) == "resplit"))
  expect_gt(length(on$groups), length(off$groups))
  # Every original category is still assigned exactly once.
  expect_equal(sort(unlist(on$groups)), sort(unlist(off$groups)))
})

test_that("allow_resplit keeps ordered groups contiguous and terminates", {
  fixture <- chaid_resplit_fixture(list(
    A = c(12, 29, 19), B = c(17, 19, 24), C = c(12, 22, 25), D = c(22, 15, 23),
    E = c(21, 32, 6), F = c(8, 17, 35), G = c(15, 21, 24)
  ))
  levels_in_order <- LETTERS[1:7]
  result <- merge_categories(fixture$values, fixture$target, ordered = TRUE,
                             ordered_levels = levels_in_order, alpha_merge = 0.05,
                             bonferroni = FALSE, chi_square = "pearson",
                             allow_resplit = TRUE)
  for (group in result$groups) {
    positions <- sort(match(group, levels_in_order))
    expect_true(all(diff(positions) == 1))   # no non-contiguous interval
  }
})

test_that("chaid tidy exposes 0-based breadth-first node ids on every surface", {
  set.seed(11); n <- 1200
  df <- data.frame(
    y  = sample(c("a", "b", "c"), n, TRUE),
    p1 = sample(c("x", "y", "z", "w"), n, TRUE),
    p2 = sample(c("m", "n", "o"), n, TRUE),
    p3 = round(rnorm(n, 50, 15)),
    stringsAsFactors = FALSE
  )
  df$y <- ifelse(df$p1 %in% c("x", "y") & df$p3 > 55, "a",
                 ifelse(df$p2 == "m", "b", df$y))
  model_df <- suppressWarnings(
    df %>% exp_chaid(y, p1, p2, p3, max_depth = 3, min_split = 20, min_bucket = 5))
  nodes <- model_df %>% tidy_rowwise(model, type = "tree_nodes")

  expect_gt(nrow(nodes), 3)                                        # a tree that branches
  expect_equal(sort(nodes$node_id), seq_len(nrow(nodes)) - 1L)     # gapless, 0-based
  expect_equal(nodes$node_id[is.na(nodes$parent_id)], 0L)          # SPSS: root = 0
  # Growth is depth-first, so this ordering only holds because the ids are
  # renumbered breadth-first afterwards.
  expect_true(all(is.na(nodes$parent_id) | nodes$parent_id < nodes$node_id))
  expect_false(is.unsorted(nodes$depth))

  # Every report surface must quote the SAME ids as the chart.
  valid <- nodes$node_id
  for (type in c("split_summary", "node_summary", "rules", "category_merges",
                 "numeric_intervals")) {
    tbl <- model_df %>% tidy_rowwise(model, type = type)
    if ("Node" %in% names(tbl) && nrow(tbl) > 0) {
      expect_true(all(tbl$Node %in% valid), info = type)
    }
  }

  # The model itself stays 1-based: chaid_assign_nodes seeds its queue with node
  # 1 and root row counts are looked up by it, so only the tidy is shifted.
  expect_equal(model_df$model[[1]]$nodes$node_id[
    is.na(model_df$model[[1]]$nodes$parent_id)], 1L)
})

test_that("tree_nodes edge labels collapse contiguous numeric bins (tam #37177)", {
  set.seed(11); n <- 700
  df <- data.frame(
    salary = round(runif(n, 1000, 15000)),
    dept = sample(c("sales", "rnd", "hr"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  df$churn <- df$salary < 4000 | runif(n) < 0.1
  model_df <- suppressWarnings(exp_chaid(df, churn, salary, dept,
                                         min_split = 40, min_bucket = 20,
                                         max_depth = 2))
  nodes <- model_df %>% tidy_rowwise(model, type = "tree_nodes")
  edges <- nodes[!is.na(nodes$parent_id) & nodes$cond_column == "salary", ]
  expect_gt(nrow(edges), 0)
  for (i in seq_len(nrow(edges))) {
    values <- jsonlite::fromJSON(edges$cond_value[i])
    # Every numeric branch collapses its contiguous bin run to ONE range label.
    expect_equal(length(values), 1)
    expect_true(grepl("^(<=|>|\\()", values))
    # cond_value stays machine-parseable bin labels; edge_label is readable
    # (CHAID report Condition / tree chart — not "salary = <= x").
    expect_equal(
      edges$edge_label[i],
      chaid_readable_one_condition(paste0("salary in {", values, "}"))
    )
    expect_false(grepl(" = <=| = \\(| = >", edges$edge_label[i]))
  }
  # A categorical branch keeps its member enumeration untouched.
  cat_edges <- nodes[!is.na(nodes$parent_id) & nodes$cond_column == "dept", ]
  if (nrow(cat_edges) > 0) {
    cat_values <- jsonlite::fromJSON(cat_edges$cond_value[1])
    expect_true(all(cat_values %in% c("sales", "rnd", "hr")))
  }
})

test_that("exp_chaid stores partial dependence for rf_partial_dependence()", {
  skip_if_not_installed("mmpf")
  df <- make_binary_df(n = 500, seed = 42)
  model_df <- suppressWarnings(
    exp_chaid(df, is_churn, plan, region, tenure,
              min_split = 20, min_bucket = 5, max_pd_vars = 3,
              pd_with_bin_means = TRUE)
  )
  model <- model_df$model[[1]]
  expect_false(is.null(model$partial_dependence))
  expect_gt(nrow(model$partial_dependence), 0)
  expect_true(length(model$imp_vars) >= 1)
  expect_true(length(model$imp_vars) <= 3)
  expect_false(is.null(model$partial_binning))

  pd <- model_df %>% rf_partial_dependence()
  expect_gt(nrow(pd), 0)
  expect_true(all(c("x_name", "x_value", "y_name", "y_value") %in% names(pd)))
  expect_true(any(pd$y_name %in% c("Predicted", "Actual")))
})

test_that("chaid_partial_dependence_vars prefers importance order", {
  predictors <- c("a", "b", "c", "d")
  terms_mapping <- c(a = "A", b = "B", c = "C", d = "D")
  importance <- data.frame(
    variable = c("C", "A", "D", "B"),
    importance = c(0.4, 0.3, 0.2, 0.1),
    stringsAsFactors = FALSE
  )
  expect_equal(
    chaid_partial_dependence_vars(importance, predictors, terms_mapping, 2),
    c("c", "a")
  )
  expect_equal(
    chaid_partial_dependence_vars(NULL, predictors, terms_mapping, 2),
    c("a", "b")
  )
})

# tam#37466: importance_measure = "firm" reuses the model-agnostic
# importance_firm() the other tree models already share. CHAID splits on
# chi-square significance, not impurity, so there is no "impurity" option --
# the choice is permutation (default) or FIRM.
test_that("importance_measure = 'firm' returns the same schema as permutation", {
  df <- make_binary_df()
  perm <- suppressWarnings(exp_chaid(df, is_churn, plan, region, tenure,
                                     min_split = 20, min_bucket = 5))
  firm <- suppressWarnings(exp_chaid(df, is_churn, plan, region, tenure,
                                     min_split = 20, min_bucket = 5,
                                     importance_measure = "firm"))
  perm_imp <- perm %>% tidy_rowwise(model, type = "importance")
  firm_imp <- firm %>% tidy_rowwise(model, type = "importance")

  # Identical column contract -- chaid_partial_dependence_vars() and the report
  # tables read these columns, so a 2-column FIRM table would break them.
  expect_equal(colnames(firm_imp), colnames(perm_imp))
  expect_true(all(firm_imp$metric == "firm"))
  expect_true(all(perm_imp$metric == "log_loss"))
  # FIRM is derived from the PD curves, so it has no permutation repeats / SE.
  expect_true(all(is.na(firm_imp$std_error)))
  expect_true(all(is.na(firm_imp$repeats)))
  expect_true(all(firm_imp$evaluation_data == "Training"))
  # Ranks are dense and ordered.
  expect_equal(firm_imp$rank[[1]], 1L)
  expect_false(is.unsorted(firm_imp$rank))
})

test_that("importance_measure = 'firm' keeps display names and feeds partial dependence", {
  df <- make_binary_df()
  names(df)[names(df) == "tenure"] <- "tenure months"
  firm <- suppressWarnings(exp_chaid(df, is_churn, plan, region, `tenure months`,
                                     min_split = 20, min_bucket = 5,
                                     importance_measure = "firm"))
  imp <- firm %>% tidy_rowwise(model, type = "importance")
  # Display (original) names in the table ...
  expect_true("tenure months" %in% imp$variable)
  # ... and the PD variable list is still resolvable back to clean names.
  model <- firm$model[[1]]
  expect_true(length(model$imp_vars) > 0)
  expect_true(all(model$imp_vars %in% names(model$terms_mapping)))
  expect_false(is.null(model$partial_dependence))
})

test_that("importance_measure = 'firm' honors max_pd_vars when trimming partial dependence", {
  set.seed(11)
  n <- 400
  df <- data.frame(
    is_churn = c(rep(TRUE, 150), rep(FALSE, 250)),
    v1 = c(rnorm(150, 5), rnorm(250, 2)),
    v2 = c(rnorm(150, 3), rnorm(250, 2)),
    v3 = rnorm(n),
    v4 = rnorm(n)
  )
  firm <- suppressWarnings(exp_chaid(df, is_churn, v1, v2, v3, v4,
                                     min_split = 20, min_bucket = 5,
                                     importance_measure = "firm", max_pd_vars = 2))
  model <- firm$model[[1]]
  # FIRM needs PD over every predictor to rank them, then trims back down.
  expect_equal(length(model$imp_vars), 2)
  expect_equal(sort(attr(model$partial_dependence, "vars")), sort(model$imp_vars))
  # The importance table itself still lists every predictor.
  expect_equal(nrow(firm %>% tidy_rowwise(model, type = "importance")), 4)
})

test_that("importance_measure falls back to permutation when FIRM cannot apply", {
  df <- make_binary_df()
  # A single predictor gives FIRM nothing to compare against.
  single <- suppressWarnings(exp_chaid(df, is_churn, plan,
                                       min_split = 20, min_bucket = 5,
                                       importance_measure = "firm"))
  expect_true(all((single %>% tidy_rowwise(model, type = "importance"))$metric == "log_loss"))

  # An unrecognized value must not error out -- it takes the default path.
  bogus <- suppressWarnings(exp_chaid(df, is_churn, plan, region,
                                      min_split = 20, min_bucket = 5,
                                      importance_measure = "bogus"))
  expect_true(all((bogus %>% tidy_rowwise(model, type = "importance"))$metric == "log_loss"))

  # Optional UI settings can be absent or empty; both use the default path.
  null_measure <- suppressWarnings(exp_chaid(df, is_churn, plan, region,
                                             min_split = 20, min_bucket = 5,
                                             importance_measure = NULL))
  expect_true(all((null_measure %>% tidy_rowwise(model, type = "importance"))$metric == "log_loss"))

  empty_measure <- suppressWarnings(exp_chaid(df, is_churn, plan, region,
                                              min_split = 20, min_bucket = 5,
                                              importance_measure = character()))
  expect_true(all((empty_measure %>% tidy_rowwise(model, type = "importance"))$metric == "log_loss"))
})

test_that("importance_measure = 'firm' labels training PD data in test mode", {
  df <- make_binary_df()
  firm <- suppressWarnings(exp_chaid(df, is_churn, plan, region, tenure,
                                     min_split = 20, min_bucket = 5,
                                     importance_measure = "firm", test_rate = 0.3))
  imp <- firm %>% tidy_rowwise(model, type = "importance")
  expect_true(all(imp$evaluation_data == "Training"))
})

test_that("chaid_firm_importance returns NULL when there is no partial dependence", {
  expect_null(chaid_firm_importance(NULL, list(classification_type = "binary"), c("a", "b")))
  expect_null(chaid_firm_importance(data.frame(a = 1), list(classification_type = "binary"), character()))
})

# tam #38107: a predictor column name containing a comma reaches the report
# tables mangled ("A, B" -> "A. B") -- cleanup_df(..., map_name = FALSE)
# replaces commas with periods for mmpf::marginalPrediction compatibility
# before chaid_fit() ever runs, and the report functions that read
# model$nodes$split_variable / rule / category_merge_map$variable directly
# (chaid_node_summary, chaid_rule_table, chaid_split_summary,
# chaid_numeric_intervals, chaid_category_merge_table) never mapped that
# CLEAN name back to the real column name -- unlike build_chaid_tree_nodes()
# (the interactive tree / dtree_report_characteristic_groups() feed), which
# already did via its own local map_name() closure.
test_that("a comma-containing predictor column name survives report tables uncorrupted (tam #38107)", {
  set.seed(11)
  n <- 300
  # A near-deterministic signal on the comma-named predictor so a real split
  # is found reliably at the default alpha, and a category-merge is likely
  # too (4 categories, 2 of which behave identically).
  df <- data.frame(
    `気に入った, 理由` = sample(c("価格", "品質", "デザイン", "サポート"), n, replace = TRUE),
    price = rnorm(n, 50, 10),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  df$satisfaction <- ifelse(
    df[["気に入った, 理由"]] %in% c("価格", "品質"),
    sample(c("高", "低"), n, replace = TRUE, prob = c(0.85, 0.15)),
    sample(c("高", "低"), n, replace = TRUE, prob = c(0.15, 0.85))
  )

  model_df <- suppressWarnings(exp_chaid(
    df, satisfaction, `気に入った, 理由`, price,
    min_split = 20, min_bucket = 5, alpha_split = 0.2, alpha_merge = 0.2
  ))
  model <- model_df$model[[1]]

  node_summary <- chaid_node_summary(model)
  split_summary <- chaid_split_summary(model)
  rules <- chaid_rule_table(model)

  # At least one split must actually have been found on the comma-named
  # column, or this test proves nothing about the display path.
  expect_true("気に入った, 理由" %in% split_summary$`Split Variable`)
  expect_false(any(grepl("気に入った. 理由", split_summary$`Split Variable`, fixed = TRUE)))

  expect_true(any(grepl("気に入った, 理由", node_summary$Rule, fixed = TRUE)))
  expect_false(any(grepl("気に入った. 理由", node_summary$Rule, fixed = TRUE)))

  expect_true(any(grepl("気に入った, 理由", rules$Rule, fixed = TRUE)))
  expect_false(any(grepl("気に入った. 理由", rules$Rule, fixed = TRUE)))

  # If any category merge happened on the comma-named column, its Variable
  # column must show the original name too.
  merges <- chaid_category_merge_table(model)
  if (nrow(merges) > 0 && "気に入った, 理由" %in% c(merges$Variable, "気に入った. 理由")) {
    expect_true("気に入った, 理由" %in% merges$Variable)
    expect_false("気に入った. 理由" %in% merges$Variable)
  }

  # A numeric predictor with no comma is unaffected either way (regression
  # guard: the fix must not touch names that need no mapping).
  intervals <- chaid_numeric_intervals(model)
  if (nrow(intervals) > 0) {
    expect_true(all(intervals$Variable %in% c("price", "気に入った, 理由")))
  }
})
