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
  expect_equal(colnames(ni),
               c("Node", "Variable", "Initial Binning", "Final Intervals"))
  # Only numeric predictors appear.
  expect_true(all(ni$Variable %in% c("age", "salary")))
  expect_true(all(grepl("bins$", ni[["Initial Binning"]])))
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
               c("Node", "Variable", "Initial Binning", "Final Intervals"))
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

test_that("exp_chaid rejects a numeric target", {
  df <- data.frame(y = 1:10, x = rep(c("a", "b"), 5))
  expect_error(exp_chaid(df, y, x), "categorical target")
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
