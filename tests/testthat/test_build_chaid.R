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
  root <- nodes[nodes$node_id == 1, ]
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
