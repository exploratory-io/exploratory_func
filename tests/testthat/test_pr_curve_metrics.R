context("custom PR-curve metrics: Average Precision (XGBoost) and PR AUC (LightGBM)")

suppressPackageStartupMessages(library(dplyr))

# Synthetic binary data with signal. No network access needed.
make_pr_test_data <- function(n = 3000) {
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- runif(n)
  y <- as.integer((1.2 * x1 + 0.6 * x2 + rnorm(n)) > 1) # roughly 25% positive
  tibble::tibble(x1 = x1, x2 = x2, x3 = x3, y = as.logical(y))
}

test_that("average_precision_score matches scikit-learn", {
  # sklearn.metrics.average_precision_score([0, 0, 1, 1], [0.1, 0.4, 0.35, 0.8]) = 0.8333333333333333
  expect_equal(exploratory:::average_precision_score(c(0.1, 0.4, 0.35, 0.8), c(FALSE, FALSE, TRUE, TRUE)), 5 / 6)
  # Perfect ranking.
  expect_equal(exploratory:::average_precision_score(c(0.9, 0.8, 0.2, 0.1), c(TRUE, TRUE, FALSE, FALSE)), 1)
  # Ties are grouped into one threshold: 1/3*1 + 1/3*2/3 + 1/3*0.6 = 34/45 (sklearn gives 0.7555...).
  expect_equal(exploratory:::average_precision_score(c(0.9, 0.8, 0.8, 0.7, 0.6, 0.5), c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)), 34 / 45)
  # All scores tied: one threshold with recall 1 and precision = positive rate.
  expect_equal(exploratory:::average_precision_score(rep(0.5, 4), c(TRUE, FALSE, FALSE, FALSE)), 0.25)
  # Input order does not matter.
  s <- c(0.9, 0.8, 0.8, 0.7, 0.6, 0.5)
  b <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
  expect_equal(exploratory:::average_precision_score(rev(s), rev(b)), 34 / 45)
  # Numeric 0/1 labels work too.
  expect_equal(exploratory:::average_precision_score(s, as.numeric(b)), 34 / 45)
})

test_that("average_precision_score handles NAs and undefined cases", {
  # Pairs with NA in either side are dropped, like aupr().
  expect_equal(exploratory:::average_precision_score(c(0.1, 0.4, 0.35, 0.8, NA, 0.5), c(FALSE, FALSE, TRUE, TRUE, TRUE, NA)), 5 / 6)
  # Only one class, or nothing left, is NA.
  expect_true(is.na(exploratory:::average_precision_score(c(0.1, 0.2), c(TRUE, TRUE))))
  expect_true(is.na(exploratory:::average_precision_score(c(0.1, 0.2), c(FALSE, FALSE))))
  expect_true(is.na(exploratory:::average_precision_score(c(NA, NA), c(TRUE, FALSE))))
  expect_true(is.na(exploratory:::average_precision_score(numeric(0), logical(0))))
})

test_that("aupr (trapezoid PR AUC) is unchanged", {
  # Hand-computed trapezoid: points (R, P) = (0, 1), (1/2, 1), (1/2, 1/2), (1, 2/3), (1, 1/2)
  # 0.5 * (1 + 1) / 2 + 0 + 0.5 * (0.5 + 2/3) / 2 + 0 = 0.5 + 0.2916667
  expect_equal(exploratory:::aupr(c(0.9, 0.8, 0.7, 0.6), c(TRUE, FALSE, TRUE, FALSE)), 0.5 + 0.5 * (0.5 + 2 / 3) / 2)
  # The two metrics differ on the same data.
  expect_false(isTRUE(all.equal(
    exploratory:::aupr(c(0.9, 0.8, 0.7, 0.6), c(TRUE, FALSE, TRUE, FALSE)),
    exploratory:::average_precision_score(c(0.9, 0.8, 0.7, 0.6), c(TRUE, FALSE, TRUE, FALSE))
  )))
})

test_that("xgboost custom Average Precision metric equals average_precision_score on predictions", {
  testthat::skip_if_not_installed("xgboost")
  set.seed(1)
  d <- make_pr_test_data()
  dtrain <- xgboost::xgb.DMatrix(data = as.matrix(d[, c("x1", "x2", "x3")]), label = as.numeric(d$y))
  fit <- xgboost::xgb.train(
    params = list(objective = "binary:logistic", disable_default_eval_metric = TRUE),
    data = dtrain, nrounds = 5, evals = list(train = dtrain),
    custom_metric = exploratory:::xgb_custom_binary_metric("average_precision"), maximize = TRUE, verbose = 0
  )
  log <- attr(fit, "evaluation_log")
  expect_equal(colnames(log), c("iter", "train_average_precision"))
  expect_equal(log$train_average_precision[[5]], exploratory:::average_precision_score(predict(fit, dtrain), d$y))
})

test_that("exp_xgboost(binary) learning curve with eval_metric_binary = average_precision", {
  testthat::skip_if_not_installed("xgboost")
  set.seed(1)
  d <- make_pr_test_data()
  model_df <- d %>%
    exp_xgboost(y, x1, x2, x3,
                predictor_funs = list(x1 = "none", x2 = "none", x3 = "none"),
                nrounds = 10, test_rate = 0, watchlist_rate = 0.2,
                importance_measure = "xgboost", max_pd_vars = 0, pd_with_bin_means = FALSE,
                eval_metric_binary = "average_precision")
  ret <- model_df %>% tidy_rowwise(model, type = "evaluation_log")
  expect_equal(colnames(ret), c("iter", "type", "name", "value"))
  expect_equal(sort(unique(ret$type)), c("train", "validation"))
  expect_equal(unique(ret$name), "average_precision")
  expect_equal(nrow(ret), 20)
  expect_false(any(is.na(ret$value)))
  expect_true(all(ret$value > 0 & ret$value <= 1))
})

test_that("xgboost early stopping maximizes the custom Average Precision metric", {
  testthat::skip_if_not_installed("xgboost")
  set.seed(2)
  d <- make_pr_test_data(2000)
  # Mostly noise labels, so that the validation AP peaks early and then declines.
  d$y <- ifelse(runif(nrow(d)) < 0.35, !d$y, d$y)
  model_df <- d %>%
    exp_xgboost(y, x1, x2, x3,
                predictor_funs = list(x1 = "none", x2 = "none", x3 = "none"),
                nrounds = 200, early_stopping_rounds = 5, eta = 0.5, max_depth = 6,
                test_rate = 0, watchlist_rate = 0.2,
                importance_measure = "xgboost", max_pd_vars = 0, pd_with_bin_means = FALSE,
                eval_metric_binary = "average_precision")
  ret <- model_df %>% tidy_rowwise(model, type = "evaluation_log")
  val <- ret %>% filter(type == "validation") %>% arrange(iter)
  n_iter <- nrow(val)
  expect_lt(n_iter, 200) # Stopped early.
  best <- which.max(val$value)
  # Stops exactly early_stopping_rounds after the maximum (it would be after the minimum if it did not maximize).
  expect_equal(n_iter, best + 5)
})

test_that("xgboost evaluation_log names of the built-in metrics are unchanged", {
  testthat::skip_if_not_installed("xgboost")
  set.seed(1)
  d <- make_pr_test_data(1500)
  for (metric in c("auc", "logloss", "error", "aucpr")) {
    model_df <- d %>%
      exp_xgboost(y, x1, x2, x3,
                  predictor_funs = list(x1 = "none", x2 = "none", x3 = "none"),
                  nrounds = 5, test_rate = 0, watchlist_rate = 0.2,
                  importance_measure = "xgboost", max_pd_vars = 0, pd_with_bin_means = FALSE,
                  eval_metric_binary = metric)
    ret <- model_df %>% tidy_rowwise(model, type = "evaluation_log")
    expect_equal(unique(ret$name), metric, info = metric)
    expect_equal(sort(unique(ret$type)), c("train", "validation"), info = metric)
    expect_equal(nrow(ret), 10, info = metric)
  }
  # Regression and multiclass metrics.
  reg <- d %>% mutate(z = x1 * 2 + rnorm(n())) %>%
    exp_xgboost(z, x2, x3, predictor_funs = list(x2 = "none", x3 = "none"),
                nrounds = 5, test_rate = 0, watchlist_rate = 0.2,
                importance_measure = "xgboost", max_pd_vars = 0, pd_with_bin_means = FALSE)
  expect_equal(unique((reg %>% tidy_rowwise(model, type = "evaluation_log"))$name), "rmse")
  multi <- d %>% mutate(k = as.character(cut(x1, c(-Inf, -0.5, 0.5, Inf), labels = c("a", "b", "c")))) %>%
    exp_xgboost(k, x2, x3, predictor_funs = list(x2 = "none", x3 = "none"),
                nrounds = 5, test_rate = 0, watchlist_rate = 0.2,
                importance_measure = "xgboost", max_pd_vars = 0, pd_with_bin_means = FALSE)
  expect_equal(unique((multi %>% tidy_rowwise(model, type = "evaluation_log"))$name), "merror")
})

test_that("xgboost prettify names Average Precision and PR AUC", {
  df <- data.frame(iter = 1:2, train_average_precision = c(0.5, 0.6), validation_average_precision = c(0.4, 0.5),
                   train_aucpr = c(0.5, 0.6), validation_aucpr = c(0.4, 0.5))
  ret <- exploratory:::prettify_xgboost_evaluation_log(df, pretty.name = TRUE)
  expect_equal(colnames(ret), c("Number of Iteration", "Average Precision", "Validation Average Precision", "PR AUC", "Validation PR AUC"))
})

test_that("lightgbm custom PR AUC metric equals aupr on predictions", {
  testthat::skip_if_not_installed("lightgbm")
  set.seed(1)
  d <- make_pr_test_data()
  m <- as.matrix(d[, c("x1", "x2", "x3")])
  dtrain <- lightgbm::lgb.Dataset(m, label = as.numeric(d$y))
  fit <- lightgbm::lgb.train(
    params = list(objective = "binary", metric = "None", verbosity = -1),
    data = dtrain, nrounds = 5, valids = list(train = dtrain),
    eval = exploratory:::lgb_custom_binary_metric("pr_auc"), record = TRUE
  )
  res <- lightgbm::lgb.get.eval.result(fit, data_name = "train", eval_name = "pr_auc")
  expect_equal(length(res), 5)
  expect_equal(res[[5]], exploratory:::aupr(predict(fit, m), d$y))
})

test_that("exp_lightgbm(binary) learning curve with eval_metric_binary = pr_auc", {
  testthat::skip_if_not_installed("lightgbm")
  set.seed(1)
  d <- make_pr_test_data()
  model_df <- d %>%
    exp_lightgbm(y, x1, x2, x3,
                 predictor_funs = list(x1 = "none", x2 = "none", x3 = "none"),
                 nrounds = 10, test_rate = 0, watchlist_rate = 0.2,
                 importance_measure = "lightgbm", max_pd_vars = 0, pd_with_bin_means = FALSE,
                 eval_metric_binary = "pr_auc")
  ret <- model_df %>% tidy_rowwise(model, type = "evaluation_log")
  expect_equal(colnames(ret), c("Iter", "type", "name", "value"))
  expect_equal(sort(unique(ret$type)), c("train", "validation"))
  expect_equal(unique(ret$name), "pr_auc")
  expect_equal(nrow(ret), 20)
  expect_false(any(is.na(ret$value)))
  # tam filters the learning curve with this alias expansion.
  expect_equal(exploratory:::expand_lightgbm_metric_aliases("pr_auc"), "pr_auc")
})

test_that("lightgbm early stopping maximizes the custom PR AUC metric", {
  testthat::skip_if_not_installed("lightgbm")
  set.seed(2)
  d <- make_pr_test_data(2000)
  d$y <- ifelse(runif(nrow(d)) < 0.35, !d$y, d$y)
  model_df <- d %>%
    exp_lightgbm(y, x1, x2, x3,
                 predictor_funs = list(x1 = "none", x2 = "none", x3 = "none"),
                 nrounds = 300, early_stopping_rounds = 5, learning_rate = 0.3, num_leaves = 31,
                 test_rate = 0, watchlist_rate = 0.2,
                 importance_measure = "lightgbm", max_pd_vars = 0, pd_with_bin_means = FALSE,
                 eval_metric_binary = "pr_auc")
  ret <- model_df %>% tidy_rowwise(model, type = "evaluation_log")
  val <- ret %>% filter(type == "validation") %>% arrange(Iter)
  expect_lt(nrow(val), 300) # Stopped early.
  # Stops exactly early_stopping_rounds after the maximum (it would be after the minimum if it did not maximize).
  expect_equal(nrow(val), which.max(val$value) + 5)
})

test_that("lightgbm evaluation_log names of the built-in metrics are unchanged", {
  testthat::skip_if_not_installed("lightgbm")
  set.seed(1)
  d <- make_pr_test_data(1500)
  for (metric in c("auc", "binary_logloss", "average_precision")) {
    model_df <- d %>%
      exp_lightgbm(y, x1, x2, x3,
                   predictor_funs = list(x1 = "none", x2 = "none", x3 = "none"),
                   nrounds = 5, test_rate = 0, watchlist_rate = 0.2,
                   importance_measure = "lightgbm", max_pd_vars = 0, pd_with_bin_means = FALSE,
                   eval_metric_binary = metric)
    ret <- model_df %>% tidy_rowwise(model, type = "evaluation_log")
    expect_equal(unique(ret$name), metric, info = metric)
    expect_equal(nrow(ret), 10, info = metric)
  }
})
