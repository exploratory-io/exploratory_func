# how to run this test:
# devtools::test(filter="stratified_test_split")
#
# tam#39809: test_split_type = "stratified" splits Test Mode data so that the target
# (or the event status for survival models) keeps the same share in the test set as in the data.

context("test stratified train/test split in Test Mode")

library(dplyr)

N_ROWS <- 500
RARE_ROWS <- 25     # 5% TRUE rows
TEST_RATE <- 0.4    # 200 test rows, of which 10 should be TRUE when stratified.
EXPECTED_RARE_IN_TEST <- RARE_ROWS * TEST_RATE

# Shuffled data frame with a rare logical target, a numeric predictor and a factor predictor.
make_imbalanced_logical_df <- function() {
  set.seed(11)
  df <- data.frame(
    y = c(rep(TRUE, RARE_ROWS), rep(FALSE, N_ROWS - RARE_ROWS)),
    x1 = rnorm(N_ROWS),
    x2 = factor(sample(c("a", "b", "c"), N_ROWS, TRUE)),
    stringsAsFactors = FALSE
  )
  df[sample(N_ROWS), ]
}

# Number of rows of a model's test data where the (original-named) target column is TRUE.
rare_in_test <- function(model_df, target = "y") {
  source_data <- model_df$source.data[[1]]
  # Some models turn a logical target into a factor, so compare as character.
  sum(as.character(source_data[[target]][model_df$.test_index[[1]]]) == "TRUE")
}

# Count of each expected category in a test data target column, in the order of `expected`.
category_counts <- function(test_target, expected) {
  as.numeric(table(factor(as.character(test_target), levels = names(expected))))
}

test_that("exp_rpart stratified split keeps the rare TRUE share in the test set", {
  df <- make_imbalanced_logical_df()
  model_df <- suppressWarnings(exp_rpart(df, y, x1, x2, test_rate = TEST_RATE, test_split_type = "stratified"))
  expect_equal(length(model_df$.test_index[[1]]), N_ROWS * TEST_RATE)
  expect_equal(rare_in_test(model_df), EXPECTED_RARE_IN_TEST)
  # Negative control: the plain random split with the same seed does not hit the exact share.
  random_df <- suppressWarnings(exp_rpart(df, y, x1, x2, test_rate = TEST_RATE, test_split_type = "random"))
  expect_false(rare_in_test(random_df) == EXPECTED_RARE_IN_TEST)
})

test_that("calc_feature_imp stratified split keeps the rare TRUE share in the test set", {
  df <- make_imbalanced_logical_df()
  model_df <- suppressWarnings(calc_feature_imp(df, y, x1, x2, test_rate = TEST_RATE, test_split_type = "stratified"))
  expect_equal(rare_in_test(model_df), EXPECTED_RARE_IN_TEST)
})

test_that("exp_chaid stratified split keeps the rare TRUE share in the test set", {
  df <- make_imbalanced_logical_df()
  model_df <- suppressWarnings(exp_chaid(df, y, x2, test_rate = TEST_RATE, test_split_type = "stratified"))
  expect_equal(rare_in_test(model_df), EXPECTED_RARE_IN_TEST)
})

test_that("exp_xgboost stratified split keeps the rare TRUE share in the test set", {
  df <- make_imbalanced_logical_df()
  model_df <- suppressWarnings(exp_xgboost(df, y, x1, x2, nrounds = 3, test_rate = TEST_RATE, test_split_type = "stratified"))
  expect_equal(rare_in_test(model_df), EXPECTED_RARE_IN_TEST)
})

test_that("build_lm.fast glm stratified split keeps the rare TRUE share in the test set", {
  df <- make_imbalanced_logical_df()
  model_df <- suppressWarnings(build_lm.fast(df, y, x1, x2, model_type = "glm", family = "binomial",
                                             test_rate = TEST_RATE, test_split_type = "stratified"))
  # build_lm.fast renames columns to c1_, c2_, ...; the target is the first one.
  expect_equal(rare_in_test(model_df, "c1_"), EXPECTED_RARE_IN_TEST)
})

test_that("build_lm.fast lm stratified split spreads the test rows evenly over target quantile bins", {
  set.seed(11)
  df <- data.frame(y = c(rexp(N_ROWS - 5), 50, 60, 70, 80, 90), x1 = rnorm(N_ROWS))
  df <- df[sample(N_ROWS), ]
  model_df <- suppressWarnings(build_lm.fast(df, y, x1, test_rate = TEST_RATE, test_split_type = "stratified"))
  source_data <- model_df$source.data[[1]]
  bins <- dplyr::ntile(source_data$c1_, 5)
  expect_equal(as.numeric(table(bins[model_df$.test_index[[1]]])), rep(N_ROWS * TEST_RATE / 5, 5))
})

test_that("build_polr and build_multinom_logit stratified split keep the target category shares", {
  set.seed(11)
  y <- c(rep("hi", 350), rep("mid", 100), rep("lo", 50))
  df <- data.frame(
    y = factor(y, levels = c("lo", "mid", "hi"), ordered = TRUE),
    x1 = rnorm(N_ROWS),
    x2 = factor(sample(c("a", "b", "c"), N_ROWS, TRUE))
  )
  df <- df[sample(N_ROWS), ]
  expected <- c(lo = 20, mid = 40, hi = 140)

  polr_df <- suppressWarnings(build_polr(df, y, x1, x2, test_rate = TEST_RATE, test_split_type = "stratified"))
  expect_equal(category_counts(polr_df$.test_data[[1]]$y, expected), unname(expected))

  df$y <- factor(as.character(df$y)) # multinom takes an unordered factor.
  multinom_df <- suppressWarnings(build_multinom_logit(df, y, x1, x2, test_rate = TEST_RATE, test_split_type = "stratified"))
  expect_equal(category_counts(multinom_df$.test_data[[1]]$y, expected), unname(expected))
})

test_that("survival models stratify the test set on the event status", {
  set.seed(11)
  df <- data.frame(
    time = rexp(N_ROWS, 0.1),
    status = c(rep(TRUE, 50), rep(FALSE, N_ROWS - 50)), # 10% events
    x1 = rnorm(N_ROWS),
    x2 = rnorm(N_ROWS)
  )
  df <- df[sample(N_ROWS), ]
  expected_events <- 50 * TEST_RATE

  cox_df <- suppressWarnings(build_coxph.fast(df, time, status, x1, x2, test_rate = TEST_RATE, test_split_type = "stratified"))
  cox_test <- suppressWarnings(augment_rowwise(cox_df, model, data_type = "test"))
  expect_equal(nrow(cox_test), N_ROWS * TEST_RATE)
  expect_equal(sum(cox_test$status), expected_events)

  forest_df <- suppressWarnings(exp_survival_forest(df, time, status, x1, x2, test_rate = TEST_RATE, test_split_type = "stratified"))
  forest_test <- suppressWarnings(augment_rowwise(forest_df, model, data_type = "test"))
  expect_equal(nrow(forest_test), N_ROWS * TEST_RATE)
  expect_equal(sum(forest_test$status), expected_events)
})

test_that("stratified split works per group (Repeat By)", {
  df <- bind_rows(make_imbalanced_logical_df() %>% mutate(grp = "g1"),
                  make_imbalanced_logical_df() %>% mutate(grp = "g2"))
  model_df <- suppressWarnings(df %>% dplyr::group_by(grp) %>%
    exp_rpart(y, x1, x2, test_rate = TEST_RATE, test_split_type = "stratified"))
  expect_equal(nrow(model_df), 2)
  for (i in seq_len(nrow(model_df))) {
    expect_equal(sum(as.character(model_df$source.data[[i]]$y[model_df$.test_index[[i]]]) == "TRUE"), EXPECTED_RARE_IN_TEST)
  }
})

test_that("every function that takes test_split_type rejects an unknown value", {
  df <- make_imbalanced_logical_df()
  dfo <- df %>% mutate(o = factor(sample(c("a", "b", "c"), n(), TRUE), ordered = TRUE))
  dfs <- df %>% mutate(time = rexp(n()), status = y)
  msg <- "test_split_type must be one of"
  expect_error(exp_rpart(df, y, x1, x2, test_rate = 0.3, test_split_type = "stratfied"), msg)
  expect_error(calc_feature_imp(df, y, x1, x2, test_rate = 0.3, test_split_type = "bad"), msg)
  expect_error(exp_xgboost(df, y, x1, x2, test_rate = 0.3, test_split_type = "bad"), msg)
  expect_error(build_lm.fast(df, y, x1, x2, test_rate = 0.3, test_split_type = "bad"), msg)
  expect_error(build_polr(dfo, o, x1, x2, test_rate = 0.3, test_split_type = "bad"), msg)
  expect_error(build_multinom_logit(dfo, o, x1, x2, test_rate = 0.3, test_split_type = "bad"), msg)
  expect_error(build_coxph.fast(dfs, time, status, x1, x2, test_rate = 0.3, test_split_type = "bad"), msg)
  expect_error(exp_survival_forest(dfs, time, status, x1, x2, test_rate = 0.3, test_split_type = "bad"), msg)
  expect_error(exp_chaid(df, y, x2, test_rate = 0.3, test_split_type = "bad"), "should be one of")
  if (requireNamespace("lightgbm", quietly = TRUE)) {
    expect_error(exp_lightgbm(df, y, x1, x2, test_rate = 0.3, test_split_type = "bad"), msg)
  }
  if (requireNamespace("catboost", quietly = TRUE)) {
    expect_error(exp_catboost(df, y, x1, x2, test_rate = 0.3, test_split_type = "bad"), msg)
  }
})

if (requireNamespace("lightgbm", quietly = TRUE)) {
  test_that("exp_lightgbm stratified split keeps the rare TRUE share in the test set", {
    df <- make_imbalanced_logical_df()
    model_df <- suppressWarnings(exp_lightgbm(df, y, x1, x2, nrounds = 3, test_rate = TEST_RATE, test_split_type = "stratified"))
    expect_equal(rare_in_test(model_df), EXPECTED_RARE_IN_TEST)
  })
}

if (requireNamespace("catboost", quietly = TRUE)) {
  test_that("exp_catboost stratified split keeps the rare TRUE share in the test set", {
    df <- make_imbalanced_logical_df()
    model_df <- suppressWarnings(exp_catboost(df, y, x1, x2, nrounds = 3, test_rate = TEST_RATE, test_split_type = "stratified"))
    expect_equal(rare_in_test(model_df), EXPECTED_RARE_IN_TEST)
  })
}
