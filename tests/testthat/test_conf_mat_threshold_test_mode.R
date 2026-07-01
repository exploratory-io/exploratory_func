# how to run this test:
# devtools::test(filter="conf_mat_threshold_test_mode")

context("confusion matrix threshold sensitivity in test mode")

testdata_dir <- tempdir()
testdata_filename <- "airline_2013_10_tricky_v3_5k.csv"
testdata_file_path <- paste0(testdata_dir, '/', testdata_filename)

filepath <- if (!testdata_filename %in% list.files(testdata_dir)) {
  "https://exploratory-download.s3.us-west-2.amazonaws.com/test/airline_2013_10_tricky_v3.csv"
} else {
  testdata_file_path
}

flight <- exploratory::read_delim_file(
  filepath, ",", quote = "\"", skip = 0, col_names = TRUE, na = c("", "NA"),
  locale = readr::locale(encoding = "UTF-8", decimal_mark = "."),
  trim_ws = FALSE, progress = FALSE
) %>% exploratory::clean_data_frame()

if (!testdata_filename %in% list.files(testdata_dir)) {
  set.seed(1)
  flight <- flight %>% slice_sample(n = 5000)
  write.csv(flight, testdata_file_path)
}

expect_test_conf_mat_changes_with_threshold <- function(model_df, eval_fn,
                                                        threshold_high = 0.5,
                                                        threshold_low = 0.1) {
  cm_high <- eval_fn(model_df, threshold_high)
  cm_low <- eval_fn(model_df, threshold_low)

  test_high <- cm_high %>% dplyr::filter(is_test_data == TRUE)
  test_low <- cm_low %>% dplyr::filter(is_test_data == TRUE)

  expect_gt(nrow(test_high), 0)
  expect_gt(nrow(test_low), 0)
  expect_false(identical(test_high, test_low),
               info = "Test confusion matrix should change when threshold changes")
}

rf_conf_mat_eval <- function(model_df, threshold) {
  rf_evaluation_training_and_test(
    model_df,
    type = "conf_mat",
    binary_classification_threshold = threshold
  )
}

glm_conf_mat_eval <- function(model_df, threshold) {
  prediction_training_and_test(
    model_df,
    prediction_type = "conf_mat",
    threshold = threshold
  )
}

test_that("conf_mat test data respects threshold - Random Forest", {
  set.seed(1)
  df <- data.frame(
    y = sample(c(TRUE, FALSE), 300, replace = TRUE, prob = c(0.7, 0.3)),
    x1 = rnorm(300),
    x2 = rnorm(300)
  )
  model_df <- df %>% calc_feature_imp(y, x1, x2, test_rate = 0.3)
  expect_test_conf_mat_changes_with_threshold(model_df, rf_conf_mat_eval)
})

test_that("conf_mat test data respects threshold - Decision Tree (rpart)", {
  set.seed(1)
  df <- data.frame(
    y = sample(c(TRUE, FALSE), 500, replace = TRUE, prob = c(0.55, 0.45)),
    x1 = rnorm(500),
    x2 = rnorm(500),
    x3 = rnorm(500)
  )
  model_df <- df %>% exp_rpart(y, x1, x2, x3, test_rate = 0.3, binary_classification_threshold = 0.5)
  expect_test_conf_mat_changes_with_threshold(model_df, rf_conf_mat_eval)
})

test_that("conf_mat test data respects threshold - XGBoost", {
  set.seed(1)
  data <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`))
  model_df <- data %>% exp_xgboost(
    is_delayed, `DIS TANCE`, `DEP TIME`,
    predictor_funs = list(`DIS TANCE` = "none", `DEP TIME` = "none"),
    test_rate = 0.3,
    max_pd_vars = 0,
    pd_with_bin_means = FALSE
  )
  expect_test_conf_mat_changes_with_threshold(model_df, rf_conf_mat_eval)
})

test_that("conf_mat test data respects threshold - LightGBM", {
  skip_if_not_installed("lightgbm")
  set.seed(1)
  data <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`))
  model_df <- data %>% exp_lightgbm(
    is_delayed, `DIS TANCE`, `DEP TIME`,
    predictor_funs = list(`DIS TANCE` = "none", `DEP TIME` = "none"),
    test_rate = 0.3,
    max_pd_vars = 0,
    pd_with_bin_means = FALSE,
    importance_measure = "lightgbm"
  )
  expect_test_conf_mat_changes_with_threshold(model_df, rf_conf_mat_eval)
})

test_that("conf_mat test data respects threshold - CatBoost", {
  skip_if_not_installed("catboost")
  set.seed(1)
  df <- data.frame(
    y = rep(c(TRUE, FALSE), 150),
    num = rnorm(300),
    grp = factor(rep(letters[1:3], length.out = 300)),
    stringsAsFactors = FALSE
  )
  model_df <- df %>% exp_catboost(
    y, num, grp,
    iterations = 10,
    depth = 3,
    learning_rate = 0.2,
    test_rate = 0.3,
    max_pd_vars = 0,
    eval_metric_binary = "Logloss"
  )
  expect_test_conf_mat_changes_with_threshold(model_df, rf_conf_mat_eval)
})

test_that("conf_mat test data respects threshold - Logistic Regression", {
  set.seed(1)
  model_df <- flight %>%
    build_lm.fast(
      `is delayed`, `DIS TANCE`, `DEP TIME`,
      model_type = "glm",
      test_rate = 0.3
    )
  expect_test_conf_mat_changes_with_threshold(model_df, glm_conf_mat_eval)
})

test_that("conf_mat test data respects threshold - Binomial GLM", {
  set.seed(1)
  model_df <- flight %>%
    build_lm.fast(
      `is delayed`, `DIS TANCE`, `DEP TIME`,
      model_type = "glm",
      family = "binomial",
      test_rate = 0.3
    )
  expect_test_conf_mat_changes_with_threshold(model_df, glm_conf_mat_eval)
})

test_that("rf test conf_mat matches threshold-aware labels at same threshold", {
  set.seed(1)
  df <- data.frame(
    y = sample(c(TRUE, FALSE), 300, replace = TRUE, prob = c(0.7, 0.3)),
    x1 = rnorm(300),
    x2 = rnorm(300)
  )
  model_df <- df %>% calc_feature_imp(y, x1, x2, test_rate = 0.3)
  threshold <- 0.3

  pred <- prediction(model_df, data = "test")
  actual_col <- model_df$model[[1]]$terms_mapping[
    all.vars(model_df$model[[1]]$formula_terms)[1]
  ]
  actual <- pred[[actual_col]]
  predicted <- get_test_predicted_labels(model_df$model[[1]], threshold)
  manual <- calc_conf_mat(actual, predicted)

  cm <- rf_evaluation_training_and_test(
    model_df,
    type = "conf_mat",
    binary_classification_threshold = threshold
  )
  test_cm <- cm %>% dplyr::filter(is_test_data == TRUE) %>%
    dplyr::select(actual_value, predicted_value, count) %>%
    dplyr::arrange(actual_value, predicted_value)
  manual <- manual %>% dplyr::arrange(actual_value, predicted_value)

  expect_equal(test_cm, manual)
})
