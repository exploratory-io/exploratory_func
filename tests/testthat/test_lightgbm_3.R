context("lightgbm negative test case")

testthat::skip_if_not_installed("lightgbm")

df <- exploratory::read_excel_file("https://www.dropbox.com/s/y08x4y7dlavwp9g/Global_Sales.xlsx?dl=1", sheet = "Sheet 1", na = c("", "NA"), skip = 0, col_names = TRUE, trim_ws = TRUE, tzone = "America/Los_Angeles") %>%
  readr::type_convert() %>%
  exploratory::clean_data_frame()

test_that("exp_lightgbm - Failed variable importance for a group", {
  set.seed(1)
  df <- df %>% sample_rows(1000)

  df <- df %>%
    dplyr::mutate(`Market` = forcats::fct_lump(factor(`Market`), n = 20, other_level = "Others", ties.method = "first")) %>%
    dplyr::group_by(`Market`)

  model_df <- df %>% exp_lightgbm(`Returned`, `Quantity`, `Category`, `Segment`, nrounds = 20, sparse = FALSE, output_type_regression = "regression", eval_metric_regression = "rmse", output_type_binary = "probability", eval_metric_binary = "auc", output_type_multiclass = "softprob", eval_metric_multiclass = "multi_error", smote = FALSE, pd_with_bin_means = TRUE, test_split_type = "random")
  res <- model_df %>% rf_evaluation_training_and_test(type = "evaluation", pretty.name = TRUE)
  res <- model_df %>% rf_importance()
  expect_true("data.frame" %in% class(res))
})

test_that("exp_lightgbm - Early stop test", {
  model_df <- df %>% exp_lightgbm(`Market`, `Quantity`, `Category`, `Segment`,
    nrounds = 50,
    sparse = FALSE,
    early_stopping_rounds = 1,
    watchlist_rate = 0.1,
    # Encourage early stop deterministically
    num_leaves = 2,
    min_data_in_leaf = 5000,
    learning_rate = 0.2,
    output_type_regression = "regression", eval_metric_regression = "rmse",
    output_type_binary = "probability", eval_metric_binary = "auc",
    output_type_multiclass = "softprob", eval_metric_multiclass = "multi_error",
    smote = FALSE, pd_with_bin_means = TRUE, test_split_type = "random"
  )
  ret <- model_df %>% tidy_rowwise(model, type = "evaluation_log")
  expect_true(max(ret$iter) < 50)
})


