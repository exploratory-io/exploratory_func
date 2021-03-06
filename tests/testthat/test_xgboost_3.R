context("xgboost negative test case")

df <- exploratory::read_excel_file( "https://www.dropbox.com/s/y08x4y7dlavwp9g/Global_Sales.xlsx?dl=1", sheet = "Sheet 1", na = c('','NA'), skip=0, col_names=TRUE, trim_ws=TRUE, tzone='America/Los_Angeles') %>%
  readr::type_convert() %>%
  exploratory::clean_data_frame()

test_that("exp_xgboost - Failed variable importance for a group", {
  # For Africa, variable importance throws error. This test tests how it is handled.
  set.seed(1)
  df <- df %>% sample_rows(1000)
  
  df <- df %>% dplyr::mutate(`Market` = forcats::fct_lump(factor(`Market`), n=20, other_level = "Others", ties.method ="first")) %>% 
    dplyr::group_by(`Market`)
  
  model_df <- df %>% exp_xgboost(`Returned`, `Quantity`, `Category`, `Segment`, nrounds = 20, sparse = FALSE, booster = "gbtree", output_type_regression = "linear", eval_metric_regression = "rmse", output_type_binary = "logistic", eval_metric_binary = "auc", output_type_multiclass = "softprob", eval_metric_multiclass = "merror", smote = FALSE, pd_with_bin_means = TRUE, test_split_type = "random")
  res <- model_df %>% rf_evaluation_training_and_test(type='evaluation', pretty.name = TRUE)
  res <- model_df %>% rf_importance()
  expect_true("data.frame" %in% class(res))
})

test_that("exp_xgboost - Early stop test", {
  model_df <- df %>% exp_xgboost(`Market`, `Quantity`, `Category`, `Segment`, nrounds = 50, sparse = FALSE, booster = "gbtree", early_stopping_rounds = 1, output_type_regression = "linear", eval_metric_regression = "rmse", output_type_binary = "logistic", eval_metric_binary = "auc", output_type_multiclass = "softprob", eval_metric_multiclass = "merror", smote = FALSE, pd_with_bin_means = TRUE, test_split_type = "random", watchlist_rate = 0.1)
  ret <- model_df %>% tidy_rowwise(model, type="evaluation_log")
  expect_true(max(ret$iter) < 50) # Early stop should stop iterations before reaching the max iterations (nrounds).
})

