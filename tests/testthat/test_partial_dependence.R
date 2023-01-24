
context("Test partial dependence")

aq_data <- airquality %>%
  readr::type_convert() %>%
  exploratory::clean_data_frame() %>%
  mutate(Ozone_category = format_cut_output(exp_cut(Ozone, breaks = 10, lower.range = NA, upper.range = NA, include.outside.range = TRUE, dig.lab = 10), decimal.digits = 0), .after = ifelse("Ozone" %in% names(.), "Ozone", last_col()))
aq_data <- aq_data %>% mutate(Ozone_char = as.character(Ozone_category))
aq_data <- aq_data %>% mutate(Temp_Over_80 = Temp > 80)

cancer_data <- survival::cancer %>%
  readr::type_convert() %>%
  exploratory::clean_data_frame() %>%
  mutate(Start_Date = as.Date("2020-01-01")) %>%
  mutate(End_Date = Start_Date + lubridate::days(time)) %>%
  mutate(status = str_logical(status-1)) %>%
  mutate(sex_category = factor(case_when(sex == 1 ~ "Male" , TRUE ~ "Female"), levels=c("Female","Male"))) %>%
  mutate(sex_char = as.character(sex_category))

test_that("Test partial dependence by character predictor with random forest", {
  model_df <- aq_data %>% calc_feature_imp(`Temp`, `Ozone_char`, target_fun = "none", predictor_funs = list(`Ozone_char`="none"), smote = FALSE, importance_measure = "permutation", pd_with_bin_means = TRUE, test_split_type = "random", test_rate = 0.1)
  res <- model_df %>% rf_partial_dependence()
  predicted_df <- res %>% filter(y_name=="Predicted")
  expect_equal(sort(predicted_df$y_value, decreasing = TRUE), predicted_df$y_value)
})

test_that("Test partial dependence by factor predictor with random forest", {
  model_df <- aq_data %>% calc_feature_imp(`Temp`, `Ozone_category`, target_fun = "none", predictor_funs = list(`Ozone_char`="none"), smote = FALSE, importance_measure = "permutation", pd_with_bin_means = TRUE, test_split_type = "random", test_rate = 0.1)
  res <- model_df %>% rf_partial_dependence()
  predicted_df <- res %>% filter(y_name=="Predicted")
  # To verify it is sorted according to the original factor level, check the first level comes first and the last level comes last.
  expect_true(predicted_df$x_value[1] == "1 - 18")
  expect_true(predicted_df$x_value[length(predicted_df$x_value)] == "(Missing)")
})

test_that("Test partial dependence by character predictor with xgboost", {
  model_df <- aq_data %>% exp_xgboost(`Temp`, `Ozone_char`, target_fun = "none", predictor_funs = list(`Ozone_char`="none"), smote = FALSE, importance_measure = "permutation", pd_with_bin_means = TRUE, test_split_type = "random", test_rate = 0.1)
  res <- model_df %>% rf_partial_dependence()
  predicted_df <- res %>% filter(y_name=="Predicted")
  expect_equal(sort(predicted_df$y_value, decreasing = TRUE), predicted_df$y_value)
})

test_that("Test partial dependence by factor predictor with xgboost", {
  model_df <- aq_data %>% exp_xgboost(`Temp`, `Ozone_category`, target_fun = "none", predictor_funs = list(`Ozone_char`="none"), smote = FALSE, importance_measure = "permutation", pd_with_bin_means = TRUE, test_split_type = "random", test_rate = 0.1)
  res <- model_df %>% rf_partial_dependence()
  predicted_df <- res %>% filter(y_name=="Predicted")
  # To verify it is sorted according to the original factor level, check the first level comes first and the last level comes last.
  expect_true(predicted_df$x_value[1] == "1 - 18")
  expect_true(predicted_df$x_value[length(predicted_df$x_value)] == "(Missing)")
})

test_that("Test partial dependence by character predictor with rpart", {
  model_df <- aq_data %>% exp_rpart(`Temp`, `Ozone_char`, target_fun = "none", predictor_funs = list(`Ozone_char`="none"), smote = FALSE, importance_measure = "permutation", pd_with_bin_means = TRUE, test_split_type = "random", test_rate = 0.1)
  res <- model_df %>% rf_partial_dependence()
  predicted_df <- res %>% filter(y_name=="Predicted")
  expect_equal(sort(predicted_df$y_value, decreasing = TRUE), predicted_df$y_value)
})

test_that("Test partial dependence by factor predictor with rpart", {
  model_df <- aq_data %>% exp_rpart(`Temp`, `Ozone_category`, target_fun = "none", predictor_funs = list(`Ozone_char`="none"), smote = FALSE, importance_measure = "permutation", pd_with_bin_means = TRUE, test_split_type = "random", test_rate = 0.1)
  res <- model_df %>% rf_partial_dependence()
  predicted_df <- res %>% filter(y_name=="Predicted")
  # To verify it is sorted according to the original factor level, check the first level comes first and the last level comes last.
  expect_true(predicted_df$x_value[1] == "1 - 18")
  expect_true(predicted_df$x_value[length(predicted_df$x_value)] == "(Missing)")
})

test_that("Test partial dependence by character predictor with rpart", {
  model_df <- aq_data %>% build_lm.fast(`Temp`, `Ozone_char`, target_fun = "none", predictor_funs = list(`Ozone_char`="none"), model_type = "lm", importance_measure = "permutation", test_split_type = "random", test_rate = 0.1)
  res <- model_df %>% lm_partial_dependence()
  predicted_df <- res %>% filter(y_name=="Predicted")
  expect_equal(sort(predicted_df$y_value, decreasing = TRUE), predicted_df$y_value)
})

test_that("Test partial dependence by factor predictor with rpart", {
  model_df <- aq_data %>% build_lm.fast(`Temp`, `Ozone_category`, target_fun = "none", predictor_funs = list(`Ozone_char`="none"), model_type = "lm", importance_measure = "permutation", test_split_type = "random", test_rate = 0.1)
  res <- model_df %>% lm_partial_dependence()
  predicted_df <- res %>% filter(y_name=="Predicted")
  # To verify it is sorted according to the original factor level, check the first level comes first and the last level comes last.
  expect_true(predicted_df$x_value[1] == "1 - 18")
  expect_true(predicted_df$x_value[length(predicted_df$x_value)] == "(Missing)")
})

test_that("Test partial dependence by character predictor with Cox regression", {
  model_df <- cancer_data %>% build_coxph.fast(NULL, `status`, `age`, `sex_char`, predictor_funs = list(`age`="none", `sex`="none"), start_time = `Start_Date`, end_time = `End_Date`, time_unit = "day", test_split_type = "random")
  res <- model_df %>% tidy_rowwise(model, type='partial_dependence')
  predicted_df <- res %>% filter(type=="Prediction")
  expect_true(predicted_df$value[1] == "Male")
})

test_that("Test partial dependence by factor predictor with Cox regression", {
  model_df <- cancer_data %>% build_coxph.fast(NULL, `status`, `age`, `sex_category`, predictor_funs = list(`age`="none", `sex`="none"), start_time = `Start_Date`, end_time = `End_Date`, time_unit = "day", test_split_type = "random")
  res <- model_df %>% tidy_rowwise(model, type='partial_dependence')
  predicted_df <- res %>% filter(type=="Prediction")
  expect_true(predicted_df$value[1] == "Female")
})

test_that("Test partial dependence by character predictor with Cox regression", {
  model_df <- cancer_data %>% exp_survival_forest(NULL, `status`, `age`, `sex_char`, predictor_funs = list(`age`="none", `sex`="none"), start_time = `Start_Date`, end_time = `End_Date`, time_unit = "day", test_split_type = "random")
  res <- model_df %>% tidy_rowwise(model, type='partial_dependence')
  predicted_df <- res %>% filter(type=="Prediction")
  expect_true(predicted_df$value[1] == "Male")
})

test_that("Test partial dependence by factor predictor with Cox regression", {
  model_df <- cancer_data %>% exp_survival_forest(NULL, `status`, `age`, `sex_category`, predictor_funs = list(`age`="none", `sex`="none"), start_time = `Start_Date`, end_time = `End_Date`, time_unit = "day", test_split_type = "random")
  res <- model_df %>% tidy_rowwise(model, type='partial_dependence')
  predicted_df <- res %>% filter(type=="Prediction")
  expect_true(predicted_df$value[1] == "Female")
})

