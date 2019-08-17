context("logistic regression - handle failed model building")


test_that("logistic regression can handle failed model building", {
  # TODO: optimize performance
  df <- exploratory::read_delim_file("https://www.dropbox.com/s/nv3oxz7w9usnfu3/airline_2013_10.csv?dl=1" , ",", quote = "\"", skip = 0 , col_names = TRUE , na = c("","NA") , locale=readr::locale(encoding = "UTF-8", decimal_mark = "."), trim_ws = FALSE , progress = FALSE) %>%
    exploratory::clean_data_frame() %>%
    mutate(delayed = ARR_DELAY > 30)

  set.seed(0) # In this case, the models happens to be all NULL, which is a case we should handle.
  df1 <- df %>% sample_n(50000)
  
  filtered <- df1 %>%  filter(CARRIER %in% c("9E", "AA", "AS"))
  model_df <- filtered %>% dplyr::group_by(`CARRIER`) %>% build_lm.fast(`delayed`, `YEAR`, `MONTH`, `DAY_OF_MONTH`, `FL_DATE`, `FL_NUM`, `ORIGIN`, `ORIGIN_CITY_NAME`, `ORIGIN_STATE_ABR`, `DEST`, `DEST_CITY_NAME`, `DEST_STATE_ABR`, `DEP_TIME`, `DEP_DELAY`, `ARR_TIME`, `CANCELLED`, `CANCELLATION_CODE`, `AIR_TIME`, `DISTANCE`, `WEATHER_DELAY`, `X23`, model_type = "glm", smote = TRUE, variable_metric = "ame", with_marginal_effects_confint = FALSE, test_rate = 0.3)
  res <- model_df %>% prediction_training_and_test(prediction_type = 'conf_mat', threshold = 0.5)

  set.seed(1) # In this case, the first model happens to be NULL, but 2nd model is not, which is another case we should handle.
  df1 <- df %>% sample_n(50000)
  
  filtered <- df1 %>%  filter(CARRIER %in% c("9E", "AA", "AS"))
  model_df <- filtered %>% dplyr::group_by(`CARRIER`) %>% build_lm.fast(`delayed`, `YEAR`, `MONTH`, `DAY_OF_MONTH`, `FL_DATE`, `FL_NUM`, `ORIGIN`, `ORIGIN_CITY_NAME`, `ORIGIN_STATE_ABR`, `DEST`, `DEST_CITY_NAME`, `DEST_STATE_ABR`, `DEP_TIME`, `DEP_DELAY`, `ARR_TIME`, `CANCELLED`, `CANCELLATION_CODE`, `AIR_TIME`, `DISTANCE`, `WEATHER_DELAY`, `X23`, model_type = "glm", smote = TRUE, variable_metric = "ame", with_marginal_effects_confint = FALSE, test_rate = 0.3)
  res <- model_df %>% prediction_training_and_test(prediction_type = 'conf_mat', threshold = 0.5)
})
