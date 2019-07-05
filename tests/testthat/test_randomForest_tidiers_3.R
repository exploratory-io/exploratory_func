# how to run this test:
# devtools::test(filter="randomForest_tidiers_3")

context("test tidiers for randomForest 3 (training and test data)")

testdata_dir <- "~/.exploratory/"
testdata_filename <- "airline_2013_10_tricky_v3_5k.csv" 
testdata_file_path <- paste0(testdata_dir, testdata_filename)

filepath <- if (!testdata_filename %in% list.files(testdata_dir)) {
  "https://www.dropbox.com/s/f47baw5f3v0xoll/airline_2013_10_tricky_v3.csv?dl=1"
} else {
  testdata_file_path
}

flight <- exploratory::read_delim_file(filepath, ",", quote = "\"", skip = 0 , col_names = TRUE , na = c("","NA") , locale=readr::locale(encoding = "UTF-8", decimal_mark = "."), trim_ws = FALSE , progress = FALSE) %>% exploratory::clean_data_frame()

if (!testdata_filename %in% list.files(testdata_dir)) {
  flight <- flight %>% sample_n(5000)
  write.csv(flight, testdata_file_path) # save sampled-down data for performance.
}

test_that("calc_feature_map(regression) evaluate training and test", {
  model_df <- flight %>%
                calc_feature_imp(`ARR DELAY`, `CAR RIER`, `ORI GIN`, `DEP DELAY`, `AIR TIME`, test_rate = 0.3)

  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  expect_equal(nrow(test_ret), 1500)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  expect_equal(nrow(train_ret), 3500)

  ret <- rf_evaluation_training_and_test(model_df, pretty.name = TRUE)
  expect_equal(nrow(ret), 2) # 2 for train and test

  model_df <- flight %>%
                calc_feature_imp(`FL NUM`, `DIS TANCE`, `DEP TIME`, test_rate = 0)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  expect_equal(nrow(train_ret), 5000)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train
})

test_that("calc_feature_map(binary) evaluate training and test", {
  # `is delayed` is not logical for some reason.
  # To test binary prediction, need to cast it into logical.
  model_df <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`)) %>%
                calc_feature_imp(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0.3)

  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  expect_equal(nrow(test_ret), 1500)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  expect_equal(nrow(train_ret), 3500)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 2) # 2 for train and test

  ret <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class")
  expect_equal(nrow(ret), 4) # 4 for train/test times TRUE/FALSE

  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")

  model_df <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`)) %>%
                calc_feature_imp(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  expect_equal(nrow(train_ret), 5000)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train

  ret <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class")
  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")
})

test_that("calc_feature_map(multi) evaluate training and test", {
  model_df <- flight %>%
                calc_feature_imp(`ORI GIN`, `DIS TANCE`, `DEP TIME`, test_rate = 0.3)

  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  expect_equal(nrow(test_ret), 1500)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  expect_equal(nrow(train_ret), 3500)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 2) # 2 for train and test

  ret <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class")
  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")

  model_df <- flight %>%
                calc_feature_imp(`ORI GIN`, `DIS TANCE`, `DEP TIME`, test_rate = 0)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  expect_equal(nrow(train_ret), 5000)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train
})

