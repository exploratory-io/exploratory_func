# how to run this test:
# devtools::test(filter="randomForest_tidiers")

context("test rpart prediction with training/test data")

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
  set.seed(1)
  flight <- flight %>% sample_n(5000)
  write.csv(flight, testdata_file_path) # save sampled-down data for performance.
}


test_that("build_lm.fast (linear regression) evaluate training and test", {
  model_df <- flight %>%
                build_lm.fast(`FL NUM`, `DIS TANCE`, `DEP TIME`, test_rate = 0.3)

  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  expect_equal(nrow(test_ret), 1464)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  expect_equal(nrow(train_ret), 3418)
  ret <- model_df %>% evaluate_lm_training_and_test(pretty.name=TRUE)
  expect_equal(nrow(ret), 2) # 2 for train and test
})

test_that("build_lm.fast (logistic regression) evaluate training and test", {
  model_df <- flight %>%
                build_lm.fast(`is delayed`, `DIS TANCE`, `DEP TIME`, model_type = "glm", test_rate = 0.3)

  ret <- model_df %>% prediction_binary(data="training_and_test", threshold = 0.5)
  test_ret <- ret %>% filter(is_test_data==TRUE)
  expect_equal(nrow(test_ret), 1470)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  expect_equal(nrow(train_ret), 3433)
  ret <- model_df %>% evaluate_binary_training_and_test("is delayed", pretty.name=TRUE)
  expect_equal(nrow(ret), 2) # 2 for train and test
  ret <- model_df %>% prediction_training_and_test(prediction_type = 'conf_mat', threshold = 0.5)
})

test_that("build_lm.fast (gaussian regression) evaluate training and test", {
  model_df <- flight %>%
                build_lm.fast(`FL NUM`, `DIS TANCE`, `DEP TIME`, model_type = "glm", family = "gaussian", test_rate = 0.3)

  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  expect_equal(nrow(test_ret), 1464)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  expect_equal(nrow(train_ret), 3418)
  ret <- model_df %>% evaluate_lm_training_and_test(pretty.name=TRUE)
  expect_equal(nrow(ret), 2) # 2 for train and test
})

test_that("build_lm.fast (binomial regression) evaluate training and test", {
  model_df <- flight %>%
                build_lm.fast(`is delayed`, `DIS TANCE`, `DEP TIME`, model_type = "glm", family = "binomial", test_rate = 0.3)

  ret <- model_df %>% prediction_binary(data="training_and_test", threshold = 0.5)
  test_ret <- ret %>% filter(is_test_data==TRUE)
  expect_equal(nrow(test_ret), 1470)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  expect_equal(nrow(train_ret), 3433)
  ret <- model_df %>% evaluate_binary_training_and_test("is delayed", pretty.name=TRUE)
  expect_equal(nrow(ret), 2) # 2 for train and test
  ret <- model_df %>% prediction_training_and_test(prediction_type = 'conf_mat', threshold = 0.5)
})

