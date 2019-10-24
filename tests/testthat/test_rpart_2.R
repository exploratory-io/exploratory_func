# how to run this test:
# devtools::test(filter="rpart_2")

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

filepath <- if (!testdata_filename %in% list.files(testdata_dir)) {
  flight <- flight %>% sample_n(5000)
  write.csv(flight, testdata_file_path) # save sampled-down data for performance.
}

test_that("exp_rpart(regression) evaluate training and test", {
  model_df <- flight %>%
                exp_rpart(`FL NUM`, `DIS TANCE`, `DEP TIME`,
                          test_rate = 0.3,
                          test_split_type = "ordered") # testing ordered split too.

  ret <-  model_df %>% rf_partial_dependence()

  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1483) # Not very stable for some reason. Will revisit.
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3461) # Not very stable for some reason. Will revisit.

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 2) # 2 for train and test

  # Training only case
  model_df <- flight %>%
                exp_rpart(`FL NUM`, `DIS TANCE`, `DEP TIME`, test_rate = 0)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 4944) # Not very stable for some reason. Will revisit.

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train
})

test_that("exp_rpart(binary) evaluate training and test", {
  model_df <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`)) %>%
                exp_rpart(is_delayed, `DIS TANCE`, `DEP DELAY`, test_rate = 0.3, binary_classification_threshold=0.5)

  ret <-  model_df %>% rf_partial_dependence()

  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1483) # Not very stable for some reason. Will revisit.
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3461) # Not very stable for some reason. Will revisit.

  ret <- model_df %>% rf_evaluation_training_and_test()
  expect_equal(nrow(ret), 2) # 2 for train and test

  # Training only case
  model_df <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`)) %>%
                exp_rpart(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 4944) # Not very stable for some reason. Will revisit.

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train
})

test_that("exp_rpart(binary) evaluate training and test with SMOTE", {
  model_df <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`)) %>%
                exp_rpart(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0.3, smote = T)

  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1483) # Not very stable for some reason. Will revisit.
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3461) # Not very stable for some reason. Will revisit.

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 2) # 2 for train and test

  # Training only case
  model_df <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`)) %>%
                exp_rpart(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0, smote = T)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 4944) # Not very stable for some reason. Will revisit.

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train
})

test_that("exp_rpart(multi) evaluate training and test", {
  model_df <- flight %>%
                exp_rpart(`ORI GIN`, `DIS TANCE`, `DEP TIME`, test_rate = 0.3)

  ret <-  model_df %>% rf_partial_dependence()

  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1483) # Not very stable for some reason. Will revisit.
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3461) # Not very stable for some reason. Will revisit.

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 2) # 2 for train and test

  # Training only case
  model_df <- flight %>%
                exp_rpart(`ORI GIN`, `DIS TANCE`, `DEP TIME`, test_rate = 0)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 4944) # Not very stable for some reason. Will revisit.

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train
})
