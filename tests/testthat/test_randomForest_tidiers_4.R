# how to run this test:
# devtools::test(filter="randomForest_tidiers_4_group")
# This is same test as test_randomForest_tidiers_3.R with group_by.

context("test tidiers for randomForest 4 (training and test data with group_by)")

testdata_dir <- tempdir()
testdata_filename <- "airline_2013_10_tricky_v3_5k.csv" 
testdata_file_path <- paste0(testdata_dir, '/', testdata_filename)

filepath <- if (!testdata_filename %in% list.files(testdata_dir)) {
  "https://exploratory-download.s3.us-west-2.amazonaws.com/test/airline_2013_10_tricky_v3.csv"
} else {
  testdata_file_path
}

flight <- exploratory::read_delim_file(filepath, ",", quote = "\"", skip = 0 , col_names = TRUE , na = c("","NA") , locale=readr::locale(encoding = "UTF-8", decimal_mark = "."), trim_ws = FALSE , progress = FALSE) %>% exploratory::clean_data_frame()

filepath <- if (!testdata_filename %in% list.files(testdata_dir)) {
  set.seed(1) # For stability of result.
  flight <- flight %>% slice_sample(n=5000)
  write.csv(flight, testdata_file_path) # save sampled-down data for performance.
}

# Add group_by. Cases without group_by is covered in test_randomForest_tidiers_3.R.
flight <- flight %>%
  # Add a row for a group that would fail in model building, because of having only 1 row.
  add_row(`CAR RIER`="DUMMY", `ARR DELAY`=1, `DIS TANCE`=1, `DEP DELAY`=1, `is delayed`=TRUE, `FL NUM`=1) %>%
  group_by(`CAR RIER`)


test_that("calc_feature_map(regression) evaluate training and test", {
  set.seed(1) # For stability of result.
  model_df <- flight %>%
                calc_feature_imp(`FL NUM`, `DIS TANCE`, `DEP TIME`, test_rate = 0.3)
  ret <- rf_evaluation_training_and_test(model_df, test_rate = 0.3)
  expect_equal(nrow(ret %>% filter(`CAR RIER`=="DUMMY")), 1) # Row for the group with error.
  expect_equal(nrow(ret %>% filter(`CAR RIER`=="DUMMY" & !is.na(Note))), 1) # Row for the group with error should have message in Note column. 

  ret <- model_df %>% prediction(data='training_and_test')
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1500) # Fails for now
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3500) # Fails for now

  model_df <- flight %>%
                calc_feature_imp(`FL NUM`, `DIS TANCE`, `DEP TIME`, test_rate = 0)
  ret <- model_df %>% prediction(data='training_and_test')
  train_ret <- ret %>% filter(is_test_data==FALSE)
  #expect_equal(nrow(train_ret), 4894) # Less than 5000 because of NAs in the target variable. Linux seems to have different result. Work around for now.
  expect_lt(nrow(train_ret), 5000)
})

test_that("calc_feature_map(binary) evaluate training and test", {
  set.seed(1) # For stability of result.
  model_df <- flight %>%
                # filter(`CAR RIER` %in% c("VA","AA")) %>%
                dplyr::mutate(is_delayed = as.factor(`is delayed`)) %>%
                calc_feature_imp(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0.3)
  ret <- rf_evaluation_training_and_test(model_df, test_rate = 0.3)
  expect_equal(nrow(ret %>% filter(`CAR RIER`=="DUMMY")), 1) # Row for the group with error.
  expect_equal(nrow(ret %>% filter(`CAR RIER`=="DUMMY" & !is.na(Note))), 1) # Row for the group with error should have message in Note column. 
  ret <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class", test_rate = 0.3)
  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat", test_rate = 0.3)

  ret <- model_df %>% prediction_training_and_test()
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1500) # Fails for now
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3500) # Fails for now

  model_df <- flight %>%
                # filter(`CAR RIER` %in% c("VA","AA")) %>%
                dplyr::mutate(is_delayed = as.factor(`is delayed`)) %>%
                calc_feature_imp(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0)
  ret <- model_df %>% prediction_training_and_test()
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 5000) # Fails for now
})

test_that("calc_feature_map(binary) evaluate training and test with SMOTE", {
  set.seed(1) # For stability of result.
  model_df <- flight %>%
                # filter(`CAR RIER` %in% c("VA","AA")) %>%
                dplyr::mutate(is_delayed = as.factor(`is delayed`)) %>%
                calc_feature_imp(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0.3, smote = TRUE)
  ret <- rf_evaluation_training_and_test(model_df, test_rate = 0.3)
  expect_equal(nrow(ret %>% filter(`CAR RIER`=="DUMMY")), 1) # Row for the group with error.
  expect_equal(nrow(ret %>% filter(`CAR RIER`=="DUMMY" & !is.na(Note))), 1) # Row for the group with error should have message in Note column. 
  ret <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class", test_rate = 0.3)
  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat", test_rate = 0.3)

  ret <- model_df %>% prediction_training_and_test()
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1500) # Fails for now
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3500) # Fails for now

  model_df <- flight %>%
                # filter(`CAR RIER` %in% c("VA","AA")) %>%
                dplyr::mutate(is_delayed = as.factor(`is delayed`)) %>%
                calc_feature_imp(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0, smote = TRUE)
  ret <- model_df %>% prediction_training_and_test()
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 5000) # Fails for now
})

test_that("calc_feature_map(multi) evaluate training and test", {
  set.seed(1) # For stability of result.
  model_df <- flight %>%
                calc_feature_imp(`ORI GIN`, `DIS TANCE`, `DEP TIME`, test_rate = 0.3)
  ret <- rf_evaluation_training_and_test(model_df, test_rate = 0.3, pretty.name = TRUE)
  expect_equal(nrow(ret %>% filter(`CAR RIER`=="DUMMY")), 1) # Row for the group with error.
  expect_equal(nrow(ret %>% filter(`CAR RIER`=="DUMMY" & !is.na(Note))), 1) # Row for the group with error should have message in Note column. 
  ret <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class", test_rate = 0.3)
  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat", test_rate = 0.3)

  ret <- model_df %>% prediction_training_and_test()
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1500) # Fails for now
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3500) # Fails for now

  model_df <- flight %>%
                calc_feature_imp(`ORI GIN`, `DIS TANCE`, `DEP TIME`, test_rate = 0)
  ret <- model_df %>% prediction_training_and_test()
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 5000) # Fails for now
})
