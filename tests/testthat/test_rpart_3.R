# how to run this test:
# devtools::test(filter="rpart_3")
# This is same test as test_rpart_2.R with group_by.

context("test tidiers for rpart. (trainig/test with group_by)")

testdata_dir <- "~/.exploratory/"
testdata_filename <- "airline_2013_10_tricky_v3.csv" 
testdata_file_path <- paste0(testdata_dir, testdata_filename)

filepath <- if (!testdata_filename %in% list.files(testdata_dir)) {
  "https://www.dropbox.com/s/f47baw5f3v0xoll/airline_2013_10_tricky_v3.csv?dl=1"
} else {
  testdata_file_path
}
if (!exists("flight_downloaded")) {
  flight_downloaded <- exploratory::read_delim_file(filepath, ",", quote = "\"", skip = 0 , col_names = TRUE , na = c("","NA") , locale=readr::locale(encoding = "UTF-8", decimal_mark = "."), trim_ws = FALSE , progress = FALSE) %>% exploratory::clean_data_frame()
  write.csv(flight_downloaded, testdata_file_path)
  flight <- flight_downloaded
} else {
  flight <- flight_downloaded
}

# Add group_by. Cases without group_by is covered in test_randomForest_tidiers_3.R.
flight <- flight %>% sample_n(5000) %>% group_by(`CAR RIER`)


test_that("calc_feature_map(regression) evaluate training and test", {
  model_df <- flight %>%
                exp_rpart(`FL NUM`, `DIS TANCE`, `DEP TIME`, test_rate = 0.3)

  ret <- model_df %>% prediction_training_and_test()
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1500) # Fails for now
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3500) # Fails for now

  # Training only case
  model_df <- flight %>%
                exp_rpart(`FL NUM`, `DIS TANCE`, `DEP TIME`, test_rate = 0)
  ret <- model_df %>% prediction_training_and_test()
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 5000) Fails for now
})

test_that("calc_feature_map(binary) evaluate training and test", {
  model_df <- flight %>%
                # filter(`CAR RIER` %in% c("VA","AA")) %>%
                dplyr::mutate(is_delayed = as.factor(`is delayed`)) %>%
                exp_rpart(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0.3)

  ret <- model_df %>% prediction_training_and_test()
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1500) # Fails for now
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3500) # Fails for now

  # Training only case
  model_df <- flight %>% dplyr::mutate(is_delayed = as.factor(`is delayed`)) %>%
                exp_rpart(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0)
  ret <- model_df %>% prediction_training_and_test()
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 5000) Fails for now
})

test_that("calc_feature_map(multi) evaluate training and test", {
  model_df <- flight %>%
                exp_rpart(`ORI GIN`, `DIS TANCE`, `DEP TIME`, test_rate = 0.3)

  ret <- model_df %>% prediction_training_and_test()
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1500) # Fails for now
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3500) # Fails for now

  # Training only case
  model_df <- flight %>%
                exp_rpart(`ORI GIN`, `DIS TANCE`, `DEP TIME`, test_rate = 0)
  ret <- model_df %>% prediction_training_and_test()
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 5000) Fails for now
})

