# how to run this test:
# devtools::test(filter="lm_1")

context("lm/glm negative test")

testdata_dir <- tempdir()
testdata_filename <- "airline_2013_10_tricky_v3_5k.csv" 
testdata_file_path <- paste0(testdata_dir, '/', testdata_filename)

filepath <- if (!testdata_filename %in% list.files(testdata_dir)) {
  "https://exploratory-download.s3.us-west-2.amazonaws.com/test/airline_2013_10_tricky_v3.csv"
} else {
  testdata_file_path
}

flight <- exploratory::read_delim_file(filepath, ",", quote = "\"", skip = 0 , col_names = TRUE , na = c("","NA") , locale=readr::locale(encoding = "UTF-8", decimal_mark = "."), trim_ws = FALSE , progress = FALSE) %>% exploratory::clean_data_frame()

if (!testdata_filename %in% list.files(testdata_dir)) {
  set.seed(1)
  flight <- flight %>% slice_sample(n=5000)
  write.csv(flight, testdata_file_path) # save sampled-down data for performance.
}

test_that("build_lm.fast (logistic regression) with marginal effect with NA Date value for an entire category", {
  # There used to be an issue where marginal effect throws error because of unused factor level. This is handled now by pre-filtering rows with NA Date values.
  df <- flight %>% mutate(`FL DATE`=if_else(`CAR RIER`!="UA",`FL DATE`,as.Date(NA)))
  model_df <- df %>%
                build_lm.fast(`is delayed`, `CAR RIER`, `FL DATE`, model_type = "glm", test_rate = 0.3, variable_metric = "ame", with_marginal_effects_confint = FALSE)

  # Just run prediction etc. for sanity.
  ret <- model_df %>% prediction_binary(data="training_and_test", threshold = 0.5)
  ret <- model_df %>% evaluate_binary_training_and_test("is delayed", pretty.name=TRUE)
  ret <- model_df %>% prediction_training_and_test(prediction_type = 'conf_mat', threshold = 0.5)
})

test_that("build_lm.fast (linear regression) with single predictor should skip relative importance", {
  model_df <- flight %>%
                build_lm.fast(`ARR DELAY`, `CAR RIER`, test_rate = 0.3, seed=1)

  # Just run prediction for sanity.
  ret <- model_df %>% prediction(data="training_and_test")

  ret <- model_df %>% evaluate_lm_training_and_test(pretty.name=TRUE)
  expect_true("Note" %nin% names(ret)) # There should *not* be the Note column with error.
})

