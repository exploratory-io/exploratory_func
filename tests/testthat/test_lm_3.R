# how to run this test:
# devtools::test(filter="lm_1")

context("lm/glm negative test")

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

test_that("build_lm.fast (linear regression) with single predictor should skip relative importance", {
  model_df <- flight %>%
                build_lm.fast(`ARR DELAY`, `CAR RIER`, test_rate = 0.3, seed=1, relimp = TRUE, relimp_type = "first", relimp_bootstrap_type = "perc")

  # Just running prediction.
  ret <- model_df %>% prediction(data="training_and_test")

  ret <- model_df %>% evaluate_lm_training_and_test(pretty.name=TRUE)
  expect_true("Note" %nin% names(ret)) # There should *not* be the Note column with error.
})

