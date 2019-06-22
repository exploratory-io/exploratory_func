# how to run this test:
# devtools::test(filter="randomForest_tidiers")

context("test rpart prediction with training/test data")

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
flight <- flight %>% sample_n(5000)


test_that("calc_feature_map(regression) evaluate training and test", {
  model_df <- flight %>%
                build_lm.fast(`FL NUM`, `DIS TANCE`, `DEP TIME`, test_rate = 0.3)

  ret <- model_df %>% evaluate_lm_training_and_test(pretty.name=TRUE)
  expect_equal(nrow(ret), 2) # 2 for train and test
  ret
})


