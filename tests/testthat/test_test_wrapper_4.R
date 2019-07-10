context("T-Test/ANOVA with groups with only single category")

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

test_that("exp_ttest with groups with only single category", {
  flight_by_carrier <- flight %>% dplyr::group_by(`CAR RIER`)
  model_df <- flight_by_carrier %>% filter(!is.na(`is UA or AA`)) %>% exp_ttest(`ARR DELAY`, `is UA or AA`)
  res <- model_df %>% tidy(model, type="model")
  res <- model_df %>% tidy(model, type="data")
  res <- model_df %>% tidy(model, type="data_summary", conf_level=0.95)
  expect_true(is.data.frame(res)) #TODO: better expectation
})

test_that("exp_anova with groups with only single category", {
  flight_by_carrier <- flight %>% dplyr::group_by(`CAR RIER`)
  model_df <- flight_by_carrier %>% filter(!is.na(`is UA or AA`)) %>% exp_anova(`ARR DELAY`, `is UA or AA`)
  res <- model_df %>% tidy(model, type="model")
  res <- model_df %>% tidy(model, type="data")
  res <- model_df %>% tidy(model, type="data_summary", conf_level=0.95)
  expect_true(is.data.frame(res)) #TODO: better expectation
})

test_that("exp_wilcox with groups with only single category", {
  flight_by_carrier <- flight %>% dplyr::group_by(`CAR RIER`)
  model_df <- flight_by_carrier %>% filter(!is.na(`is UA or AA`)) %>% exp_wilcox(`ARR DELAY`, `is UA or AA`)
  res <- model_df %>% tidy(model, type="model")
  res <- model_df %>% tidy(model, type="data")
  res <- model_df %>% tidy(model, type="data_summary", conf_level=0.95)
  expect_true(is.data.frame(res)) #TODO: better expectation
})

test_that("exp_kruskal with groups with only single category", {
  flight_by_carrier <- flight %>% dplyr::group_by(`CAR RIER`)
  model_df <- flight_by_carrier %>% filter(!is.na(`is UA or AA`)) %>% exp_kruskal(`ARR DELAY`, `is UA or AA`)
  res <- model_df %>% tidy(model, type="model")
  res <- model_df %>% tidy(model, type="data")
  res <- model_df %>% tidy(model, type="data_summary", conf_level=0.95)
  expect_true(is.data.frame(res)) #TODO: better expectation
})



