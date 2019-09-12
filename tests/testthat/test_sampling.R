# how to run this test:
# devtools::test(filter="sampling")

context("test sampling down to default 50000 rows in preprocessing of various Analytics View functions.")

testdata_dir <- "~/.exploratory/"
testdata_filename <- "airline_2013_10_tricky_v3.csv" 
testdata_file_path <- paste0(testdata_dir, testdata_filename)

filepath <- if (!testdata_filename %in% list.files(testdata_dir)) {
  "https://www.dropbox.com/s/f47baw5f3v0xoll/airline_2013_10_tricky_v3.csv?dl=1"
} else {
  testdata_file_path
}

flight <- exploratory::read_delim_file(filepath, ",", quote = "\"", skip = 0 , col_names = TRUE , na = c("","NA") , locale=readr::locale(encoding = "UTF-8", decimal_mark = "."), trim_ws = FALSE , progress = FALSE) %>% exploratory::clean_data_frame()

filepath <- if (!testdata_filename %in% list.files(testdata_dir)) {
  write.csv(flight, testdata_file_path) # save data for performance.
}

test_that("build_coxph.fast() samples down input to 50000 rows", {
  model_df <- flight %>% build_coxph.fast(`ARR DELAY`, `delay ed`, `DEP DELAY`, `CAR RIER`)
  ret <- model_df %>% glance(model)
  expect_lte(ret$n, 50000) # should be less than or equal 50000 after sampling.
})

test_that("calc_feature_map(regression) samples down input to 50000 rows", {
  model_df <- flight %>%
                calc_feature_imp(`ARR DELAY`, `CAR RIER`, `ORI GIN`, `DEP DELAY`, `AIR TIME`,
                                 test_rate = 0.3,
                                 test_split_type = "ordered") # testing ordered split too.

  ret <- model_df %>% prediction(data="training_and_test")
  expect_lte(nrow(ret), 50000) # should be less than or equal 50000 after sampling.
})

test_that("calc_feature_map(regression) samples down input to 50000 rows", {
  model_df <- flight %>%
                calc_feature_imp(`ARR DELAY`, `CAR RIER`, `ORI GIN`, `DEP DELAY`, `AIR TIME`,
                                 test_rate = 0.3,
                                 test_split_type = "ordered") # testing ordered split too.

  ret <- model_df %>% prediction(data="training_and_test")
  expect_lte(nrow(ret), 50000) # should be less than or equal 50000 after sampling.
})

test_that("exp_rpart(binary) samples down input to 50000 rows", {
  model_df <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`)) %>%
                exp_rpart(is_delayed, `DIS TANCE`, `DEP DELAY`, test_rate = 0.3, binary_classification_threshold=0.5)

  ret <- model_df %>% prediction(data="training_and_test")
  expect_lte(nrow(ret), 50000) # should be less than or equal 50000 after sampling.
})

test_that("build_lm.fast (linear regression) samples down input to 50000 rows", {
  model_df <- flight %>%
                build_lm.fast(`ARR DELAY`, `DIS TANCE`, `DEP DELAY`, `CAR RIER`, test_rate = 0.3, seed=1)

  ret <- model_df %>% prediction(data="training_and_test")
  expect_lte(nrow(ret), 50000) # should be less than or equal 50000 after sampling.
})

