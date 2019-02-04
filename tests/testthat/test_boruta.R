# how to run this test:
# devtools::test(filter="rpart")

if (!exists("flight")) {
  # To skip repeated data loading, run the following outside of the context of the test,
  # so that it stays even after the test.
  flight <- exploratory::read_delim_file("https://www.dropbox.com/s/f47baw5f3v0xoll/airline_2013_10_tricky_v3.csv?dl=1", ",", quote = "\"", skip = 0 , col_names = TRUE , na = c("","NA") , locale=readr::locale(encoding = "UTF-8", decimal_mark = "."), trim_ws = FALSE , progress = FALSE) %>% exploratory::clean_data_frame()
  flight <- flight %>% sample_n(5000)
}

context("test Boruta functions")

test_that("exp_boruta regression", {
  model_df <- flight %>% exp_boruta(`ARR DELAY`,`DEP DELAY`)
  res <- model_df %>% tidy(model)
  expect_equal(names(res), c("variable", "importance", "decision"))
  res <- model_df %>% glance(model, pretty.name = TRUE)
  expect_equal(names(res), c("Iterations", "Time Taken (Second)", "P Value Threshold"))
})
