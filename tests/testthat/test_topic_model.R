# how to run this test:
# devtools::test(filter="textanal")
context("test topic model function, exp_topic_model")
twitter_df <- exploratory::read_delim_file("https://www.dropbox.com/s/w1fh7j8iq6g36ry/Twitter_No_Spectator_Olympics_Ja.csv?dl=1", delim = ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", tz = "America/Los_Angeles", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE)



test_that("tidy.textmodel_lda_exploratory to complete topic with no document belonging to it.", {
  # Create a dummy model_df. We haven't seen this situation from real data yet.
  model <- list(k=2)
  doc_df <- tibble::tibble(max_topic=c(1,1))
  x <- list(model=model, doc_df=doc_df)
  class(x) <- "textmodel_lda_exploratory"
  model_df <- tibble::tibble(model = list(x))
  res <- model_df %>% tidy_rowwise(model, type="topics_summary")
  expect_equal(res$topic, c(1,2))
  expect_equal(res$n, c(2,0))
})


