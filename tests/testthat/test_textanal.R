# how to run this test:
# devtools::test(filter="textanal")
context("test text analysis function, exp_textanal")

test_that("exp_factanal", {
  browser()
  df <- tibble::tibble(text=c("すもももももももものうち","隣の客はよく柿食う客だ。","赤巻紙青巻紙黄巻紙"))
  model_df <- df %>% exp_textanal(text)
  browser()
  res <- model_df %>% tidy_rowwise(model, type="word_count")
  browser()
  expect_equal(colnames(res),
               c("x"))
  res <- model_df %>% tidy_rowwise(model, type="y")
  expect_equal(colnames(res),
               c("y"))
})
