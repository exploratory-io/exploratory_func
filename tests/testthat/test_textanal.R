# how to run this test:
# devtools::test(filter="textanal")
context("test text analysis function, exp_textanal")

test_that("exp_factanal", {
  df <- tibble::tibble(text="すもももももももものうち")
  model_df <- df %>% exp_textanal(text)
  res <- model_df %>% tidy_rowwise(model, type="x")
  expect_equal(colnames(res),
               c("x"))
  res <- model_df %>% tidy_rowwise(model, type="y")
  expect_equal(colnames(res),
               c("y"))
})
