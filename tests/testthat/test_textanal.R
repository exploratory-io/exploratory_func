# how to run this test:
# devtools::test(filter="textanal")
context("test text analysis function, exp_textanal")

test_that("exp_factanal", {
  df <- mtcars %>% mutate(new_col = c(rep("A", n() - 10), rep("B", 10)))
  model_df <- exp_textanal(df)
  res <- model_df %>% tidy_rowwise(model, type="x")
  expect_equal(colnames(res),
               c("x"))
  res <- model_df %>% tidy_rowwise(model, type="y")
  expect_equal(colnames(res),
               c("y"))
})
