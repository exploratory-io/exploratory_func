loadNamespace("broom")
context("test broom wrappers")

set.seed(0)
test_df <- data.frame(vec1 = seq(10), vec2 = seq(10), random = runif(10, min=0, max=10))

test_that("test augment_lm newdata argument", {
  result <- (
    test_df
    %>%  do_lm(vec1~random)
    %>%  augment_lm(.model, newdata=data.frame(random=c(1,3)))
  )
  expect_equal(dim(result)[[1]], 2)
})

test_that("test if tidy_lm arguments work", {
  result <- (
    test_df
    %>%  do_lm(vec1~random)
    %>%  tidy_lm(.model)
  )
  result_ <- (
    test_df
    %>%  do_lm(vec1~log(random))
    %>%  tidy_lm(.model, conf.int = TRUE, conf.level = 0.5, quick=TRUE)
  )
  expect_true(all(result[["estimate"]] != result_[["estimate"]]))
  expect_equal(ncol(result_), 2)
})

