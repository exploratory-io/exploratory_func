testthat::context("test value check functions")

testthat::test_that("is_empty", {
  vec <- c("not empty", "",  "  ", NA)
  ret <- is_empty(vec)
  expect_equal(ret, c(FALSE, TRUE, TRUE, TRUE))
})
