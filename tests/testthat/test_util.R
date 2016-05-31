context("check util functions")

test_that("test upper_gather", {
  mat <- matrix(seq(20),nrow=5, ncol=4)
  colnames(mat) <- paste("col", seq(4))
  result <- upper_gather(mat)
  expect_equal(result$value, sort(result$value))
  expect_equal(nrow(result), 6)
})

test_that("test upper_gather with vector", {
  mat <- seq(6)
  names <- paste("entity", seq(4))
  result <- upper_gather(mat,names)
  expect_equal(result$value, sort(result$value))
  expect_equal(nrow(result), 6)
})

test_that("test upper_gather with vector diag true", {
  mat <- seq(6)
  names <- paste("entity", seq(4))
  result <- upper_gather(mat,names, diag=1)
  expect_equal(nrow(result), 10)
})
