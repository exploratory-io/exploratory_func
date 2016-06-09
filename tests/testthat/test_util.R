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

test_that("test group_exclude", {
  test_df <- data.frame(
    col1=rep(paste("col1", seq(2)), 5),
    col2=rep(paste("col2", seq(2)), each=5),
    col3=paste("col3", seq(10))
    )
  test_df$list <- as.list(paste("list", seq(10)))
  ret <- group_exclude(test_df, col1, col2)
  expect_equal(colnames(attr(ret, "label")), "col3")
})

test_that("test group_exclude one col", {
  test_df <- data.frame(
    col1=rep(paste("col1", seq(2)), 5)
  )
  ret <- group_exclude(test_df, col1)
  expect_equal(attr(ret, "label"), NULL)
})
