context("test pairwise functions")

vec_with_na <- seq(12)
vec_with_na[3] <- NA

test_df <- data.frame(
  row=rep(paste("row", seq(4)), each=3),
  col=rep(paste("col", seq(3)), 4) ,
  val=seq(12),
  with_na=vec_with_na)

set.seed(0)
test_df$rand <- vapply(seq(nrow(test_df)), function(x){
  if(x <= 6) {
    runif(1, min=-0.1, max=0.1)
  } else {
    10+runif(1, min=-0.1, max=0.1)
  }
}, FUN.VALUE=1)

test_that("test pairwise_sim", {
  loadNamespace("dplyr")
  result <- (
    test_df %>%
      pairwise_sim(row, col, val)
  )
  expect_equal(nrow(result), 12)
})

test_that("test pairwise_sim with NA value", {
  loadNamespace("dplyr")
  result <- (
    test_df %>%
      pairwise_sim(row, col, with_na)
  )
  expect_equal(nrow(result), 12)
})

test_that("test pairwise_sim diag TRUE", {
  loadNamespace("dplyr")
  result <- (
    test_df %>%
      pairwise_sim(row, col, val, diag=FALSE, method="Euclidean")
  )
  expect_equal(nrow(result), 12)
})

test_that("test pairwise_sim method cosine", {
  loadNamespace("dplyr")
  result <- (
    test_df %>%
      pairwise_sim(row, col, val, upper=TRUE, method="cosine")
  )
  expect_equal(nrow(result), 6)
})

test_that("test pairwise_sim method cosine diag TRUE", {
  loadNamespace("dplyr")
  result <- (
    test_df %>%
      pairwise_sim(row, col, val, method="cosine", diag=TRUE)
  )
  expect_equal(nrow(result), 16)
})

test_that("test pairwise_dist diag TRUE", {
  loadNamespace("dplyr")
  result <- (
    test_df %>%
      pairwise_sim(row, col, val, diag=TRUE)
  )
  expect_equal(nrow(result), 16)
})

test_that("test pairwise_dist diag TRUE", {
  loadNamespace("dplyr")
  result <- (
    test_df %>%
      pairwise_dist(row, col, val, diag=TRUE)
  )
  expect_equal(nrow(result), 16)
})
