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

test_that("test do_cosine_sim.kv", {
  loadNamespace("dplyr")
  result <- (
    test_df %>%
      do_cosine_sim.kv(row, col, val)
  )
  expect_equal(nrow(result), 12)
})

test_that("test do_cosine_sim.kv with NA value", {
  loadNamespace("dplyr")
  result <- (
    test_df %>%
      do_cosine_sim.kv(row, col, with_na)
  )
  expect_equal(nrow(result), 12)
  expect_equal( typeof(result[[1]]), "character")
  expect_equal( typeof(result[[2]]), "character")
})

test_that("test do_cosine_sim.kv diag TRUE", {
  loadNamespace("dplyr")
  result <- (
    test_df %>%
      do_cosine_sim.kv(row, col, val, diag=TRUE)
  )
  expect_equal(nrow(result), 16)
})

test_that("test do_cosine_sim.kv ", {
  loadNamespace("dplyr")
  result <- (
    test_df %>%
      do_cosine_sim.kv(row, col, val, distinct=TRUE)
  )
  expect_equal(nrow(result), 6)
  expect_equal( typeof(result[[1]]), "character")
  expect_equal( typeof(result[[2]]), "character")
})

test_that("test do_cosine_sim.kv method cosine diag TRUE", {
  loadNamespace("dplyr")
  result <- (
    test_df %>%
      do_cosine_sim.kv(row, col, val, diag=TRUE)
  )
  expect_equal(nrow(result), 16)
  expect_equal(result[[3]][[1]], 1)
})

test_that("test do_cosine_sim.kv diag TRUE", {
  loadNamespace("dplyr")
  result <- (
    test_df %>%
      do_cosine_sim.kv(row, col, val, diag=TRUE)
  )
  expect_equal(nrow(result), 16)
})

test_that("test do_dist.kv diag TRUE", {
  loadNamespace("dplyr")
  result <- (
    test_df %>%
      do_dist.kv(row, col, val, diag=TRUE)
  )
  expect_equal(nrow(result), 16)
  expect_equal(result[[3]][1], 0)
})

test_that("test do_dist.kv distinct TRUE", {
  loadNamespace("dplyr")
  result <- (
    test_df %>%
      do_dist.kv(row, col, val, distinct=TRUE)
  )

  expect_equal(nrow(result), 6)
})
