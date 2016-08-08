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

test_that("test do_cosine_sim.kv for grouped data frame as subject error", {
  data <- data.frame(group=rep(c(1,2,3), each=6),
                     row = rep(c(1, 1, 2, 2, 3,3), 3),
                     col = rep(c(1,2), 9),
                     val = rep(0, 18))
  expect_error({
    ret <- data %>%
      dplyr::group_by(group) %>%
      do_cosine_sim.kv(group, col, val)
  }, "group is a grouping column\\. ungroup\\(\\) may be necessary before this operation\\.")
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

test_that("test do_dist.cols", {
  loadNamespace("dplyr")

  test_df <- data.frame(var1=c(1,2,2,2), var2=c(2,1,1,1))

  result <- (
    test_df %>%
      do_dist.cols(dplyr::starts_with("var"))
  )

  expect_equal(result$dist.value, c(2,2))
})

test_that("test do_dist.kv for grouped data frame as subject error", {
  data <- data.frame(group=rep(c(1,2,3), each=6),
                     row = rep(c(1, 1, 2, 2, 3,3), 3),
                     col = rep(c(1,2), 9),
                     val = rep(0, 18))
  expect_error({
    ret <- data %>%
      dplyr::group_by(group) %>%
      do_dist.kv(group, col, val)
  }, "group is a grouping column\\. ungroup\\(\\) may be necessary before this operation\\.")
})
