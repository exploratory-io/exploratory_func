context("tests for wrappers of tests")

test_df <- data.frame(
  cat=rep(c("cat1", "cat2"), 20),
  dim = sort(rep(paste0("dim", seq(4)), 5)),
  dim_na=c(paste0("dim", seq(10)), paste0("dim", seq(10)+3)))

test_df$list_c <- as.list(seq(20))

test_df[["with space"]] <- seq(20)

test_that("test two sample t-test more than 2 levels", {
  expect_error({
    result <- test_df %>%
      dplyr::group_by(dim) %>%
      do_t.test(`with space`, dim_na)
  })
})

test_that("test two sample t-test less than 2 levels", {
  expect_error({
    result <- test_df %>%
      dplyr::group_by(dim) %>%
      do_t.test(`with space`, dim)
  })
})

test_that("test one sample t-test", {
  result <- test_df %>%
    dplyr::group_by(dim) %>%
    do_t.test("with space", mu=3)
  expect_equal(result[result[["dim"]]=="dim1", "p.value"][[1]], 1)
})
