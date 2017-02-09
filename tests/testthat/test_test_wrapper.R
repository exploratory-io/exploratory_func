context("tests for wrappers of tests")

test_df <- data.frame(
  cat=rep(c("cat1", "cat2"), 20),
  dim = sort(rep(paste0("dim", seq(4)), 5)),
  dim_na=c(paste0("dim", seq(10)), paste0("dim", seq(10)+3)))

test_df$list_c <- as.list(seq(20))

test_df[["with space"]] <- seq(20)

test_that("test two sample t-test", {
  result <- test_df %>%
    dplyr::group_by(dim) %>%
    do_t.test(`with space`, cat)
  expect_equal(ncol(result), 11)
})

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

test_that("test t-test with 3 groups", {
  data <- data.frame(val = seq(12), group = rep(c(1,2,3), each = 4))
  expect_error({
    result <- data %>%
      do_t.test(val, group)
  }, "Group Column has to have 2 unique values")
})

test_that("test f-test", {
  result <- test_df %>%
    dplyr::group_by(dim) %>%
    do_var.test(`with space`, cat)
  expect_equal(ncol(result), 10)
})

test_that("test f-test with 3 groups", {
  data <- data.frame(val = seq(12), group = rep(c(1,2,3), each = 4))
  expect_error({
    result <- data %>%
      do_var.test(val, group)
  }, "Group Column has to have 2 unique values")
})

test_that("test chisq.test with 2 columns", {
  test_df <- data.frame(
    group = rep(letters[1:2], each = 500),
    cat1 = letters[round(runif(1000)*5)+1],
    cat2 = letters[round(runif(1000)*3)+1]
  ) %>% dplyr::group_by(group)

  colnames(test_df) <- c("group", "cat 1", "cat2")

  ret <- test_df %>%
    do_chisq.test(`cat 1`, cat2)

  expect_equal(nrow(ret), 2)

})

test_that("test chisq.test with 3 columns", {
  test_df <- data.frame(
    group = rep(letters[1:2], each = 500),
    cat1 = letters[round(runif(1000)*5)+1],
    cat2 = letters[round(runif(1000)*3)+1]
  ) %>%
    group_by(group, cat1, cat2) %>%
    summarize(count = n()) %>%
    dplyr::group_by(group)

  colnames(test_df) <- c("group", "cat 1", "cat2", "count")

  ret <- test_df %>%
    do_chisq.test(`cat 1`, cat2, count)

  expect_equal(nrow(ret), 2)

})

test_that("test chisq.test with one column", {
  test_df <- data.frame(
    group = rep(letters[1:2], each = 500),
    cat1 = letters[round(runif(1000)*5)+1],
    cat2 = letters[round(runif(1000)*3)+1]
  ) %>%
    group_by(group, cat1, cat2) %>%
    summarize(count = n()) %>%
    dplyr::group_by(group)

  ret <- test_df %>%
    do_chisq.test(count)

  expect_equal(nrow(ret), 2)

})
