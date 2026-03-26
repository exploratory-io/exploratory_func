context("tests for wrappers of stats package")

spread_test_df <- data.frame(var1 = c(1, 3, 2, NA), var2 = c(1, 3, 2, 10))












test_that("normalize", {
  test_vec <- c(seq(10), NA, 10 - seq(10))
  ans <- scale(test_vec) %>% as.numeric()
  ret <- normalize(ans)
  expect_equal(ans, ret)
})

test_that("normalize with constant data", {
  test_vec <- rep(0, 10) # zero constant input
  ret <- normalize(test_vec, center=TRUE, scale=TRUE)
  expect_equal(ret, rep(0, 10))
  ret <- normalize(test_vec, center=TRUE, scale=FALSE)
  expect_equal(ret, rep(0, 10))
  ret <- normalize(test_vec, center=FALSE, scale=TRUE)
  expect_equal(ret, rep(0, 10))
  ret <- normalize(test_vec, center=FALSE, scale=FALSE)
  expect_equal(ret, rep(0, 10))

  test_vec <- rep(1, 10) # non-zero constant input
  ret <- normalize(test_vec, center=TRUE, scale=TRUE)
  expect_equal(ret, rep(0, 10))
  ret <- normalize(test_vec, center=TRUE, scale=FALSE)
  expect_equal(ret, rep(0, 10))
  ret <- normalize(test_vec, center=FALSE, scale=TRUE)
  ans <- scale(test_vec, center=FALSE, scale=TRUE) %>% as.numeric()
  expect_equal(ans, ret)
  ret <- normalize(test_vec, center=FALSE, scale=FALSE)
  expect_equal(ret, rep(1, 10))
})



tidy_test_df <- data.frame(
  cat=rep(c("cat1", "cat2"), 20),
  dim = sort(rep(paste0("dim", seq(4)), 5)),
  val=seq(20),
  dim_na=c(paste0("dim", seq(10)), paste0("dim", seq(10)+3)))

# test data for group_by.
tidy_group_test_df <- dplyr::bind_rows(tidy_test_df, tidy_test_df) %>% dplyr::mutate(grp = c(rep("A",40), rep("B",40)))

test_that("test do_cor.cols", {
  result <- spread_test_df %>%
    do_cor.cols(dplyr::starts_with("var"))
  expect_equal(result[["correlation"]], rep(1, 2))
  expect_equal(result[["p_value"]], c(0, 0))
})

test_that("test do_cor.cols with model output", {
  result <- spread_test_df %>%
    do_cor.cols(dplyr::starts_with("var"), return_type = "model")
  expect_equal(colnames(result), "model")
  result_cor <- result %>% tidy_rowwise(model, type = "cor")
  expect_equal(result_cor[["correlation"]], rep(1, 2))
  expect_equal(result_cor[["p_value"]], rep(0, 2))
  result_data <- result %>% tidy_rowwise(model, type = "data")
  expect_equal(colnames(result_data), c("var1", "var2"))
})

test_that("test do_cor.cols for grouped df", {
  loadNamespace("dplyr")
  group1 <- cbind(spread_test_df, data.frame(group=rep("group1", 4)))
  group2 <- cbind(spread_test_df, data.frame(group=rep("group2", 4)))
  group2$var2 <- -group2$var2
  test_df <- rbind(group1, group2)
  result <- (
    test_df
    %>%  dplyr::group_by(group)
    %>%  do_cor.cols(dplyr::starts_with("var")))
  expect_equal(dim(result), c(4, 6))
})

test_that("test do_cor.cols for grouped df with model output", {
  loadNamespace("dplyr")
  group1 <- cbind(spread_test_df, data.frame(group=rep("group1", 4)))
  group2 <- cbind(spread_test_df, data.frame(group=rep("group2", 4)))
  group2$var2 <- -group2$var2
  test_df <- rbind(group1, group2)
  result <- (
    test_df
    %>%  dplyr::group_by(group)
    %>%  do_cor.cols(dplyr::starts_with("var"), return_type = "model"))

  result_cor <- result %>% tidy_rowwise(model)
  expect_equal(dim(result_cor), c(4, 6))
  result_data <- result %>% tidy_rowwise(model, type = "data")
  expect_equal(colnames(result_data), c("group", "var1", "var2"))
})






















