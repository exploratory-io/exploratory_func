context("tests for wrappers of stats package")

spread_test_df <- data.frame(var1 = c(1, 3, 2, NA), var2 = c(1, 3, 2, 10))
tidy_test_df <- data.frame(
  cat=rep(c("cat1", "cat2"), 20),
  dim = sort(rep(paste0("dim", seq(4)), 5)),
  val=seq(20))

test_that("test calc_cor_var", {
  result <- (
    spread_test_df
    %>%  calc_cor_var(starts_with("var")))
  expect_equal(result[["cor.value"]], rep(1, 4))
})

test_that("test calc_cor_var for grouped df", {
  loadNamespace("dplyr")
  group1 <- cbind(spread_test_df, data.frame(group=rep("group1", 4)))
  group2 <- cbind(spread_test_df, data.frame(group=rep("group2", 4)))
  group2$var2 <- -group2$var2
  test_df <- rbind(group1, group2)
  result <- (
    test_df
    %>%  dplyr::group_by(group)
    %>%  calc_cor_var(starts_with("var")))
  expect_equal(dim(result), c(8, 4))
})

test_that("test calc_cor_cat for duplicated pair", {
  result <- tidy_test_df %>%  calc_cor_cat(cat, dim, val)
  expect_equal(ncol(result), 3)
  expect_equal(result[["cor.value"]], replicate(4, 1))
})
