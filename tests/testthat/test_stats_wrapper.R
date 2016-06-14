context("tests for wrappers of stats package")
options(warn=2)
spread_test_df <- data.frame(var1 = c(1, 3, 2, NA), var2 = c(1, 3, 2, 10))
tidy_test_df <- data.frame(
  cat=rep(c("cat1", "cat2"), 20),
  dim = sort(rep(paste0("dim", seq(4)), 5)),
  val=seq(20),
  dim_na=c(paste0("dim", seq(10)), paste0("dim", seq(10)+3)))

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

test_that("test calc_cor for duplicated pair", {
  result <- tidy_test_df %>%  calc_cor(cat, dim, val)
  expect_equal(ncol(result), 3)
  expect_equal(result[["pair.name.1"]], c("cat1", "cat2"))
  expect_equal(result[["pair.name.2"]], c("cat2", "cat1"))
  expect_equal(result[["cor.value"]], replicate(2, 1))
})

test_that("test calc_cor_cat for empty value", {
  result <- tidy_test_df %>%  calc_cor(cat, dim_na, val)
})

test_that("test do_svd", {
  if(requireNamespace("broom")){
    test_df <- data.frame(
      vec1=seq(20),
      vec2=20-seq(20),
      rand=runif(20, min = 0, max=10),
      na=as.vector(replicate(5,c(NA,5))),
      group=paste("group",c(rep(1,5), rep(2, 5), rep(3, 5), rep(4, 5)), sep=""),
      col=rep(seq(5),4))
    loadNamespace("dplyr")
    result <- (
      test_df
      %>%  do_svd(group, col, rand, n_component=3))
    expect_true(!is.unsorted(result[,1]))
    expect_equal(colnames(result), c("group","component", "value.svd"))
    expect_true(any(result[,1]=="group1"))
    expect_true(any(result[,2]==1))
  }
})

test_that("test do_svd with group_by", {
  if(requireNamespace("broom")){
    test_df <- data.frame(
      vec1=seq(20),
      vec2=20-seq(20),
      rand=runif(20, min = 0, max=10),
      na=as.vector(replicate(5,c(NA,5))),
      group=paste("group",c(rep(1,5), rep(2, 5), rep(3, 5), rep(4, 5)), sep=""),
      group2=paste("group",c(rep(1,10), rep(2, 10)), sep=""),
      col=rep(seq(5),4), stringsAsFactors = FALSE)
    loadNamespace("dplyr")
    result <- (
      test_df
      %>%  dplyr::group_by(group2)
      %>%  do_svd(group, col, rand, n_component=3))
    expect_true(!is.unsorted(result[,1]))
    expect_equal(colnames(result), c("group2","group","component", "value.svd"))
    expect_true(any(result[,1]=="group2"))
    expect_true(any(result[,3]==1))
  }
})


test_that("test do_svd of dimension", {
  if(requireNamespace("broom")){
    loadNamespace("dplyr")
    test_df <- data.frame(
      vec1=seq(20),
      vec2=20-seq(20),
      rand=runif(20, min = 0, max=10),
      na=as.vector(replicate(5,c(NA,5))),
      group=paste("group",c(rep(1,5), rep(2, 5), rep(3, 5), rep(4, 5)), sep=""),
      col=rep(seq(5),4))
    result <- (
      test_df
      %>%  do_svd(group, col, rand, type="dimension"))
    expect_true(!is.unsorted(result[,1]))
    expect_equal(colnames(result), c("dimension","component", "value.svd"))
    expect_true(any(result[,1]==1))
    expect_true(any(result[,2]==1))
  }
})

test_that("test do_svd_var of dimension", {
  if(requireNamespace("broom")){
    loadNamespace("dplyr")
    test_df <- data.frame(
      vec1=seq(20),
      vec2=20-seq(20),
      rand=runif(20, min = 0, max=10),
      na=as.vector(replicate(4,c(NA,5,3,2,9))),
      group=paste("group",c(rep(1,5), rep(2, 5), rep(3, 5), rep(4, 5)), sep=""),
      col=rep(seq(5),4))

    result <- (
      test_df
      %>%  dplyr::group_by(group)
      %>%  do_svd_var(col, rand,na))
    expect_equal(colnames(result), c("group","group.new", "component", "value.svd"))
  }
})


test_that("test do_svd_var of dimension", {
  if(requireNamespace("broom")){
    loadNamespace("dplyr")
    test_df <- data.frame(
      vec1=seq(20),
      vec2=20-seq(20),
      rand=runif(20, min = 0, max=10),
      na=as.vector(replicate(4,c(NA,5,3,2,9))),
      group=paste("group",c(rep(1,5), rep(2, 5), rep(3, 5), rep(4, 5)), sep=""),
      col=rep(seq(5),4))
    result <- (
      test_df
      %>%  dplyr::group_by(group)
      %>%  do_svd_var(col, rand,na, type="dimension"))
    expect_equal(colnames(result), c("group","dimension", "component", "value.svd"))
  }
})

test_that("test do_svd of variance", {
  if(requireNamespace("broom")){
    loadNamespace("dplyr")
    test_df <- data.frame(
      vec1=seq(20),
      vec2=20-seq(20),
      rand=runif(20, min = 0, max=10),
      na=as.vector(replicate(5,c(NA,5))),
      group=paste("group",c(rep(1,5), rep(2, 5), rep(3, 5), rep(4, 5)), sep=""),
      col=rep(seq(5),4))
    result <- (
      test_df
      %>%  do_svd(group, col, rand, type="variance", n_component=2))
    expect_equal(colnames(result), c("component", "value.svd"))
    expect_equal(nrow(result),2)
  }
})

