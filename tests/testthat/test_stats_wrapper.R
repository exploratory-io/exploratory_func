context("tests for wrappers of stats package")

spread_test_df <- data.frame(var1 = c(1, 3, 2, NA), var2 = c(1, 3, 2, 10))
tidy_test_df <- data.frame(
  cat=rep(c("cat1", "cat2"), 20),
  dim = sort(rep(paste0("dim", seq(4)), 5)),
  val=seq(20),
  dim_na=c(paste0("dim", seq(10)), paste0("dim", seq(10)+3)))

test_that("test do_cor.cols", {
  result <- (
    spread_test_df
    %>%  do_cor.cols(dplyr::starts_with("var")))
  expect_equal(result[["cor.value"]], rep(1, 2))
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
  expect_equal(dim(result), c(4, 4))
})

test_that("test do_cor.kv for duplicated pair", {
  result <- tidy_test_df %>%  do_cor.kv(cat, dim, val)
  expect_equal(ncol(result), 3)
  expect_equal(result[["pair.name.1"]], c("cat1", "cat2"))
  expect_equal(result[["pair.name.2"]], c("cat2", "cat1"))
  expect_equal(result[["cor.value"]], replicate(2, 1))
})

test_that("test do_cor.kv for grouped data frame as subject error", {
  data <- data.frame(group=rep(c(1,2,3), each=6),
                     row = rep(c(1, 1, 2, 2, 3,3), 3),
                     col = rep(c(1,2), 9),
                     val = rep(0, 18))
  expect_error({
    ret <- data %>%
      dplyr::group_by(group) %>%
      do_cor.kv(group, col, val)
  }, "group is a gruoping column\\. You can use ungroup\\(\\) to solve this\\.")
})

test_that("test do_cor.kv for empty value", {
  result <- tidy_test_df %>%  do_cor.kv(cat, dim_na, val)
})

test_that("test do_svd.kv output wide", {
  if(requireNamespace("broom")){
    test_df <- data.frame(
      rand=runif(20, min = 0, max=10),
      axis2=paste("group",c(rep(1,5), rep(2, 5), rep(3, 5), rep(4, 5)), sep=""),
      col=rep(seq(5),4))
    loadNamespace("dplyr")
    result <- (
      test_df
      %>%  do_svd.kv(axis2, col, rand, n_component=3, output="wide"))
    expect_equal(colnames(result), c("axis2","axis1", "axis2.new", "axis3"))
    expect_true(any(result[,1]=="group1"))
  }
})

test_that("test do_svd.kv", {
  if(requireNamespace("broom")){
    test_df <- data.frame(
      rand=runif(20, min = 0, max=10),
      axis2=paste("group",c(rep(1,5), rep(2, 5), rep(3, 5), rep(4, 5)), sep=""),
      col=rep(seq(5),4))
    loadNamespace("dplyr")
    result <- (
      test_df
      %>%  do_svd.kv(axis2, col, rand, n_component=3))
    expect_equal(colnames(result), c("axis2","new.dimension", "svd.value"))
    expect_true(any(result[[1]]=="group1"))
    expect_true(any(result[[2]]==1))
  }
})

test_that("test do_svd.kv with group_by, output=wide", {
  if(requireNamespace("broom")){
    test_df <- data.frame(
      rand=runif(20, min = 0, max=10),
      group=c(rep(1,5), rep(2, 5), rep(3, 5), rep(4, 5)),
      axis1=paste("group",c(rep(1,10), rep(2, 10)), sep=""),
      col=rep(seq(5),4), stringsAsFactors = FALSE)
    loadNamespace("dplyr")
    result <- (
      test_df
      %>%  dplyr::group_by(axis1)
      %>%  do_svd.kv(group, col, rand, output="wide"))
    expect_equal(colnames(result), c("axis1","group","axis1.new", "axis2"))
    expect_true(any(result[[1]]=="group2"))
    expect_equal(result[[2]], c(1, 2, 3, 4))
  }
})

test_that("test do_svd.kv with group_by output=long", {
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
      %>%  do_svd.kv(group, col, rand, n_component=3))
    expect_true(!is.unsorted(result[,1]))
    expect_equal(colnames(result), c("group2","group","new.dimension", "svd.value"))
    expect_true(any(result[,1]=="group2"))
    expect_true(any(result[,3]==1))
  }
})


test_that("test do_svd of dimension, output=wide", {
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
      %>%  do_svd.kv(group, col, rand, type="dimension", output="wide"))
    expect_true(!is.unsorted(result[,1]))
    expect_equal(colnames(result), c("col","axis1", "axis2", "axis3"))
    expect_true(any(result[[1]]=="1"))
  }
})

test_that("test do_svd of dimension output long", {
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
      %>%  do_svd.kv(group, col, rand, type="dimension"))
    expect_equal(colnames(result), c("col","new.dimension", "svd.value"))
    expect_true(any(result[[1]]==1))
    expect_true(any(result[[2]]==1))
  }
})

test_that("test do_svd of variance output=wide", {
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
      %>%  do_svd.kv(group, col, rand, type="variance", n_component=2, output="wide"))
    expect_equal(colnames(result),c("axis1", "axis2"))
    expect_equal(nrow(result),1)
  }
})

test_that("test do_svd of variance output", {
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
      %>%  do_svd.kv(group, col, rand, type="variance", n_component=2))
    expect_equal(colnames(result), c("new.dimension", "svd.value"))
    expect_equal(nrow(result),2)
  }
})

test_that("test do_svd.kv for grouped data frame as subject error", {
  data <- data.frame(group=rep(c(1,2,3), each=6),
                     row = rep(c(1, 1, 2, 2, 3,3), 3),
                     col = rep(c(1,2), 9),
                     val = rep(0, 18))

  expect_error({
    ret <- data %>%
      dplyr::group_by(group) %>%
      do_svd.kv(group, col, val)
  }, "group is a gruoping column\\. You can use ungroup\\(\\) to solve this\\.")
})

test_that("test do_cmdscale", {
  loadNamespace("reshape2")
  mat <- matrix(c(1,2,3,3,4,5,5,6,6,8,1,2), nrow=4)
  rownames(mat) <- paste("row", seq(nrow(mat)))
  distance <- dist(mat)
  mds_result <- cmdscale(distance, eig=TRUE)
  points <- mds_result$points
  eig <- mds_result$eig

  test_df <- reshape2::melt(mat)

  df_tt <- do_dist.kv(test_df, Var1, Var2, value, distinct=TRUE ,diag=TRUE)
  df_tf <- do_dist.kv(test_df, Var1, Var2, value, distinct=TRUE ,diag=FALSE)
  df_ft <- do_dist.kv(test_df, Var1, Var2, value, distinct=FALSE ,diag=TRUE)
  df_ff <- do_dist.kv(test_df, Var1, Var2, value, distinct=FALSE ,diag=FALSE)
  ret_tt <- do_cmdscale(df_tt, pair.name.1, pair.name.2, dist.value)
  ret_tf <- do_cmdscale(df_tf, pair.name.1, pair.name.2, dist.value)
  ret_ft <- do_cmdscale(df_ft, pair.name.1, pair.name.2, dist.value)
  ret_ff <- do_cmdscale(df_ff, pair.name.1, pair.name.2, dist.value)
  expect_equal(c(ret_tt[[2]], ret_tf[[2]], ret_ft[[2]], ret_ff[[2]]), setNames(rep(points[,1], 4), NULL))

  half_df <- distance %>% as.vector()  %>%  upper_gather(attr(distance, "Labels"), diag=TRUE)
  result_half <- do_cmdscale(half_df, Var1, Var2, value)

})

