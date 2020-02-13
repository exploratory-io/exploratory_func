context("tests for wrappers of stats package")

spread_test_df <- data.frame(var1 = c(1, 3, 2, NA), var2 = c(1, 3, 2, 10))

test_that("do_dist with NA results", {
  data <- data.frame(
    x = rep(letters[1:3], 3),
    y = rep(letters[1:3], each=3)
  )
  ret <- do_dist(data, skv = c("x", "y"), distinct = TRUE)
  expect_true(nrow(ret) > 0)
})

test_that("do_cor with NA results", {
  data <- data.frame(
    x = rep(letters[1:3], 3),
    y = rep(letters[1:3], each=3)
  )
  ret <- do_cor(data, skv = c("x", "y"))
  expect_true(nrow(ret) > 0)
})

test_that("do_cor with NA results", {
  data <- data.frame(
    x = rep(letters[1:3], 3),
    y = rep(letters[1:3], each=3)
  )
  ret <- do_cor(data, skv = c("x", "y"), distinct = TRUE)
  expect_true(nrow(ret) > 0)
})

test_that("do_cor with date aggregation", {
  set.seed(0)
  rownum <- 60
  test_df <- data.frame(
    rows = rep(c("a", "b", "c"), each = rownum / 3),
    dt = rep(as.Date("2014-01-01"), rownum) + lubridate::days(rep(seq(rownum / 3), 3)),
    val = runif(rownum)
  )

  mat <- test_df %>%
    dplyr::mutate(week_round = lubridate::floor_date(dt, unit = "weeks")) %>%
    dplyr::select(-dt) %>%
    dplyr::group_by(rows, week_round) %>%
    dplyr::summarize(mean_val = mean(val)) %>%
    tidyr::spread(rows, mean_val) %>%
    dplyr::select(-week_round) %>%
    as.matrix()

  cor_ret <- cor(mat, use = "pairwise.complete.obs")

  melt_ret <- reshape2::melt(cor_ret)

  ret <- test_df %>%
    do_cor(skv = c("rows", "dt", "val"), time_unit = "weeks", diag = TRUE)

  for(i in seq(nrow(cor_ret))){
    for(j in seq(ncol(cor_ret))){
      mat_answer <- cor_ret[i, j]
      df_answer <- ret[ret[[1]] == letters[[i]] & ret[[2]] == letters[[j]], 3][[1]]
      expect_equal(mat_answer, df_answer)
    }
  }
})

test_that("do_cor with zero correlations", {
  # Steps to produce the output
  df <- data.frame(x=c(1,1,0,0),y=c(1,0,1,0),z=c(T,T,F,F))
  model_df <- df %>% do_cor(`x`, `y`, `z`, method = "pearson", distinct = FALSE, diag = TRUE, return_type = "model")
  res <- model_df %>% tidy(model, type='cor')
  expect_equal(nrow(res), 9) # Make sure rows for all 9 combinations are there even though some have 0 correlation values.
})

test_that("test do_svd.kv with fill", {
  test_df <- data.frame(
    rand=runif(20, min = 0, max=10),
    axis2=paste("group",c(rep(1,5), rep(2, 5), rep(3, 5), rep(4, 5)), sep=""),
    col=rep(seq(5),4))
  loadNamespace("dplyr")
  result <- (
    test_df  %>%
      do_svd.kv(axis2, col, n_component=3, fill = 1)
  )
  expect_equal(colnames(result), c("axis2","new.dimension", "value"))
  expect_true(any(result[[1]]=="group1"))
  expect_true(any(result[[2]]==1))

})

test_that("test normalize", {
  test_vec <- c(seq(10), NA, 10 - seq(10))
  ans <- scale(test_vec) %>% as.numeric()
  ret <- normalize(ans)
  expect_equal(ans, ret)
})

test_that("do_cor with NA values", {
  loadNamespace("reshape2")
  nrow <- 10
  ncol <- 20
  vec <- rnorm(nrow * ncol)
  mat <- matrix(vec, nrow = nrow)
  melt_mat <- reshape2::melt(mat)
  colnames(melt_mat)[[2]] <- "Var 2"

  ret <- do_cor(melt_mat, skv = c("Var 2", "Var1", "value"), diag = TRUE)

  cor_ret <- cor(mat, use = "pairwise.complete.obs")
  melt_ret <- reshape2::melt(cor_ret)

  for(i in seq(ncol)){
    for(j in seq(ncol)){
      mat_answer <- cor_ret[i, j]
      df_answer <- ret[ret[[1]] == i & ret[[2]] == j, 3][[1]]
      expect_equal(mat_answer, df_answer)
    }
  }
})

test_that("do_cor with NA values with model output", {
  loadNamespace("reshape2")
  nrow <- 10
  ncol <- 20
  vec <- rnorm(nrow * ncol)
  mat <- matrix(vec, nrow = nrow)
  melt_mat <- reshape2::melt(mat)
  colnames(melt_mat)[[2]] <- "Var 2"

  ret <- do_cor(melt_mat, skv = c("Var 2", "Var1", "value"), diag = TRUE, return_type = "model")
  ret <- ret %>% tidy(model, type = "cor")

  cor_ret <- cor(mat, use = "pairwise.complete.obs")
  melt_ret <- reshape2::melt(cor_ret)

  for(i in seq(ncol)){
    for(j in seq(ncol)){
      mat_answer <- cor_ret[i, j]
      df_answer <- ret[ret[[1]] == i & ret[[2]] == j, 3][[1]]
      expect_equal(mat_answer, df_answer)
    }
  }
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
  expect_equal(result[["value"]], rep(1, 2))
})

test_that("test do_cor.cols with model output", {
  result <- spread_test_df %>%
    do_cor.cols(dplyr::starts_with("var"), return_type = "model")
  expect_equal(colnames(result), "model")
  result_cor <- result %>% tidy(model, type = "cor")
  expect_equal(result_cor[["value"]], rep(1, 2))
  result_data <- result %>% tidy(model, type = "data")
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
  expect_equal(dim(result), c(4, 4))
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

  result_cor <- result %>% tidy(model)
  expect_equal(dim(result_cor), c(4, 4))
  result_data <- result %>% tidy(model, type = "data")
  expect_equal(colnames(result_data), c("var1", "var2", "group"))
})

test_that("test do_cor.kv for duplicated pair", {
  result <- tidy_test_df %>%  do_cor.kv(cat, dim, val)
  expect_equal(ncol(result), 3)
  expect_equal(result[["cat.x"]], c("cat1", "cat2"))
  expect_equal(result[["cat.y"]], c("cat2", "cat1"))
  expect_equal(result[["value"]], replicate(2, 1))
})

test_that("test do_cor.kv with model output", {
  result <- tidy_test_df %>%  do_cor.kv(cat, dim, val, return_type = "model")
  result_cor <- result %>% tidy(model, type = "cor")
  expect_equal(ncol(result_cor), 3)
  expect_equal(result_cor[["cat.x"]], c("cat1", "cat2"))
  expect_equal(result_cor[["cat.y"]], c("cat2", "cat1"))
  expect_equal(result_cor[["value"]], replicate(2, 1))
  result_data <- result %>% tidy(model, type = "data")
  expect_equal(colnames(result_data), c("cat", "dim", "val", "dim_na"))
})

test_that("test do_cor.kv with group_by with model output", {
  result <- tidy_group_test_df %>% group_by(grp) %>% do_cor.kv(cat, dim, val, return_type = "model")
  result_cor <- result %>% tidy(model, type = "cor")
  expect_equal(ncol(result_cor), 4)
  expect_equal(result_cor[["grp"]], c("A", "A", "B", "B"))
  expect_equal(result_cor[["cat.x"]], c("cat1", "cat2", "cat1", "cat2"))
  expect_equal(result_cor[["cat.y"]], c("cat2", "cat1", "cat2", "cat1"))
  expect_equal(result_cor[["value"]], replicate(4, 1))
  result_data <- result %>% tidy(model, type = "data")
  expect_equal(colnames(result_data), c("cat", "dim", "val", "dim_na", "grp")) # TODO: group column comes as the last column, but it might be easier to understand if it comes first.
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
  }, "group is a grouping column\\. ungroup\\(\\) may be necessary before this operation\\.")
})

test_that("test do_cor.kv for empty value", {
  result <- tidy_test_df %>%  do_cor.kv(cat, dim_na, val)
})

test_that("test do_cor without val", {
  loadNamespace("dplyr")

  test_df <- data.frame(
    subject = paste0("subject", rep(4-seq(3), each=3)),
    key = paste0("key", c(rep(3-seq(2), 4), 1)))

  result <- (
    test_df %>%
      do_cor( skv = c("subject", "key") )
  )

  expect_equal(result[[3]][1:2], c(1, -1))
})

test_that("do_svd.kv with NA value", {
  data <- data.frame(row = c(1, 1, 2, 2, 3,3),
                     col = rep(c(1,2), 3),
                     val = seq(6)) %>%
    dplyr::slice(-3)
  # this slice creates missing value by removing a row

  colnames(data)[1] <- "ro w"

  # expect no error
  do_svd.kv(data, `ro w`, col, val, fill=0)
  expect_error({
    do_svd.kv(data, `ro w`, col, val, fill=NA)
  }, "NA is not supported as value")
})

test_that("test do_svd.kv output wide", {
  test_df <- data.frame(
    rand=runif(20, min = 0, max=10),
    axis2=paste("group",c(rep(1,5), rep(2, 5), rep(3, 5), rep(4, 5)), sep=""),
    col=rep(seq(5),4))
  loadNamespace("dplyr")
  result <- test_df  %>%
      do_svd.kv(axis2, col, rand, n_component=3, output="wide")
  expect_equal(colnames(result), c("axis2","axis1", "axis2.new", "axis3"))
  expect_true(any(result[,1]=="group1"))
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
    expect_equal(colnames(result), c("axis2","new.dimension", "value"))
    expect_true(any(result[[1]]=="group1"))
    expect_true(any(result[[2]]==1))
  }
})

test_that("test do_svd.kv without value", {
  if(requireNamespace("broom")){
    test_df <- data.frame(
      axis2=paste("group",c(rep(1,5), rep(2, 5), rep(3, 5), rep(4, 5)), sep=""),
      col=rep(seq(5),4))
    loadNamespace("dplyr")
    result <- test_df  %>%
      do_svd.kv(axis2, col, n_component=3)
    expect_equal(colnames(result), c("axis2","new.dimension", "value"))
    expect_true(any(result[[1]]=="group1"))
    expect_true(any(result[[2]]==1))
  }
})

test_that("test do_svd.kv", {
  test_df <- data.frame(
    rand=runif(20, min = 0, max=10),
    axis2=paste("group",c(rep(1,5), rep(2, 5), rep(3, 5), rep(4, 5)), sep=""),
    col=rep(seq(5),4))
  loadNamespace("dplyr")
  result <- (
    test_df  %>%
      do_svd.kv(axis2, col, n_component=3)
    )
  expect_equal(colnames(result), c("axis2","new.dimension", "value"))
  expect_true(any(result[[1]]=="group1"))
  expect_true(any(result[[2]]==1))

})

test_that("test do_svd.kv with group_by, output=wide", {
  if(requireNamespace("broom")){
    test_df <- data.frame(
      rand=runif(20, min = 0, max=10),
      group=c(rep(1,5), rep(2, 5), rep(3, 5), rep(4, 5)),
      axis1=paste("group", c(rep(1,10), rep(2, 10)), sep=""),
      col=rep(seq(5),4), stringsAsFactors = FALSE)
    loadNamespace("dplyr")
    result <- test_df %>%
      dplyr::group_by(axis1) %>%
      do_svd.kv(group, col, rand, output="wide", n_component = 1)
    expect_equal(colnames(result), c("axis1","group","axis1.new"))
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
      %>%  do_svd.kv(group, col, rand, n_component=1))
    expect_true(!is.unsorted(result[,1]))
    expect_equal(colnames(result), c("group2","group","new.dimension", "value"))
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
    expect_equal(colnames(result), c("col","new.dimension", "value"))
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
    expect_equal(colnames(result), c("new.dimension", "value"))
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
  }, "group is a grouping column\\. ungroup\\(\\) may be necessary before this operation\\.")
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
  # test column name with space
  colnames(test_df)[1] <- "Var 1"

  df_tt <- do_dist.kv(test_df, `Var 1`, Var2, value, distinct=TRUE ,diag=TRUE)
  df_tf <- do_dist.kv(test_df, `Var 1`, Var2, value, distinct=TRUE ,diag=FALSE)
  df_ft <- do_dist.kv(test_df, `Var 1`, Var2, value, distinct=FALSE ,diag=TRUE)
  df_ff <- do_dist.kv(test_df, `Var 1`, Var2, value, distinct=FALSE ,diag=FALSE)
  ret_tt <- do_cmdscale(df_tt, `Var 1.x`, `Var 1.y`, value)
  ret_tf <- do_cmdscale(df_tf, `Var 1.x`, `Var 1.y`, value)
  ret_ft <- do_cmdscale(df_ft, `Var 1.x`, `Var 1.y`, value)
  ret_ff <- do_cmdscale(df_ff, `Var 1.x`, `Var 1.y`, value)
  expect_equal(c(ret_tt[[2]], ret_tf[[2]], ret_ft[[2]], ret_ff[[2]]), setNames(rep(points[,1], 4), NULL))

  half_df <- distance %>% as.vector()  %>%  upper_gather(attr(distance, "Labels"), diag=TRUE)
  result_half <- do_cmdscale(half_df, Var1, Var2, value)

})

test_that("do_cmdscale undefined column name error", {
  data <- data.frame(var1 = c(1, 3, 2, NA), var2 = c(1, 3, 2, 10))
  expect_error({
    do_cmdscale(data, var1, var2, var3)
  }, "object 'var3' not found")
})

test_that("do_cmdscale all 0 distances error", {
  data <- data.frame(var1 = c(1, 2, 3, 4), var2 = c(4, 1, 2, 3), val = c(0,0,0,0))
  expect_error({
    do_cmdscale(data, var1, var2, val)
  }, "All distances are 0. Multidimensional scaling cannot be calculated.")
})
