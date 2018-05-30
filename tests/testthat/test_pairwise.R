context("test pairwise functions")

vec_with_na <- seq(12)
vec_with_na[3] <- NA

test_df <- data.frame(
  row=rep(paste("row", seq(4)), each=3),
  col=rep(paste("col", seq(3)), 4) ,
  val=seq(12),
  with_na=vec_with_na)

test_df <- test_df %>% rename(`ro w`=row, `co l`=col) #TODO when val column has space there are errors

set.seed(0)
test_df$rand <- vapply(seq(nrow(test_df)), function(x){
  if(x <= 6) {
    runif(1, min=-0.1, max=0.1)
  } else {
    10+runif(1, min=-0.1, max=0.1)
  }
}, FUN.VALUE=1)

test_that("test do_cosine_sim.kv with NA value", {
  loadNamespace("dplyr")
  result <- test_df %>%
      do_cosine_sim.kv(`ro w`, `co l`, with_na)
  expect_equal(nrow(result), 12)
  expect_equal( typeof(result[[1]]), "character")
  expect_equal( typeof(result[[2]]), "character")
})

test_that("test do_cosine_sim.kv", {
  loadNamespace("dplyr")
  df <- test_df
  # test with a column name with a space
  colnames(df)[1] <- "ro w"
  result <- (
    df %>%
      do_cosine_sim.kv(`ro w`, `co l`, val)
  )
  # row1 and row2 pair result
  expect_equal(colnames(result), c("ro w.x", "ro w.y", "value"))
  expect_equal(result[1, "value"][[1]], (1*4+2*5+3*6)/sqrt(1^2+2^2+3^2)/sqrt(4^2+5^2+6^2))
})

test_that("test do_cosine_sim.kv without val", {
  loadNamespace("dplyr")

  test_df <- data.frame(
    subject = paste0("subject", rep(4-seq(3), each=3)),
    key = paste0("key", c(rep(3-seq(2), 4), 1)))

  result <- (
    test_df %>%
      do_cosine_sim.kv( subject, key)
  )

  expect_equal(result[[3]][1:2], c(1, 4/5))
})

test_that("test sparse_cast with duplicate", {
  test_df <- data.frame(
    rowname = rep(c("row1", "row02", "row3"), each=3),
    colname = c("col1", "col1", "col5", "col02", "col3", "col1", "col02", "col4", "col5"),
    val = seq(9),
    stringsAsFactors = FALSE
  )
  result <- (
    test_df %>%
      do_cosine_sim.kv(rowname, colname, val, fun.aggregate=min)
  )
  expect_equal(result[1, 3][[1]], (1*6)/sqrt(1^2+3^2)/sqrt(4^2+5^2+6^2))
})

test_that("test do_cosine_sim.kv with NA value", {
  loadNamespace("dplyr")
  result <- (
    test_df %>%
      do_cosine_sim.kv(`ro w`, `co l`, with_na)
  )
  expect_equal(nrow(result), 12)
  expect_equal( typeof(result[[1]]), "character")
  expect_equal( typeof(result[[2]]), "character")
})

test_that("test do_cosine_sim.kv diag TRUE", {
  loadNamespace("dplyr")
  result <- (
    test_df %>%
      do_cosine_sim.kv(`ro w`, `co l`, val, diag=TRUE)
  )
  expect_equal(nrow(result), 16)
})

test_that("test do_cosine_sim.kv with distinct", {
  loadNamespace("dplyr")
  result <- (
    test_df %>%
      do_cosine_sim.kv(`ro w`, `co l`, val, distinct=TRUE)
  )
  expect_equal(nrow(result), 6)
  expect_equal( typeof(result[[1]]), "character")
  expect_equal( typeof(result[[2]]), "character")
})

test_that("test do_cosine_sim.kv method cosine diag TRUE", {
  loadNamespace("dplyr")
  result <- (
    test_df %>%
      do_cosine_sim.kv(`ro w`, `co l`, val, diag=TRUE)
  )
  expect_equal(nrow(result), 16)
  expect_equal(result[[3]][[1]], 1)
})

test_that("test do_cosine_sim.kv diag TRUE", {
  loadNamespace("dplyr")
  result <- (
    test_df %>%
      do_cosine_sim.kv(`ro w`, `co l`, val, diag=TRUE)
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

test_that("test do_dist.kv", {
  loadNamespace("dplyr")
  result <- test_df %>%
    do_dist.kv(`ro w`, `co l`, val, diag=TRUE)
  expect_equal(nrow(result), 16)
  expect_equal(result[[3]][1], 0)
})

test_that("test do_dist with cmd_scale", {
  loadNamespace("dplyr")
  test_df <- data.frame(
    row=rep(paste("row", seq(4)), each=6),
    col=rep(paste("col", seq(6)), 4) ,
    val=seq(24)
  )
  result_kv <- test_df %>%
    do_dist(skv = c("row", "col", "val"), diag=TRUE, cmdscale_k = 3)

  result_cols <- test_df %>%
    tidyr::spread(col, val) %>% dplyr::select(-row) %>%
    do_dist(dplyr::everything(), diag=TRUE, cmdscale_k = 3)

  expect_equal(ncol(result_kv), 4)
  expect_equal(ncol(result_cols), 4)
  expect_equal(result_kv[[2]], result_kv[[2]])
  expect_equal(result_kv[[3]], result_kv[[3]])
  expect_equal(result_kv[[4]], result_kv[[4]])
})

test_that("test do_dist with cmd_scale with normalize", {
  loadNamespace("dplyr")
  test_df <- data.frame(
    row=rep(paste("row", seq(4)), each=6),
    col=rep(paste("col", seq(6)), 4) ,
    val=c(seq(7),seq(7),seq(10))
  )
  result_kv <- test_df %>%
    do_dist(skv = c("row", "col", "val"), diag=TRUE, cmdscale_k = 3, normalize=TRUE)

  expect_equal(ncol(result_kv), 4)
  expect_equal(result_kv[[2]], result_kv[[2]])
  expect_equal(result_kv[[3]], result_kv[[3]])
  expect_equal(result_kv[[4]], result_kv[[4]])
})

test_that("test do_dist.kv diag TRUE", {
  loadNamespace("dplyr")
  result <- (
    test_df %>%
      do_dist.kv(`ro w`, `co l`, val, diag=TRUE)
  )
  expect_equal(nrow(result), 16)
  expect_equal(result[[3]][1], 0)
})

test_that("test do_dist.kv without val", {
  loadNamespace("dplyr")

  test_df <- data.frame(
    subject = paste0("subject", rep(4-seq(3), each=3)),
    key = paste0("key", c(rep(3-seq(2), 4), 1)))

  result <- (
    test_df %>%
      do_dist.kv(subject, key)
  )

  expect_equal(result[[3]][1:2], c(0, sqrt(2)))
})

test_that("test do_dist without val", {
  loadNamespace("dplyr")

  test_df <- data.frame(
    subject = paste0("subject", rep(4-seq(3), each=3)),
    key = paste0("key", c(rep(3-seq(2), 4), 1)))

  result <- (
    test_df %>%
      do_dist( skv = c("subject", "key") )
  )

  expect_equal(result[[3]][1:2], c(0, sqrt(2)))
})

test_that("test do_dist.cols", {
  loadNamespace("dplyr")

  test_df <- data.frame(var1=c(1,2,2,2), var2=c(2,1,1,1))

  result <- (
    test_df %>%
      do_dist.cols(dplyr::starts_with("var"))
  )

  expect_equal(result$value, c(2,2))
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

test_that("do_dist with NA values", {
  loadNamespace("reshape2")
  nrow <- 10
  ncol <- 20
  vec <- rnorm(nrow * ncol)
  mat <- matrix(vec, nrow = nrow)
  melt_mat <- reshape2::melt(mat)
  # test column name with space
  colnames(melt_mat)[[2]] <- "Var 2"

  ret <- do_dist(melt_mat, skv = c("Var 2", "Var1", "value"), diag = TRUE)

  dist_ret <- as.matrix(dist(t(mat)))
  melt_ret <- reshape2::melt(dist_ret)

  for(i in seq(ncol)){
    for(j in seq(ncol)){
      mat_answer <- dist_ret[i, j]
      df_answer <- ret[ret[[1]] == i & ret[[2]] == j, 3][[1]]
      expect_equal(mat_answer, df_answer)
    }
  }
})
