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
  # it seems result_kv can be 3 or 2 dimensional with this data,
  # since axis3 gets near 0.
  #
  # 3 dimensional result (got on Mac).
  #
  # # A tibble: 4 x 4
  #   row    axis1        axis2         axis3
  #   <chr>  <dbl>        <dbl>         <dbl>
  # 1 row 1 -22.0   0.000000613  0           
  # 2 row 2  -7.35 -0.000000167  0.0000000359
  # 3 row 3   7.35  0.000000167  0.000000111 
  # 4 row 4  22.0   0.000000501 -0.0000000250
  #
  # 2 dimensional result (got on Linux).
  #
  # # A tibble: 4 x 3
  #   row    axis1        axis2
  #   <chr>  <dbl>        <dbl>
  # 1 row 1 -22.0   0.000000613
  # 2 row 2  -7.35 -0.000000167
  # 3 row 3   7.35  0.000000167
  # 4 row 4  22.0   0.000000501

  result_cols <- test_df %>%
    tidyr::spread(col, val) %>% dplyr::select(-row) %>%
    do_dist(dplyr::everything(), diag=TRUE, cmdscale_k = 3)

  expect_true(ncol(result_kv) %in% c(4,3))
  expect_equal(ncol(result_cols), 4)

  # this expectation used to be like following, but added abs to allow results with flipped sign,
  # which is also correct. For some reason this happens on windows 32bit.
  # expect_equal(result_kv[[2]], c(-22.045408, -7.348469, 7.348469, 22.045408), tolerance=0.01)
  expect_equal(abs(result_kv[[2]]), c(22.045408, 7.348469, 7.348469, 22.045408), tolerance=0.01)
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
  expect_equal(result_kv[[2]], c(-0.124,-1.16, -1.61, 2.89), tolerance=0.01)
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

  test_df <- data.frame(var1=c(1,2,2,2), var2=c(2,1,1,1), var3=c(0,0,2,3))

  result <- (
    test_df %>%
      do_dist.cols(dplyr::starts_with("var"))
  )

  expect_equal(sum(!is.na(result$value)), 6) # Elements other than diagonal.
})

test_that("test do_dist.cols with only lower triangle", {
  loadNamespace("dplyr")

  test_df <- data.frame(var1=c(1,2,2,2), var2=c(2,1,1,1), var3=c(0,0,2,3))

  result <- (
    test_df %>%
      do_dist.cols(dplyr::starts_with("var"), distinct=TRUE)
  )

  expect_equal(sum(!is.na(result$value)), 3) # Only lower triangle elements.
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

kl_test_df <- data.frame(
  subject = rep(paste0("s", seq(3)), each = 3),
  key = rep(paste0("k", seq(3)), 3),
  val = c(1, 2, 3, 2, 1, 3, 3, 2, 1),
  stringsAsFactors = FALSE)

test_that("test do_kl_dist.kv_", {
  loadNamespace("dplyr")
  result <- kl_test_df %>%
    do_kl_dist.kv_("subject", "key", "val")

  expect_equal(colnames(result), c("subject.x", "subject.y", "value"))
  # 3 subjects, no diagonal and no NA/zero rows -> 3 * 2 ordered pairs
  expect_equal(nrow(result), 6)
  expect_true(all(result[[1]] != result[[2]]))
  expect_true(all(result[["value"]] > 0))

  # KL distance as computed here is symmetric between the two subjects
  pair_value <- function(a, b) {
    result[result[[1]] == a & result[[2]] == b, ][["value"]]
  }
  expect_equal(pair_value("s1", "s2"), pair_value("s2", "s1"))
  expect_equal(pair_value("s1", "s3"), pair_value("s3", "s1"))
  # s1 and s2 differ in two keys, s1 and s3 in three, so s1-s3 is the larger gap
  expect_true(pair_value("s1", "s2") < pair_value("s1", "s3"))
})

test_that("test do_kl_dist.kv_ distinct TRUE", {
  loadNamespace("dplyr")
  distinct_result <- kl_test_df %>%
    do_kl_dist.kv_("subject", "key", "val", distinct = TRUE)
  full_result <- kl_test_df %>%
    do_kl_dist.kv_("subject", "key", "val", distinct = FALSE)

  # distinct = TRUE keeps the full n * n grid and puts NA outside the
  # strict upper triangle, so it is not a row subset of distinct = FALSE
  expect_equal(nrow(distinct_result), 9)
  expect_equal(colnames(distinct_result), c("subject.x", "subject.y", "value"))
  expect_equal(sum(!is.na(distinct_result[["value"]])), 3)

  kept <- distinct_result[!is.na(distinct_result[["value"]]), ]
  expect_true(all(kept[[1]] < kept[[2]]))
  for (i in seq(nrow(kept))) {
    same_pair <- full_result[[1]] == kept[[1]][i] & full_result[[2]] == kept[[2]][i]
    expect_equal(kept[["value"]][i], full_result[same_pair, ][["value"]])
  }
})

test_that("test do_kl_dist.kv_ for grouped data frame", {
  loadNamespace("dplyr")
  grouped_df <- dplyr::bind_rows(
    dplyr::mutate(kl_test_df, group = "g1"),
    dplyr::mutate(kl_test_df, group = "g2"))

  result <- grouped_df %>%
    dplyr::group_by(group) %>%
    do_kl_dist.kv_("subject", "key", "val")

  expect_true("group" %in% colnames(result))
  # each group is calculated independently and yields the ungrouped result
  expect_equal(nrow(result), 12)
  expect_equal(sort(unique(result[["group"]])), c("g1", "g2"))
  g1 <- result[result[["group"]] == "g1", ][["value"]]
  g2 <- result[result[["group"]] == "g2", ][["value"]]
  expect_equal(g1, g2)
})

test_that("test do_kl_dist.kv_ for grouped data frame as subject error", {
  loadNamespace("dplyr")
  expect_error({
    kl_test_df %>%
      dplyr::group_by(subject) %>%
      do_kl_dist.kv_("subject", "key", "val")
  })
})
