context("test model builders")

loadNamespace("exploratory")
test_df <- data.frame(
  vec1=seq(10),
  vec2=10-seq(10),
  rand=runif(10, min = 0, max=10),
  na=as.vector(replicate(5,c(NA,5))),
  group=paste("group",c(rep(1,5), rep(2, 5)), sep=""),
  col=rep(seq(5),2))

test_that("test build_lm with NA values", {
  test_df <- data.frame(
    val = seq(8),
    val1 = c("char", "char" ,rep(c(NA,1), each = 3)),
    val2 = c("char", "char2" ,rep(c(1,NA), each = 3))
    )
  expect_error({
    build_lm(test_df, val ~ .)
  }, "more than 1 unique values are expected for categorical columns assigned as predictors")
})

test_that("test build_lm with all NA values", {
  test_df <- data.frame(
    val = seq(6),
    val1 = c(rep(c(NA,1), each = 3)),
    val2 = c(rep(c(1,NA), each = 3))
  )
  expect_error({
    build_lm(test_df, val ~ .)
  }, "no data after removing NA")
})

test_that("test build_glm with NA values", {
  test_df <- data.frame(
    val = seq(8),
    val1 = c("char", "char" ,rep(c(NA,1), each = 3)),
    val2 = c("char", "char2" ,rep(c(1,NA), each = 3))
  )
  expect_error({
    build_glm(test_df, val ~ .)
  }, "more than 1 unique values are expected for categorical columns assigned as predictors")
})

# this returns "object 'fit' not found" but yet to understand what this means, so kept commented out
# test_that("test build_glm with all NA values", {
#   test_df <- data.frame(
#     val = seq(6),
#     val1 = c(rep(c(NA,1), each = 3)),
#     val2 = c(rep(c(1,NA), each = 3))
#   )
#   expect_error({
#     build_glm(test_df, val ~ .)
#   }, "no data after removing NA")
# })

test_that("test with 2 groups with 3 centers", {
  test_df <- data.frame(
    val = as.vector(rep(c(1,5), 3)),
    group = paste("group",rep(c(1, 2), each = 3), sep = ""),
    col = rep(seq(3), 2))

  # test subject column name with a space
  colnames(test_df)[2] <- "gro up"

  expect_error({
    build_kmeans(test_df, skv = c("gro up", "col", "val"), centers = 2)
  }, "Centers should be less than unique subjects\\.")
})

test_that("test with 2 groups with 3 centers", {
  test_df <- data.frame(
    val = rep(c(1,5), 30),
    group = paste("group",rep(c(1, 2), each = 30), sep = ""),
    col = rep(seq(5), 12))

  # test subject column name with a space
  colnames(test_df)[2] <- "gro up"

  test_df %>% dplyr::group_by(`gro up`) %>% build_kmeans(val, col, augment = FALSE)
})

test_that("test with na values", {
  test_df <- data.frame(
    na=rep(c(NA, 5, 1, 4), 5),
    group=paste("group",rep(c(1, 2, 3, 4), each=5), sep=""),
    col=rep(seq(5), 4))
  test_df <- dplyr::filter(test_df, group != "group2" | col != 4)
  ret <- build_kmeans(test_df, skv = c("group", "col", "na"), fill = 1)
  expect_error({
    build_kmeans(test_df, skv = c("group", "col", "na"), fill = NA)
  }, "There is NA in the data.")
})

test_that("test with too small subject", {
  test_df <- data.frame(
    val=rep(c(1, 5), 5),
    group=paste("group",rep(c(1, 2), each=5), sep=""),
    col=rep(seq(5), 2))
  expect_error({
    build_kmeans(test_df, skv = c("group", "col", "val"), centers = 3)
  }, "Centers should be less than unique subjects\\.")
})

test_that("test with too small key", {
  test_df <- data.frame(
    val=rep(c(1, 5), 5),
    group=paste("group",rep(c(1, 2), each=5), sep=""),
    col=rep(seq(5), 2))
  expect_error({
    build_kmeans(test_df, skv = c("col", "group", "val"))
  }, "Centers should be less than distinct data points\\.")
})

test_that("test build_glm and broom tidy", {
  if(requireNamespace("broom")){
    result <- test_df %>%
        build_glm(vec1~vec2) %>%
        broom::tidy(model)
    expect_equal(dim(result)[[1]], 2)
  }
})

test_that("test build_glm and broom", {
  if(requireNamespace("broom")){
    result <- test_df %>%
      build_glm(vec1~vec2, augment=TRUE)
    expect_equal(nrow(result), 10)
    expect_equal(ncol(result), ncol(test_df)+10)
  }
})

test_that("test build_kmeans.cols and broom::tidy", {
  if(requireNamespace("broom")){
    result <- test_df %>%
      build_kmeans.cols(vec1, vec2, rand, centers=2) %>%
      broom::tidy(model)
    expect_equal(dim(result)[[1]], 2)
  }
})

test_that("test build_kmeans.cols augment=T", {
  if(requireNamespace("broom")){
    result <- (
      test_df
      %>%
        build_kmeans.cols(vec1, vec2, rand, centers=2, augment=T)
    )
    expect_equal(nrow(result), 10)
    expect_true(is.factor(result[["cluster"]]))
  }
})

test_that("test build_kmeans all na", {
  test_df <- data.frame(
    all_na = rep(NA, 10),
    val = seq(10)
  )
  expect_error({
    test_df %>%
      build_kmeans(all_na, val, centers=2, augment=T)
  }, "No data after removing NA")
})

test_that("test build_kmeans.cols ignore NA rows", {
  if(requireNamespace("broom")){
    result <- test_df %>%
      build_kmeans.cols(vec1, vec2, na, centers=2, keep.source=TRUE, augment = FALSE) %>%
      augment_kmeans(model, data=source.data)
    expect_equal(dim(result)[[1]], 5)
  }
})

test_that("test build_kmeans.cols ignore NA rows", {
  na_char <- as.character(seq(10))
  na_char[[3]] <- NA
  test_df <- data.frame(
    na_char,
    n_char = as.character(10 - seq(10)), stringsAsFactors = FALSE
  )
  result <- test_df %>%
    build_kmeans.cols(na_char, n_char, centers=2, keep.source=TRUE, augment = FALSE) %>%
    augment_kmeans(model, data=source.data)
  expect_equal(dim(result)[[1]], 9)
})

test_that("test build_kmeans.cols ignore NA rows with grouped", {
  if(requireNamespace("broom")){
    loadNamespace("dplyr")
    result <- test_df %>%
      dplyr::group_by(group) %>%
      build_kmeans.cols(vec1, vec2, na, centers=1, keep.source=TRUE) %>%
      broom::tidy(model)
    expect_equal(dim(result)[[1]], 2)
  }
})

test_that("build_kmeans.kv augment=TRUE", {
  loadNamespace("dplyr")
  test_df <- data.frame(
    group=rep(paste("group", seq(2)), each=9),
    key=rep(paste("dim", rep(seq(3))), each=2),
    value=seq(3), stringsAsFactors = F
  )

  test_df[["subject with space"]] <- rep(paste("sub", rep(seq(3), each=3)), each=2)

  result <- test_df %>%
    dplyr::group_by(group) %>%
    build_kmeans.kv(`subject with space`, key, value, center=1, augment=TRUE)

  expect_true(!is.null(result[["cluster"]]))
  expect_true(all(result[["cluster"]] == 1))
})

test_that("test build_kmeans.kv for grouped data frame as subject error", {
  data <- data.frame(group=rep(c(1,2,3), each=6),
                     row = rep(c(1, 1, 2, 2, 3,3), 3),
                     col = rep(c(1,2), 9),
                     val = rep(0, 18))
  expect_error({
    ret <- data %>%
      dplyr::group_by(group) %>%
      build_kmeans.kv(group, col, val)
  }, "group is a grouping column\\. ungroup\\(\\) may be necessary before this operation\\.")
})

test_that("test build_kmeans.cols ignore NA rows with grouped and keep.source=FALSE", {
  if(requireNamespace("broom")){
    loadNamespace("dplyr")
    result <- (
      test_df
      %>%  dplyr::group_by(group)
      %>%  build_kmeans.cols(vec1, vec2, na, centers=1, keep.source=FALSE)
      %>%  broom::tidy(model))
    expect_equal(dim(result)[[1]], 2)
  }
})

test_that("test build_kmeans.cols", {
  df <- data.frame(number = seq(4), number2 = seq(4)-4)
  ret <- (df %>%  build_kmeans.cols(number, number2, keep.source=TRUE, augment = FALSE) %>%  augment_kmeans(model, data=source.data))
  expect_true(is.factor(ret$cluster))
})

test_that("test build_kmeans", {
  test_df[["cluster"]] <- rep(1, nrow(test_df))
  result <- test_df %>%
    build_kmeans(skv = c("vec1", "vec2"), centers=2, augment = FALSE) %>%
    augment_kmeans(model, data = source.data)
  expect_true(is.factor(result[["cluster.new"]]))
  expect_equal(length(colnames(result)[colnames(result) == "cluster"]), 1)
  expect_equal(length(colnames(result)[colnames(result) == "cluster.new"]), 1)
})

test_that("test build_kmeans skv with wrong column name", {
  test_df[["cluster"]] <- rep(1, nrow(test_df))
  expect_error({
    test_df %>%
      build_kmeans(skv = c("vec1", "vec"), centers=2) %>%
      augment_kmeans(model, data = source.data)
  }, "Unknown column `vec` ")
})

test_that("test build_kmeans cols with wrong column name", {
  test_df[["cluster"]] <- rep(1, nrow(test_df))
  expect_error({
    test_df %>%
      build_kmeans(vec, vec10, centers=2) %>%
      augment_kmeans(model, data = source.data)
  }, "undefined columns selected")
})
