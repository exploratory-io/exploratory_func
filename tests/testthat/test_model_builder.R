context("test model builders")

loadNamespace("exploratory")
test_df <- data.frame(
  vec1=seq(10),
  vec2=10-seq(10),
  rand=runif(10, min = 0, max=10),
  na=as.vector(replicate(5,c(NA,5))),
  group=paste("group",c(rep(1,5), rep(2, 5)), sep=""),
  col=rep(seq(5),2))

test_that("test build_glm and broom tidy", {
  if(requireNamespace("broom")){
    result <- (
      test_df
      %>%
        build_glm(vec1~vec2)
      %>%
        broom::tidy(model)
    )
    expect_equal(dim(result)[[1]], 2)
  }
})

test_that("test build_glm and broom", {
  if(requireNamespace("broom")){
    result <- (
      test_df
      %>%
        build_glm(vec1~vec2, augment=TRUE)
    )
    expect_equal(nrow(result), 10)
    expect_equal(ncol(result), ncol(test_df)+7)
  }
})

test_that("test build_kmeans.cols and broom::tidy", {
  if(requireNamespace("broom")){
    result <- (
      test_df
      %>%
        build_kmeans.cols(vec1, vec2, rand, centers=2)
      %>%
        broom::tidy(model)
    )
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
  }
})

test_that("test build_kmeans.cols ignore NA rows", {
  if(requireNamespace("broom")){
    result <- (
      test_df
      %>%  build_kmeans.cols(vec1, vec2, na, centers=2, keep.source=TRUE)
      %>%  broom::augment(model, data=source.data))
    expect_equal(dim(result)[[1]], 5)
  }
})

test_that("test build_kmeans.cols ignore NA rows with grouped", {
  if(requireNamespace("broom")){
    loadNamespace("dplyr")
    result <- (
      test_df
      %>%  dplyr::group_by(group)
      %>%  build_kmeans.cols(vec1, vec2, na, centers=1, keep.source=TRUE)
      %>%  broom::tidy(model))
    expect_equal(dim(result)[[1]], 2)
  }
})

test_that("build_kmeans.kv augment=TRUE", {
  loadNamespace("dplyr")
  test_df <- data.frame(
    group=rep(paste("group", seq(2)), each=9),
    subject=rep(paste("sub", rep(seq(3), each=3)), each=2),
    key=rep(paste("dim", rep(seq(3))), each=2),
    value=seq(3), stringsAsFactors = F
  )
  result <- (
    test_df
    %>%  dplyr::group_by(group)
    %>%  build_kmeans.kv(subject, key, value, center=1, augment=TRUE)
  )
  expect_true(all(result[[".cluster"]] == 1))
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
  df <- readRDS("~/Downloads/123flight_source_0.rds")
  (df %>%  build_kmeans.kv(CARRIER, DEST_CITY_NAME,DISTANCE,keep.source=TRUE) %>%  augment_kmeans(model, data=source.data))
})

test_that("build_lda.kv", {
  loadNamespace("dplyr")
  input_df <- data.frame(
    document_title=c(rep("The War of the Worlds", 4), rep("Pride and Prejudice", 4)),
    token = c("this", "was", "the", "deputation", "this", "was", "invitation", "enough"),
    count = rep(1, 8)
  )
  ret <- build_lda.kv(input_df, document_title, token, count, k=2, augment=TRUE)
  expect_equal(colnames(ret), c(colnames(input_df), "topic"))
})
