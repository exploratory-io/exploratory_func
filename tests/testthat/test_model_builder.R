context("test model builders")

loadNamespace("exploratory")
test_df <- data.frame(
  vec1=seq(10),
  vec2=10-seq(10),
  rand=runif(10, min = 0, max=10),
  na=as.vector(replicate(5,c(NA,5))),
  group=paste("group",c(rep(1,5), rep(2, 5)), sep=""),
  col=rep(seq(5),2))

test_that("test do_glm and broom tidy", {
  if(requireNamespace("broom")){
    result <- (
      test_df
      %>%
        do_glm(vec1~vec2)
      %>%
        broom::tidy(.model)
    )
    expect_equal(dim(result)[[1]], 2)
  }
})

test_that("test do_kmeans and broom::tidy", {
  if(requireNamespace("broom")){
    result <- (
      test_df
      %>%
        do_kmeans(vec1, vec2, rand, centers=2)
      %>%
        broom::tidy(.model)
    )
    expect_equal(dim(result)[[1]], 2)
  }
})

test_that("test do_kmeans with invalid argument", {
  if(requireNamespace("broom")){
    tryCatch({
      result <- (
        test_df
        %>%
          do_kmeans(vec1, vec2, rand, groups=2)
        %>%
          broom::tidy(.model)
      )
    }, error=function(e){
      expect_equal(e$message, "groups is undefined in the data frame and argument")
      return
    })
  }
})

test_that("test do_kmeans ignore NA rows", {
  if(requireNamespace("broom")){
    result <- (
      test_df
      %>%  do_kmeans(vec1, vec2, na, centers=2, keep.source=TRUE)
      %>%  broom::augment(.model, data=.source.data))
    expect_equal(dim(result)[[1]], 5)
  }
})

test_that("test do_kmeans ignore NA rows with grouped", {
  if(requireNamespace("broom")){
    loadNamespace("dplyr")
    result <- (
      test_df
      %>%  dplyr::group_by(group)
      %>%  do_kmeans(vec1, vec2, na, centers=1, keep.source=TRUE)
      %>%  broom::tidy(.model))
    expect_equal(dim(result)[[1]], 2)
  }
})

test_that("test do_kmeans ignore NA rows with grouped and keep.source=FALSE", {
  if(requireNamespace("broom")){
    loadNamespace("dplyr")
    result <- (
      test_df
      %>%  dplyr::group_by(group)
      %>%  do_kmeans(vec1, vec2, na, centers=1, keep.source=FALSE)
      %>%  broom::tidy(.model))
    expect_equal(dim(result)[[1]], 2)
  }
})

test_that("test compress_dimension", {
  if(requireNamespace("broom")){
    loadNamespace("dplyr")
    result <- (
      test_df
      %>%  compress_dimension(group, col, rand))
    expect_true(any(result[,1]=="group1"))
    expect_true(any(result[,2]==1))
  }
})

test_that("test compress_dimension of dimension", {
  if(requireNamespace("broom")){
    loadNamespace("dplyr")
    result <- (
      test_df
      %>%  compress_dimension(group, col, rand, type="dimension"))
    expect_true(any(result[,1]==1))
    expect_true(any(result[,2]==1))
  }
})
