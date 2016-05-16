context("test model builders")

loadNamespace("exploratory")
test_df <- data.frame(
  vec1=seq(10),
  vec2=10-seq(10),
  rand=runif(10, min = 0, max=10),
  na=as.vector(replicate(5,c(NA,5))))

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
