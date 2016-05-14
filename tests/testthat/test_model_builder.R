context("test model builders")

loadNamespace("exploratory")
test_df <- data.frame(vec1=seq(10), vec2=10-seq(10), rand=runif(10, min = 0, max=10))

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
