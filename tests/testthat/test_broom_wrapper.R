loadNamespace("broom")
context("test broom wrappers")

set.seed(0)
test_df <- data.frame(vec1 = seq(10), vec2 = seq(10), random = runif(10, min=0, max=10))

test_that("test augment_lm newdata argument", {
  result <- (
    test_df
    %>%  build_lm(vec1~random)
    %>%  augment_lm(model, newdata=data.frame(random=c(1,3)))
  )
  expect_equal(dim(result)[[1]], 2)
})

test_that("test if tidy_lm arguments work", {
  result <- (
    test_df
    %>%  build_lm(vec1~random)
    %>%  tidy_lm(model)
  )
  result_ <- (
    test_df
    %>%  build_lm(vec1~log(random))
    %>%  tidy_lm(model, conf.int = TRUE, conf.level = 0.5, quick=TRUE)
  )
  expect_true(all(result[["estimate"]] != result_[["estimate"]]))
  expect_equal(ncol(result_), 2)
})

test_that("do_kmeans.kv augment", {
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
    %>%  build_kmeans.kv(subject, key, value, keep.source=TRUE, centers=1, augment = FALSE)
    %>%  predict(model, source.data)
  )
  expect_true(is.integer(result[["cluster"]]))
  expect_true(all(result[["cluster"]] == 1))
})

test_that("do_kmeans.kv augment", {
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
    %>%  build_kmeans.kv(subject, key, value, keep.source=TRUE, centers=1, augment = FALSE)
    %>%  augment_kmeans(model, source.data)
  )
  expect_true(is.integer(result[["cluster"]]))
  expect_true(all(result[["cluster"]] == 1))
})

test_that("predict lm with new data", {
  loadNamespace("dplyr")
  fit_df <- data.frame(
    model=rep(paste("group", seq(2)), each=9),
    num1 = seq(18),
    num2 = 30-seq(18)
  )
  add_df <- data.frame(
    group=rep(paste("group", seq(2)), each=10),
    num1 = seq(20),
    num2 = 30-seq(20)
  )

  model_data <- fit_df %>% dplyr::group_by(model) %>% build_lm(num1 ~ num2)

  fit <- add_df %>% dplyr::group_by(group) %>% predict(model.new, model_df = model_data)

  expect_equal(nrow(fit), 20 * 2)
  expect_equal(names(fit), c("model", "group", "num1", "num2", ".fitted", ".se.fit"))
})
