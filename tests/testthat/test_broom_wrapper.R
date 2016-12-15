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
  result <- test_df %>%
    dplyr::group_by(group) %>%
    build_kmeans.kv(subject, key, value, keep.source=TRUE, centers=1, augment = FALSE) %>%
    predict(model, source.data)
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

  model_data <- fit_df %>% dplyr::group_by(model) %>% build_lm(num1 ~ num2, group_cols = "model")

  fit <- add_df %>% dplyr::group_by(group) %>% add_prediction(model_df = model_data)

  expect_equal(nrow(fit), 20 * 2)
  expect_equal(names(fit), c("model.group", "group", "num1", "num2", ".fitted", ".se.fit"))
})

test_that("predict lm with new data", {
  loadNamespace("dplyr")
  fit_df <- data.frame(
    group = rep(paste("group", seq(2)), each=15),
    num1 = seq(30),
    num2 = 10- (seq(30) %% 2)
  )

  model_data <- fit_df %>% build_lm(num1 ~ num2, group_cols = "group")

  coef_ret <- model_data %>% model_coef()
  expect_equal(colnames(coef_ret), c("group", "Term", "Estimate", "Std Error", "t Ratio", "Prob > |t|"))

  stats_ret <- model_data %>% model_stats()
  expect_equal(colnames(stats_ret), c("group", "RSquare", "RSquare Adj", "Root Mean Square Error",
                                      "F Ratio", "Prob > F", "Degree of Freedom", "Log Likelihood",
                                      "AIC", "BIC", "Deviance", "Residual Degree of Freedom"))

  anova_ret <- model_data %>% model_anova()
  expect_equal(colnames(anova_ret), c("group", "Term", "Degree of Freedom", "Sum of Squares", "Mean Square", "F Ratio", "Prob > F"))

  confint_ret <- model_data %>% model_confint(level = 0.99)
  expect_equal(colnames(confint_ret), c("group", "Term", "Prob 0.5", "Prob 99.5"))
})

test_that("assign_cluster", {
  test_df <- data.frame(
    na=rep(c(NA, 5, 1, 4), 5),
    group=paste("group",rep(c(1, 2, 3, 4), each=5), sep=""),
    col=rep(seq(5), 4))
  test_df <- dplyr::filter(test_df, group != "group2" | col != 4)
  ret <- build_kmeans(test_df, na, col, fill = 1, augment = FALSE, keep.source = FALSE)

  ret <- assign_cluster(ret, test_df)
  expect_true(is.numeric(ret[["cluster"]]))
})

test_that("cluster_data", {
  test_df_group1 <- data.frame(
    with_na_group1 = rep(c(NA, 5, 1, 4), 5),
    with_na_group2 = rep(c(4, 5, 1, 4), 5),
    g = 1
  )
  test_df_group2 <- data.frame(
    with_na_group1 = rep(c(4, 8, 1, 2), 5),
    with_na_group2 = rep(c(NA, 5, 1, 4), 5),
    g = 2
  )
  test_df <- dplyr::bind_rows(test_df_group1, test_df_group2)
  kmeans_ret <- build_kmeans(test_df, with_na_group1, with_na_group2, fill = 1, augment = FALSE, keep.source = FALSE, group_cols = "g")
  cluster_ret <- cluster_info(kmeans_ret)
  expect_equal(colnames(cluster_ret), c("g", "Center with_na_group1", "Center with_na_group2", "Size",
                                "Withinss"))

  ret <- kmeans_info(kmeans_ret)
  expect_equal(colnames(ret), c("g", "Total Sum of Squares", "Total Sum of Squares within Clusters",
                                "Total Sum of Squares between Clusters", "Number of Iterations"
  ))

  assigned_ret <- kmeans_ret %>% assign_cluster(test_df)
  expect_equal(colnames(assigned_ret), c("g", "with_na_group1", "with_na_group2", "cluster"))
})
