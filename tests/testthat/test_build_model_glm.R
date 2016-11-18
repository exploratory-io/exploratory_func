context("test build_model_glm")

test_that("test build_model_glm with subset and weight", {
  test_df <- data.frame(
    val = seq(8),
    val1 = c("char", "char" ,rep(c(4,1), each = 3)),
    val2 = c("char", "char2" ,rep(c(1,2), each = 3)),
    subset = seq(8) %% 2 == 0
  )
  weight <- seq(8)
  ret <- build_model_glm(test_df, val ~ ., subset = subset, weights = log(weight), output = "tidy")
  glm <- glm(val ~ ., data = test_df, subset = subset, weights = log(weight))
  answer <- tidy(glm)
  for (cindex in ncol(answer)) {
    expect_equal(ret[[cindex]], answer[[cindex]])
  }
})

test_that("test build_model_glm and broom tidy", {
  if(requireNamespace("broom")){
    result <- test_df %>%
      build_model_glm(vec1~vec2) %>%
      broom::tidy(model)
    expect_equal(dim(result)[[1]], 2)
  }
})

test_that("test build_model_glm and augment", {
  if(requireNamespace("broom")){
    result <- test_df %>%
      build_model_glm(vec1~vec2, output = "augment")
    expect_equal(nrow(result), 10)
    expect_equal(ncol(result), ncol(test_df)+7)
  }
})

test_that("test build_model_glm and tidy", {
  if(requireNamespace("broom")){
    result <- test_df %>%
      build_model_glm(vec1~vec2, output = "tidy")
    expect_equal(nrow(result), 2)
    expect_equal(ncol(result), 5)
  }
})

test_that("test build_model_glm and glance", {
  if(requireNamespace("broom")){
    result <- test_df %>%
      build_model_glm(vec1~vec2, output = "glance")
    expect_equal(nrow(result), 1)
    expect_equal(ncol(result), 7)
  }
})

test_that("test build_model_glm and anova", {
  if(requireNamespace("broom")){
    result <- test_df %>%
      build_model_glm(vec1~vec2, output = "anova")
    expect_equal(nrow(result), 2)
    expect_equal(ncol(result), 5)
  }
})

test_that("test build_model_glm and anova", {
  if(requireNamespace("broom")){
    result <- test_df %>%
      build_model_glm(vec1~vec2, output = "model") %>%
      tidy(model, matrix = "anova")
    expect_equal(nrow(result), 2)
    expect_equal(ncol(result), 5)
  }
})
