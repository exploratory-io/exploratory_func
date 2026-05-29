context("test build_catboost")

test_that("catboost helper detects categorical predictors as zero-based indices", {
  df <- data.frame(
    num = c(1, 2, 3),
    chr = c("a", "b", "a"),
    fac = factor(c("x", "y", "x")),
    int = c(1L, 2L, 3L),
    stringsAsFactors = FALSE
  )

  ret <- exploratory:::catboost_prepare_predictor_frame(df, c("num", "chr", "fac", "int"))

  expect_equal(ret$cat_features, c(1L, 2L))
  expect_true(is.factor(ret$data$chr))
  expect_true(is.factor(ret$data$fac))
  expect_false(is.factor(ret$data$num))
  expect_false(is.factor(ret$data$int))
})

test_that("catboost helper routes bootstrap parameters", {
  bayesian <- exploratory:::catboost_build_params(
    classification_type = "binary",
    iterations = 5,
    od_wait = 2,
    depth = 3,
    learning_rate = 0.1,
    l2_leaf_reg = 3,
    border_count = 32,
    rsm = 1,
    boosting_type = "Plain",
    bootstrap_type = "Bayesian",
    bagging_temperature = 2,
    subsample = 0.7,
    task_type = "CPU",
    devices = NULL,
    random_strength = 1,
    leaf_estimation_method = "Newton",
    leaf_estimation_iterations = NULL,
    score_function = "Cosine",
    output_type_regression = "regression",
    eval_metric_regression = "RMSE",
    output_type_binary = "probability",
    eval_metric_binary = "AUC",
    output_type_multiclass = "softprob",
    eval_metric_multiclass = "MultiClass",
    seed = 1
  )

  expect_equal(bayesian$bootstrap_type, "Bayesian")
  expect_equal(bayesian$bagging_temperature, 2)
  expect_null(bayesian$subsample)

  bernoulli <- exploratory:::catboost_build_params(
    classification_type = "binary",
    iterations = 5,
    od_wait = 2,
    depth = 3,
    learning_rate = 0.1,
    l2_leaf_reg = 3,
    border_count = 32,
    rsm = 1,
    boosting_type = "Plain",
    bootstrap_type = "Bernoulli",
    bagging_temperature = 2,
    subsample = 0.7,
    task_type = "CPU",
    devices = NULL,
    random_strength = 1,
    leaf_estimation_method = "Newton",
    leaf_estimation_iterations = NULL,
    score_function = "Cosine",
    output_type_regression = "regression",
    eval_metric_regression = "RMSE",
    output_type_binary = "probability",
    eval_metric_binary = "AUC",
    output_type_multiclass = "softprob",
    eval_metric_multiclass = "MultiClass",
    seed = 1
  )

  expect_equal(bernoulli$bootstrap_type, "Bernoulli")
  expect_equal(bernoulli$subsample, 0.7)
  expect_null(bernoulli$bagging_temperature)
})

test_that("catboost helper extracts weights without treating them as predictors", {
  df <- data.frame(
    y = c(1, 2, 3),
    x = c(4, 5, 6),
    w = c(0.5, 1.0, 2.0)
  )
  mf <- do.call(
    model.frame,
    list(terms(y ~ x), data = df, weights = substitute(w), na.action = na.pass)
  )

  ret <- exploratory:::catboost_extract_model_frame_parts(mf, "y")

  expect_equal(ret$predictor_cols, "x")
  expect_equal(ret$weights, df$w)
})

test_that("catboost prediction type infers direct fml regression models from loss function", {
  model <- list(params = list(loss_function = "RMSE"))
  class(model) <- c("fml_catboost", "catboost_exp")

  expect_equal(exploratory:::catboost_prediction_type(model, type = "prob"), "RawFormulaVal")
  expect_equal(exploratory:::catboost_prediction_type(model, type = "response"), "RawFormulaVal")

  model$params$loss_function <- "RMSE:use_weights=false"
  expect_equal(exploratory:::catboost_prediction_type(model, type = "prob"), "RawFormulaVal")
})

test_that("catboost helper guards incompatible ordered rsm and score function settings", {
  params <- expect_warning(
    exploratory:::catboost_build_params(
      classification_type = "regression",
      iterations = 5,
      od_wait = 2,
      depth = 3,
      learning_rate = 0.1,
      l2_leaf_reg = 3,
      border_count = 32,
      rsm = 0.5,
      boosting_type = "Ordered",
      bootstrap_type = "No",
      bagging_temperature = 2,
      subsample = 0.7,
      task_type = "GPU",
      devices = "0:1",
      random_strength = 1,
      leaf_estimation_method = "Gradient",
      leaf_estimation_iterations = 3,
      score_function = "NewtonL2",
      output_type_regression = "regression",
      eval_metric_regression = "RMSE",
      output_type_binary = "probability",
      eval_metric_binary = "AUC",
      output_type_multiclass = "softprob",
      eval_metric_multiclass = "MultiClass",
      seed = 1
    ),
    "coercing `score_function` to 'Cosine'"
  )

  expect_equal(params$rsm, 1)
  expect_equal(params$score_function, "Cosine")
  expect_equal(params$leaf_estimation_iterations, 3)
  expect_equal(params$task_type, "GPU")
  expect_equal(params$devices, "0:1")
  expect_null(params$bagging_temperature)
  expect_null(params$subsample)
})

test_that("exp_catboost builds a binary model with categorical predictors", {
  testthat::skip_if_not_installed("catboost")

  set.seed(1)
  df <- data.frame(
    y = rep(c(TRUE, FALSE), 40),
    num = rnorm(80),
    grp = factor(rep(letters[1:4], 20)),
    chr = rep(c("east", "west"), 40),
    stringsAsFactors = FALSE
  )

  ret <- df %>%
    exp_catboost(
      y, num, grp, chr,
      iterations = 5,
      depth = 2,
      learning_rate = 0.2,
      eval_metric_binary = "Logloss",
      max_pd_vars = 1
    )

  model <- ret$model[[1]]
  expect_s3_class(model, "catboost_exp")
  expect_s3_class(model, "catboost_binary")
  expect_equal(model$call_info$categorical_features, 2L)
  expect_true(is.data.frame(tidy(model, type = "importance")))
  expect_true(is.data.frame(tidy(model, type = "evaluation_log")))
})
