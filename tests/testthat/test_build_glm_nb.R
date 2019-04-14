context("test build_glm_nb")

checkSummaryOutput <- function(model_ret){
  expect_equal(colnames(model_ret), c(".test_index", "source.data", "model", ".model_metadata"))

  res <- capture.output(summary(model_ret$model[[1]]))
  expect_lt(length(res), 50)
}

test_that("test build_glm with negative.binomial family(class family)", {
  trial <- MASS::quine %>%
    build_glm(Days ~ Sex/(Age + Eth*Lrn),
              family = MASS::negative.binomial(theta = 1))
  checkSummaryOutput(trial)
})

test_that("test build_glm with negative.binomial family(class character)", {
  trial <- MASS::quine %>%
    build_glm(Days ~ Sex/(Age + Eth*Lrn),
              family = "negative.binomial")
  checkSummaryOutput(trial)
  
})

test_that("test build_glm with negative.binomial family(class function)", {
  trial <- MASS::quine %>%
    build_glm(Days ~ Sex/(Age + Eth*Lrn),
              family = MASS::negative.binomial)
  
  checkSummaryOutput(trial)
})

test_that("test build_glm (nb) with init theta params", {
  trial <- MASS::quine %>%
    build_glm(Days ~ Sex/(Age + Eth*Lrn),
              family = MASS::negative.binomial(theta=500))
  family_arg <- trial$model[[1]]$call %>% as.list() %>% .$family

  expect_equal(family_arg$theta, 500)
  expect_equal(family_arg %>% as.character() %>% `[`(1),
               "MASS::negative.binomial")
 })

test_that("test build_glm (nb) with keep.source FALSE", {
  trial <- MASS::quine %>%
    build_glm(Days ~ Sex/(Age + Eth*Lrn),
              family = "negative.binomial",
              keep.source = F)
  expect_equal(colnames(trial), c(".test_index", "model", ".model_metadata"))
})

test_that("test build_glm (nb) with grouped", {
  trial <- MASS::quine %>%
    build_glm(Days ~ Sex/Age,
              group_cols = c("Eth", "Lrn"),
              family = "negative.binomial")
  expect_equal(length(trial[["Eth"]]), 4)
  expect_equal(length(trial[["Lrn"]]), 4)

  expect_error(build_glm(MASS::quine, Days ~ Sex/(Age + Eth), group_cols = c("Eth", "Lrn")),
               "Eth is a grouping column. Please remove it from variables.")
})

test_that("test build_glm (nb) with augment TRUE", {
   trial <- MASS::quine %>%
    build_glm(Days ~ Sex/Age,
              group_cols = c("Eth", "Lrn"),
              family = "negative.binomial",
              augment = TRUE)
  expect_equal(length(trial[["Eth"]]), 146)
  expect_equal(length(trial[["Lrn"]]), 146)
})

test_that("test build_glm (nb) name conflict avoid", {
  test_df <- MASS::quine %>%
    dplyr::rename(estimate = Eth,
                  model = Lrn) %>%
    dplyr::mutate(model.group = model)
  model <- test_df %>%
    build_glm(Days ~ Sex/Age,
              group_cols = c("estimate", "model", "model.group"),
              family = "negative.binomial")
  expect_equal(colnames(model),
               c("estimate.group", "model.group",
                 "model.group1", "source.data", ".test_index",
                 "model", ".model_metadata"))
})

test_that("predict glm.nb with new data", {
  model <- MASS::quine %>%
    build_glm(Days ~ Sex,
              group_cols = "Lrn",
              family = MASS::negative.binomial)
  coef_ret <- model %>% model_coef()
  expect_equal(colnames(coef_ret),
               c("Lrn", "term", "estimate", "std_error",
                 "t_ratio", "p_value", "odds_ratio"))
  stats_ret <- model %>% model_stats()
  expect_equal(colnames(stats_ret),
               c("Lrn", "null_deviance",
                 "df_for_null_model", "log_likelihood",
                 "aic", "bic", "deviance", "residual_df",
                 "Sex_base"))
})

test_that("prediction glm.nb with categorical columns", {
  test_data <- MASS::quine %>%
    dplyr::mutate(Sex = as.integer(Sex)) %>%
    { dplyr::bind_rows(., .) }

  model <- build_glm(test_data,
                     Sex ~ Age + Lrn + Days,
                     family = "negative.binomial",
                     test_rate = 0.6)
  ret <- prediction(model, data = "test",
                    type.predict = "response")
  test_ret <- prediction(model, data = "test")
  train_ret <- prediction(model, data = "training")
  expect_true(nrow(ret) > 0)
  expect_true(all(ret["predicted_value"] >= 1 & ret["predicted_value"] <= 2))
  expect_equal(colnames(test_ret),
               c("Eth", "Sex", "Age",
                 "Lrn", "Days", "predicted_value",
                 "standard_error", "conf_low", "conf_high",
                 "predicted_response"))
  expect_equal(colnames(train_ret),
               c("Eth", "Sex", "Age",
                 "Lrn", "Days", "predicted_value",
                 "standard_error", "conf_low", "conf_high",
                 "residuals", "hat", "residual_standard_deviation",
                 "cooks_distance", "standardised_residuals",
                 "predicted_response"))
})

test_that("test prediction binary glm.nb", {
  test_data <- MASS::quine %>%
    dplyr::mutate(Sex = as.integer(Sex)) %>%
    { dplyr::bind_rows(., .) }

  model <- build_glm(test_data,
                     Sex ~ Age + Lrn + Days,
                     family = "negative.binomial",
                     test_rate = 0.6)
  coef_ret <- model %>% model_coef()
  expect_true(!is.null(coef_ret[["odds_ratio"]]))
  prediction_train_ret <- prediction_binary(model, data = "training")
  expect_true("predicted_label" %in% colnames(prediction_train_ret))
  expect_true("predicted_value" %in% colnames(prediction_train_ret))
  expect_true("predicted_probability" %in% colnames(prediction_train_ret))

 })

