context("build model test")

test_that("lm with select", {
  test_data <- data.frame(
    sin = sin(seq(10)),
    x = seq(10),
    y = c(seq(9), NA_real_),
    z = rep(letters[1:2], each = 5),
    weight = rep(c(3, 4), 5),
    group = rep(letters[1:2], 5)
  ) %>%
    dplyr::rename(`with space` = x)

  lm_model_df <- test_data %>%
    dplyr::group_by(group) %>%
    build_model_with_select(response = sin, `with space`, y, weight, model_func = stats::lm, params = list(na.action = na.omit, weights = log(weight)))
  expect_equal(nrow(lm_model_df), 2)
})

test_that("lm with select everything", {
  test_data <- data.frame(
    sin = sin(seq(10)),
    x = seq(10),
    y = c(seq(9), NA_real_),
    z = rep(letters[1:2], each = 5),
    weight = rep(c(3, 4), 5),
    group = rep(letters[1:2], 5)
  ) %>%
    dplyr::rename(`with space` = x)

  lm_model_df <- test_data %>%
    dplyr::group_by(group) %>%
    build_model_with_select(response = sin, dplyr::everything(), model_func = stats::lm, params = list(na.action = na.omit, weights = log(weight)))
  valid_formula <- vapply(lm_model_df$model, function(mod){
    all(all.vars(mod$formula) != "sin")
  }, FUN.VALUE = FALSE)
  expect_true(all(valid_formula))
})

test_that("test nnet build_model", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))
  model_ret <- nnet::multinom(CARRIER ~ DISTANCE, data = test_data)

  model_df <- build_model(test_data,
                          model_func = nnet::multinom,
                          formula = CARRIER ~ DISTANCE,
                          test_rate = 0.1,
                          seed=0)

  coef_ret <- model_coef(model_df)
  stats_ret <- model_stats(model_df)


  prediction_training_ret <- prediction(model_df, data = "training")
  prediction_ret <- prediction(model_df, data = "test")

  evaluate_ret <- evaluate_multi(prediction_ret, CARRIER, predicted_label)

  expect_equal(evaluate_ret[["misclassification_rate"]], 1)

})

test_that("loess", {
  test_data <- data.frame(
    sin = sin(seq(10)),
    x = seq(10),
    group = rep(letters[1:2], 5)
  )

  loess_model_df <- build_model(test_data, model_func = stats::loess, formula = sin ~ x)
  expect_equal(class(loess_model_df$model[[1]]), "loess")
})
