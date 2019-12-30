context("build model test")

test_that("test nnet build_model", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))
  test_data <- test_data %>% rename(`DIST ANCE`=DISTANCE)
  model_ret <- nnet::multinom(CARRIER ~ `DIST ANCE`, data = test_data)

  model_df <- build_model(test_data,
                          model_func = nnet::multinom,
                          formula = CARRIER ~ `DIST ANCE`,
                          test_rate = 0.1,
                          seed=1)

  coef_ret <- model_coef(model_df)
  stats_ret <- model_stats(model_df)


  prediction_training_ret <- prediction(model_df, data = "training")
  prediction_ret <- prediction(model_df, data = "test")

  evaluate_ret <- evaluate_multi(prediction_ret, CARRIER, predicted_label)

  expect_gte(evaluate_ret[["misclassification_rate"]], 0)
  expect_lte(evaluate_ret[["misclassification_rate"]], 1)

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
