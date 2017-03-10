context("test na_util")

test_that("test predict_na", {
  test_data <- data.frame(
    rep(c(NA, NA, seq(7), NA), 10),
    rep(c(10, seq(8), NA), 10),
    rep(c(NA, seq(8), NA), 10),
  )
  colnames(test_data) <- c("col 1", "col-2", "col_3")
  ret <- dplyr::mutate(test_data, model_func = xgboost_reg, formula = `label 1` ~ ., nrounds = 5)
  prediction_ret <- prediction(model_ret)
  expect_true(all(prediction_ret$predicted_label %in% c(5, 10, 15)))
})
