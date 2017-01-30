context("test build_multinom")

test_that("test nnet build_model", {
  test_data <- data.frame(
    label = c(rep(letters[1:3], 12), NA, "a", "a"),
    num = seq(39),
    num2 = 39 - seq(39),
    weight = seq(39)/39,
    term = rep(letters[1:3], each = 13),
    stringsAsFactors = FALSE
  )

  model_df <- build_multinom(
    test_data,
    formula = label ~ num + num2,
    group_cols = "term",
    # weights = weight,
    test_rate = 0.4)
  expect_true(any(colnames(model_df) %in% "term.group"))

  coef_ret <- model_coef(model_df)
  stats_ret <- model_stats(model_df)

  expect_equal(nrow(coef_ret), 18)
  expect_equal(nrow(stats_ret), 3)


  prediction_training_ret <- prediction(model_df, data = "training")
  prediction_ret <- prediction(model_df, data = "test")

  evaluation_ret <- evaluate_multi(prediction_ret, predicted_label, label)

  expect_equal(nrow(evaluation_ret), 3)
  expect_equal(ncol(evaluation_ret), 4)

})
