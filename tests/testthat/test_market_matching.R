context("test MarketMatching functions")

test_that("lagp lags vector by p with zero padding", {
  result <- exploratory:::lagp(c(1, 2, 3, 4), 2)
  expect_equal(result, c(0, 0, 1, 2))

  result2 <- exploratory:::lagp(c(10, 20, 30), 1)
  expect_equal(result2, c(0, 10, 20))

  result3 <- exploratory:::lagp(c(5, 6, 7, 8), 0)
  expect_equal(result3, c(5, 6, 7, 8))
})

test_that("CMean calculates mean excluding zeros", {
  expect_equal(exploratory:::CMean(c(0, 2, 4, 0)), 3)
  expect_equal(exploratory:::CMean(c(1, 2, 3)), 2)
  # All zeros returns 0
  expect_equal(exploratory:::CMean(c(0, 0, 0)), 0)
  expect_equal(exploratory:::CMean(c(0)), 0)
})

test_that("stopif stops when value equals clause", {
  expect_error(exploratory:::stopif(TRUE, TRUE, "should stop"), "should stop")
  expect_error(exploratory:::stopif(5, 5, "equal values"), "equal values")
  # Should not error when value != clause
  expect_silent(exploratory:::stopif(TRUE, FALSE, "should not stop"))
  expect_silent(exploratory:::stopif(1, 2, "should not stop"))
})

test_that("check_inputs validates parameters", {
  df <- data.frame(id = c("a", "b", "c"), date = as.Date("2020-01-01") + 0:2, value = 1:3)
  # Should not error with valid inputs
  expect_silent(exploratory:::check_inputs(data = df, id = "id", matching_variable = "value", date_variable = "date"))
  # Missing data
  expect_error(exploratory:::check_inputs(data = NULL, id = "id", matching_variable = "value", date_variable = "date"), "No data")
  # Missing id
  expect_error(exploratory:::check_inputs(data = df, id = NULL, matching_variable = "value", date_variable = "date"), "No ID")
  # Column not found
  expect_error(exploratory:::check_inputs(data = df, id = "nonexistent", matching_variable = "value", date_variable = "date"), "ID variable not found")
  # Need at least 3 unique markets
  df_small <- data.frame(id = c("a", "b"), date = as.Date("2020-01-01") + 0:1, value = 1:2)
  expect_error(exploratory:::check_inputs(data = df_small, id = "id", matching_variable = "value", date_variable = "date"), "at least 3")
})

test_that("mape_no_zeros calculates MAPE excluding zero test values", {
  test_vec <- c(1, 2, 0, 4)
  ref_vec <- c(1.1, 2.2, 0.5, 4.4)
  result <- exploratory:::mape_no_zeros(test_vec, ref_vec)
  expect_true(is.numeric(result))
  expect_true(result >= 0)
  # With no zeros, all values used
  test_vec2 <- c(1, 2, 3)
  ref_vec2 <- c(1, 2, 3)
  result2 <- exploratory:::mape_no_zeros(test_vec2, ref_vec2)
  expect_true(is.numeric(result2))
  expect_true(result2 >= 0)
})

test_that("dw calculates Durbin-Watson statistic", {
  y <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  yhat <- c(1.1, 1.9, 3.2, 3.8, 5.1, 5.9, 7.2, 7.8, 9.1, 9.9)
  result <- exploratory:::dw(y, yhat)
  expect_true(is.numeric(result))
  # DW statistic should be between 0 and 4
  expect_true(result >= 0)
  expect_true(result <= 4)
  # Perfect predictions should give DW near 2 (no autocorrelation)
  y_perfect <- 1:20
  yhat_perfect <- 1:20
  result_perfect <- exploratory:::dw(y_perfect, yhat_perfect)
  # With zero residuals, correlation is undefined, but function should return numeric
  expect_true(is.numeric(result_perfect))
})
