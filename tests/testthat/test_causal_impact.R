context("test causal_impact functions")

test_that("glance.bsts extracts model summary statistics", {
  # Create a mock bsts object with the summary data embedded
  mock_bsts <- structure(list(
    residual.sd = 0.5,
    prediction.sd = 0.8,
    rsquare = 0.92,
    size = c(1, 2, 3, 3.5, 4, 5)  # min, 1st qu, median, mean, 3rd qu, max
  ), class = "bsts")
  # Register summary.bsts as a proper S3 method so dispatch works
  registerS3method("summary", "bsts", function(x, ...) {
    list(
      residual.sd = x$residual.sd,
      prediction.sd = x$prediction.sd,
      rsquare = x$rsquare,
      size = x$size
    )
  })
  result <- glance.bsts(mock_bsts)
  expect_true(is.data.frame(result))
  expect_equal(result$residual_sd, 0.5)
  expect_equal(result$prediction_sd, 0.8)
  expect_equal(result$r_square, 0.92)
  expect_equal(result$n_coef_min, 1)
  expect_equal(result$n_coef_1st_quartile, 2)
  expect_equal(result$n_coef_median, 3)
  expect_equal(result$n_coef_mean, 3.5)
  expect_equal(result$n_coef_3rd_quartile, 4)
  expect_equal(result$n_coef_max, 5)
})

test_that("tidy.bsts extracts and renames coefficients", {
  coef_mat <- matrix(c(0.5, 0.1, 0.8, 0.3, 0.05, 0.6), nrow = 2, ncol = 3)
  rownames(coef_mat) <- c("market_A", "market_B")
  colnames(coef_mat) <- c("mean.inc", "sd.inc", "inc.prob")
  mock_bsts <- structure(list(coefficients = coef_mat), class = "bsts")
  # Register summary.bsts as a proper S3 method so dispatch works
  registerS3method("summary", "bsts", function(x, ...) {
    list(coefficients = x$coefficients)
  })
  result <- tidy.bsts(mock_bsts)
  expect_true(is.data.frame(result))
  expect_true("market" %in% colnames(result))
  expect_true("mean_when_included" %in% colnames(result))
  expect_true("sd_when_included" %in% colnames(result))
  expect_true("include_prob" %in% colnames(result))
  expect_equal(nrow(result), 2)
  expect_equal(result$market, c("market_A", "market_B"))
  expect_equal(result$mean_when_included, c(0.5, 0.1))
})
