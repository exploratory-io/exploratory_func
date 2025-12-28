test_that("LightGBM metric aliases are expanded (common cross-version aliases)", {
  # internal helper in build_lightgbm.R
  f <- exploratory:::expand_lightgbm_metric_aliases
  expect_true(is.function(f))

  expect_equal(sort(f("mae")), sort(c("mae", "l1")))
  expect_equal(sort(f("l1")), sort(c("l1", "mae")))

  expect_equal(sort(f("mse")), sort(c("mse", "l2")))
  expect_equal(sort(f("l2")), sort(c("l2", "mse")))

  expect_equal(sort(f("rmse")), sort(c("rmse", "l2_root", "l2root")))
  expect_equal(sort(f("l2_root")), sort(c("l2_root", "rmse", "l2root")))
  expect_equal(sort(f("l2root")), sort(c("l2root", "rmse", "l2_root")))

  expect_equal(sort(f("binary_logloss")), sort(c("binary_logloss", "logloss")))
  expect_equal(sort(f("logloss")), sort(c("logloss", "binary_logloss")))

  expect_equal(sort(f("binary_error")), sort(c("binary_error", "error")))
  expect_equal(sort(f("error")), sort(c("error", "binary_error")))

  expect_equal(sort(f("mlogloss")), sort(c("mlogloss", "multi_logloss")))
  expect_equal(sort(f("multi_logloss")), sort(c("multi_logloss", "mlogloss")))

  expect_equal(sort(f("merror")), sort(c("merror", "multi_error")))
  expect_equal(sort(f("multi_error")), sort(c("multi_error", "merror")))

  # keep unknown metrics as-is
  expect_equal(f("foo_metric"), "foo_metric")
})


