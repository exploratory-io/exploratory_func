test_that("LightGBM metric aliases are expanded (MAE <-> l1)", {
  # internal helper in build_lightgbm.R
  f <- exploratory:::expand_lightgbm_metric_aliases
  expect_true(is.function(f))

  expect_equal(sort(f("mae")), sort(c("mae", "l1")))
  expect_equal(sort(f("l1")), sort(c("l1", "mae")))

  expect_equal(sort(f("binary_error")), sort(c("binary_error", "error")))
  expect_equal(sort(f("error")), sort(c("error", "binary_error")))

  # keep unknown metrics as-is
  expect_equal(f("foo_metric"), "foo_metric")
})


