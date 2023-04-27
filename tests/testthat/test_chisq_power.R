context("test chisq power analysis")
test_that("chisq AB test effect size calculation", {
  res <- exploratory:::calculate_cohens_w_for_ab_test(0.5, 0.3, 0.02)
  expect_equal(res, 0.02182179, tolerance=0.0001)
})

test_that("chisq AB test power analysis", {
  model_df <- airquality %>% exploratory:::exp_chisq_power_for_ab_test(a_ratio=0.5, conversion_rate=0.1, diff=0.01, sig.level = 0.05, beta = 0.2)
  expect_true(!is.null(model_df$model))
  res <- model_df %>% tidy_rowwise(model, type="summary")
  # For the UI, "Degree of Freedom" will be stripped out since it is always 1, but at the R level, we output it.
  expect_equal(colnames(res), c("Probability of Type 1 Error","Probability of Type 2 Error","Power","Effect Size (Cohen's w)","Degree of Freedom","Required Sample Size"))
  expect_equal(res$`Required Sample Size`, 28255.9, tolerance=0.1)
  res <- model_df %>% tidy_rowwise(model, type="n_to_power")
  expect_equal(colnames(res), c("n","power"))
  res <- model_df %>% tidy_rowwise(model, type="density")
  expect_equal(colnames(res), c("x","y","type","statistic","critical","df","ncp"))
})

test_that("chisq test power analysis", {
  model_df <- airquality %>% exp_chisq_power(rows = 3, cols = 2, w = 0.1, sig.level = 0.05, beta = 0.1)
  expect_true(!is.null(model_df$model))
  res <- model_df %>% tidy_rowwise(model, type="summary")
  expect_equal(colnames(res), c("Probability of Type 1 Error","Probability of Type 2 Error","Power","Effect Size (Cohen's w)","Degree of Freedom","Required Sample Size"))
  res <- model_df %>% tidy_rowwise(model, type="n_to_power")
  expect_equal(colnames(res), c("n","power"))
  res <- model_df %>% tidy_rowwise(model, type="density")
  expect_equal(colnames(res), c("x","y","type","statistic","critical","df","ncp"))
})
