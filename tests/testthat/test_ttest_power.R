context("test ttest power analysis")
test_that("ttest power analysis density plot data generation", {
  # Case where a_ratio is skewed.
  mtcars %>% exp_ttest_power(a_ratio = 0.01, alternative = "two.sided", paired = FALSE)
  # Case with paired test.
  mtcars %>% exp_ttest_power(a_ratio = 0.5, paired=TRUE)
  # Case with "less" alternative.
  mtcars %>% exp_ttest_power(a_ratio = 0.5, alternative = "less")

  res <- exploratory:::generate_ttest_density_data_for_power(0.2, 100, 100, 2, 100)

  model_df <- exp_ttest_power(mtcars, a_ratio=0.5, d=0.2, sig.level=0.05, beta=0.2, alternative="two.sided", n_start=10, n_end=1000, n_step=10)
  res <- model_df %>% tidy_rowwise(model, type="summary")
  expect_equal(as.numeric(res[1,]), c(0.0500, 0.2000, 0.8000, 0.2000, 784.8114, 786.8114), tolerance=0.0001)
  expect_equal(colnames(res), c("Type 1 Error","Type 2 Error","Power","Cohen's D","DF","Required Sample Size"))
  res <- model_df %>% tidy_rowwise(model, type="n_to_power")
  expect_equal(colnames(res), c("n","power"))
  res <- model_df %>% tidy_rowwise(model, type="density")
  expect_equal(colnames(res), c("x","y","ncp","type","critical","df","statistic"))
})
