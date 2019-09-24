
context("tests for wrappers of Kraskal-Wallis tests")

test_that("test exp_kruskal", {
  model_df <- exp_kruskal(mtcars, mpg, gear)
  ret <- model_df %>% tidy(model, type="model")
  ret <- model_df %>% tidy(model, type="data_summary")
  ret
})

test_that("test exp_kruskal with group_by", {
  model_df <- mtcars %>%
    group_by(am) %>%
    mutate(gear = factor(gear)) %>% exp_kruskal(mpg, gear)
  ret <- model_df %>% tidy(model, type="model")
  ret <- model_df %>% tidy(model, type="data_summary")
  ret
})

test_that("test exp_binom_test", {
  browser()
  model_df <- exp_binom_test(mtcars, am, p=0.5)
  browser()
  ret <- model_df %>% tidy(model, type="distribution")
  #ret <- model_df %>% tidy(model, type="model")
  browser()
  ret <- model_df %>% tidy(model, type="data")
  browser()
  ret
})
