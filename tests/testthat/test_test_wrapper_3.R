
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

