context("test build_coxph")

test_that("test build_coxph.fast", {
  df <- survival::lung
  model_df <- df %>% build_coxph.fast(time, status, age, sex, ph.ecog, ph.karno, pat.karno, meal.cal, wt.loss)
  expect_equal(class(model_df$model[[1]]), c("coxph_exploratory","coxph"))
  ret <- model_df %>% broom::tidy(model)
  ret <- model_df %>% broom::glance(model, pretty.name=TRUE)
})

