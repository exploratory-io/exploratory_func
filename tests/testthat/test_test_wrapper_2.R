
context("tests for wrappers of tests")

test_that("test exp_wilcox", {
  mtcars2 <- mtcars
  mtcars2$am[[1]] <- NA # test NA filtering
  model_df <- exp_wilcox(mtcars2, mpg, am)
  ret <- model_df %>% tidy(model, type="model")
  ret <- model_df %>% tidy(model, type="data_summary")
  ret
})

test_that("test exp_wilcox with conf.int = TRUE", {
  mtcars2 <- mtcars
  mtcars2$am[[1]] <- NA # test NA filtering
  model_df <- exp_wilcox(mtcars2, mpg, am, conf.int=TRUE)
  ret <- model_df %>% tidy(model, type="model")
  ret <- model_df %>% tidy(model, type="data_summary")
  ret
})

test_that("test exp_wilcox with paired = TRUE", {
  # Make sample size equal between groups for paired t-test.
  mtcars2 <- mtcars %>% group_by(am) %>% sample_n(6) %>% ungroup()
  model_df <- exp_wilcox(mtcars2, mpg, am, paired=TRUE)
  ret <- model_df %>% tidy(model, type="model")
  ret <- model_df %>% tidy(model, type="data_summary")
  ret
})

test_that("test exp_wilcox with paired = TRUE, conf.int = TRUE", {
  # Make sample size equal between groups for paired t-test.
  mtcars2 <- mtcars %>% group_by(am) %>% sample_n(6) %>% ungroup()
  model_df <- exp_wilcox(mtcars2, mpg, am, paired=TRUE, conf.int = TRUE)
  ret <- model_df %>% tidy(model, type="model")
  ret <- model_df %>% tidy(model, type="data_summary")
  ret
})

