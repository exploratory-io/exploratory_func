
context("tests for wrappers of Kraskal-Wallis tests")

test_that("test exp_kruskal", {
  model_df <- exp_kruskal(mtcars, mpg, gear)
  ret <- model_df %>% tidy_rowwise(model, type="model")
  ret <- model_df %>% tidy_rowwise(model, type="data_summary")
  expect_equal(colnames(ret),
               c("gear","Number of Rows","Mean","Conf Low","Conf High","Std Error of Mean","Std Deviation",
                 "Minimum","Maximum"))
})

test_that("test exp_kruskal with group_by", {
  model_df <- mtcars %>%
    group_by(am) %>%
    mutate(gear = factor(gear)) %>% exp_kruskal(mpg, gear)
  ret <- model_df %>% tidy_rowwise(model, type="model")
  ret <- model_df %>% tidy_rowwise(model, type="data_summary")
  expect_equal(colnames(ret),
               c("am","gear","Number of Rows","Mean","Conf Low","Conf High","Std Error of Mean","Std Deviation",
                 "Minimum","Maximum"))
})

