context("test exp_survival_forest")

test_that("test exp_survival_forest", {
  df <- survival::lung # this data has NAs.
  df <- df %>% mutate(status = status==2)
  df <- df %>% rename(`ti me`=time, `sta tus`=status, `a ge`=age, `se-x`=sex)
  df <- df %>% mutate(ph.ecog = factor(ph.ecog, ordered=TRUE)) # test handling of ordered factor
  df <- df %>% mutate(`se-x` = `se-x`==1) # test handling of logical
  model_df <- df %>% exp_survival_forest(`ti me`, `sta tus`, `a ge`, `se-x`, ph.ecog, ph.karno, pat.karno, meal.cal, wt.loss, predictor_n = 2)
  ret <- model_df %>% broom::augment(model)
  ret <- model_df %>% broom::tidy(model, type='partial_dependence_survival_curve')
  expect_equal(class(model_df$model[[1]]), c("ranger_survival_exploratory", "ranger"))
  ret <- model_df %>% broom::tidy(model, type='partial_dependence')
  ret <- model_df %>% broom::tidy(model, type='importance')
})

test_that("test exp_survival_forest with outtlier filtering", {
  df <- survival::lung # this data has NAs.
  df <- df %>% mutate(status = status==2)
  df <- df %>% rename(`ti me`=time, `sta tus`=status, `a ge`=age, `se-x`=sex)
  df <- df %>% mutate(ph.ecog = factor(ph.ecog, ordered=TRUE)) # test handling of ordered factor
  df <- df %>% mutate(`se-x` = `se-x`==1) # test handling of logical
  model_df <- df %>% exp_survival_forest(`ti me`, `sta tus`, `a ge`, `se-x`, ph.ecog, ph.karno, pat.karno, meal.cal, wt.loss, predictor_n = 2, predictor_outlier_filter_type = 'percentile', predictor_outlier_filter_threshold = 0.95)
  ret <- model_df %>% broom::augment(model)
  ret <- model_df %>% broom::tidy(model, type='partial_dependence_survival_curve')
  expect_equal(class(model_df$model[[1]]), c("ranger_survival_exploratory", "ranger"))
  ret <- model_df %>% broom::tidy(model, type='partial_dependence')
  ret <- model_df %>% broom::tidy(model, type='importance')
})

test_that("exp_survival_forest error handling for predictor with single unique value", {
  expect_error({
    df <- survival::lung # this data has NAs.
    df <- df %>% mutate(status = status==2)
    df <- df %>% mutate(age = 50) # Test for single unique value error handling.
    df <- df %>% rename(`ti me`=time, `sta tus`=status, `a ge`=age, `se-x`=sex)
    df <- df %>% mutate(ph.ecog = factor(ph.ecog, ordered=TRUE)) # test handling of ordered factor
    df <- df %>% mutate(`se-x` = `se-x`==1) # test handling of logical
    model_df <- df %>% exp_survival_forest(`ti me`, `sta tus`, `a ge`)
  }, "Invalid Predictors: Only one unique value.")
})

# Note: we used to have Japanese column name test, but removed since
# it was not a simple matter to make it work on Windows where we need to use
# SJIS. We test multibyte column names with other test suite.
