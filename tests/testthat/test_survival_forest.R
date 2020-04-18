context("test exp_survival_forest")

test_that("test exp_survival_forest", {
  df <- survival::lung # this data has NAs.
  #df <- df %>% rename(`ti me`=time, `sta tus`=status, `a ge`=age, `se-x`=sex)
  #df <- df %>% mutate(ph.ecog = factor(ph.ecog, ordered=TRUE)) # test handling of ordered factor
  #df <- df %>% mutate(`se-x` = `se-x`==1) # test handling of logical
  browser()
  model_df <- df %>% exp_survival_forest(`time`, `status`, `age`, `sex`, ph.ecog, ph.karno, pat.karno, meal.cal, wt.loss)
  #model_df <- df %>% exp_survival_forest(`ti me`, `sta tus`, `a ge`, `se-x`, ph.ecog, ph.karno, pat.karno, meal.cal, wt.loss, predictor_n = 2)
  browser()
  expect_equal(class(model_df$model[[1]]), c("ranger_survival_exploratory"))
  browser()
  ret <- model_df %>% broom::tidy(model, type='partial_dependence')
  browser()
  ret <- model_df %>% broom::tidy(model, type='importance')
  browser()
})

if(F){
test_that("exp_survival_forest error handling for predictor with single unique value", {
  expect_error({
    df <- survival::lung # this data has NAs.
    df <- df %>% mutate(age = 50) # Test for single unique value error handling.
    df <- df %>% rename(`ti me`=time, `sta tus`=status, `a ge`=age, `se-x`=sex)
    df <- df %>% mutate(ph.ecog = factor(ph.ecog, ordered=TRUE)) # test handling of ordered factor
    df <- df %>% mutate(`se-x` = `se-x`==1) # test handling of logical
    model_df <- df %>% exp_survival_forest(`ti me`, `sta tus`, `a ge`)
  }, "Invalid Predictors: Only one unique value.")
})
}

# Note: we used to have Japanese column name test, but removed since
# it was not a simple matter to make it work on Windows where we need to use
# SJIS. We test multibyte column names with other test suite.
