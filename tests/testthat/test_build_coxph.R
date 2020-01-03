context("test build_coxph")

test_that("test build_coxph.fast", {
  df <- survival::lung # this data has NAs.
  df <- df %>% rename(`ti me`=time, `sta tus`=status, `a ge`=age, `se-x`=sex)
  df <- df %>% mutate(ph.ecog = factor(ph.ecog, ordered=TRUE)) # test handling of ordered factor
  df <- df %>% mutate(`se-x` = `se-x`==1) # test handling of logical
  model_df <- df %>% build_coxph.fast(`ti me`, `sta tus`, `a ge`, `se-x`, ph.ecog, ph.karno, pat.karno, meal.cal, wt.loss, predictor_n = 2)
  expect_equal(class(model_df$model[[1]]), c("coxph_exploratory","coxph"))
  ret <- model_df %>% broom::tidy(model)
  # Verify that base levels are not NA for `se-x` (testing - in the name) columns.
  ret2 <- ret %>% dplyr::filter(stringr::str_detect(term,"(se-x)")) %>% dplyr::summarize(na_count=sum(is.na(base.level)))
  expect_equal(ret2$na_count, 0)

  ret <- model_df %>% broom::glance(model, pretty.name=TRUE)
})

test_that("build_coxpy.fast() error handling for predictor with single unique value", {
  expect_error({
    df <- survival::lung # this data has NAs.
    df <- df %>% mutate(age = 50) # Test for single unique value error handling.
    df <- df %>% rename(`ti me`=time, `sta tus`=status, `a ge`=age, `se-x`=sex)
    df <- df %>% mutate(ph.ecog = factor(ph.ecog, ordered=TRUE)) # test handling of ordered factor
    df <- df %>% mutate(`se-x` = `se-x`==1) # test handling of logical
    model_df <- df %>% build_coxph.fast(`ti me`, `sta tus`, `a ge`, predictor_n = 2)
  }, "The selected predictor variables are invalid since they have only one unique values.")
})

# Note: we used to have Japanese column name test, but removed since
# it was not a simple matter to make it work on Windows where we need to use
# SJIS. We test multibyte column names with other test suite.
