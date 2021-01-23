context("test exp_survival_forest")

test_that("exp_survival_forest basic", {
  df <- survival::lung # this data has NAs.
  df <- df %>% mutate(status = status==2)
  df <- df %>% rename(`ti me`=time, `sta tus`=status, `a ge`=age, `se-x`=sex)
  df <- df %>% mutate(ph.ecog = factor(ph.ecog))
  df <- df %>% mutate(`se-x` = `se-x`==1) # test handling of logical
  model_df <- df %>% exp_survival_forest(`ti me`, `sta tus`, `a ge`, `se-x`, ph.ecog, ph.karno, pat.karno, meal.cal, wt.loss,
                                         predictor_funs=list(`a ge`="none", `se-x`="none", ph.ecog=rlang::expr(forcats::fct_relevel(.,"1")), ph.karno="none", pat.karno="none", meal.cal="none", wt.loss="none"), predictor_n = 2)
  ret <- model_df %>% prediction2(pretty.name=TRUE)
  ret <- df %>% select(-`ti me`, -`sta tus`) %>% add_prediction(model_df=model_df, pred_survival_time=5)
  ret <- model_df %>% evaluation()
  expect_false("Data Type" %in% colnames(ret))
  ret <- model_df %>% prediction2()
  ret2 <- ret %>% do_survival_roc_("Predicted Survival Rate","ti me","sta tus", at=NULL, grid=10, revert=TRUE, with_auc=TRUE)
  # Most of the time, true positive rate should be larger than false positive rate. If this is 
  expect_true(sum((ret2 %>% mutate(positive=true_positive_rate >= false_positive_rate))$positive) > 0.8 * nrow(ret2))

  ret <- model_df %>% glance_rowwise(model)
  ret <- model_df %>% augment_rowwise(model)

  ret <- model_df %>% tidy_rowwise(model, type='partial_dependence_survival_curve')
  expect_equal(class(model_df$model[[1]]), c("ranger_survival_exploratory", "ranger"))
  ret <- model_df %>% tidy_rowwise(model, type='partial_dependence')
  ret <- model_df %>% tidy_rowwise(model, type='importance')
})

test_that("exp_survival_forest basic with start_time and end_time", {
  df <- survival::lung # this data has NAs.
  df <- df %>% mutate(status = status==2)
  df <- df %>% mutate(start = as.Date("2021-01-01"), end = start + lubridate::days(time))
  df <- df %>% rename(`ti me`=time, `sta tus`=status, `a ge`=age, `se-x`=sex)
  df <- df %>% mutate(ph.ecog = factor(ph.ecog))
  df <- df %>% mutate(`se-x` = `se-x`==1) # test handling of logical
  model_df <- df %>% exp_survival_forest(NULL, `sta tus`, `a ge`, `se-x`, ph.ecog, ph.karno, pat.karno, meal.cal, wt.loss, start_time=start, end_time=end, time_unit="auto", 
                                         predictor_funs=list(`a ge`="none", `se-x`="none", ph.ecog=rlang::expr(forcats::fct_relevel(.,"1")), ph.karno="none", pat.karno="none", meal.cal="none", wt.loss="none"), predictor_n = 2)
  ret <- model_df %>% prediction2(pretty.name=TRUE)
  ret <- df %>% select(-`ti me`, -`sta tus`) %>% add_prediction(model_df=model_df, pred_survival_time=5)
  ret <- model_df %>% evaluation()
  expect_false("Data Type" %in% colnames(ret))
  ret <- model_df %>% prediction2()
  ret2 <- ret %>% do_survival_roc_("Predicted Survival Rate","Survival Time","sta tus", at=NULL, grid=10, revert=TRUE, with_auc=TRUE)
  # Most of the time, true positive rate should be larger than false positive rate. If this is 
  expect_true(sum((ret2 %>% mutate(positive=true_positive_rate >= false_positive_rate))$positive) > 0.8 * nrow(ret2))

  ret <- model_df %>% glance_rowwise(model)
  ret <- model_df %>% augment_rowwise(model)

  ret <- model_df %>% tidy_rowwise(model, type='partial_dependence_survival_curve')
  expect_equal(class(model_df$model[[1]]), c("ranger_survival_exploratory", "ranger"))
  ret <- model_df %>% tidy_rowwise(model, type='partial_dependence')
  ret <- model_df %>% tidy_rowwise(model, type='importance')
})

test_that("exp_survival_forest with group-by", {
  df <- survival::lung # this data has NAs.
  df <- df %>% mutate(status = status==2)
  df <- df %>% rename(`ti me`=time, `sta tus`=status, `a ge`=age, `se-x`=sex)
  df <- df %>% mutate(ph.ecog = factor(ph.ecog, ordered=TRUE)) # test handling of ordered factor
  df <- df %>% mutate(`se-x` = `se-x`==1) # test handling of logical
  df <- df %>% group_by(`se-x`)
  model_df <- df %>% exp_survival_forest(`ti me`, `sta tus`, `a ge`, `se-x`, ph.ecog, ph.karno, pat.karno, meal.cal, wt.loss, predictor_n = 2)
  ret <- model_df %>% evaluation()
  expect_false("Data Type" %in% colnames(ret))
  ret <- model_df %>% prediction2()
  ret <- model_df %>% glance_rowwise(model)
  ret <- model_df %>% augment_rowwise(model)
  ret <- model_df %>% tidy_rowwise(model, type='partial_dependence_survival_curve')
  expect_equal(class(model_df$model[[1]]), c("ranger_survival_exploratory", "ranger"))
  ret <- model_df %>% tidy_rowwise(model, type='partial_dependence')
  ret <- model_df %>% tidy_rowwise(model, type='importance')
})

test_that("exp_survival_forest with test mode", {
  df <- survival::lung # this data has NAs.
  df <- df %>% mutate(status = status==2)
  df <- df %>% rename(`ti me`=time, `sta tus`=status, `a ge`=age, `se-x`=sex)
  df <- df %>% mutate(ph.ecog = factor(ph.ecog, ordered=TRUE)) # test handling of ordered factor
  df <- df %>% mutate(`se-x` = `se-x`==1) # test handling of logical
  model_df <- df %>% exp_survival_forest(`ti me`, `sta tus`, `a ge`, `se-x`, ph.ecog, ph.karno, pat.karno, meal.cal, wt.loss, predictor_n = 2, test_rate=0.3)
  ret <- model_df %>% evaluation()
  expect_true("Data Type" %in% colnames(ret))
  ret <- model_df %>% prediction2()
  ret <- model_df %>% augment_rowwise(model, data_type="training")
  ret <- model_df %>% augment_rowwise(model, data_type="test")
  ret <- model_df %>% glance_rowwise(model)
  ret <- model_df %>% tidy_rowwise(model, type='partial_dependence_survival_curve')
  expect_equal(class(model_df$model[[1]]), c("ranger_survival_exploratory", "ranger"))
  ret <- model_df %>% tidy_rowwise(model, type='partial_dependence')
  ret <- model_df %>% tidy_rowwise(model, type='importance')
})

test_that("exp_survival_forest with test mode with group-by", {
  df <- survival::lung # this data has NAs.
  df <- df %>% mutate(status = status==2)
  df <- df %>% rename(`ti me`=time, `sta tus`=status, `a ge`=age, `se-x`=sex)
  df <- df %>% mutate(ph.ecog = factor(ph.ecog, ordered=TRUE)) # test handling of ordered factor
  df <- df %>% mutate(`se-x` = `se-x`==1) # test handling of logical
  df <- df %>% group_by(`se-x`)
  model_df <- df %>% exp_survival_forest(`ti me`, `sta tus`, `a ge`, `se-x`, ph.ecog, ph.karno, pat.karno, meal.cal, wt.loss, predictor_n = 2, test_rate=0.3)
  ret <- model_df %>% evaluation()
  expect_true("Data Type" %in% colnames(ret))
  ret <- model_df %>% prediction2()
  ret <- model_df %>% augment_rowwise(model, data_type="training")
  ret <- model_df %>% augment_rowwise(model, data_type="test")
  ret <- model_df %>% glance_rowwise(model)
  ret <- model_df %>% tidy_rowwise(model, type='partial_dependence_survival_curve')
  expect_equal(class(model_df$model[[1]]), c("ranger_survival_exploratory", "ranger"))
  ret <- model_df %>% tidy_rowwise(model, type='partial_dependence')
  ret <- model_df %>% tidy_rowwise(model, type='importance')
})

test_that("exp_survival_forest with outtlier filtering", {
  df <- survival::lung # this data has NAs.
  df <- df %>% mutate(status = status==2)
  df <- df %>% rename(`ti me`=time, `sta tus`=status, `a ge`=age, `se-x`=sex)
  df <- df %>% mutate(ph.ecog = factor(ph.ecog, ordered=TRUE)) # test handling of ordered factor
  df <- df %>% mutate(`se-x` = `se-x`==1) # test handling of logical
  model_df <- df %>% exp_survival_forest(`ti me`, `sta tus`, `a ge`, `se-x`, ph.ecog, ph.karno, pat.karno, meal.cal, wt.loss, predictor_n = 2, predictor_outlier_filter_type = 'percentile', predictor_outlier_filter_threshold = 0.95)
  ret <- model_df %>% augment_rowwise(model)
  ret <- model_df %>% tidy_rowwise(model, type='partial_dependence_survival_curve')
  expect_equal(class(model_df$model[[1]]), c("ranger_survival_exploratory", "ranger"))
  ret <- model_df %>% tidy_rowwise(model, type='partial_dependence')
  ret <- model_df %>% tidy_rowwise(model, type='importance')
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
