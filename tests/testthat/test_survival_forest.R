context("test exp_survival_forest")

test_that("exp_survival_forest basic with start_time and end_time", {
  # Repeat the same test for Date input and POSIXct input. We do not support units like hour, min, sec, but still it should work with POSIXct type time input.
  for (type in c("Date", "POSIXct")) {
    df <- survival::lung # this data has NAs.
    df <- df %>% mutate(status = status==2)
    if (type == "Date") {
      df <- df %>% mutate(start = as.Date("2021-01-01"), end = start + lubridate::days(time))
    }
    else {
      df <- df %>% mutate(start = as.POSIXct("2021-01-01"), end = start + lubridate::days(time))
    }
    df <- df %>% rename(`ti me`=time, `sta tus`=status, `a ge`=age, `se-x`=sex)
    df <- df %>% mutate(ph.ecog = factor(ph.ecog))
    df <- df %>% mutate(`se-x` = `se-x`==1) # test handling of logical
    model_df <- df %>% exp_survival_forest(NULL, `sta tus`, `a ge`, `se-x`, ph.ecog, ph.karno, pat.karno, meal.cal, wt.loss, start_time=start, end_time=end, time_unit="auto", 
                                           predictor_funs=list(`a ge`="none", `se-x`="none", ph.ecog=rlang::expr(forcats::fct_relevel(.,"1")), ph.karno="none", pat.karno="none", meal.cal="none", wt.loss="none"), predictor_n = 2)
    # Survival-time-based prediction. Still used in the Analytics View, for example, for ROC chart.
    df2 <- df %>% select(-`ti me`, -`sta tus`)
    ret <- df2 %>% add_prediction(model_df=model_df, pred_survival_time=5)
    expect_equal(colnames(df2), colnames(ret)[1:length(colnames(df2))]) # Check that the df2 column order is kept.
    expected_colnames <- c("inst", "a ge", "se-x", "ph.ecog",
                           "ph.karno", "pat.karno", "meal.cal", "wt.loss",
                           "start", "end", "Prediction Survival Time", "Predicted Survival Rate", "Predicted Survival")
    expect_equal(colnames(ret), expected_colnames)

    # Survival-rate-based event time prediction.
    ret <- df %>% select(-`ti me`) %>% add_prediction(model_df=model_df, pred_survival_rate=0.5)
    # Without status column in the new data.
    ret <- df %>% select(-`ti me`, -`sta tus`) %>% add_prediction(model_df=model_df, pred_survival_rate=0.5)
    # Without status column and end date colum in the new data.
    ret <- df %>% select(-`ti me`, -`sta tus`, -end) %>% add_prediction(model_df=model_df, pred_survival_rate=0.5)
    expected_colnames <- c("inst", "a ge", "se-x", "ph.ecog", "ph.karno", "pat.karno", "meal.cal", "wt.loss", "start",
                           "Survival Rate for Prediction", "Predicted Survival Time", "Predicted Event Time", "Note")
    expect_equal(colnames(ret), expected_colnames)
    expect_equal(sum(is.na(ret$`Predicted Event Time`)), 0)
    # Point-of-time-based survival rate prediction with base time specified as a specific date.
    ret <- df %>% select(-`ti me`, -end, -`sta tus`) %>% add_prediction(model_df=model_df, base_time_type="value", base_time=as.Date("2022-01-01"), pred_time=5)
    expected_colnames <- c("inst", "a ge", "se-x", "ph.ecog", "ph.karno", "pat.karno", "meal.cal", "wt.loss",
                           "start", "Base Time", "Base Survival Time", "Prediction Time", "Prediction Survival Time",
                           "Predicted Survival Rate", "Note")
    expect_equal(colnames(ret), expected_colnames)
    expect_equal(sum(is.na(ret$`Predicted Survival Rate`)), 0)
    expect_true(max(ret$`Predicted Survival Rate`) <= 1)
    expect_true(min(ret$`Predicted Survival Rate`) >= 0)
    # Point-of-time-based survival rate prediction with base time specified as the max of start time column.
    ret <- df %>% select(-`ti me`, -end, -`sta tus`) %>% add_prediction(model_df=model_df, base_time_type="max", pred_time=5)
    expected_colnames <- c("inst", "a ge", "se-x", "ph.ecog", "ph.karno", "pat.karno", "meal.cal", "wt.loss",
                           "start", "Base Time", "Base Survival Time", "Prediction Time", "Prediction Survival Time",
                           "Predicted Survival Rate", "Note")
    expect_equal(colnames(ret), expected_colnames)
    expect_equal(sum(is.na(ret$`Predicted Survival Rate`)), 0)
    expect_true(max(ret$`Predicted Survival Rate`) <= 1)
    expect_true(min(ret$`Predicted Survival Rate`) >= 0)
    # Point-of-time-based survival rate prediction with base time specified as today.
    ret <- df %>% select(-`ti me`, -end, -`sta tus`) %>% add_prediction(model_df=model_df, base_time_type="today", pred_time=5)
    expect_equal(colnames(ret), expected_colnames)
    expect_equal(sum(is.na(ret$`Predicted Survival Rate`)), 0)
    expect_true(max(ret$`Predicted Survival Rate`) <= 1)
    expect_true(min(ret$`Predicted Survival Rate`) >= 0)

    ret <- model_df %>% prediction2(pretty.name=TRUE)
    ret <- model_df %>% evaluation()
    expect_false("Data Type" %in% colnames(ret))
    ret <- model_df %>% prediction2()
    ret2 <- ret %>% do_survival_roc_("Predicted Survival Rate","Survival Time","sta tus", at=NULL, grid=10, revert=TRUE, with_auc=TRUE)
    # Most of the time, true positive rate should be larger than false positive rate. If this is 
    expect_true(sum((ret2 %>% mutate(positive=true_positive_rate >= false_positive_rate))$positive) > 0.8 * nrow(ret2))

    ret <- model_df %>% glance_rowwise(model)
    ret <- model_df %>% augment_rowwise(model)

    ret <- model_df %>% tidy_rowwise(model, type='partial_dependence_survival_curve')
    # Verify that period starts with 0. (If the input data does not, we add a data point for time 0.)
    expect_equal(ret$period[1], 0)
    expect_equal(class(model_df$model[[1]]), c("ranger_survival_exploratory", "ranger"))
    ret <- model_df %>% tidy_rowwise(model, type='importance')
    ret2 <- model_df %>% tidy_rowwise(model, type='partial_dependence')
    variables <- (ret %>% arrange(desc(importance)))$variable
    names(variables) <- NULL
    expect_equal(unique(ret2$variable), variables) # Factor order of the PDP should be the same as the importance.
  }
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
