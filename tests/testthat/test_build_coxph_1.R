context("test build_coxph")

test_that("build_coxph.fast with start_time and end_time", {
  Sys.setenv(TZ="UTC") # set time zone for test stability for tests with time unit smaller than day.
  # Repeat the same test for Date input and POSIXct input. We do not support units like hour, min, sec, but still it should work with POSIXct type time input.
  for (type in c("Date", "POSIXct")) {
    df <- survival::lung # this data has NAs.
    df <- df %>% mutate(status = status==2)
    # Set the start time to be 1 year before. 
    if (type == "Date") {
      df <- df %>% mutate(start = lubridate::today() - lubridate::years(1), end = start + lubridate::days(time))
    }
    else {
      df <- df %>% mutate(start = lubridate::now() - lubridate::years(1), end = start + lubridate::days(time))
    }
    df <- df %>% rename(`ti me`=time, `sta tus`=status, `a ge`=age, `se-x`=sex)
    df <- df %>% mutate(ph.ecog = factor(ph.ecog, ordered=TRUE)) # test handling of ordered factor
    df <- df %>% mutate(`se-x` = `se-x`==1) # test handling of logical
    model_df <- df %>% build_coxph.fast(NULL, `sta tus`, `a ge`, `se-x`, ph.ecog, ph.karno, pat.karno, meal.cal, wt.loss, start_time=start, end_time=end, time_unit="auto", predictor_funs=list(`a ge`="none", `se-x`="none", ph.ecog="none", ph.karno="none", pat.karno="none", meal.cal="none", wt.loss="none"), predictor_n = 2)
    expect_equal(class(model_df$model[[1]]), c("coxph_exploratory","coxph"))
    # Make sure the auto-picked prediction survival time is an integer.
    expect_equal(model_df$model[[1]]$pred_survival_time, floor(model_df$model[[1]]$pred_survival_time))

    # Survival-time-based prediction. Still used in the Analytics View, for example, for ROC chart.
    df2 <- df %>% select(-`ti me`, -`sta tus`)
    ret <- df2 %>% add_prediction(model_df=model_df, pred_survival_time=5)
    expect_equal(colnames(df2), colnames(ret)[1:length(colnames(df2))]) # Check that the df2 column order is kept.
    expected_colnames <- c("inst", "a ge", "se-x", "ph.ecog",
                           "ph.karno", "pat.karno", "meal.cal", "wt.loss",
                           "start", "end", "Prediction Survival Time", "Predicted Survival Rate", "Predicted Survival",
                           "Linear Predictor", "Std Error")
    expect_equal(colnames(ret), expected_colnames)

    # Survival-rate-based event time prediction.
    ret <- df %>% select(-`ti me`) %>% add_prediction(model_df=model_df, pred_survival_rate=0.5)
    # Without status column in the new data.
    ret <- df %>% select(-`ti me`, -`sta tus`) %>% add_prediction(model_df=model_df, pred_survival_rate=0.5)
    # Without status column and end date colum in the new data.
    ret <- df %>% select(-`ti me`, -`sta tus`, -end) %>% add_prediction(model_df=model_df, pred_survival_rate=0.5)
    expected_colnames <- c("inst", "a ge", "se-x", "ph.ecog", "ph.karno", "pat.karno", "meal.cal", "wt.loss",
                           "start", "Survival Rate for Prediction", "Predicted Survival Time", "Predicted Event Time",
                           "Linear Predictor", "Std Error", "Note")
    expect_equal(colnames(ret), expected_colnames)
    expect_equal(sum(is.na(ret$`Predicted Event Time`)), 0)

    # Point-of-time-based survival rate prediction with base time specified as a specific date.
    # Set 5 days before the max of start time column as the base time.
    ret <- df %>% select(-`ti me`, -end, -`sta tus`) %>% add_prediction(model_df=model_df, base_time_type="value", base_time= lubridate::today() - lubridate::days(5), pred_time=5)
    expected_colnames <- c("inst", "a ge", "se-x", "ph.ecog", "ph.karno", "pat.karno", "meal.cal", "wt.loss",
                           "start", "Base Time", "Base Survival Time", "Prediction Time",
                           "Prediction Survival Time", "Predicted Survival Rate", "Linear Predictor", "Std Error",
                           "Note")
    expect_equal(colnames(ret), expected_colnames)
    expect_equal(sum(is.na(ret$`Predicted Survival Rate`)), 0)
    expect_true(max(ret$`Predicted Survival Rate`) <= 1)
    expect_true(min(ret$`Predicted Survival Rate`) >= 0)
    # Point-of-time-based survival rate prediction with base time specified as the max of start time column.
    ret <- df %>% select(-`ti me`, -end, -`sta tus`) %>% add_prediction(model_df=model_df, base_time_type="max", pred_time=5)
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

    # prediction2, which is used for ROC, and Data tab in the Analytics View.
    ret <- model_df %>% prediction2(pretty.name=TRUE)
    ret <- model_df %>% prediction2()
    ret2 <- ret %>% do_survival_roc_("Predicted Survival Rate","Survival Time","sta tus", at=NULL, grid=10, revert=TRUE)
    # Most of the time, true positive rate should be larger than false positive rate.
    expect_true(sum((ret2 %>% mutate(positive=true_positive_rate >= false_positive_rate))$positive) > 0.8 * nrow(ret2))

    ret <- model_df %>% evaluation(pretty.name=TRUE)
    expect_false("Data Type" %in% colnames(ret))
    ret <- model_df %>% tidy_rowwise(model, type='permutation_importance')
    ret2 <- model_df %>% tidy_rowwise(model, type='partial_dependence')
    variables <- (ret %>% arrange(desc(importance)))$term
    names(variables) <- NULL
    expect_equal(unique(ret2$variable), variables) # Factor order of the PDP should be the same as the importance.
    expect_true(all(c("estimate", "p.value") %in% colnames(ret2))) # Make sure that estimate and p.value are joined to the result for hover on Prediction tab.
    ret <- model_df %>% tidy_rowwise(model, type='partial_dependence_survival_curve')
    # Verify that period starts with 0. (If the input data does not, we add a data point for time 0.)
    expect_equal(ret$period[1], 0)
    ret <- model_df %>% tidy_rowwise(model, type='vif')
    ret <- model_df %>% tidy_rowwise(model)
    expect_equal(colnames(ret),
                 c("term","estimate","std_error","t_ratio",
                   "p_value","conf_low","conf_high","hazard_ratio","base.level"))
    # Verify that base levels are not NA for `se-x` (testing - in the name) columns.
    ret2 <- ret %>% dplyr::filter(stringr::str_detect(term,"(se-x)")) %>% dplyr::summarize(na_count=sum(is.na(base.level)))
    expect_equal(ret2$na_count, 0)

    ret <- model_df %>% glance_rowwise(model, pretty.name=TRUE)
    expect_equal(colnames(ret),
                 c("Concordance","Std Error Concordance",
                   "Time-dependent AUC",
                   "Likelihood Ratio Test","Likelihood Ratio Test P Value",
                   "Score Test","Score Test P Value","Wald Test","Wald Test P Value",
                   # "Robust Statistic","Robust P Value", # These columns are hidden for now.
                   "R Squared","Max R Squared",
                   "Log Likelihood","AIC","BIC",
                   "Max VIF",
                   "Rows","Rows (TRUE)"))
    ret <- model_df %>% augment_rowwise(model)
  }
})

test_that("build_coxph.fast basic with group-by", {
  df <- survival::lung # this data has NAs.
  df <- df %>% mutate(status = status==2)
  df <- df %>% rename(`ti me`=time, `sta tus`=status, `a ge`=age, `se-x`=sex)
  df <- df %>% mutate(ph.ecog = factor(ph.ecog, ordered=TRUE)) # test handling of ordered factor
  df <- df %>% mutate(`se-x` = `se-x`==1) # test handling of logical
  df <- df %>% group_by(`se-x`)
  model_df <- df %>% build_coxph.fast(`ti me`, `sta tus`, `a ge`, ph.ecog, ph.karno, pat.karno, meal.cal, wt.loss, predictor_n = 2)
  expect_equal(class(model_df$model[[1]]), c("coxph_exploratory","coxph"))
  ret <- model_df %>% prediction2()
  ret <- model_df %>% evaluation(pretty.name=TRUE)
  expect_false("Data Type" %in% colnames(ret))
  ret <- model_df %>% tidy_rowwise(model, type='permutation_importance')
  ret <- model_df %>% tidy_rowwise(model, type='partial_dependence')
  ret <- model_df %>% tidy_rowwise(model, type='partial_dependence_survival_curve')
  ret <- model_df %>% tidy_rowwise(model, type='vif')
  ret <- model_df %>% tidy_rowwise(model)
  expect_equal(colnames(ret),
               c("se-x","term","estimate","std_error","t_ratio",
                 "p_value","conf_low","conf_high","hazard_ratio","base.level"))

  ret <- model_df %>% glance_rowwise(model, pretty.name=TRUE)
  expect_equal(colnames(ret),
               c("se-x",
                 "Concordance","Std Error Concordance",
                 "Time-dependent AUC",
                 "Likelihood Ratio Test","Likelihood Ratio Test P Value",
                 "Score Test","Score Test P Value","Wald Test","Wald Test P Value",
                 # "Robust Statistic","Robust P Value", # These columns are hidden for now.
                 "R Squared","Max R Squared",
                 "Log Likelihood","AIC","BIC",
                 "Max VIF",
                 "Rows","Rows (TRUE)"))
  ret <- model_df %>% augment_rowwise(model)
})

test_that("test build_coxph.fast with test mode", {
  df <- survival::lung # this data has NAs.
  df <- df %>% mutate(status = status==2)
  df <- df %>% rename(`ti me`=time, `sta tus`=status, `a ge`=age, `se-x`=sex)
  df <- df %>% mutate(ph.ecog = factor(ph.ecog, ordered=TRUE)) # test handling of ordered factor
  df <- df %>% mutate(`se-x` = `se-x`==1) # test handling of logical
  model_df <- df %>% build_coxph.fast(`ti me`, `sta tus`, `a ge`, `se-x`, ph.ecog, ph.karno, pat.karno, meal.cal, wt.loss, predictor_n = 2, test_rate=0.3)
  expect_equal(class(model_df$model[[1]]), c("coxph_exploratory","coxph"))
  ret <- model_df %>% prediction2()
  ret <- model_df %>% evaluation(pretty.name=TRUE)
  expect_true("Data Type" %in% colnames(ret))
  ret <- model_df %>% tidy_rowwise(model, type='permutation_importance')
  ret <- model_df %>% tidy_rowwise(model, type='partial_dependence')
  ret <- model_df %>% tidy_rowwise(model, type='partial_dependence_survival_curve')
  ret <- model_df %>% tidy_rowwise(model, type='vif')
  ret <- model_df %>% tidy_rowwise(model)
  expect_equal(colnames(ret),
               c("term","estimate","std_error","t_ratio",
                 "p_value","conf_low","conf_high","hazard_ratio","base.level"))
  # Verify that base levels are not NA for `se-x` (testing - in the name) columns.
  ret2 <- ret %>% dplyr::filter(stringr::str_detect(term,"(se-x)")) %>% dplyr::summarize(na_count=sum(is.na(base.level)))
  expect_equal(ret2$na_count, 0)

  ret <- model_df %>% glance_rowwise(model, pretty.name=TRUE)
  expect_equal(colnames(ret),
               c("Concordance","Std Error Concordance",
                 "Time-dependent AUC",
                 "Likelihood Ratio Test","Likelihood Ratio Test P Value",
                 "Score Test","Score Test P Value","Wald Test","Wald Test P Value",
                 # "Robust Statistic","Robust P Value", # These columns are hidden for now.
                 "R Squared","Max R Squared",
                 "Log Likelihood","AIC","BIC",
                 "Max VIF",
                 "Rows","Rows (TRUE)"))
  ret <- model_df %>% augment_rowwise(model)
})

test_that("test build_coxph.fast with test mode with group-by", {
  df <- survival::lung # this data has NAs.
  df <- df %>% mutate(status = status==2)
  df <- df %>% rename(`ti me`=time, `sta tus`=status, `a ge`=age, `se-x`=sex)
  df <- df %>% mutate(ph.ecog = factor(ph.ecog, ordered=TRUE)) # test handling of ordered factor
  df <- df %>% mutate(`se-x` = `se-x`==1) # test handling of logical
  df <- df %>% group_by(`se-x`)
  model_df <- df %>% build_coxph.fast(`ti me`, `sta tus`, `a ge`, ph.ecog, ph.karno, pat.karno, meal.cal, wt.loss, predictor_n = 2, test_rate=0.3)
  expect_equal(class(model_df$model[[1]]), c("coxph_exploratory","coxph"))
  ret <- model_df %>% prediction2()
  ret <- model_df %>% evaluation(pretty.name=TRUE)
  expect_true("Data Type" %in% colnames(ret))
  ret <- model_df %>% tidy_rowwise(model, type='permutation_importance')
  ret <- model_df %>% tidy_rowwise(model, type='partial_dependence')
  ret <- model_df %>% tidy_rowwise(model, type='partial_dependence_survival_curve')
  ret <- model_df %>% tidy_rowwise(model, type='vif')
  ret <- model_df %>% tidy_rowwise(model)
  expect_true(all(colnames(ret) %in%
               c("se-x", "term","estimate","std_error","t_ratio",
                 "p_value","conf_low","conf_high","hazard_ratio","base.level", "note")))
  ret <- model_df %>% glance_rowwise(model, pretty.name=TRUE)
  expect_equal(colnames(ret),
               c("se-x",
                 "Concordance","Std Error Concordance",
                 "Time-dependent AUC",
                 "Likelihood Ratio Test","Likelihood Ratio Test P Value",
                 "Score Test","Score Test P Value","Wald Test","Wald Test P Value",
                 # "Robust Statistic","Robust P Value", # These columns are hidden for now.
                 "R Squared","Max R Squared",
                 "Log Likelihood","AIC","BIC",
                 "Max VIF",
                 "Rows","Rows (TRUE)"))
  ret <- model_df %>% augment_rowwise(model)
})

test_that("test build_coxph.fast with outlier filtering", {
  df <- survival::lung # this data has NAs.
  df <- df %>% mutate(status = status==2)
  df <- df %>% rename(`ti me`=time, `sta tus`=status, `a ge`=age, `se-x`=sex)
  df <- df %>% mutate(ph.ecog = factor(ph.ecog, ordered=TRUE)) # test handling of ordered factor
  df <- df %>% mutate(`se-x` = `se-x`==1) # test handling of logical
  model_df <- df %>% build_coxph.fast(`ti me`, `sta tus`, `a ge`, `se-x`, ph.ecog, ph.karno, pat.karno, meal.cal, wt.loss, predictor_n = 2, predictor_outlier_filter_type = 'percentile', predictor_outlier_filter_threshold = 0.95)
  expect_equal(class(model_df$model[[1]]), c("coxph_exploratory","coxph"))
  ret <- model_df %>% tidy_rowwise(model, type='permutation_importance')
  ret <- model_df %>% tidy_rowwise(model, type='partial_dependence')
  ret <- model_df %>% tidy_rowwise(model, type='partial_dependence_survival_curve')
  ret <- model_df %>% tidy_rowwise(model, type='vif')
  ret <- model_df %>% tidy_rowwise(model)
  # Verify that base levels are not NA for `se-x` (testing - in the name) columns.
  ret2 <- ret %>% dplyr::filter(stringr::str_detect(term,"(se-x)")) %>% dplyr::summarize(na_count=sum(is.na(base.level)))
  expect_equal(ret2$na_count, 0)

  ret <- model_df %>% glance_rowwise(model, pretty.name=TRUE)
  ret <- model_df %>% augment_rowwise(model)
})

test_that("build_coxpy.fast() error handling for predictor with single unique value", {
  expect_error({
    df <- survival::lung # this data has NAs.
    df <- df %>% mutate(status = status==2)
    df <- df %>% mutate(age = 50) # Test for single unique value error handling.
    df <- df %>% rename(`ti me`=time, `sta tus`=status, `a ge`=age, `se-x`=sex)
    df <- df %>% mutate(ph.ecog = factor(ph.ecog, ordered=TRUE)) # test handling of ordered factor
    df <- df %>% mutate(`se-x` = `se-x`==1) # test handling of logical
    model_df <- df %>% build_coxph.fast(`ti me`, `sta tus`, `a ge`, predictor_n = 2)
  }, "Invalid Predictors: Only one unique value.")
})

test_that("build_coxph()", {
  df <- survival::lung # this data has NAs.
  df <- df %>% mutate(status = status==2)
  df <- df %>% rename(`ti me`=time, `sta tus`=status, `a ge`=age, `se-x`=sex)
  df <- df %>% mutate(`se-x` = `se-x`==1) # test handling of logical
  model_df <- df %>% build_coxph(survival::Surv(`ti me`, `sta tus`) ~ `a ge` + `se-x`, test_rate=0.3)
  res <- model_df %>% model_stats()
  res <- model_df %>% model_coef(conf_int = "default", conf.level = 0.95)
  res <- model_df %>% model_anova()
  res <- model_df %>% prediction_coxph(data = "training", type.predict = "lp", type.residuals = "martingale")
  expect_true("residuals" %in% colnames(res)) # Make sure residuals column is there. For newdata, it is not there in the output from broom::augment, but this is with training data.
  res <- model_df %>% prediction_survfit(newdata = expand.grid(`a ge` = c(40, 50) , `se-x` = c(1,2)))
})

# Note: we used to have Japanese column name test, but removed since
# it was not a simple matter to make it work on Windows where we need to use
# SJIS. We test multibyte column names with other test suite.
