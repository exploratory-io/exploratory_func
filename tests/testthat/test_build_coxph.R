context("test build_coxph")

test_that("build_coxph.fast basic", {
  df <- survival::lung # this data has NAs.
  df <- df %>% mutate(status = status==2)
  df <- df %>% rename(`ti me`=time, `sta tus`=status, `a ge`=age, `se-x`=sex)
  df <- df %>% mutate(ph.ecog = factor(ph.ecog, ordered=TRUE)) # test handling of ordered factor
  df <- df %>% mutate(`se-x` = `se-x`==1) # test handling of logical
  model_df <- df %>% build_coxph.fast(`ti me`, `sta tus`, `a ge`, `se-x`, ph.ecog, ph.karno, pat.karno, meal.cal, wt.loss, predictor_funs=list(`a ge`="none", `se-x`="none", ph.ecog="none", ph.karno="none", pat.karno="none", meal.cal="none", wt.loss="none"), predictor_n = 2)
  ret <- df %>% select(-`ti me`, -`sta tus`) %>% add_prediction(model_df=model_df, pred_survival_time=5)
  expect_equal(class(model_df$model[[1]]), c("coxph_exploratory","coxph"))
  ret <- model_df %>% prediction2()
  ret2 <- ret %>% do_survival_roc_("Predicted Survival Rate","ti me","sta tus", at=NULL, grid=10, revert=TRUE)
  # Most of the time, true positive rate should be larger than false positive rate. If this is 
  expect_true(sum((ret2 %>% mutate(positive=true_positive_rate >= false_positive_rate))$positive) > 0.8 * nrow(ret2))

  ret <- model_df %>% evaluation(pretty.name=TRUE)
  expect_false("Data Type" %in% colnames(ret))
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
                 "R Squared","R Squared Max",
                 "Log Likelihood","AIC","BIC",
                 "VIF Max",
                 "Number of Rows","Number of Events")) 
  ret <- model_df %>% augment_rowwise(model)
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
                 "R Squared","R Squared Max",
                 "Log Likelihood","AIC","BIC",
                 "VIF Max",
                 "Number of Rows","Number of Events")) 
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
                 "R Squared","R Squared Max",
                 "Log Likelihood","AIC","BIC",
                 "VIF Max",
                 "Number of Rows","Number of Events")) 
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
                 "R Squared","R Squared Max",
                 "Log Likelihood","AIC","BIC",
                 "VIF Max",
                 "Number of Rows","Number of Events")) 
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
