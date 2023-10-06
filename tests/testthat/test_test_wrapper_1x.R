context("tests for wrappers of tests")




test_that("test ANCOVA with exp_anova", {
  mtcars2 <- mtcars %>% mutate(`a m`=factor(am), `w t`=wt, `q sec`=qsec)
  model_df <- mtcars2 %>% exp_anova(mpg, `a m`, covariates=c("w t", "q sec"),
                                    covariate_funs=list("w t"="log", "q sec"="none"),
                                    with_interaction = TRUE)
  ret <- model_df %>% tidy_rowwise(model, type="model")
  ret <- model_df %>% tidy_rowwise(model, type="shapiro")
  ret <- model_df %>% tidy_rowwise(model, type="levene")
  ret <- model_df %>% tidy_rowwise(model, type="levene", levene_test_center="mean")
  ret <- model_df %>% tidy_rowwise(model, type="emmeans", pairs_adjust="tukey")
print(colnames(ret))
  expect_equal(colnames(ret),
    c("a m", "w t", "Rows", "Mean", "Std Deviation", "Std Error", 
    "Conf Low", "Conf High", "Mean (Adj)", "Std Error (Adj)", 
    "Conf Low (Adj)", "Conf High (Adj)", "DF", "Minimum", "Maximum"))

  ret <- model_df %>% tidy_rowwise(model, type="pairs", pairs_adjust="tukey")
  ret <- model_df %>% tidy_rowwise(model, type="prob_dist")
  expect_true("p.value" %in% colnames(ret))

  ret <- model_df %>% tidy_rowwise(model, type="anova")
  ret <- model_df %>% tidy_rowwise(model, type="data")
  ret <- model_df %>% tidy_rowwise(model, type="data_summary")
  expect_equal(colnames(ret),
               c("a m","Rows","Mean","Std Error", "Conf Low","Conf High","Std Deviation",   
                 "Minimum","Maximum"))

  # Test broom output to detect changes at upgrade.
  x <- model_df$model[[1]]
  ret <- broom::tidy(car::Anova(x, type="III"))
  expect_equal(colnames(ret),
               c("term", "sumsq", "df", "statistic", "p.value"))
  ret <- broom::tidy(car::leveneTest(x$residuals, x$data[[x$var2]], center=median))
  expect_equal(colnames(ret),
               c("statistic", "p.value", "df", "df.residual"))
  ret <- broom::tidy(shapiro.test(x$residuals))
  expect_equal(colnames(ret),
               c("statistic", "p.value", "method"))
})




