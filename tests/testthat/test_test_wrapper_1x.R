context("tests for wrappers of tests")






test_that("test ANCOVA", {



  # df<- readRDS(url("https://www.dropbox.com/scl/fi/ranvxxcihbdeosgvs7z96/28337.rds?rlkey=73xo6c8cig6k1o5xsu9v9gndb&dl=1"))
  df<- readRDS("~/Downloads/28337.rds")
  model_df <- df %>% exp_anova(`出発 遅れ表`, `航空 会社表`, covariates = c("到着 遅れ表"), covariate_funs = c("到着 遅れ表" = "none"), with_interaction = TRUE)
  # Make sure type="levene" doesn't fail.
  # ret <- model_df %>% tidy_rowwise(model, type="levene", levene_test_center="median")
  # expect_equal(colnames(ret), c( "F Value", "P Value", "DF", "Residual DF", "Method" ,"Result"))

  ret <- model_df %>% tidy_rowwise(model, type="emmeans")



# print(ret)
print(colnames(ret))

  # mtcars2 <- mtcars %>% mutate(`a m`=am == 1, `ge ar`=as.character(gear))
  # model_df <- mtcars2 %>% exp_anova(mpg, c("a m","ge ar"), func2=c("aschar","aschar"))
  
  # ret <- model_df %>% tidy_rowwise(model, type="pairs", pairs_adjust="tukey")
  # ret <- model_df %>% tidy_rowwise(model, type="model")
  # # Make sure the estimate is between conf.low and conf.high.
  # expect_equal(all(ret$`Difference` >= ret$`Conf Low`), TRUE)
  # expect_equal(all(ret$`Difference` <= ret$`Conf High`), TRUE)

  # ret <- model_df %>% tidy_rowwise(model, type="emmeans", pairs_adjust="tukey")
  # ret <- model_df %>% tidy_rowwise(model, type="prob_dist")
  # ret <- model_df %>% tidy_rowwise(model, type="levene")
  # ret <- model_df %>% tidy_rowwise(model, type="shapiro")
  # ret <- model_df %>% tidy_rowwise(model, type="levene", levene_test_center="mean")
  # ret <- model_df %>% tidy_rowwise(model, type="data")
  # ret <- model_df %>% tidy_rowwise(model, type="data_summary")
  # # Make sure this doesn't fail.
  # ret <- model_df %>% tidy_rowwise(model, type="pairs_per_variable", pairs_adjust="tukey")
  # # Make sure the output for "Group  1" and "Group  2" is in character.
  # expect_equal(ret$`Group 1`, c("FALSE", "3", "3", "4"))
  # expect_equal(ret$`Group 2`, c("TRUE", "4", "5", "5"))
})







