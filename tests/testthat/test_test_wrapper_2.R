
context("tests for wrappers of tests")

test_that("test exp_wilcox", {
  mtcars2 <- mtcars
  mtcars2$am[[1]] <- NA # test NA filtering
  model_df <- exp_wilcox(mtcars2, mpg, am)
  ret <- model_df %>% tidy_rowwise(model, type="model")
  ret <- model_df %>% tidy_rowwise(model, type="data_summary")
  expect_equal(colnames(ret),
               c("am","Number of Rows","Mean","Conf Low","Conf High","Std Error of Mean","Std Deviation",
                 "Minimum","Maximum"))
})

test_that("test exp_wilcox with logical explanatory variable", {
  mtcars2 <- mtcars
  mtcars2$am[[1]] <- NA # test NA filtering
  mtcars2 <- mtcars2 %>% dplyr::mutate(am=as.logical(am))
  model_df <- exp_wilcox(mtcars2, mpg, am, conf.int=TRUE) # Set conf.int TRUE to check direction of Difference.
  ret <- model_df %>% tidy_rowwise(model, type="model")
  expect_gt(ret$Difference, 0) # Checking the direction of Difference is correct.
  expect_true("Number of Rows" %in% colnames(ret))
  model_df %>% tidy_rowwise(model, type="data_summary")
})

test_that("test exp_wilcox with conf.int = TRUE", {
  mtcars2 <- mtcars
  mtcars2$am[[1]] <- NA # test NA filtering
  model_df <- exp_wilcox(mtcars2, mpg, am, conf.int=TRUE)
  ret <- model_df %>% tidy_rowwise(model, type="model")
  ret <- model_df %>% tidy_rowwise(model, type="data_summary")
  expect_equal(colnames(ret),
               c("am","Number of Rows","Mean","Conf Low","Conf High","Std Error of Mean","Std Deviation",
                 "Minimum","Maximum"))
})

test_that("test exp_wilcox with paired = TRUE", {
  # Make sample size equal between groups for paired t-test.
  mtcars2 <- mtcars %>% group_by(am) %>% slice_sample(n=6) %>% ungroup()
  model_df <- exp_wilcox(mtcars2, mpg, am, paired=TRUE)
  ret <- model_df %>% tidy_rowwise(model, type="model")
  ret <- model_df %>% tidy_rowwise(model, type="data_summary")
  expect_equal(colnames(ret),
               c("am","Number of Rows","Mean","Conf Low","Conf High","Std Error of Mean","Std Deviation",
                 "Minimum","Maximum"))
})

test_that("test exp_wilcox with paired = TRUE, conf.int = TRUE", {
  # Make sample size equal between groups for paired t-test.
  mtcars2 <- mtcars %>% group_by(am) %>% slice_sample(n=6) %>% ungroup()
  model_df <- exp_wilcox(mtcars2, mpg, am, paired=TRUE, conf.int = TRUE)
  ret <- model_df %>% tidy_rowwise(model, type="model")
  ret <- model_df %>% tidy_rowwise(model, type="data_summary")
  expect_equal(colnames(ret),
               c("am","Number of Rows","Mean","Conf Low","Conf High","Std Error of Mean","Std Deviation",
                 "Minimum","Maximum"))
})

