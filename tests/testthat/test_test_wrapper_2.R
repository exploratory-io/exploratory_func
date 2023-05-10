
context("tests for wrappers of tests")

test_that("test exp_wilcox", {
  mtcars2 <- mtcars
  mtcars2$am[[1]] <- NA # test NA filtering
  model_df <- exp_wilcox(mtcars2, mpg, am)
  ret <- model_df %>% tidy_rowwise(model, type="model")
  ret <- model_df %>% tidy_rowwise(model, type="data_summary")
  expect_equal(colnames(ret),
               c("am","Rows","Mean","Conf Low","Conf High","Std Error of Mean","Std Deviation",
                 "Minimum","Maximum"))

  ret <- model_df %>% tidy_rowwise(model, type="prob_dist")
  expect_true("p.value" %in% colnames(ret))
})

test_that("test exp_wilcox with factor explanatory variable", {
  mtcars2 <- mtcars
  mtcars2$am[[1]] <- NA # test NA filtering
  # Put unused factor levels too for test.
  mtcars2 <- mtcars2 %>% dplyr::mutate(am=factor(am, levels=c(-1,0,1,2)))
  model_df <- exp_wilcox(mtcars2, mpg, am, conf.int=TRUE) # Set conf.int TRUE to check direction of Difference.
  ret <- model_df %>% tidy_rowwise(model, type="model")
  expect_equal(ret$`Base Level`, "0") # First *used* factor level should be the base.
  expect_gt(ret$Difference, 0) # Checking the direction of Difference is correct.
  expect_true("Rows" %in% colnames(ret))
  model_df %>% tidy_rowwise(model, type="data_summary")
})

test_that("test exp_wilcox with numeric explanatory variable", {
  mtcars2 <- mtcars
  mtcars2$am[[1]] <- NA # test NA filtering
  model_df <- exp_wilcox(mtcars2, mpg, am, conf.int=TRUE) # Set conf.int TRUE to check direction of Difference.
  ret <- model_df %>% tidy_rowwise(model, type="model")
  expect_equal(ret$`Base Level`, "0") # The smaller number should be the base.
  expect_gt(ret$Difference, 0) # Checking the direction of Difference is correct.
  expect_true("Rows" %in% colnames(ret))
  model_df %>% tidy_rowwise(model, type="data_summary")
})

test_that("test exp_wilcox with character explanatory variable", {
  mtcars2 <- mtcars
  mtcars2$am[[1]] <- NA # test NA filtering
  mtcars2 <- mtcars2 %>% dplyr::mutate(am=as.character(am))
  model_df <- exp_wilcox(mtcars2, mpg, am, conf.int=TRUE) # Set conf.int TRUE to check direction of Difference.
  ret <- model_df %>% tidy_rowwise(model, type="model")
  expect_equal(ret$`Base Level`, "0") # The majority should be the base
  expect_gt(ret$Difference, 0) # Checking the direction of Difference is correct.
  expect_true("Rows" %in% colnames(ret))
  model_df %>% tidy_rowwise(model, type="data_summary")
})
test_that("test exp_wilcox with logical explanatory variable", {
  mtcars2 <- mtcars
  mtcars2$am[[1]] <- NA # test NA filtering
  mtcars2 <- mtcars2 %>% dplyr::mutate(am=as.logical(am))
  model_df <- exp_wilcox(mtcars2, mpg, am, conf.int=TRUE) # Set conf.int TRUE to check direction of Difference.
  ret <- model_df %>% tidy_rowwise(model, type="model")
  expect_equal(ret$`Base Level`, "FALSE") # FALSE should be the base
  expect_gt(ret$Difference, 0) # Checking the direction of Difference is correct.
  expect_true("Rows" %in% colnames(ret))
  model_df %>% tidy_rowwise(model, type="data_summary")
})

test_that("test exp_wilcox with conf.int = TRUE", {
  mtcars2 <- mtcars
  mtcars2$am[[1]] <- NA # test NA filtering
  model_df <- exp_wilcox(mtcars2, mpg, am, conf.int=TRUE)
  ret <- model_df %>% tidy_rowwise(model, type="model")
  ret <- model_df %>% tidy_rowwise(model, type="data_summary")
  expect_equal(colnames(ret),
               c("am","Rows","Mean","Conf Low","Conf High","Std Error of Mean","Std Deviation",
                 "Minimum","Maximum"))
})

test_that("test exp_wilcox with paired = TRUE", {
  # Make sample size equal between groups for paired t-test.
  mtcars2 <- mtcars %>% group_by(am) %>% slice_sample(n=6) %>% ungroup()
  model_df <- exp_wilcox(mtcars2, mpg, am, paired=TRUE)
  ret <- model_df %>% tidy_rowwise(model, type="model")
  ret <- model_df %>% tidy_rowwise(model, type="data_summary")
  expect_equal(colnames(ret),
               c("am","Rows","Mean","Conf Low","Conf High","Std Error of Mean","Std Deviation",
                 "Minimum","Maximum"))
})

test_that("test exp_wilcox with paired = TRUE, conf.int = TRUE", {
  # Make sample size equal between groups for paired t-test.
  mtcars2 <- mtcars %>% group_by(am) %>% slice_sample(n=6) %>% ungroup()
  model_df <- exp_wilcox(mtcars2, mpg, am, paired=TRUE, conf.int = TRUE)
  ret <- model_df %>% tidy_rowwise(model, type="model")
  ret <- model_df %>% tidy_rowwise(model, type="data_summary")
  expect_equal(colnames(ret),
               c("am","Rows","Mean","Conf Low","Conf High","Std Error of Mean","Std Deviation",
                 "Minimum","Maximum"))
})

test_that("test exp_wilcox with group-level error", {
  df <- tibble::tibble(group=c(1,1,2,2),category=c("a","a","b","b"),value=c(1,2,1,2))
  model_df <- df %>% dplyr::group_by(`group`) %>% exp_wilcox(`value`, `category`)
  ret <- model_df %>% tidy_rowwise(model, type='model')
  expect_equal(colnames(ret),
               c("group","Note"))
  ret <- model_df %>% tidy_rowwise(model, type='prob_dist')
  expect_equal(nrow(ret), 0)
})
