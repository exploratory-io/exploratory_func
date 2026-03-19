
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

  # check confidence interval
  expect_equal(round(ret$`Conf Low`, 3), c(15.299, 20.639))
  expect_equal(round(ret$`Conf High`, 3), c(18.995, 28.711))
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

test_that("test exp_wilcox paired results match direct wilcox.test", {
  # Create paired sample data (n=30 subjects, before/after measurements)
  set.seed(42)
  n <- 30
  before <- round(rnorm(n, mean = 70, sd = 10), 1)
  after <- round(rnorm(n, mean = 65, sd = 10), 1)

  # Long format data for exp_wilcox
  data_long <- tibble::tibble(
    subject_id = rep(paste0("P", sprintf("%02d", 1:n)), 2),
    period = factor(rep(c("before", "after"), each = n), levels = c("before", "after")),
    value = c(before, after)
  )

  # Direct R wilcox.test (the correct result)
  direct_result <- wilcox.test(before, after, paired = TRUE)

  # exp_wilcox result
  model_df <- exp_wilcox(data_long, value, period, paired = TRUE)
  ret <- model_df %>% tidy_rowwise(model, type = "model")

  # p-value should match exactly
  expect_equal(ret$`P Value`[[1]], direct_result$p.value[[1]])
  # W values should be complementary (sum to n*(n+1)/2) due to fct_rev reordering
  expect_equal(ret$`W Value`[[1]] + direct_result$statistic[[1]], n * (n + 1) / 2)
})

test_that("test exp_wilcox paired with conf.int results match direct wilcox.test", {
  set.seed(42)
  n <- 30
  before <- round(rnorm(n, mean = 70, sd = 10), 1)
  after <- round(rnorm(n, mean = 65, sd = 10), 1)

  data_long <- tibble::tibble(
    subject_id = rep(paste0("P", sprintf("%02d", 1:n)), 2),
    period = factor(rep(c("before", "after"), each = n), levels = c("before", "after")),
    value = c(before, after)
  )

  direct_result <- wilcox.test(before, after, paired = TRUE, conf.int = TRUE)

  model_df <- exp_wilcox(data_long, value, period, paired = TRUE, conf.int = TRUE)
  ret <- model_df %>% tidy_rowwise(model, type = "model")

  # p-value should match exactly
  expect_equal(ret$`P Value`[[1]], direct_result$p.value[[1]])
  # W values should be complementary (sum to n*(n+1)/2) due to fct_rev reordering
  expect_equal(ret$`W Value`[[1]] + direct_result$statistic[[1]], n * (n + 1) / 2)
  # Estimate (pseudo-median of differences) should have same magnitude, opposite sign
  expect_equal(abs(ret$Difference[[1]]), abs(direct_result$estimate[[1]]), tolerance = 1e-4)
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
