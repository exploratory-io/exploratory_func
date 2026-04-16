
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
  set.seed(1) # slice_sample is RNG-dependent.
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
  set.seed(1) # slice_sample is RNG-dependent.
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

test_that("test exp_wilcox paired W value matches direct wilcox.test with same vector order", {
  # Use small data without ties for exact p-value
  set.seed(123)
  n <- 10
  before <- round(rnorm(n, mean = 50, sd = 5), 1)
  after <- round(rnorm(n, mean = 45, sd = 5), 1)

  data_long <- tibble::tibble(
    subject_id = rep(paste0("P", sprintf("%02d", 1:n)), 2),
    period = factor(rep(c("before", "after"), each = n), levels = c("before", "after")),
    value = c(before, after)
  )

  # exp_wilcox uses fct_rev, so effectively calls wilcox.test(after, before).
  # Match that order for direct comparison.
  direct_result <- wilcox.test(after, before, paired = TRUE)

  model_df <- exp_wilcox(data_long, value, period, paired = TRUE)
  ret <- model_df %>% tidy_rowwise(model, type = "model")

  # W value should match exactly when using the same vector order
  expect_equal(ret$`W Value`[[1]], direct_result$statistic[[1]])
  expect_equal(ret$`P Value`[[1]], direct_result$p.value[[1]])
})

test_that("test exp_wilcox paired with repeat-by grouping", {
  set.seed(42)
  n_per_group <- 15

  # Create data with two groups (e.g., male/female), each with before/after measurements
  data_long <- tibble::tibble(
    subject_id = c(paste0("M", sprintf("%02d", 1:n_per_group)),
                   paste0("M", sprintf("%02d", 1:n_per_group)),
                   paste0("F", sprintf("%02d", 1:n_per_group)),
                   paste0("F", sprintf("%02d", 1:n_per_group))),
    group = factor(c(rep("male", n_per_group * 2), rep("female", n_per_group * 2))),
    period = factor(rep(c(rep("before", n_per_group), rep("after", n_per_group)), 2),
                    levels = c("before", "after")),
    value = c(
      round(rnorm(n_per_group, mean = 70, sd = 10), 1),  # male before
      round(rnorm(n_per_group, mean = 65, sd = 10), 1),  # male after
      round(rnorm(n_per_group, mean = 60, sd = 10), 1),  # female before
      round(rnorm(n_per_group, mean = 55, sd = 10), 1)   # female after
    )
  )

  # Run exp_wilcox with group_by (repeat-by)
  model_df <- data_long %>% dplyr::group_by(group) %>% exp_wilcox(value, period, paired = TRUE)
  ret <- model_df %>% tidy_rowwise(model, type = "model")

  # Should have results for both groups
  expect_equal(nrow(ret), 2)
  # Should have W Value column (signed-rank test)
  expect_true("W Value" %in% colnames(ret))
  expect_true("P Value" %in% colnames(ret))

  # Verify each group's result matches direct wilcox.test
  for (g in c("male", "female")) {
    group_data <- data_long %>% dplyr::filter(group == g)
    before_vals <- group_data$value[group_data$period == "before"]
    after_vals <- group_data$value[group_data$period == "after"]
    # exp_wilcox uses fct_rev, so effective order is (after, before)
    direct <- wilcox.test(after_vals, before_vals, paired = TRUE)
    group_ret <- ret %>% dplyr::filter(group == g)
    expect_equal(group_ret$`W Value`[[1]], direct$statistic[[1]])
    expect_equal(group_ret$`P Value`[[1]], direct$p.value[[1]])
  }
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
