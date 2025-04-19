context("test paired ttest")
test_that("test paired ttest", {
  # Base data for the paired sample t-test.
  wide.data <- data.frame(
    Subject = c(1:50),
    Before_Treatment = c(152.5, 149.3, 153.2, 157.6, 148.8, 148.8, 157.9, 153.8, 147.7, 152.7,
                      147.7, 147.7, 151.2, 140.4, 141.4, 147.2, 144.9, 151.6, 145.5, 142.9,
                      157.3, 148.9, 150.3, 142.9, 147.3, 150.6, 144.2, 151.9, 147.0, 148.5,
                      147.0, 159.3, 149.9, 144.7, 154.1, 143.9, 151.0, 140.2, 143.4, 151.0,
                      153.7, 150.9, 149.4, 148.5, 142.6, 146.4, 147.7, 155.3, 151.7, 141.2),
    After_Treatment = c(143.8, 142.1, 146.6, 148.4, 138.8, 139.0, 151.6, 146.5, 139.0, 142.8,
                      140.6, 140.0, 145.4, 134.8, 131.8, 136.5, 137.1, 141.6, 136.7, 136.2,
                      148.6, 137.8, 142.4, 131.7, 144.5, 140.9, 136.1, 144.5, 138.8, 144.5,
                      139.4, 150.5, 139.0, 137.7, 147.7, 136.9, 141.2, 131.5, 136.4, 142.0,
                      145.5, 140.9, 142.8, 141.1, 135.4, 141.3, 139.1, 146.8, 143.7, 133.7)
  )

  # Convert the wide data to long data.
  long.data <- wide.data %>%
     pivot_longer(cols = c(Before_Treatment, After_Treatment), names_to = "Group", values_to = "Value")

  # Run the base t-test.
  base.model <- t.test(wide.data$Before_Treatment, wide.data$After_Treatment, paired = TRUE)

  # Run the exploratory t-test.
  model_df <- exp_ttest(long.data, Value, Group, paired = TRUE)

  # Compare the base model and the exploratory model. 
  expect_equal(base.model$p.value, model_df$model[[1]]$p.value)
  expect_equal(base.model$estimate, model_df$model[[1]]$estimate)
  expect_equal(base.model$conf.int, model_df$model[[1]]$conf.int)
  expect_equal(base.model$statistic, model_df$model[[1]]$statistic)
  expect_equal(base.model$stderr, model_df$model[[1]]$stderr)

  # model type output
  ret <- model_df %>% tidy_rowwise(model, type="model")
  expect_equal(colnames(ret),
               c("t Value", "P Value", "DF", "Difference", "Conf Low", "Conf High", "Base Level", "Cohen's D", "Power", "Type 2 Error",
                 "Rows", "Rows (After_Treatment)", "Rows (Before_Treatment)"))

  # data summary type output
  ret <- model_df %>% tidy_rowwise(model, type="data_summary")
  expect_equal(colnames(ret),
               c("Group","Rows","Mean","Conf Low","Conf High","Std Error of Mean","Std Deviation",
                 "Minimum","Maximum"))

  # Test different combinations of arguments
  # Test case 1: Change significance level
  model_df1 <- exp_ttest(long.data, Value, Group, paired = TRUE, test_sig_level = 0.01)
  expect_equal(model_df1$model[[1]]$p.value, model_df$model[[1]]$p.value)
  expect_equal(model_df1$model[[1]]$estimate, model_df$model[[1]]$estimate)

  # Test case 2: Change effect size (d)
  model_df2 <- exp_ttest(long.data, Value, Group, paired = TRUE, d = 0.5)
  expect_equal(model_df2$model[[1]]$p.value, model_df$model[[1]]$p.value)
  expect_equal(model_df2$model[[1]]$estimate, model_df$model[[1]]$estimate)

  # Test case 3: Change common standard deviation
  model_df3 <- exp_ttest(long.data, Value, Group, paired = TRUE, common_sd = 5)
  expect_equal(model_df3$model[[1]]$p.value, model_df$model[[1]]$p.value)
  expect_equal(model_df3$model[[1]]$estimate, model_df$model[[1]]$estimate)

  # Test case 4: Change difference to detect
  model_df4 <- exp_ttest(long.data, Value, Group, paired = TRUE, diff_to_detect = 2)
  expect_equal(model_df4$model[[1]]$p.value, model_df$model[[1]]$p.value)
  expect_equal(model_df4$model[[1]]$estimate, model_df$model[[1]]$estimate)

  # Test case 5: Change power
  model_df5 <- exp_ttest(long.data, Value, Group, paired = TRUE, power = 0.9)
  expect_equal(model_df5$model[[1]]$p.value, model_df$model[[1]]$p.value)
  expect_equal(model_df5$model[[1]]$estimate, model_df$model[[1]]$estimate)

  # Test case 6: Change beta
  model_df6 <- exp_ttest(long.data, Value, Group, paired = TRUE, beta = 0.2)
  expect_equal(model_df6$model[[1]]$p.value, model_df$model[[1]]$p.value)
  expect_equal(model_df6$model[[1]]$estimate, model_df$model[[1]]$estimate)

  # Test case 7: Add outlier filtering
  model_df7 <- exp_ttest(long.data, Value, Group, paired = TRUE, 
                        outlier_filter_type = "percentile", 
                        outlier_filter_threshold = 0.95)
  expect_true(nrow(model_df7$model[[1]]$data) <= nrow(model_df$model[[1]]$data))

  # Test case 8: Combined test with multiple parameters
  model_df8 <- exp_ttest(long.data, Value, Group, paired = TRUE,
                        test_sig_level = 0.01,
                        d = 0.5,
                        power = 0.9,
                        outlier_filter_type = "percentile",
                        outlier_filter_threshold = 0.95)
  expect_true(nrow(model_df8$model[[1]]$data) <= nrow(model_df$model[[1]]$data))

  # Test case 9: Test with different outlier filter type
  model_df9 <- exp_ttest(long.data, Value, Group, paired = TRUE,
                        outlier_filter_type = "z_score",
                        outlier_filter_threshold = 2.5)
  expect_true(nrow(model_df9$model[[1]]$data) <= nrow(model_df$model[[1]]$data))

  # Test case 10: Test with factor explanatory variable
  long.data.factor <- long.data %>% mutate(Group = factor(Group, levels = c("After_Treatment", "Before_Treatment")))
  model_df10 <- exp_ttest(long.data.factor, Value, Group, paired = TRUE)
  expect_equal(model_df10$model[[1]]$p.value, model_df$model[[1]]$p.value)
  expect_equal(model_df10$model[[1]]$estimate, model_df$model[[1]]$estimate)

  # Test case 11: Test with numeric explanatory variable
  long.data.numeric <- long.data %>% mutate(Group = ifelse(Group == "Before_Treatment", 2, 1))
  model_df11 <- exp_ttest(long.data.numeric, Value, Group, paired = TRUE)
  expect_equal(model_df11$model[[1]]$p.value, model_df$model[[1]]$p.value)
  expect_equal(model_df11$model[[1]]$estimate, model_df$model[[1]]$estimate)

  # Test case 12: Test with logical explanatory variable
  long.data.logical <- long.data %>% mutate(Group = Group == "Before_Treatment")
  model_df12 <- exp_ttest(long.data.logical, Value, Group, paired = TRUE)
  expect_equal(model_df12$model[[1]]$p.value, model_df$model[[1]]$p.value)
  expect_equal(model_df12$model[[1]]$estimate, model_df$model[[1]]$estimate)

  # Test case 13: Test with uneven sample size between groups
  long.data.uneven <- data.frame(
    Subject = c(1:50),
    Group = c(rep("Before_Treatment", 20), rep("After_Treatment", 30)),
    Value = c(rnorm(20), rnorm(30))
  )
  expect_error(exp_ttest(long.data.uneven, Value, Group, paired = TRUE), 
               "Paired t-test requires equal number of observations in both groups")

  # Test case 14: Test with more than 2 groups
  long.data.more.than.2.groups <- data.frame(
    Subject = c(1:70),
    Group = c(rep("Before_Treatment", 20), rep("After_Treatment", 30), rep("Control", 20)),
    Value = c(rnorm(20), rnorm(30), rnorm(20))
  )
  expect_error(exp_ttest(long.data.more.than.2.groups, Value, Group, paired = TRUE), 
               "The explanatory variable needs to have 2 unique values.")
})

test_that("test paired ttest with grouped data", {
  # Base data for the paired sample t-test.
  wide.data <- data.frame(
    Subject = c(1:50),
    Before_Treatment = c(152.5, 149.3, 153.2, 157.6, 148.8, 148.8, 157.9, 153.8, 147.7, 152.7,
                       147.7, 147.7, 151.2, 140.4, 141.4, 147.2, 144.9, 151.6, 145.5, 142.9,
                       157.3, 148.9, 150.3, 142.9, 147.3, 150.6, 144.2, 151.9, 147.0, 148.5,
                       147.0, 159.3, 149.9, 144.7, 154.1, 143.9, 151.0, 140.2, 143.4, 151.0,
                       153.7, 150.9, 149.4, 148.5, 142.6, 146.4, 147.7, 155.3, 151.7, 141.2),
    After_Treatment = c(143.8, 142.1, 146.6, 148.4, 138.8, 139.0, 151.6, 146.5, 139.0, 142.8,
                       140.6, 140.0, 145.4, 134.8, 131.8, 136.5, 137.1, 141.6, 136.7, 136.2,
                       148.6, 137.8, 142.4, 131.7, 144.5, 140.9, 136.1, 144.5, 138.8, 144.5,
                       139.4, 150.5, 139.0, 137.7, 147.7, 136.9, 141.2, 131.5, 136.4, 142.0,
                       145.5, 140.9, 142.8, 141.1, 135.4, 141.3, 139.1, 146.8, 143.7, 133.7),
    Category = c(rep("Cat1", 20), rep("Cat2", 20), rep("Cat3", 10))
  )

  # Convert the wide data to long data.
  long.data <- wide.data %>%
     pivot_longer(cols = c(Before_Treatment, After_Treatment), names_to = "Group", values_to = "Value")

  # Create base models for Cat1. 
  wide.data.cat1 <- wide.data %>% filter(Category == "Cat1")
  base.model.cat1 <- t.test(wide.data.cat1$Before_Treatment, wide.data.cat1$After_Treatment, paired = TRUE)

  # Run the exploratory t-test.
  model_df <- long.data %>% group_by(Category) %>% exp_ttest(Value, Group, paired = TRUE)

  # Compare the base model and the exploratory model. 
  expect_equal(base.model.cat1$p.value, model_df$model[[1]]$p.value)
  expect_equal(base.model.cat1$estimate, model_df$model[[1]]$estimate)

  # Test different combinations of arguments with grouped data
  # Test case 1: Change significance level
  model_df1 <- long.data %>% 
    group_by(Category) %>% 
    exp_ttest(Value, Group, paired = TRUE, test_sig_level = 0.01)
  expect_equal(model_df1$model[[1]]$p.value, model_df$model[[1]]$p.value)
  expect_equal(model_df1$model[[1]]$estimate, model_df$model[[1]]$estimate)

  # Test case 2: Change effect size (d)
  model_df2 <- long.data %>% 
    group_by(Category) %>% 
    exp_ttest(Value, Group, paired = TRUE, d = 0.5)
  expect_equal(model_df2$model[[1]]$p.value, model_df$model[[1]]$p.value)
  expect_equal(model_df2$model[[1]]$estimate, model_df$model[[1]]$estimate)

  # Test case 3: Change common standard deviation
  model_df3 <- long.data %>% 
    group_by(Category) %>% 
    exp_ttest(Value, Group, paired = TRUE, common_sd = 5)
  expect_equal(model_df3$model[[1]]$p.value, model_df$model[[1]]$p.value)
  expect_equal(model_df3$model[[1]]$estimate, model_df$model[[1]]$estimate)

  # Test case 4: Change difference to detect
  model_df4 <- long.data %>% 
    group_by(Category) %>% 
    exp_ttest(Value, Group, paired = TRUE, diff_to_detect = 2)
  expect_equal(model_df4$model[[1]]$p.value, model_df$model[[1]]$p.value)
  expect_equal(model_df4$model[[1]]$estimate, model_df$model[[1]]$estimate)

  # Test case 5: Change power
  model_df5 <- long.data %>% 
    group_by(Category) %>% 
    exp_ttest(Value, Group, paired = TRUE, power = 0.9)
  expect_equal(model_df5$model[[1]]$p.value, model_df$model[[1]]$p.value)
  expect_equal(model_df5$model[[1]]$estimate, model_df$model[[1]]$estimate)

  # Test case 6: Change beta
  model_df6 <- long.data %>% 
    group_by(Category) %>% 
    exp_ttest(Value, Group, paired = TRUE, beta = 0.2)
  expect_equal(model_df6$model[[1]]$p.value, model_df$model[[1]]$p.value)
  expect_equal(model_df6$model[[1]]$estimate, model_df$model[[1]]$estimate)

  # Test case 7: Add outlier filtering
  model_df7 <- long.data %>% 
    group_by(Category) %>% 
    exp_ttest(Value, Group, paired = TRUE, 
              outlier_filter_type = "percentile", 
              outlier_filter_threshold = 0.95)
  expect_true(nrow(model_df7$model[[1]]$data) <= nrow(model_df$model[[1]]$data))

  # Test case 8: Combined test with multiple parameters
  model_df8 <- long.data %>% 
    group_by(Category) %>% 
    exp_ttest(Value, Group, paired = TRUE,
              test_sig_level = 0.01,
              d = 0.5,
              power = 0.9,
              outlier_filter_type = "percentile",
              outlier_filter_threshold = 0.95)
  expect_true(nrow(model_df8$model[[1]]$data) <= nrow(model_df$model[[1]]$data))

  # Test case 9: Test with different outlier filter type
  model_df9 <- long.data %>% 
    group_by(Category) %>% 
    exp_ttest(Value, Group, paired = TRUE,
              outlier_filter_type = "z_score",
              outlier_filter_threshold = 2.5)
  expect_true(nrow(model_df9$model[[1]]$data) <= nrow(model_df$model[[1]]$data))

  # Test case 10: Test with factor explanatory variable
  long.data.factor <- long.data %>% 
    mutate(Group = factor(Group, levels = c("After_Treatment", "Before_Treatment")))
  model_df10 <- long.data.factor %>% 
    group_by(Category) %>% 
    exp_ttest(Value, Group, paired = TRUE)
  expect_equal(model_df10$model[[1]]$p.value, model_df$model[[1]]$p.value)
  expect_equal(model_df10$model[[1]]$estimate, model_df$model[[1]]$estimate)

  # Test case 11: Test with numeric explanatory variable
  long.data.numeric <- long.data %>% 
    mutate(Group = ifelse(Group == "Before_Treatment", 2, 1))
  model_df11 <- long.data.numeric %>% 
    group_by(Category) %>% 
    exp_ttest(Value, Group, paired = TRUE)
  expect_equal(model_df11$model[[1]]$p.value, model_df$model[[1]]$p.value)
  expect_equal(model_df11$model[[1]]$estimate, model_df$model[[1]]$estimate)

  # Test case 12: Test with logical explanatory variable
  long.data.logical <- long.data %>% 
    mutate(Group = Group == "Before_Treatment")
  model_df12 <- long.data.logical %>% 
    group_by(Category) %>% 
    exp_ttest(Value, Group, paired = TRUE)
  expect_equal(model_df12$model[[1]]$p.value, model_df$model[[1]]$p.value)
  expect_equal(model_df12$model[[1]]$estimate, model_df$model[[1]]$estimate)

  # Test case 13: Test with uneven sample size between groups within categories
  long.data.uneven <- data.frame(
    Subject = c(1:70),
    Category = c(rep("Cat1", 20), rep("Cat2", 20), rep("Cat3", 30)),
    Group = c(rep("Before_Treatment", 10), rep("After_Treatment", 10), # Cat1: 10-10
              rep("Before_Treatment", 15), rep("After_Treatment", 5),  # Cat2: 15-5
              rep("Before_Treatment", 20), rep("After_Treatment", 10)), # Cat3: 20-10
    Value = c(rnorm(70))
  )
  
  # It shouldn't throw an error for paired t-test with uneven groups within categories.
  # You should get models for each category, and each model tells it has an error or not.
  model_df13 <- long.data.uneven %>% 
    group_by(Category) %>% 
    exp_ttest(Value, Group, paired = TRUE)
  expect_true("ttest_exploratory" %in% class(model_df13$model[[1]]))
  expect_true("ttest_exploratory" %in% class(model_df13$model[[2]]))
  expect_true("error" %in% class(model_df13$model[[2]]))
  expect_equal(model_df13$model[[2]]$message, "Paired t-test requires equal number of observations in both groups")
  expect_true("ttest_exploratory" %in% class(model_df13$model[[3]]))

  # Test case 14: Test with more than 2 groups within a category
  long.data.more.than.2.groups <- data.frame(
    Subject = c(1:90),
    Category = c(rep("Cat1", 30), rep("Cat2", 30), rep("Cat3", 30)),
    Group = c(rep("Before_Treatment", 10), rep("After_Treatment", 10), rep("Control", 10), # Cat1
              rep("Before_Treatment", 10), rep("After_Treatment", 10), rep("Control", 10), # Cat2
              rep("Before_Treatment", 10), rep("After_Treatment", 10), rep("Control", 10)), # Cat3
    Value = c(rnorm(90))
  )
  
  expect_error(long.data.more.than.2.groups %>% 
                group_by(Category) %>% 
                exp_ttest(Value, Group, paired = TRUE), 
               "The explanatory variable needs to have 2 unique values.")
})
