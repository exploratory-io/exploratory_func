# Test One-way ANOVA.
context("Test One-way ANOVA")



test_that("Test output between aov and oneway.test with equal variance", {
  # This test is to check if the output of aov and oneway.test with equal variance are the same.
  # This is to make sure that the output of exp_anova is correct.

  # Create sample data
  set.seed(123)  # For reproducibility
  data <- data.frame(
      group = factor(rep(c("A", "B", "C"), each = 10)),
      value = c(
          rnorm(10, mean = 10, sd = 2),  # Group A
          rnorm(10, mean = 12, sd = 2),  # Group B
          rnorm(10, mean = 15, sd = 2)   # Group C
      )
  )

  # Run both tests
  aov_result <- aov(value ~ group, data = data)
  aov_summary <- summary(aov_result)[[1]]
  aov_f_value <- unname(aov_summary$`F value`[1])  # Remove names attribute
  aov_p_value <- unname(aov_summary$`Pr(>F)`[1])   # Remove names attribute

  oneway_result <- oneway.test(value ~ group, data = data, var.equal = TRUE)
  oneway_f_value <- as.numeric(oneway_result$statistic)
  oneway_p_value <- oneway_result$p.value

  # Check if the F value and p value are the same.
  expect_equal(aov_f_value, oneway_f_value)
  expect_equal(aov_p_value, oneway_p_value)
})

test_that("Test One-way ANOVA", {
  # Set seed for reproducibility
  set.seed(123)

  # Create base data frame
  n <- 1000  # Total number of rows

  # Create group variable with 2 categories and scattered NAs
  groups <- sample(c("Group A", "Group B", NA), n, replace = TRUE, prob = c(0.4, 0.4, 0.2))

  # Create numeric values with scattered NAs
  values <- rnorm(n, mean = 10, sd = 2)  # Generate random normal values
  na_positions_values <- sample(1:n, 100)  # Select 100 random positions for NA (keeping 10% NA rate)
  values[na_positions_values] <- NA

  # Create facet variable with 2 categories and scattered NAs
  facets <- sample(c("Facet 1", "Facet 2", NA), n, replace = TRUE, prob = c(0.4, 0.4, 0.2))

  # Combine into data frame
  test_df <- data.frame(
    group = groups,
    value = values,
    facet = facets
  )

  #
  # var.equal=FALSE
  #

  # Model creation
  ret <- test_df %>% dplyr::mutate(`group` = forcats::fct_lump(factor(`group`), n=20, other_level = "Others", ties.method ="first")) %>%
    exp_anova(`value`, `group`)

  # Post-hoc test
  ret.pairs <- ret %>% tidy_rowwise(model, type="pairs", pairs_adjust="tukey")
  expect_equal(colnames(ret.pairs), 
              c("Group 1", "Group 2", "Difference", "Conf Low", "Conf High", 
                "Standard Error", "DF", "t Value", "P Value", "Method"))
  expect_equal(ret.pairs$Difference, 0.235, tolerance = 0.001)
  expect_equal(ret.pairs$`P Value`, 0.115, tolerance = 0.001)
  expect_equal(ret.pairs$DF, 720)

  ret.pairs <- ret %>% tidy_rowwise(model, type="pairs", pairs_adjust="bonferroni")
  expect_equal(colnames(ret.pairs), 
              c("Group 1", "Group 2", "Difference", "Conf Low", "Conf High", 
                "Standard Error", "DF", "t Value", "P Value", "Method"))
  expect_equal(ret.pairs$Difference, 0.235, tolerance = 0.001)
  expect_equal(ret.pairs$`P Value`, 0.115, tolerance = 0.001)
  expect_equal(ret.pairs$DF, 720)

  ret.model <- ret %>% tidy_rowwise(model, type="model")
  expect_equal(colnames(ret.model),
              c("Type of Variance", "Sum of Squares", "SS Ratio", "DF", "Mean Square", 
                "F Value", "P Value", "Power", "Type 2 Error", "Rows", 
                "Eta Squared", "Partial Eta Squared", "Cohen's F", "Omega Squared"))
  expect_equal(ret.model$`F Value`[1], 2.50, tolerance = 0.01)
  expect_equal(ret.model$`P Value`[1], 0.114, tolerance = 0.001)
  expect_equal(ret.model$Power[1], 0.236, tolerance = 0.001)


  ret.shapiro <- ret %>% tidy_rowwise(model, type="shapiro", shapiro_seed=)
  expect_equal(colnames(ret.shapiro),
              c("W Value", "P Value", "Method", "Rows", "Result"))
  expect_equal(unname(ret.shapiro$`W Value`), 0.997, tolerance = 0.001)
  expect_equal(unname(ret.shapiro$`P Value`), 0.137, tolerance = 0.001)
  expect_equal(ret.shapiro$Rows, 900)
  expect_equal(ret.shapiro$Result, "Normality assumption is valid.")

  ret.prob_dist <- ret %>% tidy_rowwise(model, type="prob_dist")
  expect_true("p.value" %in% colnames(ret.prob_dist))
  expect_equal(nrow(ret.prob_dist), 1002)
  expect_equal(ncol(ret.prob_dist), 7)

  # With group_by
  ret <- test_df %>% 
    dplyr::mutate(`facet` = `_tam_create_logical_factor`(`facet`)) %>%
    dplyr::mutate(`facet` = `_tam_convert_na`(`facet`, drop.unused.levels=FALSE)) %>%
    dplyr::mutate(`facet` = forcats::fct_lump(factor(`facet`), n=20, other_level = "Others", ties.method ="first")) %>%
    dplyr::mutate(`group` = forcats::fct_lump(factor(`group`), n=20, other_level = "Others", ties.method ="first")) %>%
    group_by(`facet`) %>%
    exp_anova(`value`, `group`)

  # Post-hoc test
  ret.pairs <- ret %>% tidy_rowwise(model, type="pairs", pairs_adjust="tukey")
  expect_equal(colnames(ret.pairs), 
              c("facet", "Group 1", "Group 2", "Difference", "Conf Low", "Conf High", 
                "Standard Error", "DF", "t Value", "P Value", "Method"))
  expect_equal(nrow(ret.pairs), 3)  # One for each facet level
  expect_equal(ret.pairs$Difference, c(0.243, 0.268, 0.125), tolerance = 0.001)
  expect_equal(ret.pairs$`P Value`, c(0.328, 0.255, 0.686), tolerance = 0.001)
  expect_equal(ret.pairs$DF, c(275, 282, 159))


  ret.model <- ret %>% tidy_rowwise(model, type="model")
  expect_equal(colnames(ret.model),
              c("facet", "Type of Variance", "Sum of Squares", "SS Ratio", "DF", "Mean Square", 
                "F Value", "P Value", "Power", "Type 2 Error", "Rows", 
                "Eta Squared", "Partial Eta Squared", "Cohen's F", "Omega Squared"))
  expect_equal(nrow(ret.model), 9)  # 3 rows for each facet level
  expect_equal(ret.model$`F Value`[c(1,4,7)], c(0.966, 1.31, 0.166), tolerance = 0.01)
  expect_equal(ret.model$Power[c(1,4,7)], c(0.210, 0.125, 0.0645), tolerance = 0.001)

  ret.pairs <- ret %>% tidy_rowwise(model, type="pairs", pairs_adjust="bonferroni")
  expect_equal(colnames(ret.pairs), 
              c("facet", "Group 1", "Group 2", "Difference", "Conf Low", "Conf High", 
                "Standard Error", "DF", "t Value", "P Value", "Method"))
  expect_equal(nrow(ret.pairs), 3)  # One for each facet level
  expect_equal(ret.pairs$Difference, c(0.243, 0.268, 0.125), tolerance = 0.001)
  expect_equal(ret.pairs$`P Value`, c(0.326, 0.252, 0.684), tolerance = 0.005)
  expect_equal(ret.pairs$DF, c(275, 282, 159))

  ret.shapiro <- ret %>% tidy_rowwise(model, type="shapiro", shapiro_seed=)
  expect_equal(colnames(ret.shapiro),
              c("facet", "W Value", "P Value", "Method", "Rows", "Result"))
  expect_equal(nrow(ret.shapiro), 3)
  expect_equal(unname(ret.shapiro$`W Value`), c(0.992, 0.994, 0.995), tolerance = 0.001)
  expect_equal(unname(ret.shapiro$`P Value`), c(0.161, 0.292, 0.833), tolerance = 0.001)
  expect_equal(ret.shapiro$Result, rep("Normality assumption is valid.", 3))

  ret.prob_dist <- ret %>% tidy_rowwise(model, type="prob_dist")
  expect_true("p.value" %in% colnames(ret.prob_dist))
  expect_equal(nrow(ret.prob_dist), 3006)  # 1002 rows for each facet level
  expect_equal(ncol(ret.prob_dist), 8)  # Including facet column

  #
  # var.equal=TRUE
  #

  # Model creation
  ret <- test_df %>% dplyr::mutate(`group` = forcats::fct_lump(factor(`group`), n=20, other_level = "Others", ties.method ="first")) %>%
  exp_anova(`value`, `group`, var.equal=TRUE)

  # Post-hoc test with var.equal=TRUE
  ret.pairs <- ret %>% tidy_rowwise(model, type="pairs", pairs_adjust="tukey")
  expect_equal(colnames(ret.pairs), 
              c("Group 1", "Group 2", "Difference", "Conf Low", "Conf High", 
                "Standard Error", "DF", "t Value", "P Value", "Method"))
  expect_equal(ret.pairs$Difference, 0.235, tolerance = 0.001)
  expect_equal(ret.pairs$`P Value`, 0.114, tolerance = 0.001)
  expect_equal(ret.pairs$DF, 720)

  ret.pairs <- ret %>% tidy_rowwise(model, type="pairs", pairs_adjust="bonferroni")
  expect_equal(colnames(ret.pairs), 
              c("Group 1", "Group 2", "Difference", "Conf Low", "Conf High", 
                "Standard Error", "DF", "t Value", "P Value", "Method"))
  expect_equal(ret.pairs$Difference, 0.235, tolerance = 0.001)
  expect_equal(ret.pairs$`P Value`, 0.114, tolerance = 0.001)
  expect_equal(ret.pairs$DF, 720)

  ret.model <- ret %>% tidy_rowwise(model, type="model")
  expect_equal(colnames(ret.model),
              c("Type of Variance", "Sum of Squares", "SS Ratio", "DF", "Mean Square", 
                "F Value", "P Value", "Power", "Type 2 Error", "Rows", 
                "Eta Squared", "Partial Eta Squared", "Cohen's F", "Omega Squared"))
  expect_equal(ret.model$`F Value`[1], 2.50, tolerance = 0.01)
  expect_equal(ret.model$`P Value`[1], 0.114, tolerance = 0.001)
  expect_equal(ret.model$Power[1], 0.236, tolerance = 0.001)


  ret.shapiro <- ret %>% tidy_rowwise(model, type="shapiro", shapiro_seed=)
  expect_equal(colnames(ret.shapiro),
              c("W Value", "P Value", "Method", "Rows", "Result"))
  expect_equal(unname(ret.shapiro$`W Value`), 0.997, tolerance = 0.001)
  expect_equal(unname(ret.shapiro$`P Value`), 0.167, tolerance = 0.001)
  expect_equal(ret.shapiro$Rows, 900)
  expect_equal(ret.shapiro$Result, "Normality assumption is valid.")

  ret.prob_dist <- ret %>% tidy_rowwise(model, type="prob_dist")
  expect_true("p.value" %in% colnames(ret.prob_dist))
  expect_equal(nrow(ret.prob_dist), 1002)
  expect_equal(ncol(ret.prob_dist), 7)

  # With group_by
  ret <- test_df %>% 
    dplyr::mutate(`facet` = `_tam_create_logical_factor`(`facet`)) %>%
    dplyr::mutate(`facet` = `_tam_convert_na`(`facet`, drop.unused.levels=FALSE)) %>%
    dplyr::mutate(`facet` = forcats::fct_lump(factor(`facet`), n=20, other_level = "Others", ties.method ="first")) %>%
    dplyr::mutate(`group` = forcats::fct_lump(factor(`group`), n=20, other_level = "Others", ties.method ="first")) %>%
    group_by(`facet`) %>%
    exp_anova(`value`, `group`, var.equal=TRUE)

  # Post-hoc test
  ret.pairs <- ret %>% tidy_rowwise(model, type="pairs", pairs_adjust="tukey")
  expect_equal(colnames(ret.pairs), 
              c("facet", "Group 1", "Group 2", "Difference", "Conf Low", "Conf High", 
                "Standard Error", "DF", "t Value", "P Value", "Method"))
  expect_equal(nrow(ret.pairs), 3)  # One for each facet level
  expect_equal(ret.pairs$Difference, c(0.243, 0.268, 0.125), tolerance = 0.001)
  expect_equal(ret.pairs$`P Value`, c(0.326, 0.252, 0.684), tolerance = 0.005)
  expect_equal(ret.pairs$DF, c(275, 282, 159))

  ret.pairs <- ret %>% tidy_rowwise(model, type="pairs", pairs_adjust="bonferroni")
  expect_equal(colnames(ret.pairs), 
              c("facet", "Group 1", "Group 2", "Difference", "Conf Low", "Conf High", 
                "Standard Error", "DF", "t Value", "P Value", "Method"))
  expect_equal(nrow(ret.pairs), 3)  # One for each facet level
  expect_equal(ret.pairs$Difference, c(0.243, 0.268, 0.125), tolerance = 0.001)
  expect_equal(ret.pairs$`P Value`, c(0.326, 0.252, 0.684), tolerance = 0.005)
  expect_equal(ret.pairs$DF, c(275, 282, 159))

  ret.model <- ret %>% tidy_rowwise(model, type="model")
  expect_equal(colnames(ret.model),
              c("facet", "Type of Variance", "Sum of Squares", "SS Ratio", "DF", "Mean Square", 
                "F Value", "P Value", "Power", "Type 2 Error", "Rows", 
                "Eta Squared", "Partial Eta Squared", "Cohen's F", "Omega Squared"))
  expect_equal(nrow(ret.model), 9)  # 3 rows for each facet level
  expect_equal(ret.model$`F Value`[c(1,4,7)], c(0.969, 1.32, 0.166), tolerance = 0.01)
  expect_equal(ret.model$Power[c(1,4,7)], c(0.210, 0.125, 0.0645), tolerance = 0.001)

  ret.shapiro <- ret %>% tidy_rowwise(model, type="shapiro", shapiro_seed=)
  expect_equal(colnames(ret.shapiro),
              c("facet", "W Value", "P Value", "Method", "Rows", "Result"))
  expect_equal(nrow(ret.shapiro), 3)
  expect_equal(unname(ret.shapiro$`W Value`), c(0.993, 0.994, 0.995), tolerance = 0.001)
  expect_equal(unname(ret.shapiro$`P Value`), c(0.200, 0.259, 0.898), tolerance = 0.001)
  expect_equal(ret.shapiro$Result, rep("Normality assumption is valid.", 3))

  ret.prob_dist <- ret %>% tidy_rowwise(model, type="prob_dist")
  expect_true("p.value" %in% colnames(ret.prob_dist))
  expect_equal(nrow(ret.prob_dist), 3006)  # 1002 rows for each facet level
  expect_equal(ncol(ret.prob_dist), 8)  # Including facet column
})
