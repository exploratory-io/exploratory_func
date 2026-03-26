context("tests for wrappers of tests")
test_df <- data.frame(
  cat=rep(c("cat1", "cat2"), 20),
  dim = sort(rep(paste0("dim", seq(4)), 5)),
  dim_na=c(paste0("dim", seq(10)), paste0("dim", seq(10)+3)))

test_df$list_c <- as.list(seq(20))

test_df[["with space"]] <- seq(20)

test_that("test t.test.aggregated with two.sided alternative (default)", {
  test_df <- data.frame(
    cat=factor(rep(c("cat1", "cat2"), 20), levels = c("cat1", "cat2")),
    val = rep(seq(10), 2)
  )
  test_df2 <- test_df %>% group_by(cat) %>% summarize(n=n(), sd=sd(val), mean=mean(val))
  # Compare the outputs between stats::t.test and exploratory:::t.test.aggregated
  res0 <- stats::t.test(data=test_df, val~cat)
  res <- exploratory:::t.test.aggregated(test_df2$n[1],test_df2$n[2],test_df2$mean[1],test_df2$mean[2],test_df2$sd[1],test_df2$sd[2],0.95,0)
  expect_equal(res$statistic, res0$statistic)
  expect_equal(res$parameter, res0$parameter)
  expect_equal(res$p.value, res0$p.value)
  expect_equal(res$conf.int, res0$conf.int)
  names(res0$estimate) <- NULL # Ignore names difference, which we did not implement.
  expect_equal(res$estimate, res0$estimate)
  expect_equal(res$stderr, res0$stderr)
  names(res0$null.value) <- NULL # Ignore names difference, which we did not implement.
  expect_equal(res$null.value, res0$null.value)
})

test_that("test t.test.aggregated with 'less' alternative", {
  test_df <- data.frame(
    cat=factor(rep(c("cat1", "cat2"), 20), levels = c("cat1", "cat2")),
    val = rep(seq(10), 2)
  )
  test_df2 <- test_df %>% group_by(cat) %>% summarize(n=n(), sd=sd(val), mean=mean(val))
  # Compare the outputs between stats::t.test and exploratory:::t.test.aggregated
  res0 <- stats::t.test(data=test_df, val~cat, alternative="less")
  res <- exploratory:::t.test.aggregated(test_df2$n[1],test_df2$n[2],test_df2$mean[1],test_df2$mean[2],test_df2$sd[1],test_df2$sd[2],0.95,0, alternative="less")
  expect_equal(res$statistic, res0$statistic)
  expect_equal(res$parameter, res0$parameter)
  expect_equal(res$p.value, res0$p.value)
  expect_equal(res$conf.int, res0$conf.int)
  names(res0$estimate) <- NULL # Ignore names difference, which we did not implement.
  expect_equal(res$estimate, res0$estimate)
  expect_equal(res$stderr, res0$stderr)
  expect_equal(res$alternative, res0$alternative)
  names(res0$null.value) <- NULL # Ignore names difference, which we did not implement.
  expect_equal(res$null.value, res0$null.value)
})
test_that("test t.test.aggregated with 'greater' alternative", {
  test_df <- data.frame(
    cat=factor(rep(c("cat1", "cat2"), 20), levels = c("cat1", "cat2")),
    val = rep(seq(10), 2)
  )
  test_df2 <- test_df %>% group_by(cat) %>% summarize(n=n(), sd=sd(val), mean=mean(val))
  # Compare the outputs between stats::t.test and exploratory:::t.test.aggregated
  res0 <- stats::t.test(data=test_df, val~cat, alternative="greater")
  res <- exploratory:::t.test.aggregated(test_df2$n[1],test_df2$n[2],test_df2$mean[1],test_df2$mean[2],test_df2$sd[1],test_df2$sd[2],0.95,0, alternative="greater")
  expect_equal(res$statistic, res0$statistic)
  expect_equal(res$parameter, res0$parameter)
  expect_equal(res$p.value, res0$p.value)
  expect_equal(res$conf.int, res0$conf.int)
  names(res0$estimate) <- NULL # Ignore names difference, which we did not implement.
  expect_equal(res$estimate, res0$estimate)
  expect_equal(res$stderr, res0$stderr)
  expect_equal(res$alternative, res0$alternative)
  names(res0$null.value) <- NULL # Ignore names difference, which we did not implement.
  expect_equal(res$null.value, res0$null.value)
})

test_that("test t.test.aggregated with equal variance assumption with two.sided alternative", {
  test_df <- data.frame(
    cat=factor(rep(c("cat1", "cat2"), 20), levels = c("cat1", "cat2")),
    val = rep(seq(10), 2)
  )
  test_df2 <- test_df %>% group_by(cat) %>% summarize(n=n(), sd=sd(val), mean=mean(val))
  # Compare the outputs between stats::t.test and exploratory:::t.test.aggregated
  res0 <- stats::t.test(data=test_df, val~cat, var.equal=TRUE)
  res <- exploratory:::t.test.aggregated(test_df2$n[1],test_df2$n[2],test_df2$mean[1],test_df2$mean[2],test_df2$sd[1],test_df2$sd[2],0.95,0, var.equal=TRUE)
  expect_equal(res$statistic, res0$statistic)
  expect_equal(res$parameter, res0$parameter)
  expect_equal(res$p.value, res0$p.value)
  expect_equal(res$conf.int, res0$conf.int)
  names(res0$estimate) <- NULL # Ignore names difference, which we did not implement.
  expect_equal(res$estimate, res0$estimate)
  expect_equal(res$stderr, res0$stderr)
  names(res0$null.value) <- NULL # Ignore names difference, which we did not implement.
  expect_equal(res$null.value, res0$null.value)
})

test_that("test t.test.aggregated with equal variance assumption with greater alternative", {
  test_df <- data.frame(
    cat=factor(rep(c("cat1", "cat2"), 20), levels = c("cat1", "cat2")),
    val = rep(seq(10), 2)
  )
  test_df2 <- test_df %>% group_by(cat) %>% summarize(n=n(), sd=sd(val), mean=mean(val))
  # Compare the outputs between stats::t.test and exploratory:::t.test.aggregated
  res0 <- stats::t.test(data=test_df, val~cat, var.equal=TRUE, alternative='greater')
  res <- exploratory:::t.test.aggregated(test_df2$n[1],test_df2$n[2],test_df2$mean[1],test_df2$mean[2],test_df2$sd[1],test_df2$sd[2],0.95,0, var.equal=TRUE, alternative='greater')
  expect_equal(res$statistic, res0$statistic)
  expect_equal(res$parameter, res0$parameter)
  expect_equal(res$p.value, res0$p.value)
  expect_equal(res$conf.int, res0$conf.int)
  names(res0$estimate) <- NULL # Ignore names difference, which we did not implement.
  expect_equal(res$estimate, res0$estimate)
  expect_equal(res$stderr, res0$stderr)
  names(res0$null.value) <- NULL # Ignore names difference, which we did not implement.
  expect_equal(res$null.value, res0$null.value)
})

test_that("test t.test.aggregated with equal variance assumption with less alternative", {
  test_df <- data.frame(
    cat=factor(rep(c("cat1", "cat2"), 20), levels = c("cat1", "cat2")),
    val = rep(seq(10), 2)
  )
  test_df2 <- test_df %>% group_by(cat) %>% summarize(n=n(), sd=sd(val), mean=mean(val))
  # Compare the outputs between stats::t.test and exploratory:::t.test.aggregated
  res0 <- stats::t.test(data=test_df, val~cat, var.equal=TRUE, alternative='less')
  res <- exploratory:::t.test.aggregated(test_df2$n[1],test_df2$n[2],test_df2$mean[1],test_df2$mean[2],test_df2$sd[1],test_df2$sd[2],0.95,0, var.equal=TRUE, alternative='less')
  expect_equal(res$statistic, res0$statistic)
  expect_equal(res$parameter, res0$parameter)
  expect_equal(res$p.value, res0$p.value)
  expect_equal(res$conf.int, res0$conf.int)
  names(res0$estimate) <- NULL # Ignore names difference, which we did not implement.
  expect_equal(res$estimate, res0$estimate)
  expect_equal(res$stderr, res0$stderr)
  names(res0$null.value) <- NULL # Ignore names difference, which we did not implement.
  expect_equal(res$null.value, res0$null.value)
})







#test_that("test one sample t-test", {
#  result <- test_df %>%
#    dplyr::group_by(dim) %>%
#    do_t.test("with space", mu=3)
#  expect_equal(result[result[["dim"]]=="dim1", "p.value"][[1]], 1)
#})






test_that("test exp_chisq with power", {
  model_df <- exp_chisq(mtcars %>% mutate(gear=factor(gear)), gear, carb, power = 0.8) # factor order should be kept in the model
  ret <- model_df %>% glance_rowwise(model)
  model_df <- exp_chisq(mtcars, gear, carb, value=cyl, power = 0.8)
  ret <- model_df %>% glance_rowwise(model)
  expect_true(all(c("Cramer's V","Chi-Square","DF","P Value","Cohen's W",
                     "Target Power","Target Type 2 Error","Current Sample Size","Required Sample Size") %in% colnames(ret)
  ))
  expect_true(ret$`Cramer's V` >= 0 && ret$`Cramer's V` <= 1)
})

test_that("test exp_chisq with grouping functions", {
  model_df <- exp_chisq(mtcars, disp, drat, func1="asintby10", func2="asint", value=mpg)
  ret <- model_df %>% glance_rowwise(model)
  expect_true(all(c("Cramer's V","Chi-Square","DF","P Value","Cohen's W","Power",
                 "Type 2 Error","Rows") %in% colnames(ret)
  ))
  expect_true(ret$`Cramer's V` >= 0 && ret$`Cramer's V` <= 1)
})

test_that("test exp_chisq with logical", {
  model_df <- exp_chisq(mtcars %>% mutate(gear=gear>3, carb=carb>3), gear, carb) # logical type should be kept in the model
  ret <- model_df %>% tidy_rowwise(model, type="residuals")
  expect_equal(class(ret$gear), "logical")
  expect_equal(class(ret$carb), "logical")
})

test_that("test exp_chisq with numeric", {
  model_df <- exp_chisq(mtcars , gear, carb) # numeric type should be kept in the model
  ret <- model_df %>% tidy_rowwise(model, type="residuals")
  expect_equal(class(ret$gear), "numeric")
  expect_equal(class(ret$carb), "numeric")
})
test_that("test exp_chisq with integer", {
  model_df <- exp_chisq(mtcars %>% mutate(gear=as.integer(gear), carb=as.integer(carb)), gear, carb) # integer type should be kept in the model
  ret <- model_df %>% tidy_rowwise(model, type="residuals")
  expect_equal(class(ret$gear), "integer")
  expect_equal(class(ret$carb), "integer")
})
test_that("test exp_chisq with group_by", {
  ret <- mtcars %>% group_by(vs) %>% exp_chisq(gear, carb, value=cyl)
  observed <- ret %>% tidy_rowwise(model, type="observed")
  expect_true("vs" %in% colnames(observed))
  summary <- ret %>% glance_rowwise(model)
  expect_true("vs" %in% colnames(summary))
  residuals <- ret %>% tidy_rowwise(model, type="residuals")
  expect_true("vs" %in% colnames(residuals))
})

test_that("test exp_chisq with group_by with single class category in one of the groups", {
  ret <- mtcars %>% filter(vs!=1 | gear==4) %>% group_by(vs) %>% exp_chisq(gear, carb, value=cyl)
  observed <- ret %>% tidy_rowwise(model, type="observed")
  summary <- ret %>% glance_rowwise(model)
  expect_equal(nrow(summary), 2) # summary for 2 groups, one of which is a row with Note, should be shown.
  residuals <- ret %>% tidy_rowwise(model, type="residuals")
})





test_that("test exp_ttest", {
  mtcars2 <- mtcars
  mtcars2$am[[1]] <- NA # test NA filtering
  model_df <- exp_ttest(mtcars2, mpg, am, test_sig_level=0.05)
  ret <- model_df %>% tidy_rowwise(model, type="model")
  expect_true("Rows" %in% colnames(ret))
  ret <- model_df %>% tidy_rowwise(model, type="data_summary")
  ret <- model_df %>% tidy_rowwise(model, type="prob_dist")
  expect_true("p.value" %in% colnames(ret))

})

test_that("test exp_ttest with factor explanatory variable", {
  mtcars2 <- mtcars
  mtcars2$am[[1]] <- NA # test NA filtering
  # Put unused factor levels too for test.
  mtcars2 <- mtcars2 %>% dplyr::mutate(am=factor(am, levels=c(-1,0,1,2)))
  model_df <- exp_ttest(mtcars2, mpg, am)
  ret <- model_df %>% tidy_rowwise(model, type="model")
  expect_equal(ret$`Base Level`, "0") # First *used* factor level should be the base.
  expect_gt(ret$Difference, 0) # Checking the direction of Difference is correct.
  expect_true("Rows" %in% colnames(ret))
  model_df %>% tidy_rowwise(model, type="data_summary")
})

test_that("test exp_ttest with numeric explanatory variable", {
  mtcars2 <- mtcars
  mtcars2$am[[1]] <- NA # test NA filtering
  model_df <- exp_ttest(mtcars2, mpg, am)
  ret <- model_df %>% tidy_rowwise(model, type="model")
  expect_equal(ret$`Base Level`, "0") # The smaller number should be the base.
  expect_gt(ret$Difference, 0) # Checking the direction of Difference is correct.
  expect_true("Rows" %in% colnames(ret))
  model_df %>% tidy_rowwise(model, type="data_summary")
})

test_that("test exp_ttest with character explanatory variable", {
  mtcars2 <- mtcars
  mtcars2$am[[1]] <- NA # test NA filtering
  mtcars2 <- mtcars2 %>% dplyr::mutate(am=as.character(am))
  model_df <- exp_ttest(mtcars2, mpg, am)
  ret <- model_df %>% tidy_rowwise(model, type="model")
  expect_equal(ret$`Base Level`, "0") # The majority should be the base
  expect_gt(ret$Difference, 0) # Checking the direction of Difference is correct.
  expect_true("Rows" %in% colnames(ret))
  model_df %>% tidy_rowwise(model, type="data_summary")
})

test_that("test exp_ttest with logical explanatory variable", {
  mtcars2 <- mtcars
  mtcars2$am[[1]] <- NA # test NA filtering
  mtcars2 <- mtcars2 %>% dplyr::mutate(am=as.logical(am))
  model_df <- exp_ttest(mtcars2, mpg, am)
  ret <- model_df %>% tidy_rowwise(model, type="model")
  expect_equal(ret$`Base Level`, "FALSE") # FALSE should be the base
  expect_gt(ret$Difference, 0) # Checking the direction of Difference is correct.
  expect_true("Rows" %in% colnames(ret))
  model_df %>% tidy_rowwise(model, type="data_summary")
})

test_that("test exp_ttest with var.equal = TRUE", {
  mtcars2 <- mtcars
  mtcars2$am[[1]] <- NA # test NA filtering
  model_df <- exp_ttest(mtcars2, mpg, am, var.equal = TRUE)
  ret <- model_df %>% tidy_rowwise(model, type="model")
  ret <- model_df %>% tidy_rowwise(model, type="data_summary")
  expect_equal(colnames(ret),
               c("am","Rows","Mean","Conf Low","Conf High","Std Error of Mean","Std Deviation",
                 "Minimum","Maximum"))
})

test_that("test exp_ttest with alternative = greater", {
  mtcars2 <- mtcars
  mtcars2$am[[1]] <- NA # test NA filtering
  model_df <- exp_ttest(mtcars2, mpg, am, alternative = "greater")
  ret <- model_df %>% tidy_rowwise(model, type="model")
  ret <- model_df %>% tidy_rowwise(model, type="data_summary")
  colnames(ret)
  expect_equal(colnames(ret),
               c("am","Rows","Mean","Conf Low","Conf High","Std Error of Mean","Std Deviation",
                 "Minimum","Maximum"))
})

test_that("test exp_ttest with paired = TRUE", {
  # Make sample size equal between groups for paired t-test.
  mtcars2 <- mtcars %>% group_by(am) %>% slice_sample(n=6) %>% ungroup()
  model_df <- exp_ttest(mtcars2, mpg, am, paired = TRUE)
  ret <- model_df %>% tidy_rowwise(model, type="model")
  ret <- model_df %>% tidy_rowwise(model, type="data_summary")
  expect_equal(colnames(ret),
               c("am","Rows","Mean","Conf Low","Conf High","Std Error of Mean","Std Deviation",
                 "Minimum","Maximum"))
  ret
})

test_that("test exp_ttest with power", {
  mtcars2 <- mtcars
  mtcars2$am[[1]] <- NA # test NA filtering
  model_df <- exp_ttest(mtcars2, mpg, am, beta = 0.2)
  ret <- model_df %>% tidy_rowwise(model, type="model")
  expect_equal(colnames(ret),
               c("t Value","P Value","DF","Difference",
                 "Conf Low","Conf High","Base Level","Cohen's D","Target Power",
                 "Target Type 2 Error","Current Sample Size (Each Group)","Required Sample Size (Each Group)","Rows",
                 "Rows (0)","Rows (1)"))
  ret <- model_df %>% tidy_rowwise(model, type="data_summary")
  expect_equal(colnames(ret),
               c("am","Rows","Mean","Conf Low","Conf High","Std Error of Mean","Std Deviation",
                 "Minimum","Maximum"))
})

test_that("test exp_ttest with power with paired = TRUE", {
  # Make sample size equal between groups for paired t-test.
  mtcars2 <- mtcars %>% group_by(am) %>% slice_sample(n=6) %>% ungroup()
  model_df <- exp_ttest(mtcars2, mpg, am, paired = TRUE, power = 0.8)
  ret <- model_df %>% tidy_rowwise(model, type="model")
  ret <- model_df %>% tidy_rowwise(model, type="data_summary")
  expect_equal(colnames(ret),
               c("am","Rows","Mean","Conf Low","Conf High","Std Error of Mean","Std Deviation",
                 "Minimum","Maximum"))
  ret
})


test_that("test exp_ttest with diff_to_detect", {
  mtcars2 <- mtcars
  mtcars2$am[[1]] <- NA # test NA filtering
  model_df <- exp_ttest(mtcars2, mpg, am, diff_to_detect = 0.5, power = 0.8)
  ret <- model_df %>% tidy_rowwise(model, type="model")
  ret <- model_df %>% tidy_rowwise(model, type="data_summary")
  expect_equal(colnames(ret),
               c("am","Rows","Mean","Conf Low","Conf High","Std Error of Mean","Std Deviation",
                 "Minimum","Maximum"))
  ret
})

test_that("test exp_ttest with diff_to_detect and common_sd", {
  mtcars2 <- mtcars
  mtcars2$am[[1]] <- NA # test NA filtering
  model_df <- exp_ttest(mtcars2, mpg, am, diff_to_detect = 0.5, common_sd = 1.5, power = 0.8)
  ret <- model_df %>% tidy_rowwise(model, type="model")
  ret <- model_df %>% tidy_rowwise(model, type="data_summary")
  expect_equal(colnames(ret),
               c("am","Rows","Mean","Conf Low","Conf High","Std Error of Mean","Std Deviation",
                 "Minimum","Maximum"))
})


test_that("test exp_ttest with asint grouping", {
  mtcars2 <- mtcars
  mtcars2$am[[1]] <- NA # test NA filtering
  model_df <- exp_ttest(mtcars2, mpg, am, func2 = "asint")
  ret <- model_df %>% tidy_rowwise(model, type="model")
  ret <- model_df %>% tidy_rowwise(model, type="data_summary")
  expect_equal(colnames(ret),
               c("am","Rows","Mean","Conf Low","Conf High","Std Error of Mean","Std Deviation",
                 "Minimum","Maximum"))
})

test_that("test exp_ttest with group_by", {
  model_df <- mtcars %>% group_by(vs) %>% exp_ttest(mpg, am)
  ret <- model_df %>% tidy_rowwise(model, type="model")
  ret <- model_df %>% tidy_rowwise(model, type="data_summary")
  expect_equal(colnames(ret),
               c("vs","am","Rows","Mean","Conf Low","Conf High","Std Error of Mean","Std Deviation",
                 "Minimum","Maximum"))
  ret
})

test_that("test exp_ttest with outlier filter", {
  model_df <- mtcars %>% group_by(vs) %>% exp_ttest(mpg, am, outlier_filter_type="percentile", outlier_filter_threshold=0.9)
  ret <- model_df %>% tidy_rowwise(model, type="model")
  ret <- model_df %>% tidy_rowwise(model, type="data_summary")
  expect_equal(colnames(ret),
               c("vs","am","Rows","Mean","Conf Low","Conf High","Std Error of Mean","Std Deviation",
                 "Minimum","Maximum"))
})

test_that("test exp_ttest with group-level error (lack of unique values)", {
  df <- tibble::tibble(group=c(1,1,2,2),category=c("a","a","b","b"),value=c(1,2,1,2))
  model_df <- df %>% dplyr::group_by(`group`) %>% exp_ttest(`value`, `category`)
  ret <- model_df %>% tidy_rowwise(model, type='model')
  expect_equal(colnames(ret),
               c("group","Note"))
  ret <- model_df %>% tidy_rowwise(model, type='prob_dist')
  expect_equal(nrow(ret), 0)
})

test_that("test exp_ttest with group-level error (not eough data)", {
  df <- tibble::tibble(group=c(1,1,2,2),category=c("a","b","a","b"),value=c(1,2,1,2))
  model_df <- df %>% dplyr::group_by(`group`) %>% exp_ttest(`value`, `category`)
  ret <- model_df %>% tidy_rowwise(model, type='model')
  expect_equal(colnames(ret),
               c("group", "Rows", "Rows (a)", "Rows (b)", "Note"))
  ret <- model_df %>% tidy_rowwise(model, type='prob_dist')
  expect_equal(nrow(ret), 0)
})






















test_that("generate_ttest_density_data returns correct structure", {
  ret <- exploratory:::generate_ttest_density_data(t=2.5, p.value=0.02, df=10)
  expect_true(tibble::is_tibble(ret))
  expect_true(all(c("x", "y", "critical", "df") %in% colnames(ret)))
  # Density values should be non-negative
  expect_true(all(ret$y >= 0))
  # There should be a row with statistic == TRUE
  stat_rows <- ret %>% dplyr::filter(statistic == TRUE)
  expect_equal(nrow(stat_rows), 1)
  expect_equal(stat_rows$x, 2.5)
  expect_equal(stat_rows$p.value, 0.02)
  # Critical region boundaries for two.sided with df=10 at 0.05
  tt <- qt(1 - 0.05/2, df=10)
  expect_true(all(ret$critical[ret$x >= tt] == TRUE, na.rm=TRUE))
  expect_true(all(ret$critical[ret$x <= -tt] == TRUE, na.rm=TRUE))
  expect_true(all(ret$critical[ret$x > -tt & ret$x < tt] == FALSE, na.rm=TRUE))
})

test_that("generate_chisq_density_data returns correct structure", {
  ret <- exploratory:::generate_chisq_density_data(stat=7.5, p.value=0.02, df=3)
  expect_true(tibble::is_tibble(ret))
  expect_true(all(c("x", "y", "critical", "df") %in% colnames(ret)))
  expect_true(all(ret$y >= 0))
  # Check statistic row
  stat_rows <- ret %>% dplyr::filter(statistic == TRUE)
  expect_equal(nrow(stat_rows), 1)
  expect_equal(stat_rows$x, 7.5)
  # Critical region: x >= qchisq(0.95, df=3)
  tx <- qchisq(1 - 0.05, df=3)
  expect_true(all(ret$critical[ret$x >= tx] == TRUE, na.rm=TRUE))
  expect_true(all(ret$critical[ret$x < tx] == FALSE, na.rm=TRUE))
})

test_that("generate_ftest_density_data returns correct structure", {
  ret <- exploratory:::generate_ftest_density_data(stat=4.0, p.value=0.03, df1=2, df2=20)
  expect_true(tibble::is_tibble(ret))
  expect_true(all(c("x", "y", "critical", "df1", "df2") %in% colnames(ret)))
  expect_true(all(ret$y >= 0))
  stat_rows <- ret %>% dplyr::filter(statistic == TRUE)
  expect_equal(nrow(stat_rows), 1)
  expect_equal(stat_rows$x, 4.0)
  expect_equal(stat_rows$p.value, 0.03)
  # Critical region: x >= qf(0.95, df1=2, df2=20)
  tx <- qf(1 - 0.05, df1=2, df2=20)
  expect_true(all(ret$critical[ret$x >= tx] == TRUE, na.rm=TRUE))
})

test_that("generate_norm_density_data returns correct structure", {
  ret <- exploratory:::generate_norm_density_data(z=1.96, p.value=0.05, mu=0, sigma=1)
  expect_true(tibble::is_tibble(ret))
  expect_true(all(c("x", "y", "critical", "mean", "sd") %in% colnames(ret)))
  expect_true(all(ret$y >= 0))
  stat_rows <- ret %>% dplyr::filter(statistic == TRUE)
  expect_equal(nrow(stat_rows), 1)
  expect_equal(stat_rows$x, 1.96)
  # Two-sided critical boundaries
  tz_upper <- qnorm(1 - 0.05/2, mean=0, sd=1)
  tz_lower <- qnorm(0.05/2, mean=0, sd=1)
  expect_true(all(ret$critical[ret$x >= tz_upper] == TRUE, na.rm=TRUE))
  expect_true(all(ret$critical[ret$x <= tz_lower] == TRUE, na.rm=TRUE))
})

test_that("wilcox_norm_dist_sd returns positive numeric", {
  # Unpaired case
  tie_counts <- rep(1, 20)
  sd_unpaired <- exploratory:::wilcox_norm_dist_sd(alternative="two.sided", paired=FALSE,
                                      statistic=50, n1=10, n2=10,
                                      tie_counts=tie_counts)
  expect_true(is.numeric(sd_unpaired))
  expect_true(sd_unpaired > 0)
  # Paired case
  tie_counts_paired <- rep(1, 10)
  sd_paired <- exploratory:::wilcox_norm_dist_sd(alternative="two.sided", paired=TRUE,
                                    statistic=25, n1=10, n2=10,
                                    tie_counts=tie_counts_paired)
  expect_true(is.numeric(sd_paired))
  expect_true(sd_paired > 0)
})