context("test prophet functions - Holiday Country Names, Repeat By")

set.seed(1)

test_that("augment.prophet_exploratory", {
  # Create training data.
  history <- data.frame(x = seq(as.Date('2015-01-01'), as.Date('2016-01-01'), by = 'd'),
                      y = sin(1:366/200) + rnorm(366)/10)

  # Create a test data with the same data range.
  # It including NAs and it drops some dates. 
  testdata <- history %>% 
    select(x) %>% 
    mutate(x  = if_else(x  == as.Date('2015-01-05'), as.Date(NA), x )) %>%
    filter(x  != as.Date('2015-01-10')) 


  model.df <- history %>% do_prophet(time=x, value=y, output="model", periods=0)

  ret <- broom::augment(model.df$model[[1]], newdata=testdata)

  #print(ret)
  expect_true("forecasted_value" %in% colnames(ret))
  expect_true("forecasted_value_high" %in% colnames(ret))
  expect_true("forecasted_value_low" %in% colnames(ret))
})
