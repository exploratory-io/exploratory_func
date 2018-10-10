context("test prophet holiday effect feature")

test_that("do_prophet with holiday column", {
  browser()
  ts <- seq.Date(as.Date("2010-01-01"), as.Date("2012-01-01"), by="day")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts)))
  ts2 <- seq.Date(as.Date("2010-01-01"), as.Date("2013-01-01"), by="day")
  regressor_data <- data.frame(timestamp=ts2, regressor=runif(length(ts2)), holiday=if_else(runif(length(ts2)) > 0.90,"holiday",NA_character_)) %>%
    mutate(holiday=as.character(holiday)) %>%
    rename(`holi day`=holiday)
  combined_data <- raw_data %>% full_join(regressor_data, by=c("timestamp"="timestamp"))
  ret <- combined_data %>%
    do_prophet(timestamp, data, 365, time_unit = "day", holiday=`holi day`)
  # verify that the last forecasted_value is not NA
  expect_true(!is.na(ret$forecasted_value[[length(ret$forecasted_value)]]))
  expect_equal(ret$timestamp[[length(ret$timestamp)]], as.Date("2012-12-31"))
})
