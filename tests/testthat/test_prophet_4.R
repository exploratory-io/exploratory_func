context("test prophet functions - Holiday Country Names")

test_that("do_prophet with holiday country", {
  ts <- seq.Date(as.Date("2010-01-01"), as.Date("2012-01-01"), by="day")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts)))
  ret <- raw_data %>%
    do_prophet(timestamp, data, 10, time_unit = "day", holiday_country_names=c("US","gb"))
  # verify the last date with forecasted_value
  expect_true("Christmas Day" %in% names(ret))
})
