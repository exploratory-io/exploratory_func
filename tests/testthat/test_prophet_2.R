context("more test on prophet functions")

test_that("do_prophet test mode with y, ds as column names", {
  ts <- seq.Date(as.Date("2010-01-01"), as.Date("2013-01-01"), by="day")
  raw_data <- data.frame(ds=ts, y=runif(length(ts)))
  raw_data$ds[[length(ts) - 2]] <- NA # inject NA near the end to test #9211
  ret <- raw_data %>%
    do_prophet(ds, y, 10, time_unit = "day", test_mode=TRUE)
  expect_true("y" %in% colnames(ret))
  expect_true("ds" %in% colnames(ret))
  # verify that the last forecasted_value is not NA to test #9211
  expect_true(!is.na(ret$forecasted_value[[length(ret$forecasted_value)]]))
})

test_that("do_prophet with NA fill options", {
  ts <- seq.Date(as.Date("2010-01-01"), as.Date("2010-12-31"), by="day")
  raw_data <- data.frame(ds=ts, y=runif(length(ts)))
  raw_data$y[[5]] <- NA # inject NA
  raw_data$y[[6]] <- NA # inject NA
  raw_data$y[[7]] <- NA # inject NA

  ret <- raw_data %>%
    do_prophet(ds, y, 10, time_unit = "day", na_fill_type = "previous")
  expect_true("y" %in% colnames(ret))
  expect_true("ds" %in% colnames(ret))

  # verify that NA values are filled.
  expect_equal(length(ret$y), 375) # 375 = 365 + 10
  expect_true(!is.na(ret$y[[5]]))
  expect_true(!is.na(ret$y[[6]]))
  expect_true(!is.na(ret$y[[7]]))
})

test_that("do_prophet with daily POSIXct data with timezone with daylight saving days.", {
  ts <- lubridate::with_tz(as.POSIXct(seq.Date(as.Date("2010-01-01"), as.Date("2010-12-31"), by="day")), tz="America/Los_Angeles")
  raw_data <- data.frame(timestamp=ts, y=runif(length(ts)))
  ret <- raw_data %>%
    do_prophet(timestamp, y, 10, time_unit = "day", na_fill_type = "previous")
  # Check that aggretated value at a daylight saving day has a valid value. Once there was an issue that broke this.
  expect_true(ret$y[200] > 0)
})
