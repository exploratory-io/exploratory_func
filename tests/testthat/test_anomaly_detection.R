context("test anomaly detection functions")

test_that("do_anomary_detection with Date data", {
  ts <- seq.Date(as.Date("2010-01-01"), as.Date("2010-12-31"), by="day")
  raw_data <- data.frame(timestamp=ts, y=runif(length(ts)))
  ret <- raw_data %>%
    do_anomaly_detection(timestamp, y, time_unit = "day")
  # Check that start and end of output matches with that of input. Once there was an issue that broke this due to handling of timezone.
  expect_equal(as.Date(ret$timestamp[1], tz="GMT"), as.Date("2010-01-01"))
  expect_equal(as.Date(ret$timestamp[length(ret$timestamp)], tz="GMT"), as.Date("2010-12-31"))
})

test_that("do_anomary_detection with daily POSIXct data with timezone with daylight saving days.", {
  ts <- lubridate::with_tz(as.POSIXct(seq.Date(as.Date("2010-01-01"), as.Date("2010-12-31"), by="day")), tz="America/Los_Angeles")
  raw_data <- data.frame(timestamp=ts, y=runif(length(ts)))
  ret <- raw_data %>%
    do_anomaly_detection(timestamp, y, time_unit = "day", na_fill_type = "value", na_fill_value = 0)
  # Check that aggretated value at a daylight saving day has a valid value. Once there was an issue that broke this.
  expect_true(ret$y[200] > 0)
})

test_that("do_anomary_detection with missin days filled", {
  ts <- seq.Date(as.Date("2018-01-01"), as.Date("2018-12-31"), by="day")
  raw_data <- data.frame(timestamp=ts, y=runif(length(ts))) %>% filter(lubridate::wday(timestamp) %nin% c(1,7))

  # Test previous value fill.
  ret <- raw_data %>%
    do_anomaly_detection(timestamp, y, time_unit = "day", na_fill_type = "previous")
  # Check that missing values are filled with previous valid value.
  expect_true(ret$y[5] == ret$y[6] & ret$y[6] == ret$y[7])

  # Test interpolate value fill.
  ret <- raw_data %>%
    do_anomaly_detection(timestamp, y, time_unit = "day", na_fill_type = "interpolate")
  # Check that missing values are filled linearly.
  interpolate_diff <- ret$y[5] - ret$y[6]
  expect_equal(ret$y[6] - ret$y[7], interpolate_diff)
  expect_equal(ret$y[7] - ret$y[8], interpolate_diff)

  # Test value fill.
  ret <- raw_data %>%
    do_anomaly_detection(timestamp, y, time_unit = "day", na_fill_type = "value")
  # Check that missing values are filled with 0. 
  expect_equal(ret$y[6], 0)
  expect_equal(ret$y[7], 0)
})
