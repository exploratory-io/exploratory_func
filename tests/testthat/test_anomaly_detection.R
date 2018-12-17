context("test anomaly detection functions")

#TODO write test to check that negative/positive anomaly is actuall negative/positive compared to expected values.

test_that("do_anomary_detection with aggregation", {
  data("raw_data", package = "AnomalyDetection")
  raw_data$timestamp <- as.POSIXct(raw_data$timestamp)
  raw_data <- raw_data %>% rename(`time stamp`=timestamp) # test column name with space
  first_ten <- raw_data %>% dplyr::slice(1:10)
  raw_data <- dplyr::bind_rows(raw_data, first_ten)
  ret <- raw_data %>%
    do_anomaly_detection(`time stamp`, e_value=TRUE, time_unit = "hour")
})

test_that("do_anomary_detection", {
  data("raw_data", package = "AnomalyDetection")
  raw_data$timestamp <- as.POSIXct(raw_data$timestamp)
  ret <- raw_data %>%
    do_anomaly_detection(timestamp, count, e_value=TRUE, time_unit = "hour")
  expect_equal(ncol(ret), 7)
})

test_that("do_anomary_detection grouped case", {
  data("raw_data", package = "AnomalyDetection")
  raw_data$timestamp <- as.POSIXct(raw_data$timestamp)
  expect_error({
    ret <- raw_data %>%
      dplyr::group_by(timestamp) %>%
      do_anomaly_detection(timestamp, count, e_value=TRUE)
  }, "timestamp is grouped. Please ungroup it.")

  expect_error({
    ret <- raw_data %>%
      dplyr::group_by(count) %>%
      do_anomaly_detection(timestamp, count, e_value=TRUE)
  }, "count is grouped. Please ungroup it.")
})

test_that("do_anomary_detection without value_col", {
  data("raw_data", package = "AnomalyDetection")
  raw_data$timestamp <- as.POSIXct(raw_data$timestamp)
  raw_data <- dplyr::bind_rows(raw_data, raw_data, raw_data)
  ret <- raw_data %>%
    do_anomaly_detection(timestamp, e_value=TRUE, time_unit = "hour")
})

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

  # Test previous value fill.
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

