context("test prophet functions")

test_that("do_prophet with aggregation", {
  data("raw_data", package = "AnomalyDetection")
  raw_data$timestamp <- as.POSIXct(raw_data$timestamp)
  raw_data <- raw_data %>% rename(`time stamp`=timestamp, `cou nt`=count)
  ret <- raw_data %>%
    do_prophet(`time stamp`, `cou nt`, 10, time_unit = "day")
  ret <- raw_data %>%
    do_prophet(`time stamp`, `cou nt`, 10, time_unit = "hour")
  # comment out for now since the following takes time. TODO: modify data and make it faster.
  # ret <- raw_data %>%
  #   do_prophet(`time stamp`, `cou nt`, 10, time_unit = "minute")
  # ret <- raw_data %>%
  #   do_prophet(`time stamp`, `cou nt`, 10, time_unit = "second")

  # test for test mode.
  ret <- raw_data %>%
    do_prophet(`time stamp`, `cou nt`, 2, time_unit = "day", test_mode=TRUE)
})

test_that("do_prophet grouped case", {
  data("raw_data", package = "AnomalyDetection")
  raw_data$timestamp <- as.POSIXct(raw_data$timestamp)
  expect_error({
    ret <- raw_data %>%
      dplyr::group_by(timestamp) %>%
      do_prophet(timestamp, count, 10)
  }, "timestamp is grouped. Please ungroup it.")

  expect_error({
    ret <- raw_data %>%
      dplyr::group_by(count) %>%
      do_prophet(timestamp, count, 10)
  }, "count is grouped. Please ungroup it.")
})

test_that("do_prophet without value_col", {
  data("raw_data", package = "AnomalyDetection")
  raw_data$timestamp <- as.POSIXct(raw_data$timestamp)
  ret <- raw_data %>%
    do_prophet(timestamp, NULL, 10)
})
