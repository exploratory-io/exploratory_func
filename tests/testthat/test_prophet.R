context("test prophet functions")

test_that("do_prophet with aggregation", {
  data("raw_data", package = "AnomalyDetection")
  raw_data$timestamp <- as.POSIXct(raw_data$timestamp)
  raw_data <- raw_data %>% rename(`time stamp`=timestamp, `cou nt`=count)
  ret <- raw_data %>%
    do_prophet(`time stamp`, `cou nt`, 10, time_unit = "day")
  ret <- raw_data %>%
    do_prophet(`time stamp`, `cou nt`, 10, time_unit = "hour")
  ret <- raw_data %>% tail(100) %>%
    do_prophet(`time stamp`, `cou nt`, 10, time_unit = "minute")
  ret <- raw_data %>% tail(100) %>%
    do_prophet(`time stamp`, `cou nt`, 10, time_unit = "second")

  # test for test mode.
  raw_data$`cou nt`[[length(raw_data$`cou nt`) - 2]] <- NA # inject NA near the end to test #9211
  ret <- raw_data %>%
    do_prophet(`time stamp`, `cou nt`, 2, time_unit = "day", test_mode=TRUE)
  # verify that the last forecasted_value is not NA to test #9211
  expect_true(!is.na(ret$forecasted_value[[length(ret$forecasted_value)]]))

  ret <- raw_data %>%
    do_prophet(`time stamp`, `cou nt`, 2, time_unit = "hour", test_mode=TRUE)
  # verify that the last forecasted_value is not NA to test #9211
  expect_true(!is.na(ret$forecasted_value[[length(ret$forecasted_value)]]))

  ret <- raw_data %>% tail(100) %>%
    do_prophet(`time stamp`, `cou nt`, 2, time_unit = "minute", test_mode=TRUE)
  # verify that the last forecasted_value is not NA to test #9211
  expect_true(!is.na(ret$forecasted_value[[length(ret$forecasted_value)]]))

  ret <- raw_data %>% tail(100) %>%
    do_prophet(`time stamp`, `cou nt`, 2, time_unit = "second", test_mode=TRUE)
  # verify that the last forecasted_value is not NA to test #9211
  expect_true(!is.na(ret$forecasted_value[[length(ret$forecasted_value)]]))
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
