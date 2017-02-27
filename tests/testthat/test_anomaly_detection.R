context("test anomaly detection functions")

test_that("do_anomary_detection with aggregation", {
  data("raw_data", package = "AnomalyDetection")
  raw_data$timestamp <- as.POSIXct(raw_data$timestamp)
  first_ten <- raw_data %>% dplyr::slice(1:10)
  raw_data <- dplyr::bind_rows(raw_data, first_ten)
  ret <- raw_data %>%
    do_anomaly_detection(timestamp, e_value=TRUE, time_unit = "hour")
})

test_that("do_anomary_detection", {
  data("raw_data", package = "AnomalyDetection")
  raw_data$timestamp <- as.POSIXct(raw_data$timestamp)
  ret <- raw_data %>%
    do_anomaly_detection(timestamp, count, e_value=TRUE, time_unit = "hour")
  expect_equal(ncol(ret), 7)
})

test_that("do_anomary_detection with aggregation", {
  data("raw_data", package = "AnomalyDetection")
  raw_data$timestamp <- as.POSIXct(raw_data$timestamp)
  first_ten <- raw_data %>% dplyr::slice(1:10)
  raw_data <- dplyr::bind_rows(raw_data, first_ten)
  expect_error({
    raw_data %>%
      do_anomaly_detection(timestamp, count, e_value=TRUE)
  }, "Try smaller time unit or make sure there is enough data for each group.")
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
