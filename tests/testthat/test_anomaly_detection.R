context("test anomaly detection functions")

test_that("do_anomary_detection", {
  data("raw_data", package = "AnomalyDetection")
  raw_data$timestamp <- as.POSIXct(raw_data$timestamp)
  ret <- raw_data %>%
    do_anomaly_detection(timestamp, count, e_value=TRUE)
  expect_equal(ncol(ret), 7)
})

test_that("do_anomary_detection with dupe", {
  data("raw_data", package = "AnomalyDetection")
  raw_data$timestamp <- as.POSIXct(raw_data$timestamp)
  first_ten <- raw_data %>% dplyr::slice(1:10)
  raw_data <- dplyr::bind_rows(raw_data, first_ten)
  expect_error({
    raw_data %>%
      do_anomaly_detection(timestamp, count, e_value=TRUE)
  }, "There are duplicated values in Date/Time column.")
})

