context("test anomaly detection functions")

test_that("do_anomary_detection", {
  data("raw_data", package = "AnomalyDetection")
  raw_data$timestamp <- as.POSIXct(raw_data$timestamp)
  ret <- raw_data %>%
    do_anomaly_detection(timestamp, count, e_value=TRUE)
  expect_equal(ncol(ret), 7)
})

