
test_that("anomary_detection", {
  data("raw_data", package = "AnomalyDetection")
  raw_data$timestamp <- as.POSIXct(raw_data$timestamp)
  ret <- raw_data %>%
    dplyr::mutate(anomaly = detect_anomaly(count, timestamp, alpha = 0.001))
  expect_equal(length(ret$anomaly[ret$anomaly]), 40)
  ret <- raw_data %>%
    dplyr::mutate(anomaly = detect_anomaly(count, alpha = 0.001, period = 300))
  expect_equal(length(ret$anomaly[ret$anomaly]), 1)
})
