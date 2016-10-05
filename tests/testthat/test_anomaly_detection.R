
test_that("anomary_detection", {
  data("raw_data", package = "AnomalyDetection")
  do_anomaly_detection(raw_data, count, timestamp, alpha = 0.001)
  do_anomaly_detection(raw_data, count, alpha = 0.001)
})
