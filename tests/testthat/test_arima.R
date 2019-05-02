context("test do_arima")

test_that("do_arima with aggregation", {
  data("raw_data", package = "AnomalyDetection")
  raw_data$timestamp <- as.POSIXct(raw_data$timestamp)
  raw_data <- raw_data %>% rename(`time stamp`=timestamp, `cou nt`=count)
  ret <- raw_data %>%
    do_arima(`time stamp`, `cou nt`, 10, time_unit = "day", test_mode=T)
})

