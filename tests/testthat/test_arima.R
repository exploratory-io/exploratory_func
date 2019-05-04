context("test do_arima")

test_that("do_arima with aggregation", {
  data("raw_data", package = "AnomalyDetection")
  raw_data$timestamp <- as.POSIXct(raw_data$timestamp)
  raw_data <- raw_data %>% rename(`time stamp`=timestamp, `cou nt`=count)
  raw_data$pre_col1 <- runif(nrow(raw_data))
  raw_data$pre_col2 <- runif(nrow(raw_data))

  ret <- raw_data %>%
   do_arima(`time stamp`, valueColumn=`cou nt`, periods=10, time_unit = "day", test_mode=F)
  ret <- raw_data %>%
  do_arima(`time stamp`, `pre_col1`, `pre_col2`, time_unit = "day", periods = 2, valueColumn = `cou nt`, fun.aggregate = sum, test_mode =TRUE, d = NA, D = NA, max.p = 5, max.q = 5, max.P = 2, max.Q = 2, max.order = 5, max.d = 2, max.D = 1, start.p = 2, start.q = 2, start.P = 1, start.Q = 1, stationary = FALSE, seasonal = TRUE, ic = "aic", allowdrift = TRUE, allowmean = TRUE, lambda = NULL, biasadj = FALSE, test = "kpss", seasonal.test = "ocsb", parallel = FALSE, num.cores = 2)
})

