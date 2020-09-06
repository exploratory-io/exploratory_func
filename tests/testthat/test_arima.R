context("test do_arima")

test_that("do_arima with aggregation", {
  data("raw_data", package = "AnomalyDetection")
  raw_data$timestamp <- as.POSIXct(raw_data$timestamp)

  # Create time gap intentionally.
  raw_data <- raw_data %>% filter(as.Date(timestamp) != as.Date("1980-09-26"))

  raw_data <- raw_data %>% rename(`time stamp`=timestamp, `cou nt`=count)
  raw_data$pre_col1 <- runif(nrow(raw_data))
  raw_data$pre_col2 <- runif(nrow(raw_data))

  ret <- raw_data %>%
    do_arima(`time stamp`, valueColumn=`cou nt`, periods=10, time_unit = "day", na_fill_type="value", test_mode=F)
  # No valueColumn (row number) case.
  ret <- raw_data %>%
    do_arima(`time stamp`, ,periods=10, time_unit = "day", na_fill_type="value", test_mode=F)
  # With more parameters
  ret <- raw_data %>%
    do_arima(`time stamp`, `pre_col1`, `pre_col2`, time_unit = "day", periods = 2, valueColumn = `cou nt`, fun.aggregate = sum, na_fill_type="value", test_mode =TRUE, d = NA, D = NA, max.p = 5, max.q = 5, max.P = 2, max.Q = 2, max.order = 5, max.d = 2, max.D = 1, start.p = 2, start.q = 2, start.P = 1, start.Q = 1, stationary = FALSE, seasonal = TRUE, ic = "aic", allowdrift = TRUE, allowmean = TRUE, lambda = NULL, biasadj = FALSE, test = "kpss", seasonal.test = "ocsb", parallel = FALSE, num.cores = 2)

  expect_true(!is.null(ret$model))

  # df <- exploratory::read_delim_file("/Users/htamakos/Downloads/airline_2013_10_tricky_v2_1k_for_testcase_dev_only.csv" , ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale=readr::locale(encoding = "UTF-8", decimal_mark = "."), trim_ws = TRUE , progress = FALSE) %>%
  # # readr::type_convert() %>%
  #   exploratory::clean_data_frame()

  # df %>%  dplyr::ungroup() %>% do_arima(`FL DATE`, `FL_NUM`, `DEP_DELAY`, time_unit = "day", periods = 10, test_mode = TRUE, d = 1, D = NA, max.p = 5, max.q = 5, max.P = 2, max.Q = 2, max.order = 5, max.d = 2, max.D = 1, start.p = 2, start.q = 2, start.P = 1, start.Q = 1, stationary = FALSE, seasonal = TRUE, ic = "aic", allowdrift = TRUE, allowmean = TRUE, lambda = NULL, biasadj = FALSE, test = "kpss", seasonal.test = "ocsb", parallel = FALSE, num.cores = 2)

  # # valueColumn is not set
  # df %>%  dplyr::ungroup() %>% do_arima(`FL DATE`, `DEP_DELAY`, time_unit = "day", periods = 10, valueColumn=`FL_NUM`, test_mode = FALSE, d = NA, D = NA, max.p = 5, max.q = 5, max.P = 2, max.Q = 2, max.order = 5, max.d = 2, max.D = 1, start.p = 2, start.q = 2, start.P = 1, start.Q = 1, stationary = FALSE, seasonal = TRUE, ic = "aic", allowdrift = TRUE, allowmean = TRUE, lambda = NULL, biasadj = FALSE, test = "kpss", seasonal.test = "ocsb", parallel = FALSE, num.cores = 2)
})

