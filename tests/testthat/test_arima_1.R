context("test ARIMA functions")
test_that("exp_arima with aggregation", {
  Sys.setenv(TZ="UTC") # set time zone for test stability for tests with time unit smaller than day.
  data("raw_data", package = "AnomalyDetection")
  raw_data$timestamp <- as.POSIXct(raw_data$timestamp)
  raw_data <- raw_data %>% rename(`time stamp`=timestamp, `cou nt`=count)

  model_df <- raw_data %>%
    exp_arima(`time stamp`, `cou nt`, 2, time_unit = "day", seasonal=F, test_mode=T) # With seasonal=T, the data would be too short.
  ret <- model_df %>% glance_with_ts_metric()
  expect_true(all(c("RMSE","MAE","MAPE (Ratio)","R Squared") %in% names(ret)))
  ret <- model_df %>% glance_rowwise(model)
  ret <- raw_data %>%
    exp_arima(`time stamp`, `cou nt`, 10, time_unit = "day", seasonal=FALSE)
  ret <- raw_data %>%
    exp_arima(`time stamp`, `cou nt`, 10, time_unit = "day", auto=FALSE, p=0, d=1, q=0)
  ret <- raw_data %>%
    exp_arima(`time stamp`, `cou nt`, 10, time_unit = "day", auto=FALSE, p=0, d=1, q=0, seasonal=FALSE)
  ret <- raw_data %>%
    exp_arima(`time stamp`, `cou nt`, 10, time_unit = "hour")
  # Test both "min" and "minute". na_fill_type is needed to exercise complete_data function.
  ret <- raw_data %>% tail(100) %>%
    exp_arima(`time stamp`, `cou nt`, 10, time_unit = "min", na_fill_type = "previous")
  ret <- raw_data %>% tail(100) %>%
    exp_arima(`time stamp`, `cou nt`, 10, time_unit = "minute", na_fill_type = "previous")
  # Test both "sec" and "second". na_fill_type is needed to exercise complete_data function.
  ret <- raw_data %>% tail(100) %>%
    exp_arima(`time stamp`, `cou nt`, 10, time_unit = "sec", na_fill_type = "previous")
  ret <- raw_data %>% tail(100) %>%
    exp_arima(`time stamp`, `cou nt`, 10, time_unit = "second", na_fill_type = "previous")

  # test for test mode.
  raw_data$`cou nt`[[length(raw_data$`cou nt`) - 2]] <- NA # inject NA near the end to test #9211
  ret <- raw_data %>%
    exp_arima(`time stamp`, `cou nt`, 2, time_unit = "day", test_mode=TRUE)
  # verify that the last forecasted_value is not NA to test #9211
  expect_true(!is.na(ret$data[[1]]$forecasted_value[[length(ret$data[[1]]$forecasted_value)]]))

  ret <- raw_data %>%
    exp_arima(`time stamp`, `cou nt`, 2, time_unit = "hour", test_mode=TRUE)

  # verify that the last forecasted_value is not NA to test #9211
  expect_true(!is.na(ret$data[[1]]$forecasted_value[[length(ret$data[[1]]$forecasted_value)]]))
})

test_that("exp_arima with minutes", {
  Sys.setenv(TZ="UTC") # set time zone for test stability for tests with time unit smaller than day.
  data("raw_data", package = "AnomalyDetection")
  raw_data$timestamp <- as.POSIXct(raw_data$timestamp)
  raw_data <- raw_data %>% rename(`time stamp`=timestamp, `cou nt`=count)

  ret <- raw_data %>% tail(100) %>%
    exp_arima(`time stamp`, `cou nt`, 2, time_unit = "minute", test_mode=TRUE)
  # verify that the last forecasted_value is not NA to test #9211
  expect_true(!is.na(ret$data[[1]]$forecasted_value[[length(ret$data[[1]]$forecasted_value)]]))
})

# This test is too slow. TODO: make it faster and enable.
test_that("exp_arima test mode with second as time units", {
  Sys.setenv(TZ="UTC") # set time zone for test stability for tests with time unit smaller than day.
  ts <- seq(as.POSIXct("2010-01-01 00:00:00"), as.POSIXct("2010-01-01 00:01:00"), by="sec")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts))) %>% dplyr::rename(`time stamp`=timestamp, `da ta`=data)
  raw_data$`da ta`[[length(ts) - 2]] <- NA # inject NA near the end to test #9211
  ret <- raw_data %>%
    exp_arima(`time stamp`, `da ta`, 10, time_unit = "second", test_mode=TRUE)
  # verify that the last forecasted_value is not NA to test #9211
  expect_true(!is.na(ret$data[[1]]$forecasted_value[[length(ret$data[[1]]$forecasted_value)]]))
})

# This test is slow. TODO: make it faster.
test_that("exp_arima test mode with minute as time units", {
  Sys.setenv(TZ="UTC") # set time zone for test stability for tests with time unit smaller than day.
  # cannot be much longer than this on win 32bit to avoid memory error.
  ts <- seq(as.POSIXct("2010-01-01 00:00:00"), as.POSIXct("2010-01-08 00:00:00"), by="min")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts))) %>% dplyr::rename(`time stamp`=timestamp, `da ta`=data)
  raw_data$`da ta`[[length(ts) - 2]] <- NA # inject NA near the end to test #9211
  ret <- raw_data %>%
    exp_arima(`time stamp`, `da ta`, 10, time_unit = "minute", test_mode=TRUE)
  # verify that the last forecasted_value is not NA to test #9211
  expect_true(!is.na(ret$data[[1]]$forecasted_value[[length(ret$data[[1]]$forecasted_value)]]))
})

test_that("exp_arima test mode with hour as time units", {
  Sys.setenv(TZ="UTC") # set time zone for test stability for tests with time unit smaller than day.
  ts <- seq(as.POSIXct("2010-01-01:00:00:00"), as.POSIXct("2010-01-15:00:00"), by="hour")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts))) %>% dplyr::rename(`time stamp`=timestamp, `da ta`=data)
  raw_data$`da ta`[[length(ts) - 2]] <- NA # inject NA near the end to test #9211
  ret <- raw_data %>%
    exp_arima(`time stamp`, `da ta`, 10, time_unit = "hour", test_mode=TRUE)
  # verify that the last forecasted_value is not NA to test #9211
  expect_true(!is.na(ret$data[[1]]$forecasted_value[[length(ret$data[[1]]$forecasted_value)]]))
})

test_that("exp_arima test mode with month as time units", {
  ts <- seq.Date(as.Date("2010-01-01"), as.Date("2030-01-01"), by="month")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts))) %>% dplyr::rename(`time stamp`=timestamp, `da ta`=data)
  raw_data$`da ta`[[length(ts) - 2]] <- NA # inject NA near the end to test #9211
  ret <- raw_data %>%
    exp_arima(`time stamp`, `da ta`, 10, time_unit = "month", test_mode=TRUE)
  # expect_gt(nrow(ret$stl[[1]]), 0) # Commenting out since stl is not always successful.
  # verify that the last forecasted_value is not NA to test #9211
  expect_true(!is.na(ret$data[[1]]$forecasted_value[[length(ret$data[[1]]$forecasted_value)]]))
})

test_that("exp_arima test mode with quarter as time units", {
  ts <- seq.Date(as.Date("2010-01-01"), as.Date("2030-01-01"), by="quarter")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts))) %>% dplyr::rename(`time stamp`=timestamp, `da ta`=data)
  raw_data$`da ta`[[length(ts) - 2]] <- NA # inject NA near the end to test #9211
  ret <- raw_data %>%
    exp_arima(`time stamp`, `da ta`, 10, time_unit = "quarter", test_mode=TRUE)
  # verify that the last forecasted_value is not NA to test #9211
  expect_true(!is.na(ret$data[[1]]$forecasted_value[[length(ret$data[[1]]$forecasted_value)]]))
})

test_that("exp_arima test mode with year as time units", {
  ts <- seq.Date(as.Date("2010-01-01"), as.Date("2030-01-01"), by="year")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts))) %>% dplyr::rename(`time stamp`=timestamp, `da ta`=data)
  raw_data$`da ta`[[length(ts) - 2]] <- NA # inject NA near the end to test #9211
  ret <- raw_data %>%
    exp_arima(`time stamp`, `da ta`, 10, time_unit = "year", test_mode=TRUE)
  # verify that the last forecasted_value is not NA to test #9211
  expect_true(!is.na(ret$data[[1]]$forecasted_value[[length(ret$data[[1]]$forecasted_value)]]))
})

test_that("exp_arima with short data", {
  ts <- seq.Date(as.Date("2010-01-01"), as.Date("2010-01-13"), by="day")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts))) %>% dplyr::rename(`time stamp`=timestamp, `da ta`=data)
  model_df <- raw_data %>%
    exp_arima(`time stamp`, `da ta`, 10, time_unit = "day", funs.aggregate.regressors = c(mean), yearly.seasonality = "auto", weekly.seasonality = "auto", output="model")

  expect_equal(last(model_df$data[[1]]$`time stamp`), as.Date("2010-01-23")) 
  # test for glance.
  ret <- model_df %>% glance_with_ts_metric()
  expect_true(all(c("RMSE","MAE","MAPE (Ratio)") %in% names(ret)))
  expect_true(!is.na(model_df$data[[1]]$forecasted_value[[length(model_df$data[[1]]$forecasted_value)]]))
})

test_that("exp_arima with extra regressors", {
  skip("Skip extra regressor/holiday test")
  ts <- seq.Date(as.Date("2010-01-01"), as.Date("2012-01-01"), by="day")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts)))
  ts2 <- seq.Date(as.Date("2010-01-01"), as.Date("2013-01-01"), by="day")
  regressor_data <- data.frame(timestamp=ts2, regressor1=runif(length(ts2)), regressor2=runif(length(ts2)))
  combined_data <- raw_data %>% full_join(regressor_data, by=c("timestamp"="timestamp"))
  model_df <- combined_data %>%
    exp_arima(timestamp, data, 10, time_unit = "day", regressors = c("regressor1","regressor2"), funs.aggregate.regressors = c(mean), output="model")
  coef_df <- model_df %>% tidy_rowwise(model, type="coef")
  expect_equal(names(coef_df), c("Variable","Importance"))
  ret <- model_df %>% tidy_rowwise(model)
  # verify the last date with forecasted_value
  expect_equal(last((ret %>% filter(!is.na(forecasted_value)))$timestamp), as.Date("2012-01-11")) 
  # verify the last date in the data is the end of regressor data
  expect_equal(ret$timestamp[[length(ret$timestamp)]], as.Date("2013-01-01"))
})

test_that("exp_arima with extra regressor with holiday column", {
  skip("Skip extra regressor/holiday test")
  ts <- seq.Date(as.Date("2010-01-01"), as.Date("2012-01-01"), by="day")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts)))
  ts2 <- seq.Date(as.Date("2010-01-01"), as.Date("2013-01-01"), by="day")
  regressor_data <- data.frame(timestamp=ts2, regressor=runif(length(ts2)), holiday=if_else(runif(length(ts2)) > 0.90,"holiday",NA_character_))
  combined_data <- raw_data %>% full_join(regressor_data, by=c("timestamp"="timestamp"))
  model_df <- combined_data %>%
    exp_arima(timestamp, data, 10, time_unit = "day", regressors = c("regressor"), funs.aggregate.regressors = c(mean), holiday=holiday, output="model")
  coef_df <- model_df %>% tidy_rowwise(model, type="coef")
  expect_equal(names(coef_df), c("Variable","Importance"))
  ret <- model_df %>% tidy_rowwise(model)
  # verify the last date with forecasted_value
  expect_equal(last((ret %>% filter(!is.na(forecasted_value)))$timestamp), as.Date("2012-01-11")) 
  # verify the last date in the data is the end of regressor data
  expect_equal(ret$timestamp[[length(ret$timestamp)]], as.Date("2013-01-01"))
})

test_that("exp_arima with holiday column", {
  skip("Skip extra regressor/holiday test")
  ts <- seq.Date(as.Date("2010-01-01"), as.Date("2012-01-01"), by="day")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts)))
  ts2 <- seq.Date(as.Date("2010-01-01"), as.Date("2013-01-01"), by="day")
  regressor_data <- data.frame(timestamp=ts2, regressor=runif(length(ts2)), holiday=if_else(runif(length(ts2)) > 0.90,"holiday",NA_character_)) %>%
    mutate(holiday=as.character(holiday)) %>%
    rename(`holi day`=holiday)
  combined_data <- raw_data %>% full_join(regressor_data, by=c("timestamp"="timestamp"))
  ret <- combined_data %>%
    exp_arima(timestamp, data, 10, time_unit = "day", holiday=`holi day`)
  # verify the last date with forecasted_value
  expect_equal(last((ret %>% filter(!is.na(forecasted_value)))$timestamp), as.Date("2012-01-11")) 
})

test_that("exp_arima with factor holiday column", {
  skip("Skip extra regressor/holiday test")
  ts <- seq.Date(as.Date("2010-01-01"), as.Date("2012-01-01"), by="day")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts)))
  ts2 <- seq.Date(as.Date("2010-01-01"), as.Date("2013-01-01"), by="day")
  regressor_data <- data.frame(timestamp=ts2, regressor=runif(length(ts2)), holiday=if_else(runif(length(ts2)) > 0.90,"holiday",NA_character_)) %>%
    mutate(holiday=as.factor(holiday)) %>%
    rename(`holi day`=holiday)
  combined_data <- raw_data %>% full_join(regressor_data, by=c("timestamp"="timestamp"))
  ret <- combined_data %>%
    exp_arima(timestamp, data, 10, time_unit = "day", holiday=`holi day`)
  # verify the last date with forecasted_value
  expect_equal(last((ret %>% filter(!is.na(forecasted_value)))$timestamp), as.Date("2012-01-11")) 
})

test_that("exp_arima with logical holiday column", {
  skip("Skip extra regressor/holiday test")
  ts <- seq.Date(as.Date("2010-01-01"), as.Date("2012-01-01"), by="day")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts)))
  ts2 <- seq.Date(as.Date("2010-01-01"), as.Date("2013-01-01"), by="day")
  regressor_data <- data.frame(timestamp=ts2, regressor=runif(length(ts2)), holiday=(runif(length(ts2)) > 0.90)) %>%
    rename(`holi day`=holiday)
  combined_data <- raw_data %>% full_join(regressor_data, by=c("timestamp"="timestamp"))
  ret <- combined_data %>%
    exp_arima(timestamp, data, 10, time_unit = "day", holiday=`holi day`)
  # verify the last date with forecasted_value
  expect_equal(last((ret %>% filter(!is.na(forecasted_value)))$timestamp), as.Date("2012-01-11")) 
})

test_that("exp_arima with numeric holiday column", {
  skip("Skip extra regressor/holiday test")
  ts <- seq.Date(as.Date("2010-01-01"), as.Date("2012-01-01"), by="day")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts)))
  ts2 <- seq.Date(as.Date("2010-01-01"), as.Date("2013-01-01"), by="day")
  regressor_data <- data.frame(timestamp=ts2, regressor=runif(length(ts2)), holiday=(runif(length(ts2)) > 0.90)) %>%
    mutate(holiday = as.numeric(holiday)) %>%
    rename(`holi day`=holiday)
  combined_data <- raw_data %>% full_join(regressor_data, by=c("timestamp"="timestamp"))
  ret <- combined_data %>%
    exp_arima(timestamp, data, 10, time_unit = "day", holiday=`holi day`)
  # verify the last date with forecasted_value
  expect_equal(last((ret %>% filter(!is.na(forecasted_value)))$timestamp), as.Date("2012-01-11")) 
})

test_that("exp_arima with regressor with holiday column with monthly data", {
  skip("Skip extra regressor/holiday test")
  ts <- seq.Date(as.Date("2010-01-01"), as.Date("2012-01-01"), by="month")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts)))
  ts2 <- seq.Date(as.Date("2010-01-01"), as.Date("2013-01-01"), by="month")
  regressor_data <- data.frame(timestamp=ts2, regressor=runif(length(ts2)), holiday=if_else(runif(length(ts2)) > 0.90,"holiday",NA_character_)) %>%
    mutate(holiday=as.character(holiday)) %>%
    rename(`holi day`=holiday)
  combined_data <- raw_data %>% full_join(regressor_data, by=c("timestamp"="timestamp"))
  ret <- combined_data %>%
    exp_arima(timestamp, data, 10, time_unit = "month", regressors = c("regressor"), funs.aggregate.regressors = c(mean), holiday=`holi day`)
  # verify the last date with forecasted_value
  expect_equal(last((ret %>% filter(!is.na(forecasted_value)))$timestamp), as.Date("2012-11-01")) 
  # verify the last date in the data is the end of regressor data
  expect_equal(ret$timestamp[[length(ret$timestamp)]], as.Date("2013-01-01"))
})

test_that("exp_arima with holiday column with hourly data", {
  skip("Skip extra regressor/holiday test")
  Sys.setenv(TZ="UTC") # set time zone for test stability.
  ts <- seq(as.POSIXct("2010-01-01 00:00:00"), as.POSIXct("2010-01-15 00:00:00"), by="hour")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts)))
  ts2 <- seq(as.POSIXct("2010-01-01 00:00:00"), as.POSIXct("2010-01-20 00:00:00"), by="hour")
  regressor_data <- data.frame(timestamp=ts2, regressor=runif(length(ts2)), holiday=if_else(runif(length(ts2)) > 0.90,"holiday",NA_character_)) %>%
    mutate(holiday=as.character(holiday))
  combined_data <- raw_data %>% full_join(regressor_data, by=c("timestamp"="timestamp"))
  ret <- combined_data %>%
    exp_arima(timestamp, data, 10, time_unit = "hour", holiday=holiday)
  # verify the last date with forecasted_value
  # Comparing between POSIXct is prone to false positive. 
  # Comparing between characters is more stable with added bonus of printed evaluation result for easier debugging.
  expect_equal(as.character(last((ret %>% filter(!is.na(forecasted_value)))$timestamp)), "2010-01-15 10:00:00")
})

test_that("exp_arima with extra regressor with cap/floor", {
  skip("Skip extra regressor/holiday test")
  ts <- seq.Date(as.Date("2010-01-01"), as.Date("2012-01-01"), by="day")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts)))
  ts2 <- seq.Date(as.Date("2010-01-01"), as.Date("2013-01-01"), by="day")
  regressor_data <- data.frame(timestamp=ts2, regressor=runif(length(ts2)))
  combined_data <- raw_data %>% full_join(regressor_data, by=c("timestamp"="timestamp"))
  ret <- combined_data %>%
    exp_arima(timestamp, data, 10, time_unit = "day", cap = 2, floor = -2, regressors = c("regressor"), funs.aggregate.regressors = c(mean))
  # verify the last date with forecasted_value
  expect_equal(last((ret %>% filter(!is.na(forecasted_value)))$timestamp), as.Date("2012-01-11")) 
  # verify the last date in the data is the end of regressor data
  expect_equal(ret$timestamp[[length(ret$timestamp)]], as.Date("2013-01-01"))
})

test_that("exp_arima test mode with extra regressor", {
  skip("Skip extra regressor/holiday test")
  ts <- seq.Date(as.Date("2010-01-01"), as.Date("2012-01-01"), by="day")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts)))
  raw_data$data[[length(ts) - 2]] <- NA # inject NA near the end to test #9211
  # here refressor data is till 2013-01-01, but the part after 2012-01-01 should be ignored.
  ts2 <- seq.Date(as.Date("2010-01-01"), as.Date("2013-01-01"), by="day")
  regressor_data <- data.frame(timestamp=ts2, regressor=runif(length(ts2)))
  combined_data <- raw_data %>% full_join(regressor_data, by=c("timestamp"="timestamp"))
  ret <- combined_data %>%
    exp_arima(timestamp, data, 10, time_unit = "day", regressors = c("regressor"), funs.aggregate.regressors = c(mean), test_mode = TRUE)
  # verify the last date with forecasted_value
  # Since it is test mode, end of original data is end of forecast.
  expect_equal(last((ret %>% filter(!is.na(forecasted_value)))$timestamp), as.Date("2012-01-01"))
  # End of forecast should be test data
  expect_equal(last((ret %>% filter(!is.na(forecasted_value)))$is_test_data), TRUE)
  # verify the last date in the data is the end of regressor data
  expect_equal(ret$timestamp[[length(ret$timestamp)]], as.Date("2013-01-01"))
  # Unused regressor data should have NA value as is_test_data
  expect_true(is.na(last(ret$is_test_data)))
})


test_that("exp_arima wrong grouping case", {
  data("raw_data", package = "AnomalyDetection")
  raw_data$timestamp <- as.POSIXct(raw_data$timestamp)
  expect_error({
    ret <- raw_data %>%
      dplyr::group_by(timestamp) %>%
      exp_arima(timestamp, count, 10)
  }, "timestamp is grouped. Please ungroup it.")

  expect_error({
    ret <- raw_data %>%
      dplyr::group_by(count) %>%
      exp_arima(timestamp, count, 10)
  }, "count is grouped. Please ungroup it.")
})

test_that("exp_arima grouped case", {
  data("raw_data", package = "AnomalyDetection")
  raw_data$timestamp <- as.POSIXct(raw_data$timestamp)
  raw_data1 <- raw_data
  raw_data2 <- raw_data
  raw_data1 <- raw_data1 %>% mutate(group='A')
  raw_data2 <- raw_data2 %>% mutate(group='B')
  raw_data3 <- raw_data1 %>% bind_rows(raw_data2) %>% group_by(group)

  model_df <- raw_data3 %>%
    exp_arima(timestamp, count, 10)
  ret <- model_df %>% glance_with_ts_metric()
  # P, D, Q, and Frequency used to be in the output column too with fable 0.2.1, but with fable 0.3.0, it started picking up a model without seasonality for some reason.
  expect_true(all(c("group", "RMSE", "MAE", "MAPE (Ratio)", ".model", "AIC", "BIC", "AICc",
                    "p", "d", "q", "Ljung-Box Test Statistic",
                    "Ljung-Box Test P Value", "Number of Rows") %in% colnames(ret)))
})

test_that("exp_arima without value_col", {
  data("raw_data", package = "AnomalyDetection")
  raw_data$timestamp <- as.POSIXct(raw_data$timestamp)
  ret <- raw_data %>%
    exp_arima(timestamp, , 10)
  # verify that the last forecasted_value is not NA to test #9211
  expect_true(!is.na(ret$data[[1]]$forecasted_value[[length(ret$data[[1]]$forecasted_value)]]))
})

test_that("exp_arima with all-NA value col", {
  data("raw_data", package = "AnomalyDetection")
  raw_data$timestamp <- as.POSIXct(raw_data$timestamp)
  data <- raw_data %>% mutate(count=NA) # Make the count column all-NA.
  ret <- data %>%
    exp_arima(timestamp, count, 10)
  # verify that the last forecasted_value is at least not NA.
  expect_true(!is.na(ret$data[[1]]$forecasted_value[[length(ret$data[[1]]$forecasted_value)]]))
})
