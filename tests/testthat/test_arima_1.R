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

test_that("exp_arima test_mode with periods longer than series errors cleanly", {
  # This reproduces the failure path:
  # - test_mode=TRUE
  # - periods >= number of rows in a group
  # -> training_data becomes empty after head(-periods)
  # Old code would later hit model_df$arima[[1]] and throw "subscript out of bounds".
  # We now expect a clear, user-facing error instead.
  ts <- seq.Date(as.Date("2020-01-01"), as.Date("2020-06-01"), by="month") # 6 rows
  raw_data <- data.frame(timestamp=ts, value=runif(length(ts))) %>%
    dplyr::rename(`time stamp`=timestamp, `va lue`=value)

  err <- tryCatch({
    raw_data %>% exp_arima(`time stamp`, `va lue`, periods=12, time_unit="month", test_mode=TRUE, seasonal=FALSE)
    NULL
  }, error = function(e) e$message)

  expect_true(!is.null(err))
  expect_true(grepl("Not enough training data to fit ARIMA in test_mode", err, fixed=TRUE))
  expect_false(grepl("subscript out of bounds", err, fixed=TRUE))
})

test_that("exp_arima test_mode with grouped short series errors cleanly per-group", {
  # Grouped version of the regression: one group is too short for periods in test_mode.
  ts_short <- seq.Date(as.Date("2020-01-01"), as.Date("2020-06-01"), by="month") # 6 rows
  ts_long <- seq.Date(as.Date("2019-01-01"), as.Date("2020-06-01"), by="month")  # 18 rows
  df_short <- data.frame(timestamp=ts_short, value=runif(length(ts_short)), group="short")
  df_long <- data.frame(timestamp=ts_long, value=runif(length(ts_long)), group="long")

  raw_data <- dplyr::bind_rows(df_short, df_long) %>%
    dplyr::rename(`time stamp`=timestamp, `va lue`=value) %>%
    dplyr::group_by(group)

  err <- tryCatch({
    raw_data %>% exp_arima(`time stamp`, `va lue`, periods=12, time_unit="month", test_mode=TRUE, seasonal=FALSE)
    NULL
  }, error = function(e) e$message)

  expect_true(!is.null(err))
  expect_true(grepl("Not enough training data to fit ARIMA in test_mode", err, fixed=TRUE))
  expect_false(grepl("subscript out of bounds", err, fixed=TRUE))
})

test_that("exp_arima handles binned grouped data in test_mode gracefully", {
  # This test simulates a scenario where:
  # - Data is binned/grouped (e.g., CPI index with 5 breaks)
  # - Some bins have sparse data (fewer observations than periods + 2)
  # - test_mode = TRUE with periods = 12
  #
  # The old code would throw "subscript out of bounds" in this scenario.
  # The fix should throw a clear, user-friendly error message instead.
  
  # Create test data similar to a binned scenario
  set.seed(42)
  ts <- seq.Date(as.Date("2020-01-01"), as.Date("2022-12-01"), by = "month")  # 36 months
  df <- data.frame(
    date = ts,
    cash_rate = runif(length(ts), 0.1, 5.0),
    cpi_value = runif(length(ts), 100, 150)
  )
  
  # Create CPI bins (5 breaks)
  df$cpi_bin <- cut(df$cpi_value, breaks = 5, labels = c("Very Low", "Low", "Medium", "High", "Very High"))
  
  # Force one group to have very few observations (sparse data)
  df_sparse <- df %>%
    dplyr::group_by(cpi_bin) %>%
    dplyr::mutate(n = dplyr::n()) %>%
    dplyr::ungroup()
  
  # Manually create a sparse group by keeping only a few rows for one group
  sparse_df <- dplyr::bind_rows(
    df_sparse %>% dplyr::filter(cpi_bin != "Very High"),
    df_sparse %>% dplyr::filter(cpi_bin == "Very High") %>% head(5)  # Only 5 observations
  )
  
  # Group by the binned column
  sparse_df <- sparse_df %>%
    dplyr::select(date, cash_rate, cpi_bin) %>%
    dplyr::group_by(cpi_bin)
  
  # This should fail with a helpful error, not "subscript out of bounds"
  err <- tryCatch({
    sparse_df %>%
      exp_arima(date, cash_rate, periods = 12, time_unit = "month",
                fun.aggregate = sum, na_fill_type = "previous",
                auto = TRUE, seasonal = FALSE, seasonal_auto = TRUE,
                ic = "aic", unit_root_test = "kpss", test_mode = TRUE)
    NULL
  }, error = function(e) e$message)
  
  # Verify we get a helpful error, not "subscript out of bounds"
  expect_true(!is.null(err))
  expect_true(grepl("Not enough training data to fit ARIMA in test_mode", err, fixed = TRUE))
  expect_false(grepl("subscript out of bounds", err, fixed = TRUE))
})

test_that("exp_arima works correctly with binned grouped data when all groups have sufficient data", {
  # This test verifies that the fix doesn't break the normal case
  # where all groups have enough data.
  
  set.seed(42)
  # Create data with 48 months to ensure enough data per group
  ts <- seq.Date(as.Date("2018-01-01"), as.Date("2021-12-01"), by = "month")  # 48 months
  df <- data.frame(
    date = ts,
    cash_rate = runif(length(ts), 0.1, 5.0),
    group = rep(c("A", "B"), each = length(ts) / 2)  # Two groups with 24 rows each
  )
  
  df <- df %>% dplyr::group_by(group)
  
  # With periods = 6 and 24 rows per group, this should work fine
  result <- tryCatch({
    df %>%
      exp_arima(date, cash_rate, periods = 6, time_unit = "month",
                auto = TRUE, seasonal = FALSE,
                test_mode = TRUE)
  }, error = function(e) e)
  
  # Should not throw an error
  expect_false(inherits(result, "error"))
  
  # Verify the result structure
  expect_true("data" %in% names(result))
  expect_true("model" %in% names(result))
})

test_that("exp_arima error message includes group context when grouped data has insufficient rows", {
  # This test ensures the error message is actionable and includes group info
  
  ts <- seq.Date(as.Date("2020-01-01"), as.Date("2020-10-01"), by = "month")  # 10 months
  df <- data.frame(
    date = ts,
    value = runif(length(ts)),
    group = c(rep("A", 5), rep("B", 5))  # Two groups with 5 rows each
  ) %>%
    dplyr::group_by(group)
  
  # With periods = 12 and only 5 rows per group, this should fail with helpful message
  err <- tryCatch({
    df %>%
      exp_arima(date, value, periods = 12, time_unit = "month",
                seasonal = FALSE, test_mode = TRUE)
    NULL
  }, error = function(e) e$message)
  
  expect_true(!is.null(err))
  
  # Check error message contains useful information
  expect_true(grepl("Not enough training data", err, fixed = TRUE))
  expect_true(grepl("test_mode", err, fixed = TRUE))
  expect_true(grepl("periods", err, fixed = TRUE))
  
  # Check error message includes group context
  expect_true(grepl("Group:", err, fixed = TRUE))
  
  # Should NOT contain cryptic error
  expect_false(grepl("subscript out of bounds", err, fixed = TRUE))
})

test_that("exp_arima emits warning for sparse grouped data in test_mode", {
  # This test verifies that a warning is emitted when groups have sparse data
  
  set.seed(42)
  ts <- seq.Date(as.Date("2020-01-01"), as.Date("2021-06-01"), by = "month")  # 18 months
  df <- data.frame(
    date = ts,
    value = runif(length(ts)),
    group = c(rep("A", 12), rep("B", 6))  # Group A has 12 rows, Group B has only 6 rows
  ) %>%
    dplyr::group_by(group)
  
  # With periods = 8, Group B has only 6 rows which is less than periods + 2 = 10
  # This should emit a warning before failing
  expect_warning(
    tryCatch(
      df %>%
        exp_arima(date, value, periods = 8, time_unit = "month",
                  seasonal = FALSE, test_mode = TRUE),
      error = function(e) NULL
    ),
    regexp = "Some groups have fewer observations"
  )
})
