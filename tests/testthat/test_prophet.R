context("test prophet functions")

test_that("do_prophet with aggregation", {
  data("raw_data", package = "AnomalyDetection")
  raw_data$timestamp <- as.POSIXct(raw_data$timestamp)
  raw_data <- raw_data %>% rename(`time stamp`=timestamp, `cou nt`=count)
  ret <- raw_data %>%
    do_prophet(`time stamp`, `cou nt`, 10, time_unit = "day")
  ret <- raw_data %>%
    do_prophet(`time stamp`, `cou nt`, 10, time_unit = "hour")
  ret <- raw_data %>% tail(100) %>%
    do_prophet(`time stamp`, `cou nt`, 10, time_unit = "minute")
  ret <- raw_data %>% tail(100) %>%
    do_prophet(`time stamp`, `cou nt`, 10, time_unit = "second")

  # test for test mode.
  raw_data$`cou nt`[[length(raw_data$`cou nt`) - 2]] <- NA # inject NA near the end to test #9211
  ret <- raw_data %>%
    do_prophet(`time stamp`, `cou nt`, 2, time_unit = "day", test_mode=TRUE)
  # verify that the last forecasted_value is not NA to test #9211
  expect_true(!is.na(ret$forecasted_value[[length(ret$forecasted_value)]]))

  ret <- raw_data %>%
    do_prophet(`time stamp`, `cou nt`, 2, time_unit = "hour", test_mode=TRUE)
  # verify that the last forecasted_value is not NA to test #9211
  expect_true(!is.na(ret$forecasted_value[[length(ret$forecasted_value)]]))

  ret <- raw_data %>% tail(100) %>%
    do_prophet(`time stamp`, `cou nt`, 2, time_unit = "minute", test_mode=TRUE)
  # verify that the last forecasted_value is not NA to test #9211
  expect_true(!is.na(ret$forecasted_value[[length(ret$forecasted_value)]]))

  ret <- raw_data %>% tail(100) %>%
    do_prophet(`time stamp`, `cou nt`, 2, time_unit = "second", test_mode=TRUE)
  # verify that the last forecasted_value is not NA to test #9211
  expect_true(!is.na(ret$forecasted_value[[length(ret$forecasted_value)]]))
})


# This test is too slow. TODO: make it faster and enable.
# test_that("do_prophet test mode with second as time units", {
#   ts <- seq(as.POSIXct("2010-01-01:00:00:00"), as.POSIXct("2010-01-3:00:00"), by="sec")
#   raw_data <- data.frame(timestamp=ts, data=runif(length(ts))) %>% dplyr::rename(`time stamp`=timestamp, `da ta`=data)
#   raw_data$`da ta`[[length(ts) - 2]] <- NA # inject NA near the end to test #9211
#   ret <- raw_data %>%
#     do_prophet(`time stamp`, `da ta`, 10, time_unit = "second", test_mode=TRUE)
#   # verify that the last forecasted_value is not NA to test #9211
#   expect_true(!is.na(ret$forecasted_value[[length(ret$forecasted_value)]]))
#   # verify that daily, weekly is enabled to test #9361.
#   expect_equal(c("daily") %in% colnames(ret),c(T))
# })

# This test is slow. TODO: make it faster.
test_that("do_prophet test mode with minute as time units", {
  ts <- seq(as.POSIXct("2010-01-01:00:00:00"), as.POSIXct("2010-01-15:00:00"), by="min")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts))) %>% dplyr::rename(`time stamp`=timestamp, `da ta`=data)
  raw_data$`da ta`[[length(ts) - 2]] <- NA # inject NA near the end to test #9211
  ret <- raw_data %>%
    do_prophet(`time stamp`, `da ta`, 10, time_unit = "minute", test_mode=TRUE)
  # verify that the last forecasted_value is not NA to test #9211
  expect_true(!is.na(ret$forecasted_value[[length(ret$forecasted_value)]]))
  # verify that daily, weekly is enabled to test #9361.
  expect_equal(c("daily","weekly") %in% colnames(ret),c(T,T))
})

test_that("do_prophet test mode with hour as time units", {
  ts <- seq(as.POSIXct("2010-01-01:00:00:00"), as.POSIXct("2010-01-15:00:00"), by="hour")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts))) %>% dplyr::rename(`time stamp`=timestamp, `da ta`=data)
  raw_data$`da ta`[[length(ts) - 2]] <- NA # inject NA near the end to test #9211
  ret <- raw_data %>%
    do_prophet(`time stamp`, `da ta`, 10, time_unit = "hour", test_mode=TRUE)
  # verify that the last forecasted_value is not NA to test #9211
  expect_true(!is.na(ret$forecasted_value[[length(ret$forecasted_value)]]))
  # verify that daily, weekly is enabled to test #9361.
  expect_equal(c("daily","weekly") %in% colnames(ret),c(T,T))
})

test_that("do_prophet test mode with month as time units", {
  ts <- seq.Date(as.Date("2010-01-01"), as.Date("2030-01-01"), by="month")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts))) %>% dplyr::rename(`time stamp`=timestamp, `da ta`=data)
  raw_data$`da ta`[[length(ts) - 2]] <- NA # inject NA near the end to test #9211
  ret <- raw_data %>%
    do_prophet(`time stamp`, `da ta`, 10, time_unit = "month", test_mode=TRUE)
  # verify that the last forecasted_value is not NA to test #9211
  expect_true(!is.na(ret$forecasted_value[[length(ret$forecasted_value)]]))
})

test_that("do_prophet test mode with quarter as time units", {
  ts <- seq.Date(as.Date("2010-01-01"), as.Date("2030-01-01"), by="quarter")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts))) %>% dplyr::rename(`time stamp`=timestamp, `da ta`=data)
  raw_data$`da ta`[[length(ts) - 2]] <- NA # inject NA near the end to test #9211
  ret <- raw_data %>%
    do_prophet(`time stamp`, `da ta`, 10, time_unit = "quarter", test_mode=TRUE)
  # verify that the last forecasted_value is not NA to test #9211
  expect_true(!is.na(ret$forecasted_value[[length(ret$forecasted_value)]]))
})

test_that("do_prophet test mode with year as time units", {
  ts <- seq.Date(as.Date("2010-01-01"), as.Date("2030-01-01"), by="year")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts))) %>% dplyr::rename(`time stamp`=timestamp, `da ta`=data)
  raw_data$`da ta`[[length(ts) - 2]] <- NA # inject NA near the end to test #9211
  ret <- raw_data %>%
    do_prophet(`time stamp`, `da ta`, 10, time_unit = "year", test_mode=TRUE)
  # verify that the last forecasted_value is not NA to test #9211
  expect_true(!is.na(ret$forecasted_value[[length(ret$forecasted_value)]]))
})

test_that("do_prophet with extra regressor", {
  ts <- seq.Date(as.Date("2010-01-01"), as.Date("2012-01-01"), by="day")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts)))
  ts2 <- seq.Date(as.Date("2010-01-01"), as.Date("2013-01-01"), by="day")
  regressor_data <- data.frame(timestamp=ts2, regressor=runif(length(ts2)))
  combined_data <- raw_data %>% full_join(regressor_data, by=c("timestamp"="timestamp"))
  ret <- combined_data %>%
    do_prophet(timestamp, data, 10, time_unit = "day", regressors = c("regressor"), funs.aggregate.regressors = c(mean))
  # verify that the last forecasted_value is not NA
  expect_true(!is.na(ret$forecasted_value[[length(ret$forecasted_value)]]))
  expect_equal(ret$timestamp[[length(ret$timestamp)]], as.Date("2012-01-11"))
})

test_that("do_prophet with extra regressor with holiday column", {
  ts <- seq.Date(as.Date("2010-01-01"), as.Date("2012-01-01"), by="day")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts)))
  ts2 <- seq.Date(as.Date("2010-01-01"), as.Date("2013-01-01"), by="day")
  regressor_data <- data.frame(timestamp=ts2, regressor=runif(length(ts2)), holiday=if_else(runif(length(ts2)) > 0.90,"holiday",NA_character_))
  combined_data <- raw_data %>% full_join(regressor_data, by=c("timestamp"="timestamp"))
  ret <- combined_data %>%
    do_prophet(timestamp, data, 10, time_unit = "day", regressors = c("regressor"), funs.aggregate.regressors = c(mean), holiday=holiday)
  # verify that the last forecasted_value is not NA
  expect_true(!is.na(ret$forecasted_value[[length(ret$forecasted_value)]]))
  expect_equal(ret$timestamp[[length(ret$timestamp)]], as.Date("2012-01-11"))
})

test_that("do_prophet with holiday column", {
  ts <- seq.Date(as.Date("2010-01-01"), as.Date("2012-01-01"), by="day")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts)))
  ts2 <- seq.Date(as.Date("2010-01-01"), as.Date("2013-01-01"), by="day")
  regressor_data <- data.frame(timestamp=ts2, regressor=runif(length(ts2)), holiday=if_else(runif(length(ts2)) > 0.90,"holiday",NA_character_)) %>%
    mutate(holiday=as.character(holiday)) %>%
    rename(`holi day`=holiday)
  combined_data <- raw_data %>% full_join(regressor_data, by=c("timestamp"="timestamp"))
  ret <- combined_data %>%
    do_prophet(timestamp, data, 10, time_unit = "day", holiday=`holi day`)
  # verify that the last forecasted_value is not NA
  expect_true(!is.na(ret$forecasted_value[[length(ret$forecasted_value)]]))
  expect_equal(ret$timestamp[[length(ret$timestamp)]], as.Date("2012-01-11"))
})

test_that("do_prophet with holiday column with monthly data", {
  ts <- seq.Date(as.Date("2010-01-01"), as.Date("2012-01-01"), by="month")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts)))
  ts2 <- seq.Date(as.Date("2010-01-01"), as.Date("2013-01-01"), by="month")
  regressor_data <- data.frame(timestamp=ts2, regressor=runif(length(ts2)), holiday=if_else(runif(length(ts2)) > 0.90,"holiday",NA_character_)) %>%
    mutate(holiday=as.character(holiday)) %>%
    rename(`holi day`=holiday)
  combined_data <- raw_data %>% full_join(regressor_data, by=c("timestamp"="timestamp"))
  ret <- combined_data %>%
    do_prophet(timestamp, data, 10, time_unit = "month", holiday=`holi day`)
  # verify that the last forecasted_value is not NA
  expect_true(!is.na(ret$forecasted_value[[length(ret$forecasted_value)]]))
  expect_equal(ret$timestamp[[length(ret$timestamp)]], as.Date("2012-11-01"))
})

test_that("do_prophet with holiday column with hourly data", {
  ts <- seq(as.POSIXct("2010-01-01 00:00:00"), as.POSIXct("2010-01-15 00:00:00"), by="hour")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts)))
  ts2 <- seq(as.POSIXct("2010-01-01 00:00:00"), as.POSIXct("2010-01-20 00:00:00"), by="hour")
  regressor_data <- data.frame(timestamp=ts2, regressor=runif(length(ts2)), holiday=if_else(runif(length(ts2)) > 0.90,"holiday",NA_character_)) %>%
    mutate(holiday=as.character(holiday))
  combined_data <- raw_data %>% full_join(regressor_data, by=c("timestamp"="timestamp"))
  ret <- combined_data %>%
    do_prophet(timestamp, data, 10, time_unit = "hour", holiday=holiday)
  # verify that the last forecasted_value is not NA
  expect_true(!is.na(ret$forecasted_value[[length(ret$forecasted_value)]]))
  expect_equal(ret$timestamp[[length(ret$timestamp)]], as.POSIXct("2010-01-15 10:00:00"))
})

test_that("do_prophet with extra regressor with cap/floor", {
  ts <- seq.Date(as.Date("2010-01-01"), as.Date("2012-01-01"), by="day")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts)))
  ts2 <- seq.Date(as.Date("2010-01-01"), as.Date("2013-01-01"), by="day")
  regressor_data <- data.frame(timestamp=ts2, regressor=runif(length(ts2)))
  combined_data <- raw_data %>% full_join(regressor_data, by=c("timestamp"="timestamp"))
  ret <- combined_data %>%
    do_prophet(timestamp, data, 10, time_unit = "day", cap = 2, floor = -2, regressors = c("regressor"), funs.aggregate.regressors = c(mean))
  # verify that the last forecasted_value is not NA
  expect_true(!is.na(ret$forecasted_value[[length(ret$forecasted_value)]]))
  expect_equal(ret$timestamp[[length(ret$timestamp)]], as.Date("2012-01-11"))
})

test_that("do_prophet test mode with extra regressor", {
  ts <- seq.Date(as.Date("2010-01-01"), as.Date("2012-01-01"), by="day")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts)))
  raw_data$data[[length(ts) - 2]] <- NA # inject NA near the end to test #9211
  # here refressor data is till 2013-01-01, but the part after 2012-01-01 should be ignored.
  ts2 <- seq.Date(as.Date("2010-01-01"), as.Date("2013-01-01"), by="day")
  regressor_data <- data.frame(timestamp=ts2, regressor=runif(length(ts2)))
  combined_data <- raw_data %>% full_join(regressor_data, by=c("timestamp"="timestamp"))
  ret <- combined_data %>%
    do_prophet(timestamp, data, 10, time_unit = "day", regressors = c("regressor"), funs.aggregate.regressors = c(mean), test_mode = TRUE)
  # verify that the last forecasted_value is not NA
  expect_true(!is.na(ret$forecasted_value[[length(ret$forecasted_value)]]))
  expect_equal(ret$timestamp[[length(ret$timestamp)]], as.Date("2012-01-01"))
})


test_that("do_prophet grouped case", {
  data("raw_data", package = "AnomalyDetection")
  raw_data$timestamp <- as.POSIXct(raw_data$timestamp)
  expect_error({
    ret <- raw_data %>%
      dplyr::group_by(timestamp) %>%
      do_prophet(timestamp, count, 10)
  }, "timestamp is grouped. Please ungroup it.")

  expect_error({
    ret <- raw_data %>%
      dplyr::group_by(count) %>%
      do_prophet(timestamp, count, 10)
  }, "count is grouped. Please ungroup it.")
})

test_that("do_prophet without value_col", {
  data("raw_data", package = "AnomalyDetection")
  raw_data$timestamp <- as.POSIXct(raw_data$timestamp)
  ret <- raw_data %>%
    do_prophet(timestamp, NULL, 10)
})
