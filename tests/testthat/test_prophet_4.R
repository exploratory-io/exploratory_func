context("test prophet functions - Holiday Country Names, Repeat By")

test_that("do_prophet with holiday country", {
  ts <- seq.Date(as.Date("2010-01-01"), as.Date("2012-01-01"), by="day")
  raw_data <- data.frame(timestamp=ts, data=runif(length(ts)))
  ret <- raw_data %>%
    do_prophet(timestamp, data, 10, time_unit = "day", holiday_country_names=c("US","gb"))
  # verify the last date with forecasted_value
  expect_true("Christmas Day" %in% names(ret))
})

test_that("do_prophet with group_by", {
  ts <- seq.Date(as.Date("2010-01-01"), as.Date("2012-01-01"), by="day")
  raw_data_1 <- tibble::tibble(timestamp=ts, data=runif(length(ts)), grp='a')
  raw_data_2 <- tibble::tibble(timestamp=ts, data=runif(length(ts)), grp='b')
  raw_data <- raw_data_1 %>% bind_rows(raw_data_2) %>% group_by(grp)
  model_df <- raw_data %>%
    do_prophet(timestamp, data, 10, time_unit = "day", output="model")
  ret <- model_df %>% tidy_rowwise(model)
  expect_true("grp" %in% colnames(ret))
})
