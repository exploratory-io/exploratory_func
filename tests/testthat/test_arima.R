context("test exp_arima")
test_that("exp_arima with aggregation", {
  data("raw_data", package = "AnomalyDetection")
  raw_data$timestamp <- as.POSIXct(raw_data$timestamp)

  # Create time gap intentionally.
  raw_data <- raw_data %>% filter(as.Date(timestamp) != as.Date("1980-09-26"))

  raw_data <- raw_data %>% rename(`time stamp`=timestamp, `cou nt`=count)
  raw_data$pre_col1 <- runif(nrow(raw_data))
  raw_data$pre_col2 <- runif(nrow(raw_data))

  model_df <- raw_data %>%
    exp_arima(`time stamp`, valueColumn=`cou nt`, periods=10, time_unit = "day", na_fill_type="value", test_mode=F)

  # Test output columns for difference data frame.
  ret <- model_df %>% dplyr::select(difference) %>% tidyr::unnest(difference)
  # With fable 0.2.1, it used to pick a model with seasonality, but with fable 0.3.0 it picks a model without seasonality for some reason.
  # expect_true(all(c('time stamp','cou nt','diff_order','seasonal_diff_order','seasonal_diff_period','p_value') %in% colnames(ret)))
  expect_true(all(c('time stamp','cou nt','diff_order','p_value') %in% colnames(ret)))

  ret <- model_df %>% dplyr::select(residuals) %>% tidyr::unnest(residuals)
  ret <- model_df %>% dplyr::select(residual_acf) %>% tidyr::unnest() %>% rename(Lag=lag, ACF=acf)
  # No valueColumn (row number) case.
  model_df <- raw_data %>%
    exp_arima(`time stamp`, ,periods=10, time_unit = "day", na_fill_type="value", test_mode=F)
  # With more parameters. Revive this test when we support ARIMAX.
  # model_df <- raw_data %>%
  #   exp_arima(`time stamp`, `pre_col1`, `pre_col2`, time_unit = "day", periods = 2, valueColumn = `cou nt`, fun.aggregate = sum, na_fill_type="value", test_mode =TRUE, d = NA, D = NA, max.p = 5, max.q = 5, max.P = 2, max.Q = 2, max.order = 5, max.d = 2, max.D = 1, start.p = 2, start.q = 2, start.P = 1, start.Q = 1, stationary = FALSE, seasonal = TRUE, ic = "aic", allowdrift = TRUE, allowmean = TRUE, lambda = NULL, biasadj = FALSE, test = "kpss", seasonal.test = "ocsb", parallel = FALSE, num.cores = 2)

  expect_true(!is.null(model_df$model))
})
