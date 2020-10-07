#' Forecast time series data by ARIMA model
# TODO: write docs
#' @param df - Data frame
#' @param time_col - Column that has time data
#' @param value_col - Column that has value data
#' @param periods - Number of time periods (e.g. days. unit is determined by time_unit) to forecast.
#' @param time_unit - "second"/"sec", "minute"/"min", "hour", "day", "week", "month", "quarter", or "year".
#' @param include_history - Whether to include history data in forecast or not.
#' @param fun.aggregate - Function to aggregate values.
#' @param na_fill_type - Type of NA fill:
#'                       NULL - Skip NA fill. Default behavior.
#'                       "previous" - Fill with previous non-NA value.
#'                       "value" - Fill with the value of na_fill_value.
#'                       "interpolate" - Linear interpolation.
#'                       "spline" - Spline interpolation.
#' @param na_fill_value - Value to fill NA when na_fill_type is "value"
#' @param ... - extra values to be passed to prophet::prophet. listed below.
#' @export
exp_arima <- function(df, time, valueColumn,
                      periods = 10,
                      time_unit = "day",
                      fun.aggregate = sum,
                      test_mode = FALSE,
                      auto = TRUE,
                      p = 0,
                      d = 0,
                      q = 0,
                      P = NULL,
                      D = NULL,
                      Q = NULL,
                      max.p = 5,
                      max.q = 5,
                      max.P = 2,
                      max.Q = 2,
                      max.order = 5,
                      max.d = 2,
                      max.D = 1,
                      start.p = 2,
                      start.q = 2,
                      start.P = 1,
                      start.Q = 1,
                      stationary = FALSE,
                      seasonal = TRUE,
                      seasonal_periods = NULL,
                      ic = "aic",
                      allowdrift = TRUE,
                      allowmean = TRUE,
                      lambda = NULL,
                      biasadj = FALSE,
                      unit_root_test = "kpss",
                      seasonal.test = "ocsb",
                      stepwise=FALSE,
                      parallel = FALSE,
                      num.cores = 2,
                      na_fill_type = NULL,
                      na_fill_value = 0,
                      trace = TRUE,
                      regressors = NULL,
                      funs.aggregate.regressors = NULL,
                      regressors_na_fill_type = NULL,
                      regressors_na_fill_value = 0,
                      conf_level = 0.8,
                      ...
                      ){
  validate_empty_data(df)

  time_col <- tidyselect::vars_pull(names(df), !! rlang::enquo(time))

  # if valueColumns is not set (value is NULL by default)
  # dplyr::select_var occurs Error
  value_col <- if(!missing(valueColumn)) { # is.null(valueColumn) gives error for some reason.
    tidyselect::vars_pull(names(df), !! rlang::enquo(valueColumn))
  }
  # xreg_cols <- dplyr::select_vars(names(df), !!! rlang::quos(...))

  grouped_col <- grouped_by(df)

  if (time_unit == "min") {
    time_unit <- "minute"
  }
  else if (time_unit == "sec") {
    time_unit <- "second"
  }

  # column name validation
  if(!time_col %in% colnames(df)){
    stop(paste0(time_col, " is not in column names"))
  }

  if(time_col %in% grouped_col){
    stop(paste0(time_col, " is grouped. Please ungroup it."))
  }

  if(!is.null(value_col)){
    if (!value_col %in% colnames(df)){
      stop(paste0(value_col, " is not in column names"))
    }
    if(value_col %in% grouped_col){
      stop(paste0(value_col, " is grouped. Please ungroup it."))
    }
  }

  if (is.null(seasonal_periods)) {
    seasonal_periods <- switch(time_unit,
                               year = NULL,
                               quarter = 4,
                               month = 12,
                               week = 52,
                               day = 7,
                               hour = 24,
                               minute = NULL,
                               second = NULL)
  }

  # Compose arguments to pass to dplyr::summarise.
  summarise_args <- list() # default empty list
  regressor_output_cols <- NULL # Just declaring variable
  if (!is.null(regressors) && !is.null(funs.aggregate.regressors)) {
    summarise_args <- purrr::map2(funs.aggregate.regressors, regressors, function(func, cname) {
      quo(UQ(func)(UQ(rlang::sym(cname))))
    })

    # Output column names.
    if (!is.null(names(regressors))) {
      regressor_output_cols <- names(regressors)
    }
    else {
      regressor_output_cols <- regressors
    }

    names(summarise_args) <- regressor_output_cols
  }

  # remove rows with NA time
  df <- df[!is.na(df[[time_col]]), ]

  do_arima_each <- function(df){
    df[[time_col]] <- if (time_unit %in% c("day", "week", "month", "quarter", "year")) {
      # Take care of issue that happened in anomaly detection here for forecast too.
      # In this case, convert (possibly) from POSIXct to Date first.
      # If we did this without converting POSIXct to Date, floor_date works, but later at complete stage,
      # data on day-light-saving days would be skipped, since the times seq.POSIXt gives and floor_date does not match.
      # We give the time column's timezone to as.Date, so that the POSIXct to Date conversion is done
      # based on that timezone.
      lubridate::floor_date(as.Date(df[[time_col]], tz = lubridate::tz(df[[time_col]])), unit = time_unit)
    } else {
      lubridate::floor_date(df[[time_col]], unit = time_unit)
    }

    if(!is.null(grouped_col)){
      # drop grouping columns
      df <- df[, !colnames(df) %in% grouped_col]
    }

    aggregated_data <- if (!is.null(value_col)){
      # remove rows with NA value_col
      df <- df[!is.na(df[[value_col]]), ]

      df %>% dplyr::select(ds=time_col, value=value_col, unname(regressors)) %>% # unname is necessary to avoid error when regressors is named vector.

        dplyr::arrange(ds) %>%
        dplyr::filter(!is.na(value)) %>% # remove NA so that we do not pass data with NA, NaN, or 0 to arima
        dplyr::group_by(ds) %>%
        dplyr::summarise(y = fun.aggregate(value), !!!summarise_args)
    } else {
      grouped_df <- df %>% dplyr::select(ds=time_col, unname(regressors)) %>% dplyr::arrange(ds) %>% dplyr::group_by(ds)

      # TODO: implement the method that summarize count and summarize_each are executed at the same time
      count_df <- grouped_df %>% dplyr::summarise(y = n())
      aggr_df <- grouped_df %>% dplyr::summarise(!!!summarise_args)
      count_df %>% dplyr::full_join(aggr_df, by=c("ds"="ds"))
    }

    if (!is.null(na_fill_type)) {
      # complete the date time with NA
      aggregated_data <- if(inherits(aggregated_data$ds, "Date")){
        aggregated_data %>%
          tidyr::complete(ds = seq.Date(min(ds), max(ds), by = time_unit))
      } else if(inherits(aggregated_data$ds, "POSIXct")) {
        aggregated_data %>%
          tidyr::complete(ds = seq.POSIXt(min(ds), max(ds), by = time_unit))
      } else {
        stop("time must be Date or POSIXct.")
      }
      # fill NAs in y with zoo
      aggregated_data <- aggregated_data %>% dplyr::mutate(y = fill_ts_na(y, ds, type = na_fill_type, val = na_fill_value))
    }

    if (test_mode) {
      # Remove end of aggregated_data as test data to make training data.

      # Fill aggregated_data$ds with missing data/time.
      # This is necessary to make forecast period correspond with test period in test mode when there is missing date/time in original aggregated_data$ds.
      # Note that this is only for the purpose of correctly determine where to start test period, and we remove those filled data once that purpose is met.
      ts <- create_ts_seq(aggregated_data$ds, min, max, time_unit)

      ts_df <- data.frame(ds=ts)
      # ts_df has to be the left-hand side to keep the row order according to time order.
      filled_aggregated_data <- dplyr::full_join(ts_df, aggregated_data, by = c("ds" = "ds"))
      
      training_data <- filled_aggregated_data
      training_data <- training_data %>% head(-periods)

      # we got correct set of training data by filling missing date/time,
      # but now, filter them out again.
      # by doing so, we affect future table, and skip prediction (interpolation)
      # for all missing date/time, which could be expensive if the training data is sparse.
      # keep the last row even if it does not have training data, to mark the end of training period, which is the start of test period.
      training_data <- training_data %>% dplyr::filter(!is.na(y) | row_number() == n())
    }
    else {
      training_data <- aggregated_data
    }

    if(length(regressors) > 0 && test_mode){
      # xreg in auto.arima must be a character vector or matrix.
      xreg <- training_data %>% dplyr::select(-ds, -y) %>% as.matrix()
      # forecast need reggression values (xreg)
      forecast_xreg <- filled_aggregated_data %>%
                         dplyr::select(-ds, -y) %>%
                         tail(periods) %>%
                         as.matrix()
    } else {
      xreg <- NULL
      forecast_xreg <- NULL
    }

    # auto.arima has no trace objects, just output to stdout/stderr
    # So, if trace value is needed, the output must be captured.
    ret <- NULL

    if (time_unit %in% c("month", "year", "quarter")) { #TODO: Take care of other time units too.
      training_tsibble <- tsibble::tsibble(ds = tsibble::yearmonth(training_data$ds), y = training_data$y)
    }
    else {
      training_tsibble <- tsibble::tsibble(ds = training_data$ds, y = training_data$y)
    }

    if (seasonal && !is.null(seasonal_periods)) {
      if (auto) {
        formula_str <- paste0("y ~ 0 + PDQ(period=", seasonal_periods, ")")
      }
      else if (!is.null(P) && !is.null(D) && !is.null(Q)) { # p, d, q are supposed to be set manually. If All of P, D, Q are specified, use them.
        formula_str <- paste0("y ~ pdq(", p, ",", d, ",", q, ") + PDQ(", P, ",", D, ",", Q, ", period=", seasonal_periods, ")")
      }
      else { # p, d, q are supposed to be set manually. Since P, D, Q are not specified, automatically search them, even though auto is not set TRUE.
        formula_str <- paste0("y ~ pdq(", p, ",", d, ",", q, ") + PDQ(period=", seasonal_periods, ")")
      }
    }
    else {
      if (auto) {
        formula_str <- "y ~ PDQ(0,0,0)"
      }
      else {
        formula_str <- paste0("y ~ pdq(", p, ",", d, ",", q, ") + PDQ(0,0,0)")
      }
    }
    fml <- as.formula(formula_str)

    model_df <- training_tsibble %>%
      fabletools::model(arima=fable::ARIMA(!!fml,
                                           ic = ic,
                                           stepwise=stepwise,
                                           ))
    if (class(model_df$arima[[1]]$fit) == "null_mdl") {
      stop("Null model was selected.") # Error, because it cannot produce forecast. https://github.com/tidyverts/fable/issues/304
    }
    forecasted_df <- model_df %>% fabletools::forecast(h=periods)

    # Old code with forecast::auto.arima kept for reference while moving to fable::ARIMA.
    #
    # trace_output <- capture.output({
    #   ret <- training_data %>% tidyr::nest() %>%
    #            dplyr::mutate(model = purrr::map(data, function(df) {
    #              forecast::auto.arima(training_data[, "y"],
    #                                   xreg = xreg,
    #                                   d = d,
    #                                   D = D,
    #                                   max.d = max.d,
    #                                   max.D = max.D,
    #                                   max.p = max.p,
    #                                   max.q = max.q,
    #                                   max.P = max.P,
    #                                   max.Q = max.Q,
    #                                   start.p = start.p,
    #                                   start.q = start.q,
    #                                   start.P = start.P,
    #                                   start.Q = start.Q,
    #                                   max.order = max.order,
    #                                   seasonal=seasonal,
    #                                   stepwise=stepwise,
    #                                   stationary = FALSE,
    #                                   ic = ic,
    #                                   allowdrift = allowdrift,
    #                                   allowmean = allowmean,
    #                                   lambda = lambda,
    #                                   biasadj = biasad,
    #                                   test = test,
    #                                   seasonal.test = seasonal.test,
    #                                   parallel = parallel,
    #                                   num.cores = num.cores,
    #                                   trace = trace)
    #            }))
    # })
    # trace_output <- trace_output[grepl("(^ ARIMA|^ Best model)", trace_output)]
    # conn <- textConnection(trace_output)
    # model_traces <- read.table(conn, sep=":")
    # close(conn)
    #
    # # Add model traces
    # ret <- ret %>% dplyr::mutate(model_traces = purrr::map(data, function(df){
    #   model_traces
    # }))
    # # Forecast
    # forecast_obj <- forecast::forecast(ret$model[[1]],
    #                                    xreg=forecast_xreg,
    #                                    h=periods, level=c(80))
    # forecast_df <- as_tibble(forecast_obj)

    fitted_df <- model_df %>% fitted()
    fitted_training_df <- training_data %>% dplyr::mutate(forecasted_value=fitted_df$.fitted)

    # # Extract fitted values for training data.
    # ret <- ret %>% dplyr::mutate(data = purrr::map2(data, model, function(df, m){
    #   # m$fitted is ts class and column name is "x"
    #   # So in order to extract ts values, use m$fitted[, "x"]
    #   fitted_values = if(is.null(dim(m$fitted))){
    #     # when auto.arima with xreg, m$fitted has no dim.
    #     m$fitted
    #   } else {
    #     m$fitted[, "x"]
    #   }
    #   df %>% dplyr::mutate(forecasted_value=fitted_values)
    # }))

    forecast_rows <- tibble(ds=forecasted_df$ds,
                            forecasted_value=mean(forecasted_df$y), # Note that y is a distribution object.
                            forecasted_value_high=quantile(forecasted_df$y, conf_level),
                            forecasted_value_low=quantile(forecasted_df$y, 1 - conf_level))

    if (test_mode){
      fitted_training_df$is_test_data <- FALSE
      forecast_rows$y <- tail(filled_aggregated_data, periods)[["y"]] #TODO: consider if this is always correct.
      forecast_rows$is_test_data <- TRUE 
    }

    if (is.null(value_col)) {
      value_col <- "count"
    }

    # Bind Training Data + Forecast Data
    # Revive Original column names(time_col, value_col)
    ret_df <- fitted_training_df %>% dplyr::bind_rows(forecast_rows)
    if (time_col != "ds") { # if time_col happens to be "ds", do not do this, since it will make the column name "ds.new"
      time_col <- avoid_conflict(colnames(ret_df), time_col)
      colnames(ret_df)[colnames(ret_df) == "ds"] <- time_col
    }
    if (value_col != "y") { # if value_col happens to be "y", do not do this, since it will make the column name "y.new".
      value_col <- avoid_conflict(colnames(ret_df), value_col)
      colnames(ret_df)[colnames(ret_df) == "y"] <- value_col
    }

    # TODO: Add regressor effect columns. Make this work again.
    # if (!is.null(regressor_output_cols)) {
    #   for (i in 1:length(regressor_output_cols)) {
    #     ret_df[[paste0(regressor_output_cols[[i]], "_effect")]] <- ret_df[[regressor_output_cols[[i]]]] * model$coef[[regressor_output_cols[[i]]]]
    #   }
    # }

    if (test_mode) {
      ret_df <- ret_df %>% dplyr::select(-is_test_data, is_test_data)
    }
    attr(ret_df, "value_col") <- value_col # We need this into to read it later to evaluate it.

    class(model_df$arima[[1]]$fit) <- c("ARIMA_exploratory", class(model_df$arima[[1]]$fit))
    # Note that model column is mable, rather than model object.
    # It seems this is how fable is designed so that multiple models can be applied to a same data at once.
    # Applying tidy() etc. on a mable seems to in turn call tidy() on each model stored in mable.
    # Reference: https://github.com/tidyverts/fable/issues/91
    ret <- tibble(data = list(ret_df), model = list(model_df))

    tryCatch({
      stl_ts <-ts(training_data$y, frequency=seasonal_periods)
      stl_res <- stl(stl_ts, "periodic")
      stl_df <- as.data.frame(stl_res$time.series)
      stl_df[[time_col]] <- training_data$ds

      # Detect change points
      cpt_res <- changepoint::cpt.var(training_data$y, method="PELT")
      cpt_vec <- rep(0, length(training_data$ds))
      cpt_vec[cpt_res@cpts] <- 1
      cpt_vec[length(cpt_vec)] <- 0 # Last data point sometimes is reported as a change point, but this is not useful for our purpose.
      stl_df$change_point <- cpt_vec
      stl_df$y <- training_data$y

      stl_seasonal_df <- stl_df %>% dplyr::slice(1:seasonal_periods) # To display only one seasonal cycle
      ret <- ret %>% mutate(stl = list(!!stl_df), stl_seasonal = list(!!stl_seasonal_df))
    }, error = function(e) { # This can fail depending on the data.
      # At least, create stl, stl_seasonal columns to avoid error.
      ret <<- ret %>% mutate(stl = list(data.frame()), stl_seasonal = list(data.frame()))
    })

    # Add ACF.
    acf_res <- acf(training_tsibble$y, plot=FALSE)
    acf_df <- data.frame(lag = acf_res$lag, acf = acf_res$acf)
    ret <- ret %>% mutate(acf = list(!!acf_df))

    # Data after differentiations
    differences <- model_df$arima[[1]]$fit$spec$d
    if (!is.null(differences) && differences > 0) {
      diff_res <- diff(training_tsibble$y, differences = differences)
    }
    else {
      diff_res <- training_tsibble$y
    }
    seasonality_differences <- model_df$arima[[1]]$fit$spec$D
    seasonality_lag <- model_df$arima[[1]]$fit$spec$period

    if (!is.null(seasonality_differences) && seasonality_differences > 0) {
      diff_res <- diff(diff_res, differences = seasonality_differences, lag = seasonality_lag)
    }
    diff_df <- tibble::tibble(ds=training_data$ds[(length(training_data$ds)-length(diff_res)+1):length(training_data$ds)],
                              diff=diff_res)

    # Run unit root test on the differentiated data.
    runTests <- function(x, test) {
      tryCatch({
        suppressWarnings(diff <- switch(test,
                                        kpss = tseries::kpss.test(x),
                                        adf = tseries::adf.test(x),
                                        pp = tseries::pp.test(x),
                                        stop("This shouldn't happen")))
        diff
      }, error = function(e) {
        # TODO: do something.
        stop(e)
      })
    }

    unit_root_test_res <- runTests(diff_res, unit_root_test)
    ret <- ret %>% mutate(unit_root_test = list(!!unit_root_test_res))

    unit_root_test_pvalue <- unit_root_test_res$p.value

    diff_df$p_value <- unit_root_test_pvalue
    colnames(diff_df)[colnames(diff_df) == "ds"] <- time_col
    colnames(diff_df)[colnames(diff_df) == "diff"] <- value_col
    ret <- ret %>% mutate(difference = list(!!diff_df))

    # Add difference ACF/PACF.
    if (length(diff_res) < 2) {
      stop("The data is too short for the required differences.")
    }
    acf_res <- acf(diff_res, plot=FALSE)
    difference_acf <- data.frame(lag = acf_res$lag, acf = acf_res$acf)
    ret <- ret %>% mutate(difference_acf = list(!!difference_acf))
    pacf_res <- pacf(diff_res, plot=FALSE)
    difference_pacf <- data.frame(lag = pacf_res$lag, acf = pacf_res$acf)
    ret <- ret %>% mutate(difference_pacf = list(!!difference_pacf))

    model_d <- model_df$arima[[1]]$fit$spec$d
    model_D <- model_df$arima[[1]]$fit$spec$D
    model_period <- model_df$arima[[1]]$fit$spec$period

    total_diffs <- model_d
    if (model_D > 0) {
      total_diffs <- total_diffs + model_D * model_period
    }

    # Add residual ACF/PACF
    residuals_df <- model_df %>% residuals()
    if (model_d > 0 || model_D > 0) {
      residuals_df <- residuals_df %>% dplyr::slice((1 + total_diffs):n()) # cut off the beginning of the residual data which is not really residual.
    }
    residual_acf <- residuals_df %>% feasts::ACF(.resid)
    residual_acf <- as.data.frame(residual_acf %>% mutate(lag = as.numeric(lag))) # as.data.frame is to avoid error from unnest() later.
    ret <- ret %>% mutate(residual_acf = list(!!residual_acf))
    residual_pacf <- residuals_df %>% feasts::PACF(.resid)
    residual_pacf <- as.data.frame(residual_pacf %>% mutate(lag = as.numeric(lag))) # as.data.frame is to avoid error from unnest() later.
    ret <- ret %>% mutate(residual_pacf = list(!!residual_pacf))

    # Add residual
    residuals_df <- as.data.frame(residuals_df) # as.data.frame is to avoid error from unnest() later.
    if (tsibble::is_yearmonth(residuals_df$ds)) { # Convert yearmonth to Date, so that the chart can plot it.
      residuals_df <- residuals_df %>% mutate(ds = as.Date(ds))
    }
    colnames(residuals_df)[colnames(residuals_df) == "ds"] <- time_col
    colnames(residuals_df)[colnames(residuals_df) == ".resid"] <- value_col
    ret <- ret %>% mutate(residuals= list(!!residuals_df))

    m <- model_df$arima[[1]]$fit$model # model of "Arima" class. Q: is this from stats package?
    residuals <- residuals(m) # residual has to be extracted from above model to get freq at the next line.
    freq <- frequency(residuals)
    degree_of_freedom <- length(m$coef) # Definition of modeldf.Arima in forecast package.
    # Logic used inside checkresiduals to automatically determine lag.
    lag <- ifelse(freq > 1, 2 * freq, 10)
    lag <- min(lag, round(length(residuals)/5))
    lag <- max(degree_of_freedom + 3, lag)
    residual_test <- feasts::ljung_box(residuals, lag=lag, dof=degree_of_freedom)
    residual_test <- tibble::tibble(statistic=residual_test[[1]], p.value=residual_test[[2]], lag=lag, dof=degree_of_freedom)
    # ret <- ret %>% mutate(residual_test = list(!!residual_test))
    attr(ret$model[[1]]$arima[[1]]$fit, "residual_test") <- residual_test

    if(F){ # Old code using forecast package. Will remove later.
    ret <- ret %>% dplyr::mutate(test_results = purrr::map(model, function(m) {
      # Repeat test for each lag.
      residuals <- residuals(m)
      freq <- frequency(residuals)
      degree_of_freedom <- length(m$coef) # Definition of modeldf.Arima in forecast package.
      # Logic used inside checkresiduals to automatically determine lag.
      lag <- ifelse(freq > 1, 2 * freq, 10)
      lag <- min(lag, round(length(residuals)/5))
      lag <- max(degree_of_freedom + 3, lag)
    
      result <- data.frame(data=I(purrr::map(as.list(1:lag), function(i){forecast::checkresiduals(m, lag = i, plot=FALSE)})))
      result <- result %>% mutate(data=purrr::map(data,function(x){
        data.frame(method=x$method,
                   data.name=x$data.name,
                   statistic = x$statistic,
                   p.value = x$p.value,
                   df = x$parameter)
      })) %>% unnest(data) %>% mutate(lag=row_number())
      result
    }))
    ret <- ret %>% dplyr::mutate(residuals = purrr::map(model, function(m) {
      result <- data.frame(residuals=as.numeric(residuals(m))) %>%
        dplyr::mutate(time = row_number())
      result
    }))
    ret <- ret %>% dplyr::mutate(acf = purrr::map(model, function(m) {
       acf_res <- acf(m$x, plot=FALSE)
       data.frame(lag = acf_res$lag, acf = acf_res$acf)
    }))
    ret <- ret %>% dplyr::mutate(difference_acf = purrr::map2(data, model, function(df, m) {
      # ACF on difference.
      differences <- (forecast::arimaorder(m))[["d"]]
      values <- df[[value_col]]
      if (!test_mode) { # Filter out NAs coming from forecasted rows
        values <- values[!is.na(values)]
      }
      if (differences > 0) {
        diff_res <- diff(values, differences=differences)
      }
      else {
        diff_res <- values
      }
      acf_res <- acf(diff_res, plot=FALSE)
      data.frame(lag = acf_res$lag, acf = acf_res$acf)
    }))
    ret <- ret %>% dplyr::mutate(residual_acf = purrr::map(model, function(m) {
       acf_res <- acf(residuals(m), plot=FALSE)
       data.frame(lag = acf_res$lag, acf = acf_res$acf)
    })) %>% dplyr::mutate(unit_root_test = purrr::map2(data, model, function(df, m) {
      differences=(forecast::arimaorder(m))[["d"]]
      values <- df[[value_col]]
      values <- values[!is.na(values)] # Remove NAs. They are appended at the end, because of binding of forecasted rows.
      if (differences > 0) {
        diff_res <- diff(values, differences=differences)
      }
      else {
        diff_res <- values 
      }
      type <- 1 # 1 menas "level". TODO: check if this is correct.
      urca_pval <- function(urca_test) {
        approx(urca_test@cval[1, ], as.numeric(sub("pct", "", 
                                                   colnames(urca_test@cval)))/100, xout = urca_test@teststat[1], 
               rule = 2)$y
      }
      kpss_wrap <- function(x, ..., use.lag = trunc(3 * sqrt(length(x))/13)) {
        urca::ur.kpss(x, ..., use.lag = use.lag)
      }

      runTests <- function(x, test) {
        tryCatch({
          suppressWarnings(diff <- switch(test, kpss = kpss_wrap(x, type = c("mu", "tau")[type]),
                                          adf = urca::ur.df(x, type = c("drift", "trend")[type]), 
                                          pp = urca::ur.pp(x, type = "Z-tau", model = c("constant", "trend")[type]),
                                          stop("This shouldn't happen")))
          diff
        }, error = function(e) {
          # TODO: do something.
          stop(e)
        })
      }

      unit_root_test_res <- runTests(diff_res, test)

      data.frame(unit_root_test_res@cval, teststat = unit_root_test_res@teststat)
    }))
    }

    ret
  }

  # Calculation is executed in each group.
  # Storing the result in this name_col and
  # unnesting the result.
  # name_col is not conflicting with grouping columns
  # thanks to avoid_conflict that is used before,
  # this doesn't overwrite grouping columns.
  tmp_col <- avoid_conflict(colnames(df), "tmp_col")
  ret <- df %>%
    dplyr::do_(.dots=setNames(list(~do_arima_each(.)), tmp_col)) %>%
    dplyr::ungroup()
  ret <- ret %>% unnest_with_drop(!!rlang::sym(tmp_col))

  if (length(grouped_col) > 0) {
    ret <- ret %>% dplyr::group_by(!!!rlang::syms(grouped_col))
  }

  ret
}

create_ts_seq <- function(ds, start_func, to_func, time_unit, start_add=0, to_add=0){
  if (time_unit == "minute") {
    time_unit_for_seq <- "min"
  }
  else if (time_unit == "second") {
    time_unit_for_seq <- "sec"
  }
  else {
    time_unit_for_seq <- time_unit
  }
  # Create periodical sequence of time to fill missing date/time
  if (time_unit %in% c("hour", "minute", "second")) { # Use seq.POSIXt for unit smaller than day.
    ts <- seq.POSIXt(as.POSIXct(start_func(ds) + start_add), as.POSIXct(to_func(ds) + to_add), by=time_unit_for_seq)
    if (!lubridate::is.POSIXct(ts)) {
      ts <- as.POSIXct(ts)
    }
  }
  else { # Use seq.Date for unit of day or larger. Using seq.POSIXct for month does not always give first day of month.
    ts <- seq.Date(as.Date(start_func(ds) + start_add), as.Date(to_func(ds) + to_add), by=time_unit_for_seq)
    if (!lubridate::is.Date(ds)) {
      ts <- as.Date(ts)
    }
  }
  ts
 }

#' @export
glance.ARIMA_exploratory <- function(x, pretty.name = FALSE, ...) { #TODO: add test
  # Note that, because of fable, x here is actually from model_df$model[1]$arima[[1]]$fit

  m <- x$model # x$model is the model object of Arima class.

  ar_terms <- m$coef %>% names() %>% .[stringr::str_detect(., "^s?ar[0-9]*")]
  ma_terms <- m$coef %>% names() %>% .[stringr::str_detect(., "^s?ma[0-9]*")]

  repeatability <- function(m, term_names){
    abs(polyroot(c(1, coef(m)[term_names])))
  }

  stationarity <- function(m, term_names){
    abs(polyroot(c(1, -coef(m)[term_names])))
  }

  ar_stationarity <- setNames(stationarity(m, ar_terms), as.list(ar_terms))
  ma_repeatability <- setNames(repeatability(m, ma_terms), as.list(ma_terms))

  # forecast::accuracy(m) seems to be returning NaNs.
  # df <- data.frame(AIC=m$aic, BIC=m$bic, AICc=m$aicc, as.list(forecast::arimaorder(m)), forecast::accuracy(m))
  # TODO: migrate out from forecast package.

  # Extract ARIMA orders (p,d,q) and Seasonal ARIMA orders (P, D, Q) from the model.
  # Reference: https://otexts.com/fpp2/seasonal-arima.html
  order <- m$arma[c(1, 6, 2, 3, 7, 4, 5)]
  names(order) <- c("p", "d", "q", "P", "D", "Q", "Frequency")
  seasonal <- (order[7] > 1 & sum(order[4:6]) > 0)
  if (!seasonal) {
    order <- order[1:3]
  }

  df <- data.frame(AIC=m$aic, BIC=m$bic, AICc=m$aicc, as.list(order))
  residual_test <-attr(x,"residual_test") 
  df$`Ljung-Box Test Statistic` <- residual_test$statistic
  df$`Ljung-Box Test P Value` <- residual_test$p.value

  if(F) { # Skipped for now. TODO: Revive it.
    if(length(ar_stationarity) > 0){
      ar_stationarity_df <- as.data.frame(as.list(ar_stationarity)) %>%
                              dplyr::rename_all(funs(stringr::str_c(., "_stationarity")))
      df <- merge(df, ar_stationarity_df)
    }

    if(length(ma_repeatability) > 0){
      ma_stationarity_df <- as.data.frame(as.list(ma_repeatability)) %>%
                               dplyr::rename_all(funs(stringr::str_c(., "_repeatability")))
      df <- merge(df, ma_stationarity_df)
    }
  }
  df
  # Version that makes use of fable:::glance.ARIMA().
  # Remove list columns ar_roots and ma_roots for now. TODO: Make use of those info too.
  # ret <- fable:::glance.ARIMA(x) %>% dplyr::select(-ar_roots, -ma_roots)
  # --- Example output ---
  # A tibble: 1 x 5
  #   sigma2 log_lik   AIC  AICc   BIC
  #    <dbl>   <dbl> <dbl> <dbl> <dbl>
  # 1 0.0293    499. -995. -995. -984.

}

#' Model agnostic function to get common time series metric and model specific glance info.
#' @export
glance_with_ts_metric <- function(df) {
  ret1 <- df %>% glance_rowwise(model)
  ret2 <- df %>% dplyr::select(data) %>% tidyr::unnest(data)
  value_col <- attr(df$data[[1]], "value_col")
  if (any(ret2$is_test_data)) {
    ret2 <- ret2 %>% dplyr::summarize(RMSE=exploratory::rmse(!!rlang::sym(value_col), forecasted_value, is_test_data), MAE=exploratory::mae(!!rlang::sym(value_col), forecasted_value, is_test_data), MAPE=exploratory::mape(!!rlang::sym(value_col), forecasted_value, is_test_data), MASE=exploratory::mase(!!rlang::sym(value_col), forecasted_value, is_test_data), `Number of Rows for Training`=sum(!is_test_data), `Number of Rows for Test`=sum(is_test_data))
  }
  else {
    ret2 <- ret2 %>% dplyr::summarize(RMSE=exploratory::rmse(!!rlang::sym(value_col), forecasted_value, !is.na(!!rlang::sym(value_col))), MAE=exploratory::mae(!!rlang::sym(value_col), forecasted_value, !is.na(!!rlang::sym(value_col))), MAPE=exploratory::mape(!!rlang::sym(value_col), forecasted_value, !is.na(!!rlang::sym(value_col))), `Number of Rows`=sum(!is.na(!!rlang::sym(value_col))))
  }
  ret <- dplyr::bind_cols(ret2, ret1) # We show the model agnostic metrics first.
  if ("Number of Rows" %in% colnames(ret)) {
    ret <- ret %>% dplyr::select(-`Number of Rows`, everything(), `Number of Rows`)
  }
  if ("Number of Rows for Training" %in% colnames(ret)) {
    ret <- ret %>% dplyr::select(-`Number of Rows for Training`, everything(), `Number of Rows for Training`)
  }
  if ("Number of Rows for Test" %in% colnames(ret)) {
    ret <- ret %>% dplyr::select(-`Number of Rows for Test`, everything(), `Number of Rows for Test`)
  }
  ret
}
