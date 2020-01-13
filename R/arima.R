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
do_arima <- function(df, time,
                     valueColumn = NULL,
                     time_unit = "day",
                     periods = 10,
                     fun.aggregate = sum,
                     test_mode = FALSE,
                     d = NA,
                     D = NA,
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
                     ic = "aic",
                     allowdrift = TRUE,
                     allowmean = TRUE,
                     lambda = NULL,
                     biasadj = FALSE,
                     test = "kpss",
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
                     ...
                     ){
  validate_empty_data(df)

  loadNamespace("dplyr")
  loadNamespace("forecast")

  time_col <- dplyr::select_var(names(df), !! rlang::enquo(time))

  # if valueColumns is not set (value is NULL by default)
  # dplyr::select_var occurs Error
  value_col <- if(!missing(valueColumn)){
    dplyr::select_var(names(df), !! rlang::enquo(valueColumn))
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
    trace_output <- capture.output({
      ret <- training_data %>% tidyr::nest() %>%
               dplyr::mutate(model = purrr::map(data, function(df) {
                 forecast::auto.arima(training_data[, "y"],
                                      xreg = xreg,
                                      d = d,
                                      D = D,
                                      max.d = max.d,
                                      max.D = max.D,
                                      max.p = max.p,
                                      max.q = max.q,
                                      max.P = max.P,
                                      max.Q = max.Q,
                                      start.p = start.p,
                                      start.q = start.q,
                                      start.P = start.P,
                                      start.Q = start.Q,
                                      max.order = max.order,
                                      seasonal=seasonal,
                                      stepwise=stepwise,
                                      stationary = FALSE,
                                      ic = ic,
                                      allowdrift = allowdrift,
                                      allowmean = allowmean,
                                      lambda = lambda,
                                      biasadj = biasad,
                                      test = test,
                                      seasonal.test = seasonal.test,
                                      parallel = parallel,
                                      num.cores = num.cores,
                                      trace = trace)
               }))
    })
    trace_output <- trace_output[grepl("(^ ARIMA|^ Best model)", trace_output)]
    conn <- textConnection(trace_output)
    model_traces <- read.table(conn, sep=":")
    close(conn)

    # Add model traces
    ret <- ret %>% dplyr::mutate(model_traces = purrr::map(data, function(df){
      model_traces
    }))

    # Forecast
    forecast_obj <- forecast::forecast(ret$model[[1]],
                                       xreg=forecast_xreg,
                                       h=periods, level=c(80))

    forecast_df <- as_tibble(forecast_obj)

    # Extract fitted values for training data.
    ret <- ret %>% dplyr::mutate(data = purrr::map2(data, model, function(df, m){
      # m$fitted is ts class and column name is "x"
      # So in order to extract ts values, use m$fitted[, "x"]
      fitted_values = if(is.null(dim(m$fitted))){
        # when auto.arima with xreg, m$fitted has no dim.
        m$fitted
      } else {
        m$fitted[, "x"]
      }

      df %>% dplyr::mutate(forecasted_value=fitted_values)
    }))

    forecast_rows <- tibble(ds=create_ts_seq(ret$data[[1]]$ds, max, max, time_unit, start_add=1, to_add=periods),
                            forecasted_value=forecast_df[["Point Forecast"]],
                            forecasted_value_high=forecast_df[["Hi 80"]],
                            forecasted_value_low=forecast_df[["Lo 80"]])

    if (test_mode){
      ret <- ret %>% dplyr::mutate(data = purrr::map(data, function(df){
        df$is_test_data <- FALSE
        df
      }))
      forecast_rows$y <- tail(filled_aggregated_data, periods)[["y"]]
      forecast_rows$is_test_data <- TRUE 
    }

    if (is.null(value_col)) {
      value_col <- "count"
    }

    # Bind Training Data + Forecast Data
    # Revive Original column names(time_col, value_col)
    ret <- ret %>% dplyr::mutate(data = purrr::map2(data, model, function(df, model){
      df <- df %>% dplyr::bind_rows(forecast_rows)
      if (time_col != "ds") { # if time_col happens to be "ds", do not do this, since it will make the column name "ds.new"
        time_col <- avoid_conflict(colnames(df), time_col)
        colnames(df)[colnames(df) == "ds"] <- time_col
      }
      if (value_col != "y") { # if value_col happens to be "y", do not do this, since it will make the column name "y.new".
       value_col <- avoid_conflict(colnames(df), value_col)
       colnames(df)[colnames(df) == "y"] <- value_col
      }

      if (!is.null(regressor_output_cols)) {
        for (i in 1:length(regressor_output_cols)) {
          df[[paste0(regressor_output_cols[[i]], "_effect")]] <- df[[regressor_output_cols[[i]]]] * model$coef[[regressor_output_cols[[i]]]]
        }
      }

      df
    }))

    if (test_mode) {
      ret <- ret %>% dplyr::mutate(data=purrr::map(data, function(df) {
        df %>% dplyr::select(-is_test_data, is_test_data)
      }))
    }

    ret <- ret %>% dplyr::mutate(model_meta = purrr::map(model, function(m){
      # TODO: imple broom::glance
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

      df <- data.frame(AIC=m$aic, BIC=m$bic, AICC=m$aicc, as.list(forecast::arimaorder(m)), forecast::accuracy(m))

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

      df

    }))
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
    if (lubridate::is.Date(aggregated_data$ds)) {
      ts <- as.Date(ts)
    }
  }
  else { # Use seq.Date for unit of day or larger. Using seq.POSIXct for month does not always give first day of month.
    ts <- seq.Date(as.Date(start_func(ds) + start_add), as.Date(to_func(ds) + to_add), by=time_unit_for_seq)
    if (!lubridate::is.Date(ds)) {
      ts <- as.POSIXct(ts)
    }
  }

  ts
 }

