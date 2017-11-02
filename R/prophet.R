# Wrapper functions around prophet.

#' NSE version of do_prophet_
#' @export
do_prophet <- function(df, time, value = NULL, ...){
  time_col <- col_name(substitute(time))
  value_col <- col_name(substitute(value))
  do_prophet_(df, time_col, value_col, ...)
}

#' Forecast time series data
#' @param df - Data frame
#' @param time_col - Column that has time data
#' @param value_col - Column that has value data
#' @param periods - Number of time periods (e.g. days. unit is determined by time_unit) to forecast.
#' @param time_unit - "day", "week", "month", "quarter", or "year"
#' @param include_history - Whether to include history data in forecast or not.
#' @param fun.aggregate - Function to aggregate values.
#' @param ... - extra values to be passed to prophet::prophet. listed below.
#' @param growth - This parameter used to specify type of Trend, which can be "linear" or "logistic",
#'        but now we determine this automatically by cap. It is here just to avoid throwing error from prophet,
#'        (about doubly specifying grouth param by our code and by "...") when old caller calls with this parameter.
#' @param cap - Achievable Maximum Capacity of the value to forecast.
#'        https://facebookincubator.github.io/prophet/docs/forecasting_growth.html
#'        It can be numeric or data frame. When numeric, the value is used as cap for both modeling and forecasting.
#'        When it is a data frame, it should be a future data frame with cap column for forecasting.
#'        When this is specified, the original data frame (df) should also have cap column for modeling.
#'        When either a numeric or a data frame is specified, growth argument for prophet becomes "logistic",
#'        as opposed to default "linear".
#' @param seasonality.prior.scale - Strength of seasonality. Default is 10.
#' @param yearly.seasonality - Whether to return yearly seasonality data.
#' @param weekly.seasonality - Whether to return weekly seasonality data.
#' @param n.changepoints - Number of potential changepoints. Default is 25.
#' @param changepoint.prior.scale - Flexibility of automatic changepoint selection. Default is 0.05.
#' @param changepoints - list of potential changepoints.
#' @param holidays.prior.scale - Strength of holiday effect. Default is 10.
#' @param holidays - Holiday definition data frame.
#' @param mcmc.samples - MCMC samples for full bayesian inference. Default is 0.
#' @param interval.width - Width of uncertainty intervals.
#' @param uncertainty.samples - Number of simulations made for calculating uncertainty intervals. Default is 1000.
#' @export
do_prophet_ <- function(df, time_col, value_col = NULL, periods, time_unit = "day", include_history = TRUE,
                        fun.aggregate = sum, cap = NULL, growth = NULL, weekly.seasonality = TRUE, yearly.seasonality = TRUE, holidays = NULL, ...){
  validate_empty_data(df)

  # we are making default for weekly/yearly.seasonality TRUE since 'auto' does not behave well.
  # it seems that there are cases that weekly.seasonality is turned off as a side-effect of yearly.seasonality turned off.
  # if that happens, since no seasonality is on, prophet forecast result becomes just a linear trend line,
  # which does not look convincing.
  # since there seems to be cases where 'auto' on yearly.seasonality triggers this situation, we are using TRUE as default.
  # we have not seen any issue on 'auto' on weekly.seasonality, but are not using it for now just to be careful.

  loadNamespace("dplyr")
  # For some reason this needs to be library() instead of loadNamespace() to avoid error.
  # Bug in prophet?
  library("prophet")

  grouped_col <- grouped_by(df)

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
    df <- df[!is.na(df[[value_col]]), ]
  }

  # remove NA data
  df <- df[!is.na(df[[time_col]]), ]

  do_prophet_each <- function(df){
    holidays_df <- NULL
    if (!is.null(holidays)) {
      holidays_df <- holidays
      for (a_grouped_col in grouped_col) {
        if (!is.null(holidays_df[[a_grouped_col]])) {
          holidays_df <- holidays_df[holidays_df[[a_grouped_col]] == df[[a_grouped_col]][[1]],]
        }
      }
    }
    cap_df <- NULL
    if (!is.null(cap) && is.data.frame(cap)) {
      cap_df <- cap
      for (a_grouped_col in grouped_col) {
        if (!is.null(cap_df[[a_grouped_col]])) {
          cap_df <- cap_df[cap_df[[a_grouped_col]] == df[[a_grouped_col]][[1]],]
        }
      }
    }

    if(!is.null(grouped_col)){
      # drop grouping columns
      df <- df[, !colnames(df) %in% grouped_col]
    }

    # note that prophet only takes columns with predetermined names like ds, y, cap, as input
    aggregated_data <- if (!is.null(value_col) && ("cap" %in% colnames(df))) {
      # preserve cap column if it is there, so that cap argument as future data frame works.
      # apply same aggregation as value to cap.
      data.frame(
        ds = lubridate::floor_date(df[[time_col]], unit = time_unit),
        value = df[[value_col]],
        cap_col = df$cap
      ) %>%
        # remove NA so that we do not pass data with NA, NaN, or 0 to prophet, which we are not very sure what would happen.
        # we saw a case where rstan crashes with the last row with 0 y value.
        dplyr::filter(!is.na(value)) %>%
        dplyr::group_by(ds) %>%
        dplyr::summarise(y = fun.aggregate(value), cap = fun.aggregate(cap_col))
    } else if (!is.null(value_col)){
      data.frame(
        ds = lubridate::floor_date(df[[time_col]], unit = time_unit),
        value = df[[value_col]]
      ) %>%
        dplyr::filter(!is.na(value)) %>% # remove NA so that we do not pass data with NA, NaN, or 0 to prophet
        dplyr::group_by(ds) %>%
        dplyr::summarise(y = fun.aggregate(value))
    } else {
      data.frame(
        ds = lubridate::floor_date(df[[time_col]], unit = time_unit)
      ) %>%
        dplyr::group_by(ds) %>%
        dplyr::summarise(y = n())
    }

    # ds column should be Date. TODO: really??
    aggregated_data[["ds"]] <- as.Date(aggregated_data[["ds"]])
    if (time_unit != "day") { # if time_unit is larger than day (the next level is week), having weekly.seasonality does not make sense.
      weekly.seasonality = FALSE
    }
    # disabling this logic for now, since setting yearly.seasonality FALSE disables weekly.seasonality too.
    # if (time_unit == "year") { # if time_unit is year (the largest unit), having yearly.seasonality does not make sense.
    #   yearly.seasonality = FALSE
    # }
    if (!is.null(cap) && is.data.frame(cap)) {
      # in this case, cap is the future data frame with cap, specified by user.
      # this is a back door to allow user to specify cap column.
      m <- prophet::prophet(aggregated_data, growth = "logistic", weekly.seasonality = weekly.seasonality, yearly.seasonality = yearly.seasonality, holidays = holidays_df, ...)
      forecast <- stats::predict(m, cap_df)
    }
    else {
      if (!is.null(cap)) { # set cap if it is there
        aggregated_data[["cap"]] <- cap
      }
      if (!is.null(cap)) { # if cap is set, use logistic. otherwise use linear.
        m <- prophet::prophet(aggregated_data, growth = "logistic", weekly.seasonality = weekly.seasonality, yearly.seasonality = yearly.seasonality, holidays = holidays_df, ...)
      }
      else {
        m <- prophet::prophet(aggregated_data, growth = "linear", weekly.seasonality = weekly.seasonality, yearly.seasonality = yearly.seasonality, holidays = holidays_df, ...)
      }
      future <- prophet::make_future_dataframe(m, periods = periods, freq = time_unit, include_history = include_history) #includes past dates
      if (!is.null(cap)) { # set cap to future table too, if it is there
        future[["cap"]] <- cap
      }
      forecast <- stats::predict(m, future)
    }
    ret <- forecast %>% dplyr::full_join(aggregated_data, by = c("ds" = "ds"))
    # drop t column, which is just scaled time, which does not seem informative.
    ret <- ret %>% dplyr::select(-t)
    # drop cap_scaled column, which is just scaled capacity, which does not seem informative.
    if ("cap_scaled" %in% colnames(ret)) {
      ret <- ret %>% dplyr::select(-cap_scaled)
    }
    # adjust order of output columns
    if ("cap.y" %in% colnames(ret)) { # cap.y exists only when cap is used.
      if ("yearly_upper" %in% colnames(ret)) { # yearly_upper/lower exists only when yearly.seasonality is TRUE
        if ("weekly_upper" %in% colnames(ret)) { # weekly_upper/lower exists only when weekly.seasonality is TRUE
          ret <- ret %>% dplyr::select(ds, y, yhat, yhat_upper, yhat_lower, trend, trend_upper, trend_lower,
                                       seasonal, seasonal_lower, seasonal_upper,
                                       yearly, yearly_lower, yearly_upper,
                                       weekly, weekly_lower, weekly_upper,
                                       cap.y, cap.x,
                                       dplyr::everything())
        }
        else {
          ret <- ret %>% dplyr::select(ds, y, yhat, yhat_upper, yhat_lower, trend, trend_upper, trend_lower,
                                       seasonal, seasonal_lower, seasonal_upper,
                                       yearly, yearly_lower, yearly_upper,
                                       cap.y, cap.x,
                                       dplyr::everything())
        }
      }
      else {
        if ("weekly_upper" %in% colnames(ret)) { # weekly_upper/lower exists only when weekly.seasonality is TRUE
          ret <- ret %>% dplyr::select(ds, y, yhat, yhat_upper, yhat_lower, trend, trend_upper, trend_lower,
                                       seasonal, seasonal_lower, seasonal_upper,
                                       weekly, weekly_lower, weekly_upper,
                                       cap.y, cap.x,
                                       dplyr::everything())
        }
        else {
          ret <- ret %>% dplyr::select(ds, y, yhat, yhat_upper, yhat_lower, trend, trend_upper, trend_lower,
                                       seasonal, seasonal_lower, seasonal_upper,
                                       cap.y, cap.x,
                                       dplyr::everything())
        }
      }
    }
    else {
      if ("yearly_upper" %in% colnames(ret)) { # yearly_upper/lower exists only when yearly.seasonality is TRUE
        if ("weekly_upper" %in% colnames(ret)) { # weekly_upper/lower exists only when weekly.seasonality is TRUE
          ret <- ret %>% dplyr::select(ds, y, yhat, yhat_upper, yhat_lower, trend, trend_upper, trend_lower,
                                       seasonal, seasonal_lower, seasonal_upper,
                                       yearly, yearly_lower, yearly_upper,
                                       weekly, weekly_lower, weekly_upper,
                                       dplyr::everything())
        }
        else {
          ret <- ret %>% dplyr::select(ds, y, yhat, yhat_upper, yhat_lower, trend, trend_upper, trend_lower,
                                       seasonal, seasonal_lower, seasonal_upper,
                                       yearly, yearly_lower, yearly_upper,
                                       dplyr::everything())
        }
      }
      else {
        if ("weekly_upper" %in% colnames(ret)) { # weekly_upper/lower exists only when weekly.seasonality is TRUE
          ret <- ret %>% dplyr::select(ds, y, yhat, yhat_upper, yhat_lower, trend, trend_upper, trend_lower,
                                       seasonal, seasonal_lower, seasonal_upper,
                                       weekly, weekly_lower, weekly_upper,
                                       dplyr::everything())
        }
        else {
          ret <- ret %>% dplyr::select(ds, y, yhat, yhat_upper, yhat_lower, trend, trend_upper, trend_lower,
                                       seasonal, seasonal_lower, seasonal_upper,
                                       dplyr::everything())
        }
      }
    }

    # revive original column names (time_col, value_col)
    colnames(ret)[colnames(ret) == "ds"] <- avoid_conflict(colnames(ret), time_col)
    if (is.null(value_col)) {
      value_col <- "count"
    }
    colnames(ret)[colnames(ret) == "y"] <- avoid_conflict(colnames(ret), value_col)

    # adjust column name style
    colnames(ret)[colnames(ret) == "yhat"] <- avoid_conflict(colnames(ret), "forecasted_value")
    colnames(ret)[colnames(ret) == "yhat_upper"] <- avoid_conflict(colnames(ret), "forecasted_value_high")
    colnames(ret)[colnames(ret) == "yhat_lower"] <- avoid_conflict(colnames(ret), "forecasted_value_low")
    colnames(ret)[colnames(ret) == "trend_upper"] <- avoid_conflict(colnames(ret), "trend_high")
    colnames(ret)[colnames(ret) == "trend_lower"] <- avoid_conflict(colnames(ret), "trend_low")
    colnames(ret)[colnames(ret) == "seasonal_upper"] <- avoid_conflict(colnames(ret), "seasonal_high")
    colnames(ret)[colnames(ret) == "seasonal_lower"] <- avoid_conflict(colnames(ret), "seasonal_low")
    colnames(ret)[colnames(ret) == "yearly_upper"] <- avoid_conflict(colnames(ret), "yearly_high")
    colnames(ret)[colnames(ret) == "yearly_lower"] <- avoid_conflict(colnames(ret), "yearly_low")
    colnames(ret)[colnames(ret) == "weekly_upper"] <- avoid_conflict(colnames(ret), "weekly_high")
    colnames(ret)[colnames(ret) == "weekly_lower"] <- avoid_conflict(colnames(ret), "weekly_low")
    colnames(ret)[colnames(ret) == "cap.x"] <- avoid_conflict(colnames(ret), "cap_forecast")
    colnames(ret)[colnames(ret) == "cap.y"] <- avoid_conflict(colnames(ret), "cap_model")
    ret
  }

  # Calculation is executed in each group.
  # Storing the result in this name_col and
  # unnesting the result.
  # name_col is not conflicting with grouping columns
  # thanks to avoid_conflict that is used before,
  # this doesn't overwrite grouping columns.
  tmp_col <- avoid_conflict(colnames(df), "tmp_col")
  test <- df %>%
    dplyr::do_(.dots=setNames(list(~do_prophet_each(.)), tmp_col)) %>%
    dplyr::ungroup()
  test %>%  unnest_with_drop_(tmp_col)
}
