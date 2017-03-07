
#' NSE version of do_prophet_
#' @export
do_prophet <- function(df, time, value = NULL, ...){
  time_col <- col_name(substitute(time))
  value_col <- col_name(substitute(value))
  do_prophet_(df, time_col, value_col, ...)
}

#' Detect anomaly data
#' @param df - Data frame
#' @param time_col - Column that has time data
#' @param value_col - Column that has value data
#' @param fun.aggregate - Function to aggregate values.
#' @param ... - extra values to be passed to prophet::prophet. listed below.
#' @param n.changepoints
#' @param changepoint.prior.scale
#' @param changepoints
#' @param holidays.prior.scale
#' @param seasonality.prior.scale
#' @param interval.width
#' @param mcmc.samples
#' @param freq
#' @export
do_prophet_ <- function(df, time_col, value_col = NULL, days, fun.aggregate = sum, ...){

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
    if(!is.null(grouped_col)){
      # drop grouping columns
      df <- df[, !colnames(df) %in% grouped_col]
    }

    aggregated_data <- if (!is.null(value_col)){
      data.frame(
        time = lubridate::round_date(df[[time_col]], unit = "day"),
        value = df[[value_col]]
      ) %>%
        dplyr::group_by(time) %>%
        dplyr::summarise(y = fun.aggregate(value))
    } else {
      data.frame(
        time = lubridate::round_date(df[[time_col]], unit = "day")
      ) %>%
        dplyr::group_by(time) %>%
        dplyr::summarise(y = n())
    }

    colnames(aggregated_data) <- c("ds", "y")

    # time column should be Date. TODO: really??
    aggregated_data[["ds"]] <- as.Date(aggregated_data[["ds"]])
    # TODO: do prophet
    m <- prophet::prophet(aggregated_data, ...)
    future <- prophet::make_future_dataframe(m, periods = days) #includes past dates
    forecast <- stats::predict(m, future)
    ret <- forecast %>% dplyr::left_join(aggregated_data, by = c("ds" = "ds"))
    colnames(ret)[colnames(ret) == "ds"] <- avoid_conflict(colnames(ret), time_col)
    if (is.null(value_col)) {
      value_col <- "count"
    }
    colnames(ret)[colnames(ret) == "y"] <- avoid_conflict(colnames(ret), value_col)
    ret
  }

  # Calculation is executed in each group.
  # Storing the result in this name_col and
  # unnesting the result.
  # name_col is not conflicting with grouping columns
  # thanks to avoid_conflict that is used before,
  # this doesn't overwrite grouping columns.
  tmp_col <- avoid_conflict(colnames(df), "tmp_col")
  test <- (df %>%  dplyr::do_(.dots=setNames(list(~do_prophet_each(.)), tmp_col)))
  test %>%  tidyr::unnest_(tmp_col)
}
