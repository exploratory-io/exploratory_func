#' Run retention cohort analysis
#' @param df Data frame to run bayes ab test
#' @export
do_retention_cohort <- function(df, timestamp, user_id, measure = NULL, time_unit = "month"){
  # this seems to be the new way of NSE column selection evaluation
  # ref: https://github.com/tidyverse/tidyr/blob/3b0f946d507f53afb86ea625149bbee3a00c83f6/R/spread.R
  timestamp_col <- dplyr::select_var(names(df), !! rlang::enquo(timestamp))
  user_id_col <- dplyr::select_var(names(df), !! rlang::enquo(user_id))
  # the way with rlang::enquo above cannot handle NULL well. using old way.
  measure_col <- col_name(substitute(measure))

  grouped_col <- grouped_by(df)

  # this will be executed to each group
  each_func <- function(df, ...){
    ret <- df %>% rename_(.dots = list(.timestamp = timestamp_col))
    if (!is.null(measure_col)) { # note that accessing measure here throws error. has to be measure_col.
      ret <- ret %>% rename_(.dots = list(.measure = measure_col))
    }
    ret <- ret %>% dplyr::mutate(.timestamp =lubridate::floor_date(.timestamp, unit = time_unit))
    if (is.null(measure_col)) { # if measure is NULL, we are calculating retention. distinct user access for the time period now.
      ret <- ret %>% dplyr::distinct_(".timestamp", user_id_col)
    }
    ret <- ret %>% dplyr::group_by_(user_id_col) %>%
      mutate(.start_time = min(.timestamp)) %>%
      ungroup()
    ret <- ret %>% mutate(period = round(as.numeric(as.Date(.timestamp) - as.Date(.start_time))/switch(time_unit, day = 1, week = 7, month = (365.25/12), quarter = (365.25/4), year = 365.25)))
    if (is.null(measure_col)) {
      ret <- ret %>% group_by(.start_time, period) %>%
        summarize(retained = n(), timestamp = first(.timestamp))
      ret <- ret %>% group_by(.start_time) %>%
        mutate(retained_pct = retained / first(retained) * 100) %>%
        ungroup()
    }
    else {
      ret <- ret %>% group_by(.start_time, period) %>%
        summarize(value = sum(.measure), timestamp = first(.timestamp))
      colnames(ret)[colnames(ret) == "value"] <- avoid_conflict(colnames(ret), measure_col)
    }
    ret <- ret %>% dplyr::rename(start_time = .start_time)
    ret
  }

  ret <- do_on_each_group(df, each_func, params = substitute(list()))
  ret
}

