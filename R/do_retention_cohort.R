#' Run retention cohort analysis
#' @param df Data frame to run bayes ab test
#' @export
do_retention_cohort <- function(df, timestamp, user_id, time_unit = "month", measure = NULL){
  # this seems to be the new way of NSE column selection evaluation
  # ref: https://github.com/tidyverse/tidyr/blob/3b0f946d507f53afb86ea625149bbee3a00c83f6/R/spread.R
  timestamp_col <- dplyr::select_var(names(df), !! rlang::enquo(timestamp))
  user_id_col <- dplyr::select_var(names(df), !! rlang::enquo(user_id))
  if (!is.null(measure)) {
    measure_col <- dplyr::select_var(names(df), !! rlang::enquo(measure))
  }

  grouped_col <- grouped_by(df)

  # this will be executed to each group
  each_func <- function(df, ...){
    ret <- df %>% rename_(.dots = list(.timestamp = timestamp_col))
    ret <- ret %>% dplyr::mutate(.timestamp =lubridate::floor_date(.timestamp, unit = time_unit))
    if (is.null(measure)) {
      ret <- ret %>% dplyr::distinct_(".timestamp", user_id_col)
    }
    ret <- ret %>% dplyr::group_by_(user_id_col) %>%
      mutate(.start_time = min(.timestamp)) %>%
      ungroup()
    ret <- ret %>% mutate(period = round(as.numeric(as.Date(.timestamp) - as.Date(.start_time))/switch(time_unit, day = 1, week = 7, month = (365.25/12), quarter = (365.25/4), year = 365.25)))
    ret <- ret %>% group_by(.start_time, period) %>%
      summarize(retained = n(), timestamp = first(.timestamp)) %>%
      group_by(.start_time)
    ret <- ret %>% mutate(value = retained / first(retained) * 100) %>%
      ungroup()
    ret <- ret %>% dplyr::rename(start_time = .start_time)
    ret
  }

  ret <- do_on_each_group(df, each_func, params = substitute(list()))
  ret
}

