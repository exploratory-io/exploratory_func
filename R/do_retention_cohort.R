#' Run retention cohort analysis
#' @param df Data frame to run bayes ab test
#' @export
do_retention_cohort <- function(df, time, value, cohort, time_unit = "month", fun.aggregate = n_distinct, cohort_time_unit = "month"){
  # this seems to be the new way of NSE column selection evaluation
  # ref: https://github.com/tidyverse/tidyr/blob/3b0f946d507f53afb86ea625149bbee3a00c83f6/R/spread.R
  time_col <- dplyr::select_var(names(df), !! rlang::enquo(time))
  value_col <- dplyr::select_var(names(df), !! rlang::enquo(value))
  cohort_col <- dplyr::select_var(names(df), !! rlang::enquo(cohort))

  grouped_col <- grouped_by(df)

  # this will be executed to each group
  each_func <- function(df, ...){
    ret <- df %>% rename_(.dots = list(.time = time_col, .value = value_col, .cohort = cohort_col))
    if (class(df[[cohort_col]]) %in% c("Date", "POSIXct")) { # floor cohort if it is time.
      ret <- ret %>% dplyr::mutate(.cohort =lubridate::floor_date(.cohort, unit = cohort_time_unit))
    }
    ret <- ret %>% dplyr::mutate(.time =lubridate::floor_date(.time, unit = time_unit))
    ret <- ret %>% dplyr::group_by(.cohort) %>%
      mutate(.start_time = min(.time)) %>%
      ungroup()
    ret <- ret %>% mutate(period = round(as.numeric(as.Date(.time) - as.Date(.start_time))/switch(time_unit, day = 1, week = 7, month = (365.25/12), quarter = (365.25/4), year = 365.25)))
    ret <- ret %>% group_by(.cohort, period) %>%
      dplyr::summarise(.value = fun.aggregate(.value))
    ret <- ret %>% arrange(.cohort, period)
    ret <- ret %>% group_by(.cohort) %>%
      mutate(.value_pct = .value / first(.value) * 100) %>%
      ungroup()

    ret <- ret %>% dplyr::rename(cohort = .cohort)
    ret <- ret %>% dplyr::rename(value = .value)
    ret <- ret %>% dplyr::rename(value_pct = .value_pct)
    ret
  }
  ret <- do_on_each_group(df, each_func, params = substitute(list()))
  ret
}
