#' Run timeseries cohort analysis
#' @param df - Data frame to run bayes ab test
#' @param time - time column
#' @param value - value to aggregate
#' @param cohort - value used to define cohort. if it is time, will be aggregated by cohort_time_unit.
#' @param time_unit - time unit to aggregate
#' @param fun.aggregate - aggregate function applied to values that falls under same cohort and period.
#' @export
do_cohort <- function(df, time, value, cohort, time_unit = "month", fun.aggregate = n_distinct){
  # this seems to be the new way of NSE column selection evaluation
  # ref: https://github.com/tidyverse/tidyr/blob/3b0f946d507f53afb86ea625149bbee3a00c83f6/R/spread.R
  time_col <- dplyr::select_var(names(df), !! rlang::enquo(time))
  value_col <- dplyr::select_var(names(df), !! rlang::enquo(value))
  cohort_col <- dplyr::select_var(names(df), !! rlang::enquo(cohort))

  if (cohort_col == time_col) {
    stop("cohort column must be different from time column.")
  }
  if (cohort_col == value_col) {
    stop("cohort column must be different from measure column.")
  }
  if (value_col == time_col) {
    stop("measure column must be different from time column.")
  }

  grouped_col <- grouped_by(df)

  # this will be executed for each group
  each_func <- function(df, ...){
    # rename columns to temporary ones first and use familiar NSE dplyr functions.
    # since rename_ throws error with column name with space, we use regular R approach with colnames().
    ret <- data.frame(.time = df[[time_col]], .value = df[[value_col]], .cohort = df[[cohort_col]])
    if (class(df[[cohort_col]]) %in% c("Date", "POSIXct")) { # floor cohort if it is time.
      ret <- ret %>% dplyr::mutate(.cohort =lubridate::floor_date(.cohort, unit = time_unit))
    }
    ret <- ret %>% dplyr::mutate(.time =lubridate::floor_date(.time, unit = time_unit))
    # obtain start time for each cohort
    ret <- ret %>% dplyr::group_by(.cohort) %>%
      mutate(.start_time = min(.time)) %>%
      ungroup()
    # calculate period. (0th period represents the first floored time a row from the cohort belongs to.)
    # division and round is for "month" case where a month can vary from 28 to 31 days.
    ret <- ret %>% mutate(period = round(as.numeric(as.Date(.time) - as.Date(.start_time))/switch(time_unit, day = 1, week = 7, month = (365.25/12), quarter = (365.25/4), year = 365.25)))
    # aggregate value.
    ret <- ret %>% group_by(.cohort, period) %>%
      dplyr::summarise(.value = fun.aggregate(.value), .time = first(.time))
    # sort for the first() function used next
    ret <- ret %>% arrange(.cohort, period)
    # calculate .value_pct. intended use is for retention ratio.
    ret <- ret %>% group_by(.cohort) %>%
      mutate(.value_pct = .value / first(.value) * 100) %>%
      ungroup()

    # rename temporary column names to final column names.
    ret <- ret %>% dplyr::rename(time = .time, cohort = .cohort, value = .value, value_pct = .value_pct)
    ret
  }
  ret <- do_on_each_group(df, each_func, params = substitute(list()))
  ret
}
