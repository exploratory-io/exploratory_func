
#' Function for Survival Analysis view.
#' It does survfit and survdiff.
#' @export
exp_survival <- function(df, time, status, start_time = NULL, end_time = NULL, time_unit = "day", cohort = NULL, cohort_func = NULL, ...){
  validate_empty_data(df)

  grouped_col <- grouped_by(df)

  if (!is.null(substitute(cohort))) {
    cohort_col <- col_name(substitute(cohort))
    if (!is.null(cohort_func)) {
      df[[cohort_col]] <- extract_from_date(df[[cohort_col]], type=cohort_func)
    }
  }
  else {
    # no cohort column is set. Just draw a single survival curve.
    cohort_col <- "1"
  }

  # substitute is needed because time can be
  # NSE (non-standard evaluation) column name and it throws an evaluation error
  # without it
  if (is.null(substitute(time))) {
    start_time_col <- col_name(substitute(start_time))
    df[[start_time_col]] <- as.Date(df[[start_time_col]]) # convert to Date in case it is POSIXct.
    if (!is.null(substitute(end_time))) { # if end_time exists, fill NA with today()
      end_time_col <- col_name(substitute(end_time))
      df[[end_time_col]] <- as.Date(df[[end_time_col]]) # convert to Date in case it is POSIXct.
      df[[end_time_col]] <- impute_na(df[[end_time_col]] ,type = "value", val=lubridate::today())
    }
    else { # if end_time does not exist, create .end_time column with value of today()
      end_time_col <- avoid_conflict(colnames(df), ".end_time")
      df[[end_time_col]] <- lubridate::today()
    }

    # as.numeric() does not support all units.
    # also support of units are different between Date and POSIXct.
    # let's do it ourselves.
    time_unit_days_str = switch(time_unit,
                                day = "1",
                                week = "7",
                                month = "(365.25/12)",
                                quarter = "(365.25/4)",
                                year = "365.25",
                                "1")
    # we are ceiling survival time to make it integer in the specified time unit.
    # this is to make resulting survival curve to have integer data point in the specified time unit.
    fml <- as.formula(paste0("survival::Surv(ceiling(as.numeric(`", end_time_col, "`-`", start_time_col, "`, units = \"days\")/", time_unit_days_str, "), `", substitute(status), "`) ~ `", cohort_col, "`"))
  }
  else {
    # need to compose formula with non-standard evaluation.
    # simply using time and status in formula here results in a formula that literally looks at
    # "time" columun and "status" column, which is not what we want.
    fml <- as.formula(paste0("survival::Surv(`", substitute(time), "`,`", substitute(status), "`) ~ `", cohort_col, "`"))
  }

  # calls survfit for each group.
  each_func1 <- function(df) {
    ret <- survival::survfit(fml, data = df)
    class(ret) <- c("survfit_exploratory", class(ret))
    ret
  }

  # calls survdiff (log rank test) for each group.
  each_func2 <- function(df) {
    ret <- NULL
    if (cohort_col != "1" && length(unique(df[[cohort_col]])) > 1) {
      tryCatch({
        ret <- survival::survdiff(fml, data = df)
        class(ret) <- c("survdiff_exploratory", class(ret))
      }, error = function(e){
        # error like following is possible. ignore it just for this group, rather than stopping whole thing.
        # Error in solve.default(vv, temp2) : Lapack routine dgesv: system is exactly singular
      })
    }
    ret
  }

  do_on_each_group_2(df, each_func1, each_func2)
}

#' @export
tidy.survfit_exploratory <- function(x, ...) {
  ret <- broom:::tidy.survfit(x, ...)

  # for better viz, add time=0 row for each group when it is not already there.
  add_time_zero_row_each <- function(df) {
    if (nrow(df[df$time==0,]) == 0) { # do this only when time=0 row is not already there.
      df <- rbind(data.frame(time=0, n.risk=df$n.risk[1], n.event=0, n.censor=0, estimate=1, std.error=0, conf.high=1, conf.low=1), df)
    }
    df
  }

  if ("strata" %in% colnames(ret)) {
    nested <- ret %>% group_by(strata) %>% nest()
    nested <- nested %>% mutate(data=purrr::map(data,~add_time_zero_row_each(.)))
    ret <- unnest(nested)
  }
  else {
    ret <- add_time_zero_row_each(ret)
  }

  colnames(ret)[colnames(ret) == "n.risk"] <- "n_risk"
  colnames(ret)[colnames(ret) == "n.event"] <- "n_event"
  colnames(ret)[colnames(ret) == "n.censor"] <- "n_censor"
  colnames(ret)[colnames(ret) == "std.error"] <- "std_error"
  colnames(ret)[colnames(ret) == "conf.low"] <- "conf_low"
  colnames(ret)[colnames(ret) == "conf.high"] <- "conf_high"
  colnames(ret)[colnames(ret) == "strata"] <- "cohort"
  ret
}

#' @export
tidy.survdiff_exploratory <- function(x, ...) {
  ret <- broom:::tidy.survdiff(x, ...)
}

#' @export
glance.survdiff_exploratory <- function(x, ...) {
  ret <- broom:::glance.survdiff(x, ...)
  colnames(ret)[colnames(ret) == "statistic"] <- "Chi-Square"
  colnames(ret)[colnames(ret) == "df"] <- "Degree of Freedom"
  colnames(ret)[colnames(ret) == "p.value"] <- "P Value"
  ret
}
