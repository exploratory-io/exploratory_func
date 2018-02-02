
#' tidy after generating survfit
#' @export
exp_survival <- function(df, time, status, start_time = NULL, end_time = NULL, time_unit = "day", ...){
  validate_empty_data(df)

  grouped_col <- grouped_by(df)

  # substitute is needed because time can be
  # NSE column name and it throws an evaluation error
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
    fml <- as.formula(paste0("survival::Surv(ceiling(as.numeric(`", end_time_col, "`-`", start_time_col, "`, units = \"days\")/", time_unit_days_str, "), `", substitute(status), "`) ~ 1"))
  }
  else {
    # need to compose formula with non-standard evaluation.
    # simply using time and status in formula here results in a formula that literally looks at
    # "time" columun and "status" column, which is not what we want.
    fml <- as.formula(paste0("survival::Surv(`", substitute(time), "`,`", substitute(status), "`) ~ 1"))
  }

  each_func <- function(df) {
    ret <- survival::survfit(fml, data = df)
    ret
  }
  do_on_each_group(df, each_func, name = "model", with_unnest = FALSE)
}
