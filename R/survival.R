
#' Function for Survival Analysis view.
#' It does survfit and survdiff.
#' @param end_time_fill - "max", "today", or actual value or string expresion of Date.
#' @export
exp_survival <- function(df, time, status, start_time, end_time, end_time_fill = "max", time_unit = "day", cohort = NULL, cohort_func = NULL, ...){
  validate_empty_data(df)

  grouped_col <- grouped_by(df)
  status_col <- tidyselect::vars_select(names(df), !! rlang::enquo(status))
  orig_cohort_col <- tidyselect::vars_select(names(df), !! rlang::enquo(cohort))
  start_time_col <- tidyselect::vars_select(names(df), !! rlang::enquo(start_time))
  end_time_col <- tidyselect::vars_select(names(df), !! rlang::enquo(end_time))
  time_col <- tidyselect::vars_select(names(df), !! rlang::enquo(time))

  orig_levels <- NULL

  if (length(orig_cohort_col) > 0) { # TODO: Better way to check if column is specified or not?
    # rename cohort column to workaround the case where the column name has space in it.
    # https://github.com/therneau/survival/issues/41
    colnames(df)[colnames(df) == orig_cohort_col] <- ".cohort" #TODO avoid conflict, and adjust cohort output value from survfit.
    cohort_col <- ".cohort"

    if (!is.null(cohort_func)) {
      df[[cohort_col]] <- extract_from_date(df[[cohort_col]], type=cohort_func)
    }
    quoted_cohort_col <- paste0("`", cohort_col, "`")

    # keep original factor levels to set them back later.
    if (is.factor(df[[cohort_col]])) {
      orig_levels <- levels(df[[cohort_col]])
    }
    else if (is.logical(df[[cohort_col]])) {
      orig_levels <- c("TRUE","FALSE")
    }
  }
  else {
    # no cohort column is set. Just draw a single survival curve.
    cohort_col <- "1"
    quoted_cohort_col <- "1" # no need to quote column name since it is not column.
  }

  if (length(time_col) == 0) {
    df[[start_time_col]] <- as.Date(df[[start_time_col]]) # convert to Date in case it is POSIXct.
    if (length(end_time_col) > 0) { # if end_time exists, fill NA with the way specified by end_time_fill.
      df[[end_time_col]] <- as.Date(df[[end_time_col]]) # convert to Date in case it is POSIXct.
      # set value to fill NAs of end time
      if (is.character(end_time_fill) && end_time_fill == "max") { # type check before comparison is necessary to avoid error.
        end_time_fill_val <- max(df[[start_time_col]], df[[end_time_col]], na.rm = TRUE)
      }
      else if (is.character(end_time_fill) && end_time_fill == "today") {
        end_time_fill_val <- lubridate::today()
      }
      else {
        end_time_fill_val <- as.Date(end_time_fill)
      }
      df[[end_time_col]] <- impute_na(df[[end_time_col]] ,type = "value", val=end_time_fill_val)
    }
    else { # if end_time does not exist, create .end_time column with value of today()
      end_time_col <- avoid_conflict(colnames(df), ".end_time")
      df[[end_time_col]] <- lubridate::today()
    }

    # as.numeric() does not support all units.
    # also support of units are different between Date and POSIXct.
    # let's do it ourselves.
    time_unit_days = switch(time_unit,
                            day = 1,
                            week = 7,
                            month = 365.25/12,
                            quarter = 365.25/4,
                            year = 365.25,
                            1)

    time_col <- avoid_conflict(colnames(df), ".time")

    # we are ceiling survival time to make it integer in the specified time unit.
    # this is to make resulting survival curve to have integer data point in the specified time unit.
    df <- df %>% dplyr::mutate(!!rlang::sym(time_col) := ceiling(as.numeric(!!rlang::sym(end_time_col) - !!rlang::sym(start_time_col), units = "days") / time_unit_days))
  }

  fml <- as.formula(paste0("survival::Surv(`", time_col, "`, `", status_col, "`) ~ ", quoted_cohort_col))

  # By default, use mean of observations with event.
  # median gave a point where survival rate was still predicted 100% in one of our test case.
  default_survival_time <- mean((df %>% dplyr::filter(!!rlang::sym(status_col)))[[time_col]], na.rm=TRUE)
  # Pick maximum of existing values equal or less than the actual mean.
  default_survival_time <- max((df %>% dplyr::filter(!!rlang::sym(time_col) <= !!default_survival_time))[[time_col]], na.rm=TRUE)

  # calls survfit for each group.
  each_func1 <- function(df) {
    ret <- survival::survfit(fml, data = df)
    # strata is ignored by survfit if all rows have same value.
    # In such case, we need to put it back later. Record the value in the model.
    if (cohort_col != "1" && length(unique(df[[cohort_col]])) == 1) {
      ret$single_strata_value = df[[cohort_col]][[1]]
    }
    ret$orig_levels <- orig_levels
    attr(ret, "default_survival_time") <- default_survival_time
    class(ret) <- c("survfit_exploratory", class(ret))
    ret
  }

  # calls survdiff (log rank test) for each group.
  each_func2 <- function(df) {
    ret <- NULL
    if (cohort_col != "1") {
      if (length(unique(df[[cohort_col]][!is.na(df[[cohort_col]])])) > 1) {
        tryCatch({
          ret <- survival::survdiff(fml, data = df)
          class(ret) <- c("survdiff_exploratory", class(ret))
        }, error = function(e){
          # error like following is possible. ignore it just for this group, rather than stopping whole thing.
          # Error in solve.default(vv, temp2) : Lapack routine dgesv: system is exactly singular
          ret_ <- list(error=e)
          class(ret_) <- c("survdiff_exploratory", class(ret))
          ret <<- ret_
        })
      }
      else {
        ret <- list(error=simpleError("Test was not performed because there is only one group."))
        class(ret) <- c("survdiff_exploratory", class(ret))
      }
    } else {
      # If there is no cohort, return the number of data except NAs (n) 
      # and number of TRUEs (nevent).
      ret <- list(n = sum(!is.na(df[[status_col]]), na.rm = TRUE), nevent = sum(df[[status_col]], na.rm = TRUE))
      class(ret) <- c("survdiff_exploratory", class(ret))
    }
    ret
  }

  do_on_each_group_2(df, each_func1, each_func2)
}

#' @export
tidy.survfit_exploratory <- function(x, type = "survival_curve", survival_time = NULL, ...) {
  ret <- broom:::tidy.survfit(x, ...)
  if (is.null(survival_time)) {
    survival_time <- attr(x, "default_survival_time")
  }

  # for line chart and pivot table, add time=0 row when it is not already there, and rows for other missing times for each group.
  complete_times_each <- function(df) {
    if (nrow(df[df$time==0,]) == 0) { # do this only when time=0 row is not already there.
      df <- rbind(data.frame(time=0, n.risk=df$n.risk[1], n.event=0, n.censor=0, estimate=1, std.error=0, conf.high=1, conf.low=1), df)
    }
    # Fill data for missing period, as if there was a row for 0 event and 0 censor.
    df <- df %>% tidyr::complete(time=0:max(time))
    # We will conservatively fill only the rows we added with the above tidyr::complete,
    # not the rows that were there in the first place, to preserve whatever behavior of survfit.
    df <- df %>% fill_between(n.risk, estimate, std.error, conf.high, conf.low)
    df <- df %>% fill_between(n.event, n.censor, value = 0)
    df
  }

  if (type == "survival_curve") {
    if ("strata" %in% colnames(ret)) {
      # Add missing time=0.
      nested <- ret %>% dplyr::group_by(strata) %>% tidyr::nest()
      nested <- nested %>% dplyr::mutate(data=purrr::map(data,~complete_times_each(.)))
      ret <- nested %>% tidyr::unnest(data) %>% dplyr::ungroup()
      # remove ".cohort=" part from strata values.
      ret <- ret %>% dplyr::mutate(strata = stringr::str_remove(strata,"^\\.cohort\\="))
      # Set original factor level back so that legend order is correct on the chart.
      # In case of logical, c("TRUE","FALSE") is stored in orig_level, so that we can
      # use it here either way.
      if (!is.null(x$orig_levels)) {
        ret <- ret %>%  dplyr::mutate(strata = factor(strata, levels=x$orig_levels))
      }
    }
    else if (!is.null(x$single_strata_value)) { # put back single strata value ignored by survfit.
      ret <- complete_times_each(ret)
      ret <- ret %>% dplyr::mutate(strata = as.character(x$single_strata_value))
    }
    else {
      ret <- complete_times_each(ret)
    }
  }
  else { # type == "survival_rate"
    if ("strata" %in% colnames(ret)) {
      # Add missing time=0.
      nested <- ret %>% dplyr::group_by(strata) %>% tidyr::nest()
      nested <- nested %>% dplyr::mutate(data=purrr::map(data,~complete_times_each(.)))
      ret <- nested %>% tidyr::unnest(data) %>% dplyr::ungroup()
      # First, filter out groups whose surivial curve ends too short for the survival time in question.
      ret <- ret %>% dplyr::group_by(strata) %>% dplyr::filter(max(time) >= !!survival_time)
      ret <- ret %>% dplyr::filter(time <= !!survival_time) %>% dplyr::filter(time==max(time)) %>% ungroup()
      # remove ".cohort=" part from strata values.
      ret <- ret %>% dplyr::mutate(strata = stringr::str_remove(strata,"^\\.cohort\\="))
    }
    else {
      # Add missing time=0.
      ret <- complete_times_each(ret)
      ret <- ret %>% dplyr::filter(time <= !!survival_time) %>% dplyr::filter(time==max(time))
    }
  }

  if (all(c("conf.high", "conf.low") %in% colnames(ret))) {
    ret <- ret %>% dplyr::relocate(conf.low, .before=conf.high)
  }

  colnames(ret)[colnames(ret) == "time"] <- "Time"
  colnames(ret)[colnames(ret) == "estimate"] <- "Survival Rate"
  colnames(ret)[colnames(ret) == "n.risk"] <- "Observations"
  colnames(ret)[colnames(ret) == "n.event"] <- "Events"
  colnames(ret)[colnames(ret) == "n.censor"] <- "Censored"
  colnames(ret)[colnames(ret) == "std.error"] <- "Std Error"
  colnames(ret)[colnames(ret) == "conf.low"] <- "Conf Low"
  colnames(ret)[colnames(ret) == "conf.high"] <- "Conf High"
  colnames(ret)[colnames(ret) == "strata"] <- "Cohort"
  ret
}

# Result from this function is not exposed on UI yet.
#' @export
tidy.survdiff_exploratory <- function(x, ...) {
  if (is.null(x$error)) {
    # If x doesn't contain pvalue, it means it is not a survdiff output.
    # Just simply create an empty data frame for now if that's the case.
    # This is the case when there is no cohort.
    if (is.null(x$pvalue)) {
      ret <- data.frame();
    } else {
      ret <- broom:::tidy.survdiff(x, ...)
      # TODO: rename .cohort column to original name
    }
  }
  else {
    ret <- data.frame(Note = x$error$message, stringsAsFactors = FALSE)
  }
  ret
}

#' @export
glance.survdiff_exploratory <- function(x, ...) {
  if (is.null(x$error)) {
    # If x doesn't contain pvalue, it means it is not a survdiff output.
    # Just simply create a data frame with x if that's the case.
    # This is the case when there is no cohort.
    if (is.null(x$pvalue)) {
      ret <- data.frame(x);
    } else {
      ret <- broom:::glance.survdiff(x, ...)
      ret <- ret %>% dplyr::mutate(n = !!sum(x$n, na.rm = TRUE), nevent = !!sum(x$obs, na.rm = TRUE))
      if ("df" %in% colnames(ret) && "p.value" %in% colnames(ret)) {
        ret <- ret %>% dplyr::relocate(df, .after=p.value) # Adjust order just to be consistent with other Analytics Views.
      }
    }
    colnames(ret)[colnames(ret) == "statistic"] <- "Chi-Square"
    colnames(ret)[colnames(ret) == "df"] <- "DF"
    colnames(ret)[colnames(ret) == "p.value"] <- "P Value"
    colnames(ret)[colnames(ret) == "n"] <- "Rows"
    colnames(ret)[colnames(ret) == "nevent"] <- "Rows (TRUE)"
  }
  else {
    ret <- data.frame(Note = x$error$message, stringsAsFactors = FALSE)
  }
  ret
}
