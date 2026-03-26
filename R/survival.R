

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
