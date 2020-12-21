exp_ts_cluster <- function(df, time, value, category, time_unit = "day", fun.aggregate = sum, centers = 3L) {
  time_col <- tidyselect::vars_select(names(df), !! rlang::enquo(time))
  value_col <- tidyselect::vars_select(names(df), !! rlang::enquo(value))
  category_col <- tidyselect::vars_select(names(df), !! rlang::enquo(category))

  # Copied from do_prophet.
  if (time_unit == "min") {
    time_unit <- "minute"
  }
  else if (time_unit == "sec") {
    time_unit <- "second"
  }

  model_df <- df %>% nest_by() %>% ungroup() %>%
    mutate(model = purrr::map(data, function(df) {
      # Floor date. The code is copied form do_prophet.
      df[[time_col]] <- if (time_unit %in% c("day", "week", "month", "quarter", "year")) {
        # Take care of issue that happened in anomaly detection here for prophet too.
        # In this case, convert (possibly) from POSIXct to Date first.
        # If we did this without converting POSIXct to Date, floor_date works, but later at complete stage,
        # data on day-light-saving days would be skipped, since the times seq.POSIXt gives and floor_date does not match.
        # We give the time column's timezone to as.Date, so that the POSIXct to Date conversion is done
        # based on that timezone.
        lubridate::floor_date(as.Date(df[[time_col]], tz = lubridate::tz(df[[time_col]])), unit = time_unit)
      } else {
        lubridate::floor_date(df[[time_col]], unit = time_unit)
      }
      # Summarize
      grouped_df <- df %>%
        dplyr::transmute(
          time = UQ(rlang::sym(time_col)),
          value = UQ(rlang::sym(value_col)),
          category = UQ(rlang::sym(value_col))
        ) %>%
        # remove NA so that we do not pass data with NA, NaN, or 0 to prophet, which we are not very sure what would happen.
        # we saw a case where rstan crashes with the last row with 0 y value.
        # dplyr::filter(!is.na(value)) %>% # Commented out, since now we handle NAs with na.rm option of fun.aggregate. This way, extra regressor info for each period is preserved better.
        dplyr::group_by(caterory, time)

      df <- grouped_df %>% 
        dplyr::summarise(value = fun.aggregate(value))
      # pivot wider
      df <- df %>% tidyr::pivot_wider(names_from="category", values_from="value")
      # TODO: fill NAs
      df <- df %>% dplyr::select(-time)
      dtwclust::tsclust(t(as.matrix(df)), k = centers, distance = "sdtw", centroid = "sdtw_cent")
    }))
  model_df <- model_df %>% rowwise()
  model_df
}

#' @export
tidy.PartitionalTSClusters <- function(x) {
  res <- as.data.frame(x@datalist)
  res <- res %>% mutate(time=row_number())
  cluster_map <- x@cluster
  cluster_map_names <- names(x@datalist)

  for (i in 1:(x@k)) {
    res <- res %>% mutate(!!rlang::sym(paste0("centroid",i)):=x@centroids[[i]])
    cluster_map <- c(cluster_map, i)
    cluster_map_names <- c(cluster_map_names, paste0("centroid",i))
  }

  names(cluster_map) <- cluster_map_names

  res <- res %>% pivot_longer(cols = -time)
  res <- res %>% mutate(cluster = cluster_map[name])
  res
}
