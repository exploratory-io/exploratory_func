#' Time series clustering by dtwclust.
#' @export
exp_ts_cluster <- function(df, time, value, category, time_unit = "day", fun.aggregate = sum, na_fill_type = "previous", na_fill_value = 0, max_category_na_ratio = 0.5,
                           variables = NULL, funs.aggregate.variables = NULL,
                           centers = 3L, with_centroids = FALSE, distance = "sdtw", centroid = "sdtw_cent",
                           roll_mean_window = NULL,
                           normalize = "none",
                           seed = 1,
                           output = "data",
                           stop_for_no_data = TRUE,
                           elbow_method_mode=FALSE,
                           max_centers = 10) {
  if(!is.null(seed)) {
    set.seed(seed)
  }

  if (centroid == "sdtw_cent" && distance == "dtw2") {
    # This combination throws error "x and y must be of same type". TODO: Look into it.
    stop("'Soft-DTW Centroids' is not supported with 'DTW with L2 Norm'. Please select other methods.")
  }

  time_col <- tidyselect::vars_select(names(df), !! rlang::enquo(time))
  value_col <- if (missing(value)) {
    # Using empty string instead of NULL, because using NULL here would cause error from UQ(rlang::sym(value_col)),
    # which seems to be evaluated as soon as the parent function is called, which is before if-condition for it to be not NULL is evaluated.
    # (rlang::sym("") does not seem to throw error unlike rlang::sym(NULL).)
    ""
  }
  else {
    tidyselect::vars_select(names(df), !! rlang::enquo(value))
  }
  # Handle the case where NULL was specified for value argument. We handle this case this way because is.null(value) throws error when value actuall has value.
  if (length(value_col) == 0) {
    value_col = ""
  }
  category_col <- tidyselect::vars_select(names(df), !! rlang::enquo(category))

  # Copied from do_prophet.
  if (time_unit == "min") {
    time_unit <- "minute"
  }
  else if (time_unit == "sec") {
    time_unit <- "second"
  }

  # remove rows with NA time
  df <- df[!is.na(df[[time_col]]), ]

  # Compose arguments to pass to dplyr::summarise.
  summarise_args <- list() # default empty list
  if (!is.null(variables) && !is.null(funs.aggregate.variables)) {
    summarise_args <- purrr::map2(funs.aggregate.variables, variables, function(func, cname) {
      # For common functions that require na.rm=TRUE to handle NA, add it.
      if (is_na_rm_func(func)) {
        quo(UQ(func)(UQ(rlang::sym(cname)), na.rm=TRUE))
      }
      else {
        quo(UQ(func)(UQ(rlang::sym(cname))))
      }
    })

    # Set final output column names.
    if (!is.null(names(variables))) {
      names(summarise_args) <- names(variables)
    }
    else {
      names(summarise_args) <- variables
    }
  }

  model_df <- df %>% dplyr::nest_by() %>% dplyr::ungroup() %>%
    dplyr::mutate(model = purrr::map(data, function(df) {
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
      renamed_df <- if (value_col != "") {
        df %>% dplyr::rename(
            time = UQ(rlang::sym(time_col)),
            value = UQ(rlang::sym(value_col)),
            category = UQ(rlang::sym(category_col))
          )
      }
      else {
        df %>% dplyr::rename(
            time = UQ(rlang::sym(time_col)),
            category = UQ(rlang::sym(category_col))
          )
      }

      # Summarize
      grouped_df <- renamed_df %>% dplyr::group_by(category, time)
      if (value_col == "") {
        df <- grouped_df %>%
          dplyr::summarise(value = n(), !!!summarise_args)
      }
      else if (is_na_rm_func(fun.aggregate)) {
        df <- grouped_df %>%
          dplyr::summarise(value = fun.aggregate(value, na.rm=TRUE), !!!summarise_args)
      }
      else {
        df <- grouped_df %>%
          dplyr::summarise(value = fun.aggregate(value), !!!summarise_args)
      }
      df <- df %>% dplyr::ungroup()
      df_summarised <- df
      df <- df %>% dplyr::select(time, value, category)
      # Pivot wider
      df <- df %>% tidyr::pivot_wider(names_from="category", values_from="value")

      # If there is columns like "Centroid 1" in the input, which would mess up the process from here, silently delete them.
      # TODO: When we have the ability to show warning without stopping the entire process, show this.
      df <- df %>% dplyr::select(-matches("^Centroid [0-9]+$"))

      # Complete the time column.
      df <- df %>% complete_date("time", time_unit = time_unit)
      orig_n_categories <- length(colnames(df)) - 1 # -1 for time column.
      # Drop columns (represents category) that has more NAs than max_category_na_ratio, considering them to have not enough data.
      df <- df %>% dplyr::select_if(function(x){sum(is.na(x))/length(x) < max_category_na_ratio})
      if (length(colnames(df)) <= centers) {
        if (stop_for_no_data) {
          stop("EXP-ANA-2 :: [] :: There is not enough data left after removing high NA ratio data.")
        }
        # For Analytics View, keep going to show at least the diagnostic chart to show where NAs are.
        model <- list()
        # Pass original data.
        # - So that we can generate diagnostic chart about where NAs are.
        attr(model, "aggregated_data") <- df_summarised
        attr(model, "error") <- "There is not enough data left after removing high NA ratio data."
        class(model) <- "PartitionalTSClusters_exploratory"
        return(model)
      }
      # Fill NAs in time series
      df <- df %>% dplyr::mutate(across(-time, ~fill_ts_na(.x, time, type = na_fill_type, val = na_fill_value)))

      if (!is.null(roll_mean_window) && roll_mean_window > 1) {
        if (nrow(df) - (roll_mean_window - 1) < 1) {
          stop("EXP-ANA-4 :: [] :: The window size of moving average is too long for this data.")
        }
        df <- df %>% dplyr::mutate(across(-time, ~RcppRoll::roll_mean(.x, n = roll_mean_window, fill = NA, align="right")))
        df <- df %>% tail(-(roll_mean_window - 1)) # Remove NA rows created at the beginning of df.
      }

      time_values <- df$time
      df <- df %>% dplyr::select(-time)
      if (normalize != "none") {
        df_before_normalize <- df
      }
      switch (normalize,
        center_and_scale = {
          df <- df %>% dplyr::mutate(across(everything(), ~normalize(.x, center=TRUE, scale=TRUE)))
        },
        center = {
          df <- df %>% dplyr::mutate(across(everything(), ~normalize(.x, center=TRUE, scale=FALSE)))
        },
        scale = {
          df <- df %>% dplyr::mutate(across(everything(), ~normalize(.x, center=FALSE, scale=TRUE)))
        }
      )

      # A function to run tsclust to avoid repetition.
      run_tsclust <- function(k) {
        if (distance %in% c("dtw_lb", "lbk", "lbi")) { # Distance algorithms that require window.size.
          window_size <- as.integer(nrow(df) * 0.1) # Let's use 10% of length of data as the default window size.
          if (window_size < 1) { # window.size should be at least 1.
            window_size <- 1L
          }
          dtwclust::tsclust(t(as.matrix(df)), k = k, distance = distance, centroid = centroid,
                            args = dtwclust::tsclust_args(dist = list(window.size = window_size)))
        } else {
          dtwclust::tsclust(t(as.matrix(df)), k = k, distance = distance, centroid = centroid)
        }
      }

      # Always run for the specified number of centers.
      single_model <- tryCatch({
        run_tsclust(centers)
      }, error = function(e) {
        stop(paste0(e$message, " (while building time series cluster model with centers=", centers, ")"),
             call. = FALSE)
      })
      model <- list(model = single_model) # Since the original model is S4 object, we create an S3 object that wraps it.

      if (elbow_method_mode) {
        # Limit the numbers of centers to search up to nrow(df) - 1.
        # Otherwise we will get "Cannot have more clusters than series in the dataset" error.
        max_centers <- min(max_centers, ncol(df) - 1)

        # Only proceed if we can create at least 2 clusters
        if (max_centers >= 2) {
          n_centers <- 2:max_centers
          # Note: this part can be slow.
          models <- n_centers %>% purrr::map(function(n_center) {
            tryCatch({
              run_tsclust(n_center)
            }, error = function(e) {
              stop(paste0(e$message, " (while building time series cluster model with centers=", n_center, ")"),
                   call. = FALSE)
            })
          })
          model$models <- models # Add models for elbow method.
          model$n_centers <- n_centers
        } else {
          # Not enough series for elbow method, skip it
          model$models <- NULL
          model$n_centers <- NULL
        }
      }
      attr(model, "time_col") <- time_col
      attr(model, "value_col") <- value_col
      attr(model, "category_col") <- category_col
      attr(model, "orig_n_categories") <- orig_n_categories
      attr(model, "time_values") <- time_values
      if (!is.null(variables)) {
        attr(model, "variable_cols") <- variables
      }
      # Pass original data.
      # - So that the output has other variables too.
      # - So that we can generate diagnostic chart about where NAs are.
      attr(model, "aggregated_data") <- df_summarised
      if (normalize != "none") {
        attr(model, "before_normalize_data") <- df_before_normalize
      }
      class(model) <- "PartitionalTSClusters_exploratory"
      model
    }))
  model_df <- model_df %>% rowwise()
  if (output == "data") {
    model_df %>% tidy_rowwise(model, with_centroids = with_centroids)
  }
  else { # output == "model"
    model_df
  }
}

#' Extracts results from the model as a data frame.
#' The output is original long-format set of time series with Cluster column.
#' @export
tidy.PartitionalTSClusters_exploratory <- function(x, with_centroids = TRUE, type = "result", with_before_normalize_data = TRUE) {
  model <- x$model
  switch(type,
    result = {
      if (is.null(model)) {
        return(tibble::tibble()) # Return empty data frame to show "No Data" screen.
      }
      # Create map of time series names to clustering results
      cluster_map <- model@cluster
      cluster_map_names <- names(model@datalist)
      if (with_centroids) {
        for (i in 1:(model@k)) {
          cluster_map <- c(cluster_map, i)
          cluster_map_names <- c(cluster_map_names, paste0("Centroid ",i))
        }
      }
      names(cluster_map) <- cluster_map_names

      res <- tibble::as_tibble(model@datalist)
      res <- res %>% dplyr::mutate(time=!!attr(x,"time_values"))
      # Add centroids data
      if (with_centroids) {
        for (i in 1:(model@k)) {
          res <- res %>% dplyr::mutate(!!rlang::sym(paste0("Centroid ",i)):=model@centroids[[i]])
        }
      }
      res <- res %>% tidyr::pivot_longer(cols = -time)

      orig_df <- attr(x, "before_normalize_data")
      if (!is.null(orig_df) && with_before_normalize_data) { # If normalization was done and we want to show the result with before-normalize data.
        orig_df <- orig_df %>% dplyr::mutate(time=!!attr(x,"time_values"))
        orig_df <- orig_df %>% tidyr::pivot_longer(cols = -time)
        res <- res %>% dplyr::rename(value_normalized=value) # The value we have now in res is normalized one. Rename it, and get the one without normalization from orig_df.
        res <- res %>% dplyr::left_join(orig_df, by=c("time"="time", "name"="name"))
        res <- res %>% dplyr::relocate(value, .before=value_normalized) # Adjust column order.
      }

      if (!is.null(attr(x, "variable_cols"))) { # If there are other columns to keep in the result, join them.
        aggregated_data <- attr(x, "aggregated_data")
        aggregated_data <- aggregated_data %>% dplyr::select(-value) # Drop value column from aggregated_data since res already has it.
        res <- res %>% dplyr::left_join(aggregated_data, by=c("time"="time", "name"="category"))
      }
      res <- res %>% dplyr::mutate(Cluster = cluster_map[name])
      value_col <- attr(x, "value_col")
      if (value_col == "") {
        res <- res %>% dplyr::rename(!!rlang::sym(attr(x,"time_col")):=time,
                                     Number_of_Rows=value,
                                     !!rlang::sym(attr(x,"category_col")):=name)
        if (!is.null(orig_df) && with_before_normalize_data) { # If normalization was done and we want to show the result with before-normalize data.
          res <- res %>% dplyr::rename(Number_of_Rows_normalized=value_normalized)
        }
      }
      else {
        res <- res %>% dplyr::rename(!!rlang::sym(attr(x,"time_col")):=time,
                                     !!rlang::sym(value_col):=value,
                                     !!rlang::sym(attr(x,"category_col")):=name)
        if (!is.null(orig_df) && with_before_normalize_data) { # If normalization was done and we want to show the result with before-normalize data.
          res <- res %>% dplyr::rename(!!rlang::sym(paste0(value_col,"_normalized")):=value_normalized)
        }
      }
    },
    aggregated = { # Return raw aggretated time series data before filling NAs and feeding to the clustering algorithm. This is for Data Validation tab.
      res <- attr(x, "aggregated_data")
    },
    summary = {
      if (is.null(model)) {
        return(tibble::tibble()) # Return empty data frame to show "No Data" screen.
      }
      res <- model@clusinfo
      res <- res %>% dplyr::mutate(cluster = row_number()) %>% select(cluster, everything())
      orig_n_categories <- attr(x, "orig_n_categories")
      n_categories <- sum(res$size, na.rm=TRUE)
      if (!is.null(orig_n_categories) && orig_n_categories > n_categories) { # Add a row for categories removed at preprocessing.
        tmp_df <- tibble::tibble(size = orig_n_categories - n_categories, Note = "Removed due to high NA ratio.")
        res <- res %>% dplyr::bind_rows(tmp_df)
      }
    },
    elbow_method = {
      if (is.null(model)) {
        return(tibble::tibble()) # Return empty data frame to show "No Data" screen.
      }
      res <- purrr::map(x$models, function(model) {
        df <- model@clusinfo
        df <- df %>% dplyr::summarize(av_dist=sum(size*av_dist)/sum(size))
        df <- df %>% dplyr::mutate(iter=model@iter, converged=model@converged)
        df
      })
      res <- tibble::tibble(n_center=x$n_centers, data=res)
      res <- res %>% tidyr::unnest(data)
    }
  )
  res
}
