
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
