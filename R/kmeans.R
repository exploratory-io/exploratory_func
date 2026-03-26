# internal function to iterate number of centers (k) from 1 to max_centers for elbow method to find optimal k.
iterate_kmeans <- function(df, max_centers = 10,
                           iter.max = 10,
                           nstart = 1,
                           algorithm = "Hartigan-Wong",
                           trace = FALSE,
                           normalize_data = TRUE,
                           seed = NULL
                           ) {
  # Limit the numbers of centers to search up to nrow(df) - 1.
  # Otherwise we will get "Centers should be less than rows" error.
  n_centers <- seq(min(max_centers, nrow(df) - 1))
  ret <- data.frame(center = n_centers)
  ret <- ret %>% dplyr::mutate(model = purrr::map(center, function(x) {
    tryCatch({
      model_df <- df %>% build_kmeans.cols(everything(),
                                           centers=x,
                                           iter.max = iter.max,
                                           nstart = nstart,
                                           algorithm = algorithm,
                                           trace = trace,
                                           normalize_data = normalize_data,
                                           seed=seed,
                                           keep.source=FALSE,
                                           augment=FALSE,
                                           na.rm = FALSE) # NA filtering is already done. Skip it to save time.
      model_df$model[[1]]
    }, error = function(e) {
      stop(paste0(e$message, " (while building k-means model with centers=", x, ")"),
           call. = FALSE)
    })
  }))
  ret %>% rowwise(center) %>% glance_rowwise(model)
}

