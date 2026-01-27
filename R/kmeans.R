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

#' analytics function for K-means view
#' @export
exp_kmeans <- function(df, ...,
                       centers=3, # build_kmeans.cols arguments.
                       iter.max = 10,
                       nstart = 1,
                       algorithm = "Hartigan-Wong",
                       trace = FALSE,
                       normalize_data = TRUE,
                       max_nrow = NULL,
                       seed = 1,
                       elbow_method_mode=FALSE,
                       max_centers = 10
                       ) {
  # this evaluates select arguments like starts_with
  selected_cols <- tidyselect::vars_select(names(df), !!! rlang::quos(...))

  grouped_cols <- grouped_by(df)

  # Set seed just once.
  if(!is.null(seed)) { # Set seed before starting to call sample_n.
    set.seed(seed)
  }

  # list and difftime etc. causes error in tidy_rowwise(model, type="biplot").
  # For now, we are removing them upfront.
  df <- df %>% dplyr::select(-where(is.list),
                             -where(lubridate::is.difftime),
                             -where(lubridate::is.duration),
                             -where(lubridate::is.interval),
                             -where(lubridate::is.period))

  sampled_nrow <- NULL
  if (!is.null(max_nrow) && nrow(df) > max_nrow) {
    # Record that sampling happened.
    sampled_nrow <- max_nrow
    df <- df %>% sample_rows(max_nrow)
  }

  # As the name suggests, this preprocessing function was originally designed to be done
  # before sampling, but we found that for this k-means function, that makes the
  # process as a whole slower in the cases we tried. So, we are doing this after sampling.
  nrow_before_filter <- nrow(df)
  filtered_df <- preprocess_factanal_data_before_sample(df, selected_cols)
  excluded_nrow <- nrow_before_filter - nrow(filtered_df)
  selected_cols <- attr(filtered_df, 'predictors') # predictors are updated (removed) in preprocess_factanal_data_before_sample. Sync with it.
  df <- filtered_df

  # Always compute the normal (elbow_method_mode = FALSE) results
  kmeans_model_df <- df %>% build_kmeans.cols(!!!rlang::syms(selected_cols),
                                              centers = centers,
                                              iter.max = iter.max,
                                              nstart = nstart,
                                              algorithm = algorithm,
                                              trace = trace,
                                              normalize_data = normalize_data,
                                              keep.source = FALSE,
                                              augment = FALSE,
                                              seed = NULL, # Seed is already done. Skip it.
                                              na.rm = FALSE) # NA filtering is already done. Skip it to save time. 

  # This is about how UI-side is done, but it can handle single column case, only if it is single column from the beginnig.
  # Check that and pass that info to do_prcomp() as allow_single_column.
  allow_single_column <- length(selected_cols) == 1
  ret <- do_prcomp(df, normalize_data = normalize_data, allow_single_column = allow_single_column, seed = NULL,
                   na.rm = FALSE, # Skip NA filtering since it is already done.
                   !!!rlang::syms(selected_cols))
  ret <- dplyr::ungroup(ret) # ungroup once so that the following mutate with purrr::map2 works.

  # If elbow_method_mode is TRUE, also compute the elbow method results and attach to model
  elbow_result <- NULL
  if (elbow_method_mode) {
    kmeans_df <- df %>% dplyr::select(!!!rlang::syms(selected_cols))
    elbow_result <- iterate_kmeans(kmeans_df,
                                   max_centers = max_centers,
                                   iter.max = iter.max,
                                   nstart = nstart,
                                   algorithm = algorithm,
                                   trace = trace,
                                   normalize_data = normalize_data,
                                   seed=NULL) # Seed is already done in do_prcomp. Skip it.
  }

  ret <- ret %>% dplyr::mutate(model = purrr::imap(model, function(x, idx) {
    tryCatch({
      y <- kmeans_model_df$model[[idx]]
      x$kmeans <- y # Might need to be more careful on guaranteeing x and y are from same group, but we are not supporting group_by on UI at this point.
      x$sampled_nrow <- sampled_nrow
      x$excluded_nrow <- excluded_nrow
      if (!is.null(elbow_result)) {
        x$elbow_result <- elbow_result
      }
      x
    }, error = function(e) {
      stop(paste0(e$message, " (while merging PCA and k-means models at index ", idx, ")"),
           call. = FALSE)
    })
  }))

  # Rowwise grouping has to be redone with original grouped_cols, so that summarize(tidy(model)) later can add back the group column.
  if (length(grouped_cols) > 0) {
    ret <- ret %>% dplyr::rowwise(grouped_cols)
  } else {
    ret <- ret %>% dplyr::rowwise()
  }
  ret
}
