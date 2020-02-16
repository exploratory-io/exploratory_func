# internal function to iterate number of centers (k) from 1 to max_centers for elbow method to find optimal k.
iterate_kmeans <- function(df, max_centers = 10,
                           iter.max = 10,
                           nstart = 1,
                           algorithm = "Hartigan-Wong",
                           trace = FALSE,
                           normalize_data = TRUE,
                           seed = NULL
                           ) {
  n_centers <- seq(max_centers)
  ret <- data.frame(center = n_centers)
  ret <- ret %>% dplyr::mutate(model = purrr::map(center, function(x) {
    model_df <- df %>% build_kmeans.cols(everything(),
                                         centers=x,
                                         iter.max = iter.max,
                                         nstart = nstart,
                                         algorithm = algorithm,
                                         trace = trace,
                                         normalize_data = normalize_data,
                                         seed=seed,
                                         keep.source=FALSE,
                                         augment=FALSE)
    ret <- model_df$model[[1]]
    ret
  }))
  ret %>% rowwise() %>% glance(model)
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

  # Set seed just once.
  if(!is.null(seed)) { # Set seed before starting to call sample_n.
    set.seed(seed)
  }
  sampled_nrow <- NULL
  if (!is.null(max_nrow) && nrow(df) > max_nrow) {
    # Record that sampling happened.
    sampled_nrow <- max_nrow
    df <- df %>% sample_rows(max_nrow)
  }
  if (!elbow_method_mode) {
    kmeans_model_df <- df %>% build_kmeans.cols(...,
                                                centers=centers,
                                                iter.max = iter.max,
                                                nstart = nstart,
                                                algorithm = algorithm,
                                                trace = trace,
                                                normalize_data = normalize_data,
                                                keep.source=FALSE,
                                                augment=FALSE,
                                                seed=NULL) # Seed is already done. Skip it.
  }

  if (!elbow_method_mode) {
    # This is about how UI-side is done, but it can handle single column case, only if it is single column from the beginnig.
    # Check that and pass that info to do_prcomp() as allow_single_column.
    allow_single_column <- length(rlang::quos(...)) == 1
    ret <- do_prcomp(df, normalize_data = normalize_data, allow_single_column = allow_single_column, seed = NULL, ...)
    ret <- ret %>% dplyr::mutate(model = purrr::map2(model, !!kmeans_model_df$model, function(x, y) {
      x$kmeans <- y # Might need to be more careful on guaranteeing x and y are from same group, but we are not supporting group_by on UI at this point.
      x$sampled_nrow <- sampled_nrow
      x
    }))
  }
  else {
    kmeans_df <- df %>% dplyr::select(!!!rlang::quos(...))
    ret <- iterate_kmeans(kmeans_df,
                          max_centers = max_centers,
                          iter.max = iter.max,
                          nstart = nstart,
                          algorithm = algorithm,
                          trace = trace,
                          normalize_data = normalize_data,
                          seed=NULL) # Seed is already done in do_prcomp. Skip it.
    ret <- data.frame(model = I(list(ret))) # Follow current output format for now. I() is to avoid unwanted expansion of list at creation of data frame. TODO: Revisit and support group_by.
  }
  ret %>% rowwise()
}
