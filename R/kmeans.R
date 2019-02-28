# internal function to iterate number of centers (k) from 1 to max_centers for elbow method to find optimal k.
iterate_kmeans <- function(df, max_centers = 10,
                           iter.max = 10,
                           nstart = 1,
                           algorithm = "Hartigan-Wong",
                           trace = FALSE,
                           seed = NULL,
                           elbow_method_mode=FALSE
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
                       n_pcs = NULL,
                       max_nrow = NULL,
                       seed = 1,
                       elbow_method_mode=FALSE,
                       max_centers = 10
                       ) {

  # Set seed just once inside do_prcomp where we call sample_n().
  ret <- do_prcomp(df, normalize_data = normalize_data, max_nrow = max_nrow, seed = seed, ...)

  if (!elbow_method_mode) {
    ret <- ret %>% dplyr::mutate(model = purrr::map(model, function(x) {
      kmeans_df <- as.data.frame(x$x)
      if (!is.null(n_pcs)) {
        kmeans_df <- kmeans_df %>% dplyr::select(1:n_pcs) # keep only n_pcs principal components
      }
      kmeans_model <- kmeans_df %>% build_kmeans.cols(everything(),
                                                   centers=centers,
                                                   iter.max = iter.max,
                                                   nstart = nstart,
                                                   algorithm = algorithm,
                                                   trace = trace,
                                                   keep.source=FALSE,
                                                   augment=FALSE,
                                                   seed=NULL) # Seed is already done in do_prcomp. Skip it.
      x$kmeans <- kmeans_model$model[[1]]
      x
    }))
  }
  else {
    ret <- ret %>% ungroup()
    ret <- ret %>% dplyr::mutate(model = purrr::map(model, function(x) {
      kmeans_df <- as.data.frame(x$x)
      ret <- iterate_kmeans(kmeans_df,
                            max_centers = max_centers,
                            iter.max = iter.max,
                            nstart = nstart,
                            algorithm = algorithm,
                            trace = trace,
                            seed=NULL) # Seed is already done in do_prcomp. Skip it.
      ret
    }))
  }
  ret %>% rowwise()
}
