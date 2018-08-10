
iterate_kmeans <- function(df, ..., max_centers = 10,
                           iter.max = 10,
                           nstart = 1,
                           algorithm = "Hartigan-Wong",
                           trace = FALSE,
                           seed=0,
                           elbow_method_mode=FALSE
                           ) {
  n_centers <- seq(max_centers)
  selected <- df %>% dplyr::select(...)
  ret <- data.frame(center = n_centers)
  ret <- ret %>% dplyr::mutate(model = purrr::map(center, function(x) {
    model_df <- selected %>% build_kmeans.cols(everything(),
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
                       seed=0,
                       elbow_method_mode=FALSE
                       ) {

  ret <- do_prcomp(df, ...)
  ret <- ret %>% dplyr::mutate(model = purrr::map(model, function(x) {
    kmeans_df <- as.data.frame(x$x)
    kmeans_model <- kmeans_df %>% build_kmeans.cols(everything(),
                                                 centers=centers,
                                                 iter.max = iter.max,
                                                 nstart = nstart,
                                                 algorithm = algorithm,
                                                 trace = trace,
                                                 seed=seed,
                                                 keep.source=FALSE,
                                                 augment=FALSE)
    x$kmeans <- kmeans_model$model[[1]] #TODO: is just calling kmeans() directly enough?
    x
  }))
  ret
}
