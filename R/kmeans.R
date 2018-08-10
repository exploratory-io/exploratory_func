
iterate_kmeans <- function(df, max_centers = 10,
                           iter.max = 10,
                           nstart = 1,
                           algorithm = "Hartigan-Wong",
                           trace = FALSE,
                           seed=0,
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
                       seed=0,
                       elbow_method_mode=FALSE,
                       max_centers = 10
                       ) {

  ret <- do_prcomp(df, ...)

  if (!elbow_method_mode) {
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
  }
  else {
    ret <- ret %>% dplyr::mutate(model = purrr::map(model, function(x) {
      kmeans_df <- as.data.frame(x$x)
      ret <- iterate_kmeans(kmeans_df,
                            max_centers = max_centers,
                            iter.max = iter.max,
                            nstart = nstart,
                            algorithm = algorithm,
                            trace = trace,
                            seed=seed)
      ret
    }))
  }
  ret
}
