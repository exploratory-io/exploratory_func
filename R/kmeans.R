#' analytics function for K-means view
#' @export
exp_kmeans <- function(df, ...,
                       centers=3, # build_kmeans.cols arguments.
                       iter.max = 10,
                       nstart = 1,
                       algorithm = "Hartigan-Wong",
                       trace = FALSE,
                       seed=0) {

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
