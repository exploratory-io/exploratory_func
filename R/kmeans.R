#' analytics function for K-means view
#' @export
exp_kmeans <- function(df, ...,
                       centers=3, # build_kmeans.cols arguments.
                       iter.max = 10,
                       nstart = 1,
                       algorithm = "Hartigan-Wong",
                       trace = FALSE,
                       seed=0) {

  do_prcomp(df, ...,
            centers=centers, # build_kmeans.cols arguments.
            iter.max = iter.max,
            nstart = nstart,
            algorithm = algorithm,
            trace = trace,
            seed=seed)
}
