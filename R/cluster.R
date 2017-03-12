#' Output cluster labels based on vectors
#' @param ...  numeric vectors whose legthes are the same.
#' @param type  Type of cluster. Currently, only kmeans is supported.
#' @param n_cluster  Number of cluster.
#' @export
cluster <- function(..., type = "kmeans", n_cluster = 3) {
  data <- list(...) %>% as.data.frame() %>% as.matrix()
  na_removed <- data %>% na.omit()
  if(nrow(na_removed) == 0) {
    stop("not enough valid data to perform clustering")
  } else if (nrow(na_removed) <= n_cluster) {
    stop("n_cluster must be smaller than the number of valid data")
  }
  model <- kmeans(na_removed, centers = n_cluster)
  cluster <- if (ncol(data) == 1) {
    # in this case, cluster id should be assigned based on
    # how large the centers are

    # sort by centers
    sorted <- sort(model$centers[,1], index.return = TRUE)
    # create mapping
    map <- rep(NA_real_, n_cluster)
    map[sorted$ix] <- seq(n_cluster)
    map[model$cluster]
  } else {
    # in this case, cluster id should be assigned based on
    # the size of clusters (the largest cluster is 1)

    # sort by size
    sorted <- sort(model$size, index.return = TRUE, decreasing = TRUE)
    # create mapping
    map <- rep(NA_real_, n_cluster)
    map[sorted$ix] <- seq(n_cluster)
    map[model$cluster]
  }
  # fill NA
  ret <- rep(NA_real_, nrow(data))
  ret[setdiff(seq(nrow(data)), na.action(na_removed))] <- cluster
  ret <- factor(ret, ordered = TRUE)
  ret
}
