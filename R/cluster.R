#' cluster based on vectors
#' @export
cluster <- function(..., type = "kmeans", n_cluster = 3) {
  data <- list(...) %>% as.data.frame() %>% as.matrix()
  na_removed <- data %>% na.omit()
  if(nrow(na_removed) == 0) {
    stop("no valid data to cluster")
  }
  model <- kmeans(na_removed, centers = n_cluster)
  cluster <- if (ncol(data) == 1) {
    # sort by centers
    sorted <- sort(model$centers[,1], index.return = TRUE)
    # create mapping
    map <- rep(NA_real_, n_cluster)
    map[sorted$ix] <- seq(n_cluster)
    map[model$cluster]
  } else {
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
