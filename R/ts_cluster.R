exp_ts_cluster <- function(df) {
  model_df <- df %>% nest_by() %>% ungroup() %>%
    mutate(model = purrr::map(data, function(df) {
      tsclust(t(as.matrix(df)), k = 4L, distance = "sdtw", centroid = "sdtw_cent")
    }))
  model_df <- model_df %>% rowwise()
  model_df
}

#' @export
tidy.PartitionalTSClusters <- function(x) {
  res <- as.data.frame(x@datalist)
  res <- res %>% mutate(time=row_number())
  cluster_map <- x@cluster
  cluster_map_names <- names(x@datalist)

  for (i in 1:(x@k)) {
    res <- res %>% mutate(!!rlang::sym(paste0("centroid",i)):=x@centroids[[i]])
    cluster_map <- c(cluster_map, i)
    cluster_map_names <- c(cluster_map_names, paste0("centroid",i))
  }

  names(cluster_map) <- cluster_map_names

  res <- res %>% pivot_longer(cols = -time)
  res <- res %>% mutate(cluster = cluster_map[name])
  res
}
