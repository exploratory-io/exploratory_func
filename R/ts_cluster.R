exp_ts_cluster <- function(df) {
  model_df <- df %>% nest_by() %>% ungroup() %>%
    mutate(model = purrr::map(data, function(df) {
      tsclust(t(as.matrix(df)), k = 4L, distance = "sdtw", centroid = "sdtw_cent")
    }))
  model_df <- model_df %>% rowwise()
  model_df
}

tidy.PartitionalTSClusters <- function(x) {
  res <- as.data.frame(x@datalist)
  res <- res %>% mutate(time=row_number())
  cluster_map <- x@cluster
  names(cluster_map) <- names(x@datalist)
  res <- res %>% pivot_longer(cols = -time)
  res <- res %>% mutate(cluster = cluster_map[name])
  res
}
