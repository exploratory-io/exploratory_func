# internal function to iterate number of centers (k) from 1 to max_centers for elbow method to find optimal k.
iterate_kmeans <- function(df, max_centers = 10,
                           iter.max = 10,
                           nstart = 1,
                           algorithm = "Hartigan-Wong",
                           trace = FALSE,
                           normalize_data = TRUE,
                           seed = NULL
                           ) {
  # Limit the numbers of centers to search up to nrow(df) - 1.
  # Otherwise we will get "Centers should be less than rows" error.
  n_centers <- seq(min(max_centers, nrow(df) - 1))
  ret <- data.frame(center = n_centers)
  ret <- ret %>% dplyr::mutate(model = purrr::map(center, function(x) {
    tryCatch({
      model_df <- df %>% build_kmeans.cols(everything(),
                                           centers=x,
                                           iter.max = iter.max,
                                           nstart = nstart,
                                           algorithm = algorithm,
                                           trace = trace,
                                           normalize_data = normalize_data,
                                           seed=seed,
                                           keep.source=FALSE,
                                           augment=FALSE,
                                           na.rm = FALSE) # NA filtering is already done. Skip it to save time.
      model_df$model[[1]]
    }, error = function(e) {
      stop(paste0(e$message, " (while building k-means model with centers=", x, ")"),
           call. = FALSE)
    })
  }))
  ret %>% rowwise(center) %>% glance_rowwise(model)
}

# Pick the row index used for the O(n^2) silhouette dist/computation (#36126).
# Returns the full index when sample_size is unset / non-positive / not smaller than n;
# otherwise a sorted random subsample of size sample_size. The RNG seed is set once at the
# top of exp_kmeans(), so the draw is reproducible across runs.
silhouette_sample_index <- function(n, sample_size = NULL) {
  size <- suppressWarnings(as.numeric(sample_size))
  if (length(size) != 1 || is.na(size) || size < 1 || n <= size) {
    return(seq_len(n))
  }
  sort(sample.int(n, floor(size)))
}

# internal function to iterate number of centers (k) from 2 to max_centers for silhouette method to find optimal k.
# When `mat` is supplied it must already be the numeric (and, if normalize_data, scaled)
# matrix that `dist` was computed from; in that case df/normalize_data are ignored for the
# matrix build so the kmeans input and the silhouette dist stay consistent (#36126).
iterate_silhouette <- function(df, max_centers = 10,
                               iter.max = 10,
                               nstart = 1,
                               algorithm = "Hartigan-Wong",
                               trace = FALSE,
                               normalize_data = TRUE,
                               seed = NULL,
                               dist = NULL,
                               mat = NULL) {
  if (is.null(mat)) {
    mat <- as_numeric_matrix_(df, columns = colnames(df))
    if (normalize_data) {
      mat <- scale(mat)
      mat[is.nan(mat)] <- 0
    }
  }
  # Cap k by the number of distinct data points, not just the row count:
  # stats::kmeans() errors with "more cluster centers than distinct data points"
  # when centers exceed distinct points (same constraint build_kmeans.cols() guards against).
  upper <- min(max_centers, nrow(unique(mat)) - 1)
  if (upper < 2) {
    return(data.frame(center = integer(0), avg_silhouette = numeric(0),
                      min_silhouette = numeric(0), pct_negative = numeric(0)))
  }
  d <- if (is.null(dist)) stats::dist(mat) else dist
  purrr::map_dfr(seq(2, upper), function(k) {
    if (!is.null(seed)) {
      set.seed(seed)
    }
    km <- tryCatch({
      stats::kmeans(mat, centers = k, iter.max = iter.max,
                    nstart = nstart, algorithm = algorithm, trace = trace)
    }, error = function(e) {
      # Surface a clear error that names the offending k, mirroring iterate_kmeans().
      stop(paste0(e$message, " (while computing silhouette with centers=", k, ")"),
           call. = FALSE)
    })
    sil <- cluster::silhouette(km$cluster, d)
    widths <- sil[, "sil_width"]
    data.frame(
      center = k,
      avg_silhouette = mean(widths, na.rm = TRUE),
      min_silhouette = min(widths, na.rm = TRUE),
      pct_negative = mean(widths < 0, na.rm = TRUE)
    )
  })
}

# Compute per-row silhouette widths for an already-built clustering.
#
# cluster_ids: integer cluster assignment vector (length n) for the FULL data.
#              For K-Means this is x$kmeans$cluster (integer 1..k). The result is always
#              length n and positionally aligned to it (prcomp.R binds it onto x$df).
# mat:         normalized numeric matrix for the rows named by sample_idx (sample_size rows
#              when subsampling, otherwise n rows).
# d:           optional precomputed stats::dist(mat) to reuse. Computed when NULL.
# sample_idx:  positions (into 1..n) that mat/d correspond to. NULL means the full data
#              (mat has n rows). When a strict subset, the silhouette is computed only on
#              those rows and the scores are scattered back into the length-n result with
#              NA elsewhere -- this bounds the O(n^2) dist to sample_size rows (#36126).
#
# Returns a tibble with n rows and columns silhouette_score, nearest_cluster, cluster_width.
# Returns all-NA columns (no error) when silhouette is undefined
# (fewer than 2 clusters, or fewer than 2 distinct points).
compute_silhouette_per_row <- function(cluster_ids, mat, d = NULL, sample_idx = NULL) {
  n <- length(cluster_ids)
  na_result <- tibble::tibble(
    silhouette_score = rep(NA_real_, n),
    nearest_cluster = rep(NA_integer_, n),
    cluster_width = rep(NA_real_, n)
  )
  if (is.null(sample_idx)) {
    sample_idx <- seq_len(n)
  }
  # mat/d already correspond to sample_idx rows; the cluster ids must be subset to match.
  ids <- as.integer(cluster_ids[sample_idx])
  if (length(unique(ids)) < 2 || nrow(unique(mat)) < 2) {
    return(na_result)
  }
  if (is.null(d)) {
    d <- stats::dist(mat)
  }
  sil <- cluster::silhouette(ids, d)
  if (!inherits(sil, "silhouette")) {
    # silhouette() can return NA (not a matrix) for degenerate input.
    return(na_result)
  }
  widths <- as.numeric(sil[, "sil_width"])
  clusters <- as.integer(sil[, "cluster"])
  clus_avg <- tapply(widths, clusters, mean, na.rm = TRUE)
  # Scatter the per-sampled-row values back to full length (NA for non-sampled rows).
  # silhouette() preserves the observation order of `ids`, so widths[i] -> sample_idx[i].
  na_result$silhouette_score[sample_idx] <- widths
  na_result$nearest_cluster[sample_idx] <- as.integer(sil[, "neighbor"])
  na_result$cluster_width[sample_idx] <- as.numeric(clus_avg[as.character(clusters)])
  na_result
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
                       max_centers = 10,
                       # Bound the O(n^2) silhouette dist/computation to this many rows (#36126).
                       # K-means itself still runs on the full (or max_nrow) data; only the
                       # silhouette diagnostics are estimated from this subsample. NULL = no cap.
                       silhouette_sample_size = 5000
                       ) {
  # this evaluates select arguments like starts_with
  selected_cols <- tidyselect::vars_select(names(df), !!! rlang::quos(...))

  grouped_cols <- grouped_by(df)

  # Normalize elbow_method_mode: accept logical (legacy) or character enum.
  optimal_method <- if (is.logical(elbow_method_mode)) {
    if (isTRUE(elbow_method_mode)) "elbow" else "none"
  } else {
    match.arg(as.character(elbow_method_mode), c("none", "silhouette", "elbow"))
  }

  # Set seed just once.
  if(!is.null(seed)) { # Set seed before starting to call sample_n.
    set.seed(seed)
  }

  # list and difftime etc. causes error in tidy_rowwise(model, type="biplot").
  # For now, we are removing them upfront.
  df <- df %>% dplyr::select(-where(is.list),
                             -where(lubridate::is.difftime),
                             -where(lubridate::is.duration),
                             -where(lubridate::is.interval),
                             -where(lubridate::is.period))

  sampled_nrow <- NULL
  if (!is.null(max_nrow) && nrow(df) > max_nrow) {
    # Record that sampling happened.
    sampled_nrow <- max_nrow
    df <- df %>% sample_rows(max_nrow)
  }

  # As the name suggests, this preprocessing function was originally designed to be done
  # before sampling, but we found that for this k-means function, that makes the
  # process as a whole slower in the cases we tried. So, we are doing this after sampling.
  nrow_before_filter <- nrow(df)
  filtered_df <- preprocess_factanal_data_before_sample(df, selected_cols)
  excluded_nrow <- nrow_before_filter - nrow(filtered_df)
  selected_cols <- attr(filtered_df, 'predictors') # predictors are updated (removed) in preprocess_factanal_data_before_sample. Sync with it.
  df <- filtered_df

  # Always compute the normal (elbow_method_mode = FALSE) results
  kmeans_model_df <- df %>% build_kmeans.cols(!!!rlang::syms(selected_cols),
                                              centers = centers,
                                              iter.max = iter.max,
                                              nstart = nstart,
                                              algorithm = algorithm,
                                              trace = trace,
                                              normalize_data = normalize_data,
                                              keep.source = FALSE,
                                              augment = FALSE,
                                              seed = NULL, # Seed is already done. Skip it.
                                              na.rm = FALSE) # NA filtering is already done. Skip it to save time. 

  # This is about how UI-side is done, but it can handle single column case, only if it is single column from the beginnig.
  # Check that and pass that info to do_prcomp() as allow_single_column.
  allow_single_column <- length(selected_cols) == 1
  ret <- do_prcomp(df, normalize_data = normalize_data, allow_single_column = allow_single_column, seed = NULL,
                   na.rm = FALSE, # Skip NA filtering since it is already done.
                   !!!rlang::syms(selected_cols))
  ret <- dplyr::ungroup(ret) # ungroup once so that the following mutate with purrr::map2 works.

  # Build the normalized clustering matrix ONCE; the single dist() is the only
  # O(n^2) cost and is shared with iterate_silhouette() (decision (a), #36106).
  # Only build the shared dist for the ungrouped (UI) case; grouped models build
  # their own per-group matrix below.
  # The dist()/silhouette work is bounded to silhouette_sample_size rows (#36126):
  # scale over the full data (cheap, O(n*p)) so the sampled rows' coordinates stay
  # consistent with build_kmeans.cols' full-data scaling, THEN subset before dist().
  is_single_model <- length(ret$model) == 1
  shared_sil_mat <- NULL
  shared_sil_dist <- NULL
  sil_sample_idx <- NULL
  if (is_single_model) {
    kmeans_df_shared <- df %>% dplyr::select(!!!rlang::syms(selected_cols))
    shared_sil_mat <- as_numeric_matrix_(kmeans_df_shared, columns = colnames(kmeans_df_shared))
    if (normalize_data) {
      shared_sil_mat <- scale(shared_sil_mat)
      shared_sil_mat[is.nan(shared_sil_mat)] <- 0
    }
    sil_sample_idx <- silhouette_sample_index(nrow(shared_sil_mat), silhouette_sample_size)
    shared_sil_mat <- shared_sil_mat[sil_sample_idx, , drop = FALSE]
    shared_sil_dist <- stats::dist(shared_sil_mat)
  }

  # Compute elbow or silhouette results depending on optimal_method.
  elbow_result <- NULL
  silhouette_result <- NULL
  if (optimal_method == "elbow") {
    kmeans_df <- df %>% dplyr::select(!!!rlang::syms(selected_cols))
    elbow_result <- iterate_kmeans(kmeans_df,
                                   max_centers = max_centers,
                                   iter.max = iter.max,
                                   nstart = nstart,
                                   algorithm = algorithm,
                                   trace = trace,
                                   normalize_data = normalize_data,
                                   seed = NULL) # Seed is already set once at the top of exp_kmeans(). Skip it.
  } else if (optimal_method == "silhouette") {
    kmeans_df <- df %>% dplyr::select(!!!rlang::syms(selected_cols))
    silhouette_result <- iterate_silhouette(kmeans_df,
                                            max_centers = max_centers,
                                            iter.max = iter.max,
                                            nstart = nstart,
                                            algorithm = algorithm,
                                            trace = trace,
                                            normalize_data = normalize_data,
                                            seed = NULL, # Seed is already set once at the top of exp_kmeans(). Skip it.
                                            dist = shared_sil_dist, # Reuse the single (subsampled) dist when available.
                                            mat = shared_sil_mat) # Use the SAME subsampled scaled matrix so kmeans input and dist match (#36126).
  }

  ret <- ret %>% dplyr::mutate(model = purrr::imap(model, function(x, idx) {
    tryCatch({
      y <- kmeans_model_df$model[[idx]]
      x$kmeans <- y # Might need to be more careful on guaranteeing x and y are from same group, but we are not supporting group_by on UI at this point.
      # Always attach per-row silhouette for the chosen clustering (#36106).
      # The dist/silhouette is bounded to silhouette_sample_size rows; non-sampled rows
      # get NA scores (compute_silhouette_per_row scatters back to full length, #36126).
      if (!is.null(shared_sil_mat)) {
        x$silhouette <- compute_silhouette_per_row(y$cluster, shared_sil_mat, shared_sil_dist,
                                                   sample_idx = sil_sample_idx)
      } else {
        # Grouped case: build the matrix from this group's own data, then subsample.
        gmat <- as_numeric_matrix_(x$df[, selected_cols, drop = FALSE], columns = selected_cols)
        if (normalize_data) {
          gmat <- scale(gmat)
          gmat[is.nan(gmat)] <- 0
        }
        g_sample_idx <- silhouette_sample_index(nrow(gmat), silhouette_sample_size)
        x$silhouette <- compute_silhouette_per_row(y$cluster, gmat[g_sample_idx, , drop = FALSE],
                                                   sample_idx = g_sample_idx)
      }
      x$sampled_nrow <- sampled_nrow
      x$excluded_nrow <- excluded_nrow
      if (!is.null(elbow_result)) {
        x$elbow_result <- elbow_result
      }
      if (!is.null(silhouette_result)) {
        x$silhouette_result <- silhouette_result
      }
      x
    }, error = function(e) {
      stop(paste0(e$message, " (while merging PCA and k-means models at index ", idx, ")"),
           call. = FALSE)
    })
  }))

  # Rowwise grouping has to be redone with original grouped_cols, so that summarize(tidy(model)) later can add back the group column.
  if (length(grouped_cols) > 0) {
    ret <- ret %>% dplyr::rowwise(grouped_cols)
  } else {
    ret <- ret %>% dplyr::rowwise()
  }
  ret
}
