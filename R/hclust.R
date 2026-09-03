# Hierarchical clustering analytics implementation.

.hclust_safe_numeric <- function(x) {
  value <- suppressWarnings(as.numeric(x))
  if (length(value) != 1 || is.na(value) || !is.finite(value)) return(NULL)
  value
}

.hclust_empty <- function(type) {
  switch(type,
    summary = tibble::tibble(
      cluster = integer(), size = integer(), pct_size = numeric(),
      avg_silhouette = numeric(), min_silhouette = numeric(), pct_negative = numeric()
    ),
    analysis_conditions = tibble::tibble(Metric = character(), Value = character()),
    dendrogram_nodes = tibble::tibble(
      node_id = integer(), node_type = character(), left_child = integer(),
      right_child = integer(), merge_height = numeric(), merge_step = integer(),
      size = integer(), leaf_start = integer(), leaf_end = integer(),
      row_id = character(), label = character(), display_order = integer(),
      metadata_json = character(), cuts_json = character()
    ),
    merge_distance = tibble::tibble(cluster = character(), merge_distance = numeric()),
    silhouette = tibble::tibble(
      center = integer(), avg_silhouette = numeric(), min_silhouette = numeric(),
      pct_negative = numeric()
    ),
    elbow = tibble::tibble(
      center = integer(), totss = numeric(), tot.withinss = numeric(),
      betweenss = numeric(), decrease_ratio = numeric()
    ),
    elbow_diff = tibble::tibble(center = integer(), decrease_ratio = numeric()),
    profile = tibble::tibble(
      variable = character(), cluster = integer(), standardized_mean = numeric(),
      effect_size = numeric(), rank = integer(), cluster_median = numeric(),
      cluster_mean = numeric(), overall_median = numeric(), overall_mean = numeric()
    ),
    gathered_data = tibble::tibble(),
    radar = tibble::tibble(),
    variable_importance = tibble::tibble(
      variable = character(), eta_squared = numeric(), test_statistic = numeric(),
      p_value = numeric()
    ),
    distribution = tibble::tibble(
      cluster = integer(), variable = character(), value = numeric(),
      standardized_value = numeric()
    ),
    map = tibble::tibble(
      Dim1 = numeric(), Dim2 = numeric(), cluster = integer(), row_type = character(),
      label = character()
    ),
    cluster_map = tibble::tibble(
      Dim1 = numeric(), Dim2 = numeric(), cluster = integer(), row_type = character(),
      label = character()
    ),
    data = tibble::tibble(),
    counts = tibble::tibble(),
    tibble::tibble()
  )
}

.hclust_node_id <- function(reference, n) {
  if (reference < 0) -reference - 1L else n + reference - 1L
}

.hclust_build_nodes <- function(hc, row_ids, labels) {
  n <- length(row_ids)
  node_n <- 2L * n - 1L
  order_zero <- as.integer(hc$order) - 1L
  display_order <- integer(n)
  display_order[order_zero + 1L] <- seq_len(n) - 1L

  left_child <- rep(NA_integer_, node_n)
  right_child <- rep(NA_integer_, node_n)
  merge_height <- numeric(node_n)
  merge_step <- rep(NA_integer_, node_n)
  size <- rep(1L, node_n)
  leaf_start <- display_order
  leaf_end <- display_order

  for (step in seq_len(n - 1L)) {
    node_id <- n + step - 1L
    children <- vapply(hc$merge[step, ], .hclust_node_id, integer(1), n = n)
    left_child[node_id + 1L] <- children[[1]]
    right_child[node_id + 1L] <- children[[2]]
    merge_height[node_id + 1L] <- as.numeric(hc$height[[step]])
    merge_step[node_id + 1L] <- step
    size[node_id + 1L] <- size[children[[1]] + 1L] + size[children[[2]] + 1L]
    leaf_start[node_id + 1L] <- min(leaf_start[children + 1L])
    leaf_end[node_id + 1L] <- max(leaf_end[children + 1L])
  }

  tibble::tibble(
    node_id = seq_len(node_n) - 1L,
    node_type = c(rep('leaf', n), rep('internal', n - 1L)),
    left_child = left_child,
    right_child = right_child,
    merge_height = merge_height,
    merge_step = merge_step,
    size = size,
    leaf_start = leaf_start,
    leaf_end = leaf_end,
    row_id = c(as.character(row_ids), rep(NA_character_, n - 1L)),
    label = c(as.character(labels), rep(NA_character_, n - 1L)),
    display_order = c(display_order, rep(NA_integer_, n - 1L))
  )
}

.hclust_cut_info <- function(hc, nodes, k) {
  n <- nrow(nodes) %/% 2L + 1L
  membership <- as.integer(stats::cutree(hc, k = k))
  groups <- sort(unique(membership))
  roots <- vapply(groups, function(group) {
    leaves <- which(membership == group) - 1L
    start <- min(nodes$display_order[leaves + 1L])
    end <- max(nodes$display_order[leaves + 1L])
    candidate <- which(
      nodes$leaf_start == start & nodes$leaf_end == end & nodes$size == length(leaves)
    )
    if (length(candidate) != 1L) {
      stop('Unable to construct a contiguous dendrogram cluster.', call. = FALSE)
    }
    nodes$node_id[[candidate]]
  }, integer(1))
  order_index <- order(nodes$leaf_start[roots + 1L], nodes$leaf_end[roots + 1L])
  roots <- roots[order_index]
  display_ids <- match(roots, roots)
  names(display_ids) <- as.character(groups[order_index])
  display_membership <- unname(display_ids[as.character(membership)])
  list(membership = membership, roots = as.integer(roots), display_membership = as.integer(display_membership))
}

.hclust_build_cut_data <- function(hc, nodes, max_k) {
  cuts <- list()
  memberships <- list()
  for (k in seq.int(2L, max_k)) {
    info <- .hclust_cut_info(hc, nodes, k)
    cuts[[as.character(k)]] <- info$roots
    memberships[[as.character(k)]] <- info$display_membership
  }
  list(cuts = cuts, memberships = memberships)
}

.hclust_membership <- function(x, k) {
  cached <- x$memberships[[as.character(k)]]
  if (!is.null(cached)) return(cached)
  .hclust_cut_info(x$hclust, x$dendrogram_nodes, k)$display_membership
}

.hclust_summary <- function(x, with_excluded_rows = FALSE) {
  ids <- x$clustering
  silhouette <- x$silhouette_values
  result <- purrr::map_dfr(sort(unique(ids)), function(cluster_id) {
    index <- which(ids == cluster_id)
    values <- silhouette[index]
    tibble::tibble(
      cluster = as.integer(cluster_id), size = length(index),
      pct_size = length(index) / x$valid_nrow,
      avg_silhouette = mean(values, na.rm = TRUE),
      min_silhouette = if (all(is.na(values))) NA_real_ else min(values, na.rm = TRUE),
      pct_negative = mean(values < 0, na.rm = TRUE)
    )
  })
  result <- result %>% dplyr::mutate(dplyr::across(
    c(avg_silhouette, min_silhouette, pct_negative),
    ~ ifelse(is.finite(.x), .x, NA_real_)
  ))
  if (with_excluded_rows && x$excluded_nrow > 0L) {
    result <- dplyr::bind_rows(result, tibble::tibble(
      cluster = NA_integer_, size = x$excluded_nrow,
      pct_size = x$excluded_nrow / x$sampled_nrow,
      avg_silhouette = NA_real_, min_silhouette = NA_real_, pct_negative = NA_real_
    ))
  }
  result
}

.hclust_analysis_conditions <- function(x) {
  sampling <- if (isTRUE(x$sampling_applied)) {
    paste0('Yes (max_nrow = ', x$max_nrow, ')')
  } else {
    'No'
  }
  tibble::tibble(
    Metric = c('Number of Variables', 'Variable Names', 'Row Count', 'Rows Removed',
               'Number of Clusters', 'Distance', 'Linkage', 'Sampling'),
    Value = c(
      as.character(length(x$selected_cols)), paste(x$selected_cols, collapse = ', '),
      as.character(x$valid_nrow), as.character(x$excluded_nrow), as.character(x$centers),
      x$distance, x$linkage, sampling
    )
  )
}

.hclust_dendrogram_nodes <- function(x) {
  if (!is.null(x$dendrogram_nodes)) return(x$dendrogram_nodes)
  .hclust_empty('dendrogram_nodes')
}

.hclust_merge_distance <- function(x) {
  # tam#38157: bounded by max_interactive_k, NOT max_centers. The two are
  # different settings -- max_centers is how far the elbow/silhouette DIAGNOSTIC
  # sweeps over k, max_interactive_k is how far the dendrogram widget's slider
  # goes. This table exists to justify a cluster count picked with that slider,
  # and cuts_json already uses max_interactive_k, so using max_centers here made
  # the two disagree: at max_interactive_k = 2 the slider offered one cut while
  # the table listed nine, six of them unreachable.
  upper <- min(x$max_interactive_k, x$valid_nrow)
  if (upper < 2L) return(.hclust_empty('merge_distance'))
  tibble::tibble(
    cluster = paste(seq.int(2L, upper), '→', seq.int(1L, upper - 1L)),
    merge_distance = rev(as.numeric(x$hclust$height))[seq_len(upper - 1L)]
  )
}

.hclust_silhouette <- function(x) {
  upper <- min(x$max_centers, x$valid_nrow - 1L)
  if (upper < 2L || is.null(x$silhouette_sample_indices)) return(.hclust_empty('silhouette'))
  indices <- x$silhouette_sample_indices
  mat <- x$mat[indices, , drop = FALSE]
  distance_matrix <- stats::dist(mat, method = x$distance)
  purrr::map_dfr(seq.int(2L, upper), function(k) {
    ids <- .hclust_membership(x, k)[indices]
    if (length(unique(ids)) < 2L || length(unique(ids)) >= length(ids)) {
      return(tibble::tibble(center = k, avg_silhouette = NA_real_,
                            min_silhouette = NA_real_, pct_negative = NA_real_))
    }
    value <- tryCatch(cluster::silhouette(ids, distance_matrix), error = function(e) NULL)
    widths <- if (is.null(value)) numeric() else as.numeric(value[, 'sil_width'])
    tibble::tibble(
      center = k,
      avg_silhouette = if (length(widths)) mean(widths, na.rm = TRUE) else NA_real_,
      min_silhouette = if (length(widths)) min(widths, na.rm = TRUE) else NA_real_,
      pct_negative = if (length(widths)) mean(widths < 0, na.rm = TRUE) else NA_real_
    )
  })
}

.hclust_elbow <- function(x) {
  upper <- min(x$max_centers, x$valid_nrow)
  if (upper < 1L) return(.hclust_empty('elbow'))
  total <- sum(scale(x$mat, center = TRUE, scale = FALSE)^2)
  rows <- purrr::map_dfr(seq_len(upper), function(k) {
    ids <- if (k == 1L) rep(1L, x$valid_nrow) else .hclust_membership(x, k)
    within <- sum(vapply(sort(unique(ids)), function(cluster_id) {
      values <- x$mat[ids == cluster_id, , drop = FALSE]
      center <- colMeans(values)
      sum((sweep(values, 2L, center, '-'))^2)
    }, numeric(1)))
    tibble::tibble(center = k, totss = total, tot.withinss = within,
                   betweenss = total - within)
  })
  rows %>% dplyr::mutate(
    decrease_ratio = (dplyr::lag(tot.withinss) - tot.withinss) / dplyr::lag(tot.withinss)
  )
}

.hclust_profile <- function(x) {
  original <- x$original_fit_mat
  overall_mean <- colMeans(x$mat)
  overall_sd <- apply(x$mat, 2L, stats::sd)
  rows <- purrr::map_dfr(sort(unique(x$clustering)), function(cluster_id) {
    index <- which(x$clustering == cluster_id)
    raw <- original[index, , drop = FALSE]
    standardized <- (colMeans(x$mat[index, , drop = FALSE]) - overall_mean) / overall_sd
    standardized[!is.finite(standardized)] <- 0
    tibble::tibble(
      variable = colnames(original), cluster = as.integer(cluster_id),
      standardized_mean = as.numeric(standardized), effect_size = abs(as.numeric(standardized)),
      cluster_median = as.numeric(apply(raw, 2L, stats::median)),
      cluster_mean = as.numeric(colMeans(raw)),
      overall_median = as.numeric(apply(original, 2L, stats::median)),
      overall_mean = as.numeric(colMeans(original))
    )
  })
  rows <- rows %>% dplyr::group_by(cluster) %>%
    dplyr::mutate(rank = as.integer(rank(-effect_size, ties.method = 'first'))) %>%
    dplyr::ungroup()
  if (!isTRUE(x$profile_show_all)) rows <- dplyr::filter(rows, rank <= x$profile_top_n)
  if (identical(x$profile_variable_order, 'effect_size')) {
    rows <- dplyr::arrange(rows, cluster, rank)
  }
  rows
}

.hclust_distribution <- function(x) {
  n <- nrow(x$original_fit_mat)
  p <- ncol(x$original_fit_mat)
  tibble::tibble(
    cluster = rep(as.integer(x$clustering), each = p),
    variable = rep(colnames(x$original_fit_mat), times = n),
    value = as.numeric(t(x$original_fit_mat)),
    standardized_value = as.numeric(t(x$mat))
  )
}

.hclust_gathered_data <- function(x) {
  n <- nrow(x$original_fit_mat)
  p <- ncol(x$original_fit_mat)
  tibble::tibble(
    row_id = rep(x$row_ids, each = p),
    cluster = rep(as.integer(x$clustering), each = p),
    key = rep(colnames(x$original_fit_mat), times = n),
    value = as.numeric(t(x$original_fit_mat)),
    standardized_value = as.numeric(t(x$mat))
  )
}

.hclust_map <- function(x) {
  n <- x$valid_nrow
  map_n <- min(n, x$map_sample_size)
  if (map_n < n) {
    set.seed(as.integer(x$seed) + 3000L)
    map_indices <- sort(sample(seq_len(n), map_n))
  } else {
    map_indices <- seq_len(n)
  }
  mat <- x$mat[map_indices, , drop = FALSE]
  if (nrow(mat) < 2L) return(.hclust_empty('map'))
  dimensions <- min(2L, nrow(mat) - 1L, ncol(mat))
  if (dimensions < 1L) return(.hclust_empty('map'))
  pcoa <- if (exists('.kmedoids_pcoa', mode = 'function')) {
    .kmedoids_pcoa(stats::dist(mat, method = x$distance), k = dimensions,
                   seed = as.integer(x$seed) + 4000L)
  } else {
    stats::cmdscale(stats::dist(mat, method = x$distance), k = dimensions, eig = TRUE)
  }
  points <- pcoa$points
  if (is.null(dim(points))) points <- matrix(points, ncol = 1L)
  if (ncol(points) < 2L) points <- cbind(points, 0)
  points <- points[, seq_len(2L), drop = FALSE]
  colnames(points) <- c('Dim1', 'Dim2')
  result <- tibble::tibble(
    Dim1 = points[, 1], Dim2 = points[, 2],
    cluster = as.integer(x$clustering[map_indices]), row_type = 'observation',
    label = x$row_ids[map_indices]
  )
  vector_scale <- max(abs(points), na.rm = TRUE)
  if (!is.finite(vector_scale) || vector_scale == 0) vector_scale <- 1
  loadings <- vapply(seq_len(ncol(mat)), function(index) {
    c(
      suppressWarnings(stats::cor(mat[, index], points[, 1], use = 'complete.obs')),
      suppressWarnings(stats::cor(mat[, index], points[, 2], use = 'complete.obs'))
    )
  }, numeric(2))
  loadings[!is.finite(loadings)] <- 0
  loading_order <- order(sqrt(colSums(loadings^2)), decreasing = TRUE)
  loading_order <- head(loading_order, max(1L, min(x$map_variable_n, length(loading_order))))
  vector_points <- t(loadings[, loading_order, drop = FALSE]) * (vector_scale * 0.8)
  vectors <- purrr::map_dfr(seq_len(nrow(vector_points)), function(index) {
    tibble::tibble(
      Dim1 = c(0, vector_points[index, 1]), Dim2 = c(0, vector_points[index, 2]),
      cluster = NA_integer_, row_type = 'vector',
      label = rep(colnames(mat)[loading_order[[index]]], 2L)
    )
  })
  result <- dplyr::bind_rows(result, vectors)
  eig <- pcoa$eig[pcoa$eig > 0]
  rate <- if (length(eig) == 0L) c(0, 0) else {
    values <- cumsum(eig) / sum(eig)
    c(values[seq_len(min(2L, length(values)))], rep(0, 2L))
  }
  attr(result, 'representation_rate') <- rate[seq_len(2L)]
  attr(result, 'map_sample_size') <- length(map_indices)
  attr(result, 'map_sampled') <- length(map_indices) < n
  result
}

.hclust_counts <- function(x) {
  tibble::tibble(
    original_nrow = nrow(x$original_data), sampled_nrow = x$sampled_nrow,
    analysis_nrow = x$valid_nrow, excluded_nrow = x$excluded_nrow,
    exclusion_ratio = if (nrow(x$original_data)) x$excluded_nrow / x$sampled_nrow else 0,
    fit_nrow = x$valid_nrow, diagnostic_nrow = length(x$silhouette_sample_indices),
    map_sample_nrow = min(x$valid_nrow, x$map_sample_size)
  )
}

#' Hierarchical clustering analytics.
#'
#' @param df A data frame.
#' @param ... Numeric columns selected with tidyselect.
#' @param centers Default number of clusters shown in report tables.
#' @param distance Distance metric: `euclidean` or `manhattan`.
#' @param linkage Linkage method: `ward.D2`, `complete`, `average`, or `single`.
#' @param max_interactive_k Largest K for which cut roots are precomputed.
#' @param takeSample Whether to sample rows when `max_nrow` is exceeded.
#' @param max_nrow Maximum number of rows used for fitting.
#' @param normalize_data Whether to standardize selected variables before fitting.
#' @param elbow_method_mode Which diagnostics to compute: `none`, `silhouette`, or `elbow`.
#' @param max_centers Maximum number of clusters for diagnostics.
#' @param silhouette_sample_size Maximum rows used for silhouette diagnostics.
#' @param profile_top_n Maximum variables per cluster when `profile_show_all` is `FALSE`.
#' @param profile_show_all Whether to show all profile variables.
#' @param profile_variable_order Profile ordering mode.
#' @param map_variable_n Retained for parity with other clustering analytics.
#' @param map_sample_size Maximum rows used for the cluster map.
#' @param seed Random seed.
#' @return A rowwise data frame containing a hierarchical clustering model.
#' @export
exp_hclust <- function(df, ..., centers = 3, distance = 'euclidean', linkage = 'ward.D2',
                       max_interactive_k = 10, takeSample = TRUE, max_nrow = 50000,
                       normalize_data = TRUE, elbow_method_mode = 'silhouette',
                       max_centers = 10, silhouette_sample_size = 5000,
                       profile_top_n = 10, profile_show_all = FALSE,
                       profile_variable_order = 'effect_size', map_variable_n = 10,
                       map_sample_size = 2000, seed = 1, label_col = NULL) {
  selected_cols <- tidyselect::vars_select(names(df), !!!rlang::quos(...))
  # tam#38157: the column whose values label the dendrogram's leaves. Optional --
  # without it the leaves fall back to the row number, which is what the widget
  # has always shown. It takes no part in the clustering.
  label_col_name <- col_name(substitute(label_col))
  if (!is.null(label_col_name) && !identical(label_col_name, 'NULL')) {
    if (!label_col_name %in% names(df)) {
      stop(paste0('The label column ', label_col_name, ' is not in the data.'), call. = FALSE)
    }
  } else {
    label_col_name <- NULL
  }
  if (length(selected_cols) == 0L) {
    stop('At least one numeric variable is required for hierarchical clustering.', call. = FALSE)
  }
  if (!all(vapply(df[selected_cols], is.numeric, logical(1)))) {
    stop('Hierarchical clustering requires numeric variables.', call. = FALSE)
  }
  distance <- match.arg(as.character(distance), c('euclidean', 'manhattan'))
  linkage <- match.arg(as.character(linkage), c('ward.D2', 'complete', 'average', 'single'))
  elbow_method_mode <- match.arg(as.character(elbow_method_mode), c('none', 'silhouette', 'elbow'))
  if (identical(linkage, 'ward.D2') && !identical(distance, 'euclidean')) {
    stop('The ward.D2 linkage requires the euclidean distance.', call. = FALSE)
  }
  centers <- .hclust_safe_numeric(centers)
  max_interactive_k <- .hclust_safe_numeric(max_interactive_k)
  max_centers <- .hclust_safe_numeric(max_centers)
  profile_top_n <- .hclust_safe_numeric(profile_top_n)
  map_sample_size <- .hclust_safe_numeric(map_sample_size)
  silhouette_sample_size <- .hclust_safe_numeric(silhouette_sample_size)
  seed <- .hclust_safe_numeric(seed)
  if (is.null(centers) || centers < 2) stop('centers must be at least 2.', call. = FALSE)
  if (is.null(max_interactive_k) || max_interactive_k < 2) {
    stop('max_interactive_k must be at least 2.', call. = FALSE)
  }
  if (is.null(max_centers) || max_centers < 2) stop('max_centers must be at least 2.', call. = FALSE)
  if (is.null(profile_top_n) || profile_top_n < 1) stop('profile_top_n must be positive.', call. = FALSE)
  if (is.null(map_sample_size) || map_sample_size < 1) stop('map_sample_size must be positive.', call. = FALSE)
  if (is.null(silhouette_sample_size) || silhouette_sample_size < 1) {
    stop('silhouette_sample_size must be positive.', call. = FALSE)
  }
  centers <- as.integer(floor(centers))
  max_interactive_k <- as.integer(floor(max_interactive_k))
  max_centers <- as.integer(floor(max_centers))
  profile_top_n <- as.integer(floor(profile_top_n))
  map_sample_size <- as.integer(floor(map_sample_size))
  silhouette_sample_size <- as.integer(floor(silhouette_sample_size))
  max_nrow_value <- if (is.null(max_nrow)) nrow(df) else .hclust_safe_numeric(max_nrow)
  if (is.null(max_nrow_value) || max_nrow_value < 1) stop('max_nrow must be positive.', call. = FALSE)
  max_nrow <- as.integer(floor(max_nrow_value))
  map_variable_n <- .hclust_safe_numeric(map_variable_n)
  if (is.null(map_variable_n) || map_variable_n < 1) stop('map_variable_n must be positive.', call. = FALSE)
  map_variable_n <- as.integer(floor(map_variable_n))
  profile_variable_order <- match.arg(as.character(profile_variable_order), c('effect_size', 'original'))

  original_data <- df
  source_row_ids <- as.character(seq_len(nrow(df)))
  sampling_requested <- isTRUE(takeSample) || identical(toupper(as.character(takeSample)), 'TRUE')
  if (sampling_requested && nrow(df) > max_nrow) {
    set.seed(as.integer(seed))
    selected_index <- sample(seq_len(nrow(df)), max_nrow)
    df <- df[selected_index, , drop = FALSE]
    source_row_ids <- source_row_ids[selected_index]
  }
  # tam#38157: a selected variable with no usable value at all makes
  # complete.cases() drop EVERY row, and the failure then surfaced as "At least
  # two valid rows are required" -- blaming the rows for one unusable column, and
  # naming neither. Drop such a column instead, the way K-Means does, and say
  # which one, so the analytics still runs on the variables that do carry data.
  unusable <- vapply(df[selected_cols], function(column) {
    !any(is.finite(suppressWarnings(as.numeric(column))))
  }, logical(1))
  if (any(unusable)) {
    dropped <- selected_cols[unusable]
    if (all(unusable)) {
      stop(paste0('No usable variable is left for hierarchical clustering: ',
                  paste0(dropped, collapse = ', '),
                  ifelse(length(dropped) > 1L, ' have', ' has'),
                  ' no finite value.'), call. = FALSE)
    }
    warning(paste0('Dropped from the clustering because ',
                   ifelse(length(dropped) > 1L, 'they have', 'it has'),
                   ' no finite value: ', paste0(dropped, collapse = ', '), '.'),
            call. = FALSE)
    selected_cols <- selected_cols[!unusable]
  }
  # Taken from `df` AFTER any sampling and sliced by the same `valid` mask as
  # row_ids below, so a label can never drift onto another leaf. Doing it any
  # other way fails SILENTLY -- the tree still draws, just with every name on the
  # wrong row -- which is why the harness pins the alignment against the source
  # data rather than merely checking that labels appear.
  source_labels <- if (is.null(label_col_name)) NULL else as.character(df[[label_col_name]])
  source_data <- df[selected_cols]
  source_mat <- as.matrix(source_data)
  valid <- complete.cases(source_data) & rowSums(!is.finite(source_mat)) == 0L
  excluded_nrow <- sum(!valid)
  fit_data <- source_data[valid, , drop = FALSE]
  row_ids <- source_row_ids[valid]
  if (nrow(fit_data) < 2L) {
    stop('At least two valid rows are required for hierarchical clustering.', call. = FALSE)
  }
  if (centers > nrow(fit_data)) stop('centers cannot be greater than the number of valid rows.', call. = FALSE)
  if (max_interactive_k < centers) max_interactive_k <- centers
  # tam#38157: name the offending column(s). With several variables selected the
  # old message left the user to find the constant one by trial and error.
  constant <- vapply(fit_data, function(column) length(unique(column)) < 2L, logical(1))
  if (any(constant)) {
    stop(paste0('Hierarchical clustering requires every selected variable to have non-constant finite values, but ',
                paste0(names(fit_data)[constant], collapse = ', '),
                ifelse(sum(constant) > 1L, ' have', ' has'),
                ' the same value in every row.'), call. = FALSE)
  }
  mat <- as.matrix(fit_data)
  if (isTRUE(normalize_data)) {
    mat <- scale(mat)
    mat[is.nan(mat)] <- 0
  }
  distance_object <- stats::dist(mat, method = distance)
  if (!requireNamespace('fastcluster', quietly = TRUE)) {
    stop('The fastcluster package is required for hierarchical clustering.', call. = FALSE)
  }
  hc <- fastcluster::hclust(distance_object, method = linkage)
  n <- nrow(mat)
  node_labels <- if (is.null(source_labels)) row_ids else source_labels[valid]
  nodes <- .hclust_build_nodes(hc, row_ids, node_labels)
  max_interactive_k <- min(max_interactive_k, n)
  cut_data <- .hclust_build_cut_data(hc, nodes, max_interactive_k)
  default_info <- .hclust_cut_info(hc, nodes, centers)
  max_height <- if (length(hc$height)) max(hc$height) else 0
  metadata <- list(
    schemaVersion = '1.0', n = n, distanceMethod = distance,
    linkageMethod = linkage, maxHeight = max_height, defaultK = centers,
    maxInteractiveK = max_interactive_k
  )
  nodes$metadata_json <- NA_character_
  nodes$cuts_json <- NA_character_
  nodes$metadata_json[[1]] <- jsonlite::toJSON(
    list(metadata = metadata, rootId = n + n - 2L), auto_unbox = TRUE, null = 'null'
  )
  nodes$cuts_json[[1]] <- jsonlite::toJSON(cut_data$cuts, auto_unbox = FALSE, null = 'null')
  # tam#38157: always sample. elbow_method_mode selects which SWEEP runs OVER k
  # (silhouette or elbow); it says nothing about whether the quality of the cut
  # the user actually chose should be reported. Gating on it blanked the three
  # silhouette columns of the always-visible Cluster Summary the moment someone
  # switched the diagnostic to Elbow or None. The per-row silhouette is computed
  # once, on at most silhouette_sample_size rows, so the cost is the same one
  # silhouette mode already paid.
  set.seed(as.integer(seed) + 1000L)
  silhouette_indices <- sample(seq_len(n), min(n, silhouette_sample_size))
  model <- list(
    hclust = hc, distance_object = distance_object, mat = mat,
    original_fit_mat = as.matrix(fit_data), source_data = source_data,
    original_data = original_data, selected_cols = selected_cols,
    source_row_ids = source_row_ids, row_ids = row_ids, valid_indices = which(valid),
    clustering = default_info$display_membership, memberships = cut_data$memberships,
    cuts = cut_data$cuts, leaf_order = as.integer(hc$order) - 1L,
    dendrogram_nodes = nodes, silhouette_values = rep(NA_real_, n),
    silhouette_sample_indices = silhouette_indices, valid_nrow = n,
    sampled_nrow = nrow(source_data), excluded_nrow = excluded_nrow,
    centers = centers, distance = distance, linkage = linkage,
    max_interactive_k = max_interactive_k, elbow_method_mode = elbow_method_mode,
    max_centers = max_centers, silhouette_sample_size = silhouette_sample_size,
    profile_top_n = profile_top_n, profile_show_all = isTRUE(profile_show_all),
    profile_variable_order = profile_variable_order,
    map_variable_n = map_variable_n,
    map_sample_size = map_sample_size, normalize_data = isTRUE(normalize_data),
    max_nrow = max_nrow, sampling_applied = sampling_requested && nrow(original_data) > max_nrow,
    seed = as.integer(seed), medoid_like_indices = integer(),
    silhouette_result = NULL, elbow_result = NULL, map_result = NULL
  )
  # tam#38157: two DIFFERENT things used to live in this one branch.
  #   * silhouette_result is the SWEEP over k, which is exactly what
  #     elbow_method_mode selects -- it stays gated.
  #   * silhouette_values is the per-row silhouette of the cut the user actually
  #     CHOSE, which the always-visible Cluster Summary reports. Gating it on the
  #     mode blanked three of that table's columns the moment someone switched
  #     the diagnostic to Elbow or None -- two settings that say nothing about
  #     whether the chosen cut's quality should be shown. It is hoisted out.
  if (identical(elbow_method_mode, 'silhouette')) {
    model$silhouette_result <- .hclust_silhouette(model)
  }
  if (length(silhouette_indices) > 1L) {
    chosen_ids <- .hclust_membership(model, centers)[silhouette_indices]
    if (length(unique(chosen_ids)) > 1L) {
      value <- tryCatch(
        cluster::silhouette(
          chosen_ids, stats::dist(mat[silhouette_indices, , drop = FALSE], method = distance)
        ),
        error = function(e) NULL
      )
      if (!is.null(value)) model$silhouette_values[silhouette_indices] <- as.numeric(value[, 'sil_width'])
    }
  }
  if (identical(elbow_method_mode, 'elbow')) model$elbow_result <- .hclust_elbow(model)
  model$map_result <- tryCatch(.hclust_map(model), error = function(e) .hclust_empty('map'))
  class(model) <- c('hclust_exploratory', 'hclust', 'cluster')
  tibble::tibble(model = list(model)) %>% dplyr::rowwise()
}

#' Tidy a hierarchical clustering model for Analytics report sections.
#' @export
tidy.hclust_exploratory <- function(x, type = 'summary', with_excluded_rows = FALSE, ...) {
  switch(type,
    summary = .hclust_summary(x, with_excluded_rows),
    analysis_conditions = .hclust_analysis_conditions(x),
    dendrogram_nodes = .hclust_dendrogram_nodes(x),
    merge_distance = .hclust_merge_distance(x),
    silhouette = x$silhouette_result %||% .hclust_empty('silhouette'),
    elbow = x$elbow_result %||% .hclust_empty('elbow'),
    elbow_diff = if (is.null(x$elbow_result)) .hclust_empty('elbow_diff') else
      x$elbow_result %>% dplyr::select(center, decrease_ratio),
    profile = .hclust_profile(x),
    radar = .hclust_profile(x),
    gathered_data = .hclust_gathered_data(x),
    distribution = .hclust_distribution(x),
    variable_importance = cluster_variable_importance_anova(x$mat, x$clustering),
    map = x$map_result %||% .hclust_empty('map'),
    cluster_map = x$map_result %||% .hclust_empty('cluster_map'),
    counts = .hclust_counts(x),
    data = {
      cluster <- rep(NA_integer_, nrow(x$source_data))
      cluster[x$valid_indices] <- x$clustering
      dplyr::bind_cols(x$source_data, tibble::tibble(
        row_id = x$source_row_ids, cluster = cluster, is_excluded = is.na(cluster)
      ))
    },
    .hclust_empty(type)
  )
}

#' Glance at a hierarchical clustering model.
#' @export
glance.hclust_exploratory <- function(x, ...) {
  tibble::tibble(
    centers = x$centers, distance = x$distance, linkage = x$linkage,
    max_height = max(x$hclust$height), fit_nrow = x$valid_nrow
  )
}

#' Augment data with hierarchical cluster assignments.
#' @export
augment.hclust_exploratory <- function(x, data = NULL, ...) {
  if (is.null(data)) data <- x$original_fit_mat
  data <- tibble::as_tibble(data, .name_repair = 'minimal')
  if (nrow(data) != length(x$clustering)) {
    stop('data must have the same number of rows as the fitted hierarchical clustering data.', call. = FALSE)
  }
  dplyr::mutate(data, .cluster = factor(x$clustering))
}

#' @export
tidy_hclust <- tidy_rowwise

#' @export
glance_hclust <- glance_rowwise

#' @export
augment_hclust <- augment_rowwise
