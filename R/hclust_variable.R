# Hierarchical clustering of VARIABLES (tam#38161).
#
# The sibling of exp_hclust (R/hclust.R), which clusters rows. Here the leaves
# of the dendrogram are the selected variables and the distance between two
# variables is 1 - r (their correlation). The dendrogram node table, the cut
# data and the merge-distance table are built by the SAME helpers exp_hclust
# uses, so tam's dendrogram widget renders both without knowing which is which
# -- except for metadata$leafKind, which tells it a leaf is a variable, not a
# row, so there is nothing to drill down into with Show Detail.

.hclust_variable_empty <- function(type) {
  switch(type,
    summary = tibble::tibble(
      cluster = integer(), n_variables = integer(), variables = character(),
      avg_within_cor = numeric(), avg_silhouette = numeric(), min_silhouette = numeric()
    ),
    analysis_conditions = tibble::tibble(Metric = character(), Value = character()),
    silhouette = tibble::tibble(
      center = integer(), avg_silhouette = numeric(), min_silhouette = numeric(),
      pct_negative = numeric(), n_singleton_clusters = integer()
    ),
    cor = tibble::tibble(
      pair.name.x = factor(), pair.name.y = factor(), correlation = numeric(),
      cluster.x = integer(), cluster.y = integer()
    ),
    data = tibble::tibble(
      variable = character(), cluster = integer(), display_order = integer(),
      avg_cor_own = numeric(), nearest_cluster = integer(), avg_cor_nearest = numeric(),
      silhouette = numeric()
    ),
    .hclust_empty(type)
  )
}

# Mean of the off-diagonal correlations between the variables at `from` and the
# variables at `to` (integer indices into cor_mat). NA when there is no pair.
.hclust_variable_mean_cor <- function(cor_mat, from, to) {
  values <- cor_mat[from, to, drop = FALSE]
  if (identical(from, to)) {
    values <- values[upper.tri(values)]
  } else {
    values <- as.numeric(values)
  }
  values <- values[is.finite(values)]
  if (length(values) == 0L) NA_real_ else mean(values)
}

# Silhouette widths for a membership vector on the correlation distance, or all
# NA when the partition has a single cluster or only singletons (silhouette is
# undefined there, and cluster::silhouette would error).
.hclust_variable_silhouette_widths <- function(ids, distance_object) {
  n_groups <- length(unique(ids))
  if (n_groups < 2L || n_groups >= length(ids)) return(rep(NA_real_, length(ids)))
  value <- tryCatch(cluster::silhouette(ids, distance_object), error = function(e) NULL)
  if (!is.matrix(value)) return(rep(NA_real_, length(ids)))
  as.numeric(value[, 'sil_width'])
}

# TRUE for each variable that is alone in its cluster. cluster::silhouette gives
# such a variable 0 by definition (Rousseeuw 1987): it keeps counting as 0 in an
# average, but a per-variable or per-cluster 0 would read as "poorly separated",
# and it would floor every minimum at 0, so those are reported as NA instead.
.hclust_variable_singleton <- function(ids) {
  counts <- table(ids)
  as.vector(counts[as.character(ids)] == 1L)
}

.hclust_variable_silhouette <- function(x) {
  upper <- min(x$max_centers, x$valid_nrow - 1L)
  if (upper < 2L) return(.hclust_variable_empty('silhouette'))
  purrr::map_dfr(seq.int(2L, upper), function(k) {
    ids <- .hclust_membership(x, k)
    widths <- .hclust_variable_silhouette_widths(ids, x$distance_object)
    has_value <- any(is.finite(widths))
    alone <- .hclust_variable_singleton(ids)
    grouped <- widths[!alone]
    tibble::tibble(
      center = k,
      avg_silhouette = if (has_value) mean(widths, na.rm = TRUE) else NA_real_,
      min_silhouette = if (any(is.finite(grouped))) min(grouped, na.rm = TRUE) else NA_real_,
      pct_negative = if (has_value) mean(widths < 0, na.rm = TRUE) else NA_real_,
      # How many clusters hold a single variable. Their 0 lowers the average and is
      # left out of the minimum, so a k with many of them needs this to be read.
      n_singleton_clusters = as.integer(sum(alone))
    )
  })
}

.hclust_variable_summary <- function(x) {
  ids <- x$clustering
  order_index <- x$leaf_order + 1L
  purrr::map_dfr(sort(unique(ids)), function(cluster_id) {
    index <- which(ids == cluster_id)
    in_display_order <- order_index[order_index %in% index]
    values <- x$silhouette_values[index]
    has_value <- length(index) > 1L && any(is.finite(values))
    tibble::tibble(
      cluster = as.integer(cluster_id), n_variables = length(index),
      variables = paste(x$selected_cols[in_display_order], collapse = ', '),
      avg_within_cor = .hclust_variable_mean_cor(x$cor_mat, index, index),
      avg_silhouette = if (has_value) mean(values, na.rm = TRUE) else NA_real_,
      min_silhouette = if (has_value) min(values, na.rm = TRUE) else NA_real_
    )
  })
}

.hclust_variable_analysis_conditions <- function(x) {
  tibble::tibble(
    Metric = c('Number of Variables', 'Variable Names', 'Row Count', 'Rows Used for Correlation (Min)',
               'Non-finite Values (Treated as Missing)', 'Number of Clusters',
               'Correlation Method', 'Distance', 'Linkage', 'Missing Values'),
    Value = c(
      as.character(length(x$selected_cols)), paste(x$selected_cols, collapse = ', '),
      as.character(x$nrow), as.character(x$min_pair_nrow %||% x$nrow),
      as.character(x$n_nonfinite %||% 0L), as.character(x$centers), x$cor_method, '1 - Correlation',
      x$linkage, 'Pairwise'
    )
  )
}

.hclust_variable_cor <- function(x) {
  p <- length(x$selected_cols)
  levels <- x$selected_cols[x$leaf_order + 1L]
  tibble::tibble(
    pair.name.x = factor(rep(x$selected_cols, times = p), levels = levels),
    pair.name.y = factor(rep(x$selected_cols, each = p), levels = levels),
    correlation = as.numeric(x$cor_mat),
    cluster.x = rep(as.integer(x$clustering), times = p),
    cluster.y = rep(as.integer(x$clustering), each = p)
  ) %>% dplyr::arrange(pair.name.x, pair.name.y)
}

.hclust_variable_data <- function(x) {
  ids <- x$clustering
  groups <- sort(unique(ids))
  rows <- purrr::map_dfr(seq_along(x$selected_cols), function(index) {
    own <- which(ids == ids[[index]])
    own_others <- setdiff(own, index)
    avg_cor_own <- if (length(own_others)) {
      .hclust_variable_mean_cor(x$cor_mat, index, own_others)
    } else {
      NA_real_
    }
    others <- setdiff(groups, ids[[index]])
    other_means <- vapply(others, function(group) {
      .hclust_variable_mean_cor(x$cor_mat, index, which(ids == group))
    }, numeric(1))
    nearest <- if (length(others) && any(is.finite(other_means))) {
      which.max(other_means)
    } else {
      NA_integer_
    }
    tibble::tibble(
      variable = x$selected_cols[[index]], cluster = as.integer(ids[[index]]),
      display_order = as.integer(x$dendrogram_nodes$display_order[[index]]),
      avg_cor_own = avg_cor_own,
      nearest_cluster = if (is.na(nearest)) NA_integer_ else as.integer(others[[nearest]]),
      avg_cor_nearest = if (is.na(nearest)) NA_real_ else as.numeric(other_means[[nearest]]),
      silhouette = if (length(own) > 1L) x$silhouette_values[[index]] else NA_real_
    )
  })
  dplyr::arrange(rows, display_order)
}

#' Hierarchical clustering of variables based on their correlation.
#'
#' @param df A data frame.
#' @param ... Numeric columns selected with tidyselect.
#' @param centers Default number of clusters shown in report tables.
#' @param cor_method Correlation method: `pearson` or `spearman`.
#' @param linkage Linkage method: `average`, `complete`, or `single`.
#' @param max_interactive_k Largest K for which cut roots are precomputed.
#' @param elbow_method_mode `silhouette` computes the silhouette sweep over K, `none` skips it.
#' @param max_centers Maximum number of clusters for the silhouette sweep.
#' @return A rowwise data frame containing a variable clustering model.
#' @export
exp_hclust_variable <- function(df, ..., centers = 3, cor_method = 'pearson',
                                linkage = 'average', max_interactive_k = 10,
                                elbow_method_mode = 'silhouette', max_centers = 10) {
  selected_cols <- unname(tidyselect::vars_select(names(df), !!!rlang::quos(...)))
  if (length(selected_cols) == 0L) {
    stop('At least two numeric variables are required for variable clustering.', call. = FALSE)
  }
  if (!all(vapply(df[selected_cols], is.numeric, logical(1)))) {
    stop('Variable clustering requires numeric variables.', call. = FALSE)
  }
  cor_method <- match.arg(as.character(cor_method), c('pearson', 'spearman'))
  linkage <- match.arg(as.character(linkage), c('average', 'complete', 'single'))
  elbow_method_mode <- match.arg(as.character(elbow_method_mode), c('none', 'silhouette'))
  centers <- .hclust_safe_numeric(centers)
  max_interactive_k <- .hclust_safe_numeric(max_interactive_k)
  max_centers <- .hclust_safe_numeric(max_centers)
  if (is.null(centers) || centers < 2) stop('centers must be at least 2.', call. = FALSE)
  if (is.null(max_interactive_k) || max_interactive_k < 2) {
    stop('max_interactive_k must be at least 2.', call. = FALSE)
  }
  if (is.null(max_centers) || max_centers < 2) stop('max_centers must be at least 2.', call. = FALSE)
  centers <- as.integer(floor(centers))
  max_interactive_k <- as.integer(floor(max_interactive_k))
  max_centers <- as.integer(floor(max_centers))

  mat <- as.matrix(df[selected_cols])
  # Inf/-Inf would make cor() return NaN for every pair they touch; they are
  # treated as missing, and counted so the report can say so.
  infinite <- is.infinite(mat)
  mat[!is.finite(mat)] <- NA_real_
  # Same policy and wording as exp_hclust: a variable with no usable value is
  # dropped with a warning that names it, so the rest still clusters.
  unusable <- colSums(!is.na(mat)) == 0L
  if (any(unusable)) {
    dropped <- selected_cols[unusable]
    if (all(unusable)) {
      stop(paste0('No usable variable is left for variable clustering: ',
                  paste0(dropped, collapse = ', '),
                  ifelse(length(dropped) > 1L, ' have', ' has'),
                  ' no finite value.'), call. = FALSE)
    }
    warning(paste0('Dropped from the clustering because ',
                   ifelse(length(dropped) > 1L, 'they have', 'it has'),
                   ' no finite value: ', paste0(dropped, collapse = ', '), '.'),
            call. = FALSE)
    selected_cols <- selected_cols[!unusable]
    mat <- mat[, !unusable, drop = FALSE]
    infinite <- infinite[, !unusable, drop = FALSE]
  }
  # A constant variable has no correlation with anything. Name it instead of
  # letting it silently sit at distance 1 from every other variable.
  constant <- apply(mat, 2L, function(column) length(unique(column[!is.na(column)])) < 2L)
  if (any(constant)) {
    stop(paste0('Variable clustering requires every selected variable to have non-constant finite values, but ',
                paste0(selected_cols[constant], collapse = ', '),
                ifelse(sum(constant) > 1L, ' have', ' has'),
                ' the same value in every row.'), call. = FALSE)
  }
  p <- length(selected_cols)
  if (p < 2L) {
    stop('At least two numeric variables are required for variable clustering.', call. = FALSE)
  }
  if (centers > p) {
    stop('centers cannot be greater than the number of variables.', call. = FALSE)
  }
  if (max_interactive_k < centers) max_interactive_k <- centers
  max_interactive_k <- min(max_interactive_k, p)

  cor_mat <- suppressWarnings(stats::cor(mat, method = cor_method, use = 'pairwise.complete.obs'))
  present <- !is.na(mat)
  pair_nrow <- crossprod(present)
  min_pair_nrow <- as.integer(min(pair_nrow[upper.tri(pair_nrow)]))
  dimnames(cor_mat) <- list(selected_cols, selected_cols)
  # 1 - r, NOT 1 - abs(r): a negatively correlated pair stays apart. A pair with
  # no overlapping rows has no correlation; treat it as uncorrelated (distance 1)
  # -- the same choice cor_cluster_order_recursive makes for do_cor.
  distance_mat <- 1 - cor_mat
  distance_mat[!is.finite(distance_mat)] <- 1
  diag(distance_mat) <- 0
  distance_object <- stats::as.dist(distance_mat)
  .hclust_fun <- if (requireNamespace('fastcluster', quietly = TRUE)) {
    fastcluster::hclust
  } else {
    stats::hclust
  }
  hc <- .hclust_fun(distance_object, method = linkage)

  nodes <- .hclust_build_nodes(hc, selected_cols, selected_cols)
  cut_data <- .hclust_build_cut_data(hc, nodes, max_interactive_k)
  default_info <- .hclust_cut_info(hc, nodes, centers)
  max_height <- if (length(hc$height)) max(hc$height) else 0
  metadata <- list(
    schemaVersion = '1.0', n = p, leafKind = 'variable', distanceMethod = 'correlation',
    corMethod = cor_method, linkageMethod = linkage, maxHeight = max_height,
    defaultK = centers, maxInteractiveK = max_interactive_k
  )
  nodes$metadata_json <- NA_character_
  nodes$cuts_json <- NA_character_
  nodes$metadata_json[[1]] <- jsonlite::toJSON(
    list(metadata = metadata, rootId = p + p - 2L), auto_unbox = TRUE, null = 'null'
  )
  nodes$cuts_json[[1]] <- jsonlite::toJSON(cut_data$cuts, auto_unbox = FALSE, null = 'null')

  model <- list(
    hclust = hc, distance_object = distance_object, cor_mat = cor_mat,
    selected_cols = selected_cols, nrow = nrow(df),
    clustering = default_info$display_membership, memberships = cut_data$memberships,
    cuts = cut_data$cuts, leaf_order = as.integer(hc$order) - 1L,
    dendrogram_nodes = nodes, valid_nrow = p,
    centers = centers, cor_method = cor_method, linkage = linkage,
    n_nonfinite = sum(infinite), min_pair_nrow = min_pair_nrow,
    max_interactive_k = max_interactive_k, elbow_method_mode = elbow_method_mode,
    max_centers = max_centers,
    silhouette_values = NULL, silhouette_result = NULL
  )
  # The chosen cut's per-variable silhouette is always computed (the Cluster
  # Summary shows it whatever the sweep mode is -- same reasoning as exp_hclust).
  model$silhouette_values <- .hclust_variable_silhouette_widths(model$clustering, distance_object)
  if (identical(elbow_method_mode, 'silhouette')) {
    model$silhouette_result <- .hclust_variable_silhouette(model)
  }
  class(model) <- c('hclust_variable_exploratory', 'hclust', 'cluster')
  tibble::tibble(model = list(model)) %>% dplyr::rowwise()
}

#' Tidy a variable clustering model for Analytics report sections.
#' @export
tidy.hclust_variable_exploratory <- function(x, type = 'summary', ...) {
  switch(type,
    summary = .hclust_variable_summary(x),
    analysis_conditions = .hclust_variable_analysis_conditions(x),
    dendrogram_nodes = .hclust_dendrogram_nodes(x),
    merge_distance = .hclust_merge_distance(x),
    silhouette = x$silhouette_result %||% .hclust_variable_empty('silhouette'),
    cor = .hclust_variable_cor(x),
    data = .hclust_variable_data(x),
    .hclust_variable_empty(type)
  )
}

#' Glance at a variable clustering model.
#' @export
glance.hclust_variable_exploratory <- function(x, ...) {
  tibble::tibble(
    centers = x$centers, cor_method = x$cor_method, linkage = x$linkage,
    max_height = if (length(x$hclust$height)) max(x$hclust$height) else 0,
    n_variables = x$valid_nrow
  )
}
