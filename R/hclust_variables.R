# Hierarchical Clustering of Variables (tam#38161).
#
# Clusters the SELECTED VARIABLES (columns) of a data frame, not the
# observations/rows -- distinct from Row Clustering (tam#38157, not yet
# implemented as of this writing; see the paired tam design doc
# docs/plans/design/38161_variable_clustering_design.md for the full
# architecture-reuse discussion).
#
# Pipeline:
#   selected columns
#     -> variable x variable correlation matrix (do_cor.cols(), the SAME
#        type-aware Pearson / Spearman / Polychoric / Mixed / Auto engine
#        "Correlation by Columns" and Factor Analysis already use, so
#        column-type gating stays consistent across the codebase)
#     -> distance = 1 - correlation
#     -> fastcluster::hclust() (falls back to stats::hclust() when the
#        fastcluster package is not installed -- it is a drop-in,
#        API-compatible replacement)
#     -> cutree() at the requested cluster count
#     -> a small, dependency-free right-angle dendrogram segment table,
#        built directly from hc$merge / hc$height / hc$order, so a future
#        JS renderer (shared with #38157's eventual Row Clustering engine)
#        has real coordinates to draw without re-deriving the tree.

#' Build a right-angle dendrogram segment table from an `hclust` object.
#'
#' Coordinates: `x` is the merge height (0 = identical, growing outward as
#' clusters merge at greater distance); `y` is the leaf's DISPLAY position
#' (1..n, ordered per `hc$order`, i.e. left-to-right / top-to-bottom render
#' order -- NOT the original column index). Each merge step produces exactly
#' 3 segments: the left child's horizontal branch, the right child's
#' horizontal branch, and the vertical connector joining them -- mirroring
#' the "Parent 縦線 + 左右 横線" branch geometry from the #38157 requirements
#' doc, so the shape is ready to reuse for Row Clustering's dendrogram once
#' that lands.
#'
#' @param hc an `hclust` object (from `fastcluster::hclust()` or
#'   `stats::hclust()` -- both produce the same `merge` / `height` / `order`
#'   / `labels` shape).
#' @return a data.frame with one row per segment: `merge_step`, `x0`, `y0`,
#'   `x1`, `y1`.
#' @keywords internal
hclust_dendrogram_segments <- function(hc) {
  n <- length(hc$order)
  # Map ORIGINAL leaf index (1..n, as used in hc$merge's negative entries) to
  # its DISPLAY position (1..n, left-to-right render order).
  leaf_y <- stats::setNames(seq_len(n), as.character(hc$order))
  node_y <- numeric(max(n - 1, 0))   # internal node y position, by merge step
  node_h <- hc$height                # internal node height (x position), by merge step

  child_pos <- function(value) {
    if (value < 0) {
      list(y = unname(leaf_y[[as.character(-value)]]), h = 0)
    } else {
      list(y = node_y[[value]], h = node_h[[value]])
    }
  }

  n_merges <- max(n - 1, 0)
  seg_step <- integer(3 * n_merges)
  seg_x0 <- seg_y0 <- seg_x1 <- seg_y1 <- numeric(3 * n_merges)
  i <- 0L

  for (step in seq_len(n_merges)) {
    left <- child_pos(hc$merge[step, 1])
    right <- child_pos(hc$merge[step, 2])
    h <- node_h[step]
    node_y[step] <- (left$y + right$y) / 2

    # Left child's horizontal branch.
    i <- i + 1L
    seg_step[i] <- step; seg_x0[i] <- left$h;  seg_y0[i] <- left$y;  seg_x1[i] <- h; seg_y1[i] <- left$y
    # Right child's horizontal branch.
    i <- i + 1L
    seg_step[i] <- step; seg_x0[i] <- right$h; seg_y0[i] <- right$y; seg_x1[i] <- h; seg_y1[i] <- right$y
    # Vertical connector at the parent's merge height.
    i <- i + 1L
    seg_step[i] <- step; seg_x0[i] <- h; seg_y0[i] <- left$y; seg_x1[i] <- h; seg_y1[i] <- right$y
  }

  data.frame(
    merge_step = seg_step,
    x0 = seg_x0, y0 = seg_y0, x1 = seg_x1, y1 = seg_y1,
    stringsAsFactors = FALSE
  )
}

#' Hierarchical Clustering of Variables.
#'
#' @param df a data frame.
#' @param ... columns (variables) to cluster. Non-numeric columns are allowed
#'   through to the correlation step (Polychoric / Mixed correlation can use
#'   them), matching the same column-type convention as `do_cor()` /
#'   `do_cor.cols()` and Factor Analysis -- Variable Clustering does not
#'   re-decide numeric-vs-categorical handling on its own.
#' @param method correlation method: "auto" (default, resolves to Pearson /
#'   Polychoric / Mixed by column type, same as `do_cor.cols()`), "pearson",
#'   "spearman", "kendall", "polychoric", or "mixed".
#' @param linkage_method hierarchical clustering linkage: "complete"
#'   (default), "average", or "single". `"ward.D2"` is deliberately NOT
#'   accepted here -- Ward's method assumes an underlying Euclidean distance,
#'   and `1 - correlation` is not Euclidean in general (see design doc),
#'   unlike Row Clustering (#38157) where Euclidean row-distance makes Ward
#'   valid.
#' @param n_clusters number of clusters to cut the dendrogram into (>= 2,
#'   <= number of variables actually used).
#' @param max_nrow optional row sub-sampling cap applied BEFORE computing
#'   correlations (large data frames only need enough rows to estimate
#'   correlation stably). `NULL` disables sampling.
#' @param seed random seed for the row sub-sample.
#' @return a rowwise, one-row tibble with a `model` list-column holding an
#'   object of class `hclust_variables_exploratory`. Use `tidy_rowwise(model,
#'   type = ...)` to extract report tables -- see `tidy.hclust_variables_exploratory()`.
#' @export
exp_hclust_variables <- function(df, ...,
                                 method = "auto",
                                 linkage_method = "complete",
                                 n_clusters = 3,
                                 max_nrow = 50000,
                                 seed = 1) {
  validate_empty_data(df)

  selected_cols <- tidyselect::vars_select(names(df), !!! rlang::quos(...))
  if (length(selected_cols) < 2) {
    stop("Hierarchical Clustering of Variables requires 2 or more variables.")
  }

  if (identical(linkage_method, "ward.D2") || identical(linkage_method, "ward")) {
    stop(paste0(
      "Ward's linkage requires a Euclidean distance and is not supported ",
      "for correlation-based Variable Clustering. Choose Complete, ",
      "Average, or Single linkage instead."
    ))
  }
  if (!(linkage_method %in% c("complete", "average", "single"))) {
    stop(paste0("Unsupported linkage_method for Variable Clustering: ", linkage_method))
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  df_selected <- df %>% dplyr::ungroup() %>% dplyr::select(!!!rlang::syms(selected_cols))

  nrow_before_sample <- nrow(df_selected)
  sampled_nrow <- NULL
  if (!is.null(max_nrow) && !is.na(max_nrow) && nrow(df_selected) > max_nrow) {
    sampled_nrow <- max_nrow
    df_selected <- df_selected %>% sample_rows(max_nrow)
  }
  n_rows_used <- nrow(df_selected)

  # Resolved purely for display -- do_cor.cols() re-resolves "auto" itself
  # internally; keeping this call side-effect-free and cheap (no correlation
  # math, just type inspection) avoids computing the correlation matrix twice.
  cor_method_used <- resolve_correlation_method(as.data.frame(df_selected), method)

  # Full symmetric matrix (distinct = FALSE, diag = TRUE), input column order
  # preserved (variable_order = "input") -- we don't want do_cor.cols'
  # correlation-based reordering here since hclust/hc$order will supply the
  # dendrogram's own display order.
  cor_long <- do_cor.cols(df_selected, !!!rlang::syms(selected_cols),
                          method = method, use = "pairwise.complete.obs",
                          distinct = FALSE, diag = TRUE,
                          variable_order = "input",
                          return_type = "data.frame")

  # A pair with an undefined correlation (do_cor_internal/mat_to_df's
  # na.rm = TRUE silently drops NA correlation cells, including a constant /
  # all-NA column's own diagonal self-correlation, which is NA rather than 1)
  # never appears in cor_long at all. Detect excluded variables by checking
  # which selected_cols never show up on the diagonal, and drop them from
  # clustering instead of letting an NA distance reach hclust() uncaught.
  diag_rows <- cor_long[as.character(cor_long$pair.name.x) == as.character(cor_long$pair.name.y), ]
  present_vars <- unique(as.character(diag_rows$pair.name.x))
  excluded_cols <- setdiff(selected_cols, present_vars)
  used_cols <- selected_cols[selected_cols %in% present_vars] # keep original order

  if (length(used_cols) < 2) {
    stop(paste0(
      "At least 2 variables with a defined correlation (non-constant, not ",
      "all-missing) are required for Hierarchical Clustering of Variables.",
      if (length(excluded_cols) > 0) paste0(" Excluded: ", paste(excluded_cols, collapse = ", ")) else ""
    ))
  }

  cor_long_used <- cor_long[
    as.character(cor_long$pair.name.x) %in% used_cols &
      as.character(cor_long$pair.name.y) %in% used_cols,
  ]
  cor_long_used$pair.name.x <- as.character(cor_long_used$pair.name.x)
  cor_long_used$pair.name.y <- as.character(cor_long_used$pair.name.y)
  cor_mat <- reshape2::acast(cor_long_used, pair.name.x ~ pair.name.y, value.var = "correlation")
  cor_mat <- cor_mat[used_cols, used_cols, drop = FALSE] # enforce original column order on both axes

  # Any remaining NA (e.g. a pairwise-complete-obs pair with zero overlapping
  # non-missing rows, distinct from a globally-constant/all-NA column already
  # excluded above) would otherwise break hclust() opaquely.
  if (any(is.na(cor_mat))) {
    na_idx <- which(is.na(cor_mat), arr.ind = TRUE)[1, , drop = TRUE]
    bad_pair <- paste0(rownames(cor_mat)[na_idx["row"]], " / ", colnames(cor_mat)[na_idx["col"]])
    stop(paste0(
      "Could not compute a correlation for the following variable pair, ",
      "likely because they have no overlapping non-missing observations: ",
      bad_pair
    ))
  }

  n_vars <- nrow(cor_mat)
  n_clusters <- suppressWarnings(as.numeric(n_clusters))
  if (length(n_clusters) != 1 || is.na(n_clusters) || n_clusters < 2) {
    stop("Number of Clusters must be 2 or more.")
  }
  n_clusters <- as.integer(round(n_clusters))
  if (n_clusters > n_vars) {
    stop(paste0(
      "Number of Clusters (", n_clusters, ") cannot exceed the number of ",
      "variables being clustered (", n_vars, ")."
    ))
  }

  dist_obj <- stats::as.dist(1 - cor_mat)

  hc <- if (requireNamespace("fastcluster", quietly = TRUE)) {
    fastcluster::hclust(dist_obj, method = linkage_method)
  } else {
    stats::hclust(dist_obj, method = linkage_method)
  }

  clusters <- stats::cutree(hc, k = n_clusters)

  # Re-number cluster IDs by LEFT-TO-RIGHT dendrogram display order
  # (hc$order), not cutree()'s own internal numbering, so "Cluster 1" is
  # always the first cluster encountered in the rendered dendrogram (mirrors
  # #38157 requirements doc section 29 -- kept for parity even though the
  # interactive JS renderer itself is out of scope for this change; see
  # design doc).
  ordered_leaf_clusters <- clusters[hc$order]
  display_cluster_id <- match(ordered_leaf_clusters, unique(ordered_leaf_clusters))
  names(display_cluster_id) <- names(ordered_leaf_clusters)
  clusters_display <- display_cluster_id[names(clusters)]

  segments <- hclust_dendrogram_segments(hc)
  leaf_order <- data.frame(
    variable = hc$labels[hc$order],
    display_order = seq_along(hc$order),
    stringsAsFactors = FALSE
  )

  model <- list(
    hclust = hc,
    cor_matrix = cor_mat,
    cor_method_requested = method,
    cor_method_used = cor_method_used,
    distance_method = "1 - correlation",
    linkage_method = linkage_method,
    n_clusters = n_clusters,
    clusters = clusters,                 # cutree()'s own numbering
    clusters_display = clusters_display, # display-order numbering (section 29)
    used_cols = used_cols,
    excluded_cols = excluded_cols,
    n_vars_used = n_vars,
    n_rows_used = n_rows_used,
    nrow_before_sample = nrow_before_sample,
    sampled_nrow = sampled_nrow,
    segments = segments,
    leaf_order = leaf_order,
    seed = seed
  )
  class(model) <- c("hclust_variables_exploratory", class(model))

  tibble::tibble(model = list(model)) %>% dplyr::rowwise()
}

#' @export
tidy.hclust_variables_exploratory <- function(x, type = "summary", ...) {
  switch(type,
    "summary" = ,
    "analysis_conditions" = hclust_variables_analysis_conditions(x),
    "cluster_membership" = hclust_variables_cluster_membership(x),
    "correlation_matrix" = hclust_variables_correlation_matrix(x),
    "dendrogram_segments" = hclust_variables_dendrogram_segments_table(x),
    "dendrogram_leaves" = hclust_variables_dendrogram_leaves_table(x),
    stop(paste0("Unknown type for tidy.hclust_variables_exploratory(): ", type))
  )
}

#' @keywords internal
hclust_variables_analysis_conditions <- function(x) {
  data.frame(
    Metric = c(
      "Number of Variables", "Excluded Variables", "Number of Rows",
      "Correlation Method", "Linkage Method", "Number of Clusters"
    ),
    Value = c(
      as.character(x$n_vars_used),
      if (length(x$excluded_cols) > 0) paste(x$excluded_cols, collapse = ", ") else "(None)",
      as.character(x$n_rows_used),
      x$cor_method_used,
      x$linkage_method,
      as.character(x$n_clusters)
    ),
    stringsAsFactors = FALSE
  )
}

#' @keywords internal
hclust_variables_cluster_membership <- function(x) {
  df <- data.frame(
    Variable = names(x$clusters_display),
    Cluster = as.integer(x$clusters_display),
    stringsAsFactors = FALSE
  )
  df <- df %>% dplyr::left_join(x$leaf_order, by = c("Variable" = "variable"))
  df <- df %>% dplyr::arrange(display_order)
  df %>%
    dplyr::mutate(Cluster = paste0("Cluster ", Cluster)) %>%
    dplyr::select(Variable, Cluster)
}

#' @keywords internal
hclust_variables_correlation_matrix <- function(x) {
  m <- x$cor_matrix
  df <- as.data.frame(m, check.names = FALSE, stringsAsFactors = FALSE)
  df <- cbind(data.frame(Variable = rownames(m), stringsAsFactors = FALSE), df)
  rownames(df) <- NULL
  df
}

#' @keywords internal
hclust_variables_dendrogram_segments_table <- function(x) {
  x$segments
}

#' @keywords internal
hclust_variables_dendrogram_leaves_table <- function(x) {
  x$leaf_order %>%
    dplyr::mutate(Cluster = as.integer(x$clusters_display[variable])) %>%
    dplyr::rename(Variable = variable, `Display Order` = display_order)
}
