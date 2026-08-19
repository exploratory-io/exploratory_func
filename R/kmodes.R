#' K-Modes clustering for the Analytics View (issue #27877).
#'
#' K-Modes clusters CATEGORICAL data. Each cluster is represented by its Mode --
#' the combination of the most frequent category of every selected variable --
#' and a row's distance to a cluster is the number of variables whose value
#' differs from that Mode (the simple matching dissimilarity).
#'
#' The algorithm is implemented in this file rather than taken from `klaR` so
#' the package gains no new dependency (and so tam needs no installer-manifest /
#' per-platform binary rebuild).
#'
#' Deliberately NOT reused from K-Means: within-cluster sum of squares, cluster
#' means, standardized means and the PCA biplot. The cluster map here is MCA
#' based, and MCA coordinates are an interpretation aid -- they are NOT the
#' space K-Modes optimizes in.

# Numeric columns with at most this many distinct non-NA values are kept as
# categories under numeric_handling = "auto"; anything wider is binned.
KMODES_AUTO_CATEGORY_MAX_DISTINCT <- 10

# A p-value smaller than this is reported to the UI as-is; the display layer
# renders it as "< 0.001". Kept here only as documentation of the contract --
# no rounding happens R-side, so the display layer can still tell 0 from 1e-40.

#' Convert one column to the character categories K-Modes works on.
#'
#' The clustering algorithm itself only asks "same category or not", so it does
#' not care about display order (`kmodes_prepare_data`'s own integer encoding
#' still uses `sort(unique(...))`, unchanged). But the REPORT (Category
#' Composition Ratio by Cluster, Categories that Characterize Each Cluster)
#' shows these values as row headers, and those must honor the column's own
#' display order -- a user-declared factor's `levels()`, or the natural
#' ascending bin order `cut()` already assigns before it gets flattened to
#' character. `display_levels` carries that order forward; NULL means "no
#' explicit order" (character / logical / values-as-category numeric), and the
#' caller falls back to `sort(unique(...))`, i.e. today's behavior (#37936).
#' @param x The column.
#' @param numeric_handling One of "auto", "as_category", "equal_width".
#' @param numeric_bins Number of equal-width bins used when binning.
#' @return A list of the converted character vector, how a numeric column was
#'   treated, and the column's display level order (or NULL).
kmodes_prepare_column <- function(x, numeric_handling = "auto", numeric_bins = 10) {
  if (is.factor(x)) {
    # Ordered factors lose their order on purpose FOR THE ALGORITHM: K-Modes
    # only ever asks "same category or not", never "how far apart" -- but the
    # DISPLAY order (levels(x)) is preserved via display_levels below.
    return(list(values = as.character(x), conversion = NA_character_, display_levels = levels(x)))
  }
  if (is.logical(x)) {
    return(list(values = as.character(x), conversion = NA_character_, display_levels = NULL))
  }
  if (is.character(x)) {
    return(list(values = x, conversion = NA_character_, display_levels = NULL))
  }
  if (is.numeric(x)) {
    finite_values <- x[!is.na(x)]
    method <- numeric_handling
    if (identical(method, "auto")) {
      method <- if (dplyr::n_distinct(finite_values) <= KMODES_AUTO_CATEGORY_MAX_DISTINCT) {
        "as_category"
      } else {
        "equal_width"
      }
    }
    if (identical(method, "as_category")) {
      return(list(values = as.character(x), conversion = "as_category", display_levels = NULL))
    }
    # equal_width. A constant column cannot be cut, so fall back to categories.
    if (length(finite_values) == 0 || dplyr::n_distinct(finite_values) <= 1) {
      return(list(values = as.character(x), conversion = "as_category", display_levels = NULL))
    }
    binned <- cut(x, breaks = numeric_bins, include.lowest = TRUE)
    # levels(binned) is cut()'s own ascending bin order -- capture it BEFORE
    # as.character() flattens it, or "(10,11]" sorts before "(2,3]" as text.
    return(list(values = as.character(binned), conversion = "equal_width", display_levels = levels(binned)))
  }
  stop(paste0("K-Modes does not support the type of the selected variable: ", class(x)[[1]],
              ". Select character, factor, logical or numeric variables."))
}

#' Convert the selected columns of a data frame to K-Modes categories.
#' @param df The data frame.
#' @param selected_cols Names of the columns to convert.
#' @param numeric_handling One of "auto", "as_category", "equal_width".
#' @param numeric_bins Number of equal-width bins.
#' @return A list of the prepared data frame (all character), numeric
#'   conversion metadata, and a name-keyed list of each column's display
#'   level order (or NULL) -- see `kmodes_prepare_column`.
kmodes_prepare_data <- function(df, selected_cols, numeric_handling = "auto", numeric_bins = 10) {
  prepared <- list()
  conversions <- list()
  display_levels <- list()
  for (col in selected_cols) {
    converted <- kmodes_prepare_column(df[[col]], numeric_handling = numeric_handling,
                                       numeric_bins = numeric_bins)
    prepared[[col]] <- converted$values
    if (!is.na(converted$conversion)) {
      conversions[[col]] <- converted$conversion
    }
    display_levels[[col]] <- converted$display_levels
  }
  numeric_conversion <- if (length(conversions) == 0) {
    tibble::tibble(variable = character(0), conversion = character(0))
  } else {
    tibble::tibble(variable = names(conversions), conversion = unlist(conversions, use.names = FALSE))
  }
  list(prepared = tibble::as_tibble(prepared), numeric_conversion = numeric_conversion,
       display_levels = display_levels)
}

#' Display level order for each selected variable.
#'
#' The report tables are LONG (`variable`, `category`) tables. A single factor
#' cannot represent two variables that share a category label but declare
#' different orders, so keep the per-variable vectors for the numeric
#' `category_order` sort key as well as building a compatibility union below.
#' @param prepared_df Prepared (all character) data frame of the rows used.
#' @param display_levels Name-keyed list from `kmodes_prepare_data()` (NULL
#'   entries allowed, and allowed to be NULL itself for full backward
#'   compatibility with old callers).
#' @return Name-keyed list of category levels for each variable.
kmodes_category_display_levels_by_variable <- function(prepared_df, display_levels) {
  cols <- names(prepared_df)
  levels_by_variable <- lapply(cols, function(col) {
    dl <- if (!is.null(display_levels)) display_levels[[col]] else NULL
    if (!is.null(dl)) as.character(dl) else sort(unique(prepared_df[[col]]))
  })
  stats::setNames(levels_by_variable, cols)
}

#' Compatibility factor levels for a LONG (variable, category) column.
#'
#' The factor is retained for consumers that only support one global category
#' order. Consumers that need the exact source order for every variable should
#' use the `category_order` column returned by the report tables.
#' @param prepared_df Prepared (all character) data frame of the rows used.
#' @param display_levels Name-keyed list from `kmodes_prepare_data()`, or NULL.
#' @return Character vector of the factor levels, in variable order.
kmodes_category_display_levels <- function(prepared_df, display_levels) {
  unique(unlist(kmodes_category_display_levels_by_variable(prepared_df, display_levels),
                use.names = FALSE))
}

#' The most frequent value, with a deterministic tie-break.
#'
#' Ties go to the value that sorts first so that the same input always yields
#' the same Mode regardless of row order.
#' @param x A vector of category codes or labels.
#' @return The modal value.
kmodes_mode_value <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA)
  }
  counts <- table(x)
  best <- names(counts)[counts == max(counts)]
  # table() names are always character, so a numeric input has to be sorted back as
  # numbers -- otherwise code 10 would beat code 9 and the Mode chosen while fitting
  # would disagree with the label-based Mode flag in the characteristic-category table.
  if (is.numeric(x)) {
    return(sort(as.numeric(best))[[1]])
  }
  sort(best)[[1]]
}

#' Mismatch count between every row of a code matrix and one Mode.
#' @param mat Integer matrix of category codes.
#' @param mode_row Integer vector, one code per column.
#' @return Integer vector of mismatch counts, one per row.
kmodes_mismatch_to_mode <- function(mat, mode_row) {
  rowSums(mat != matrix(mode_row, nrow = nrow(mat), ncol = ncol(mat), byrow = TRUE))
}

#' Assign every row to its closest Mode.
#'
#' Ties are broken towards the lowest cluster index, which keeps the result
#' reproducible.
#' @param mat Integer matrix of category codes.
#' @param modes_mat Integer matrix, one row per cluster.
#' @return A list of the cluster assignment and each row's mismatch count.
kmodes_assign_clusters <- function(mat, modes_mat) {
  k <- nrow(modes_mat)
  distances <- matrix(NA_real_, nrow = nrow(mat), ncol = k)
  for (i in seq_len(k)) {
    distances[, i] <- kmodes_mismatch_to_mode(mat, modes_mat[i, ])
  }
  cluster <- max.col(-distances, ties.method = "first")
  list(cluster = cluster,
       mismatch = distances[cbind(seq_len(nrow(mat)), cluster)])
}

#' Recompute every cluster's Mode from its member rows.
#' @param mat Integer matrix of category codes.
#' @param cluster Integer cluster assignment.
#' @param k Number of clusters.
#' @param previous_modes Modes to keep for clusters that ended up empty.
#' @return An integer matrix of Modes, one row per cluster.
kmodes_recompute_modes <- function(mat, cluster, k, previous_modes) {
  modes_mat <- previous_modes
  for (i in seq_len(k)) {
    rows <- which(cluster == i)
    if (length(rows) == 0) {
      next
    }
    member <- mat[rows, , drop = FALSE]
    modes_mat[i, ] <- apply(member, 2, kmodes_mode_value)
  }
  modes_mat
}

#' One K-Modes run from one random initialization.
#' @param mat Integer matrix of category codes.
#' @param k Number of clusters.
#' @param iter_max Maximum number of iterations.
#' @return A list of the assignment, Modes, per-row mismatch and total cost.
kmodes_run_once <- function(mat, k, iter_max) {
  n <- nrow(mat)
  # Seed from distinct rows so two clusters never start on the same pattern.
  distinct_rows <- unique(mat)
  seed_idx <- sample.int(nrow(distinct_rows), k)
  modes_mat <- distinct_rows[seed_idx, , drop = FALSE]

  assignment <- kmodes_assign_clusters(mat, modes_mat)
  for (iteration in seq_len(iter_max)) {
    modes_mat <- kmodes_recompute_modes(mat, assignment$cluster, k, modes_mat)
    # An empty cluster is re-seeded with the row that fits its own cluster worst,
    # which is the standard way to keep k clusters alive.
    empty <- setdiff(seq_len(k), unique(assignment$cluster))
    if (length(empty) > 0) {
      worst <- order(assignment$mismatch, decreasing = TRUE)
      for (i in seq_along(empty)) {
        if (i <= length(worst)) {
          modes_mat[empty[[i]], ] <- mat[worst[[i]], ]
        }
      }
    }
    next_assignment <- kmodes_assign_clusters(mat, modes_mat)
    converged <- identical(next_assignment$cluster, assignment$cluster) && length(empty) == 0
    assignment <- next_assignment
    if (converged) {
      break
    }
  }
  modes_mat <- kmodes_recompute_modes(mat, assignment$cluster, k, modes_mat)
  assignment <- kmodes_assign_clusters(mat, modes_mat)
  list(cluster = assignment$cluster,
       modes = modes_mat,
       mismatch = assignment$mismatch,
       total_dissimilarity = sum(assignment$mismatch))
}

#' Fit K-Modes, keeping the best of several random starts.
#' @param mat Integer matrix of category codes.
#' @param k Number of clusters.
#' @param iter_max Maximum number of iterations per start.
#' @param nstart Number of random starts.
#' @return The best run, as returned by kmodes_run_once.
kmodes_fit <- function(mat, k, iter_max = 10, nstart = 5) {
  best <- NULL
  for (start in seq_len(max(1, nstart))) {
    candidate <- kmodes_run_once(mat, k, iter_max)
    if (is.null(best) || candidate$total_dissimilarity < best$total_dissimilarity) {
      best <- candidate
    }
  }
  best
}

#' Simple matching dissimilarity between all pairs of rows.
#'
#' The value is the number of variables whose category differs, i.e. exactly the
#' distance K-Modes minimizes -- not a Euclidean distance on encoded numbers.
#' @param mat Integer matrix of category codes.
#' @return A "dist" object of mismatch counts.
kmodes_matching_distance <- function(mat) {
  n <- nrow(mat)
  accumulated <- numeric(n * (n - 1) / 2)
  for (j in seq_len(ncol(mat))) {
    # dist() on a single column is |code_a - code_b|, so a non-zero value is a
    # mismatch. It runs in C, which matters because this is the O(n^2) step.
    accumulated <- accumulated + as.numeric(as.vector(stats::dist(mat[, j, drop = FALSE])) != 0)
  }
  structure(accumulated, Size = n, Diag = FALSE, Upper = FALSE,
            method = "simple_matching", class = "dist")
}

#' Row indexes to use for the O(n^2) silhouette computation.
#' @param n Number of rows.
#' @param sample_size Maximum rows to use, or NULL for all of them.
#' @return A sorted integer vector of row indexes.
kmodes_silhouette_sample_index <- function(n, sample_size = NULL) {
  size <- suppressWarnings(as.numeric(sample_size))
  if (length(size) != 1 || is.na(size) || size < 1 || n <= size) {
    return(seq_len(n))
  }
  sort(sample.int(n, floor(size)))
}

#' Per-row silhouette scores based on the simple matching dissimilarity.
#' @param cluster Integer cluster assignment for every row.
#' @param mat Integer matrix of category codes.
#' @param sample_idx Rows to compute on; the rest come back as NA.
#' @return A tibble with one row per input row.
kmodes_silhouette_per_row <- function(cluster, mat, sample_idx = NULL) {
  n <- length(cluster)
  empty <- tibble::tibble(silhouette_score = rep(NA_real_, n),
                          nearest_cluster = rep(NA_integer_, n))
  if (is.null(sample_idx)) {
    sample_idx <- seq_len(n)
  }
  sampled_cluster <- cluster[sample_idx]
  if (dplyr::n_distinct(sampled_cluster) < 2 || length(sample_idx) < 3) {
    return(empty)
  }
  result <- tryCatch({
    d <- kmodes_matching_distance(mat[sample_idx, , drop = FALSE])
    sil <- cluster::silhouette(sampled_cluster, d)
    if (is.null(sil) || !is.matrix(sil)) {
      NULL
    } else {
      sil
    }
  }, error = function(e) NULL)
  if (is.null(result)) {
    return(empty)
  }
  empty$silhouette_score[sample_idx] <- as.numeric(result[, "sil_width"])
  empty$nearest_cluster[sample_idx] <- as.integer(result[, "neighbor"])
  empty
}

#' Cluster-count comparison table for the silhouette method.
#' @param mat Integer matrix of category codes.
#' @param max_centers Largest number of clusters to try.
#' @param iter_max Maximum iterations per fit.
#' @param nstart Random starts per fit.
#' @param sample_idx Rows used for the silhouette computation.
#' @param selected_fit The fit the report itself shows, reused at its own cluster count.
#' @param selected_k The cluster count of that fit.
#' @return A tibble with one row per candidate cluster count.
kmodes_iterate_silhouette <- function(mat, max_centers, iter_max, nstart, sample_idx,
                                      selected_fit = NULL, selected_k = NULL) {
  upper <- min(max_centers, nrow(unique(mat)) - 1)
  if (is.na(upper) || upper < 2) {
    return(tibble::tibble(center = integer(0), avg_silhouette = numeric(0),
                          min_silhouette = numeric(0), pct_negative = numeric(0)))
  }
  sampled_mat <- mat[sample_idx, , drop = FALSE]
  d <- kmodes_matching_distance(sampled_mat)
  rows <- purrr::map(2:upper, function(k) {
    # At the cluster count the report is actually showing, reuse that fit. Refitting would
    # land on its own local optimum, and the table would then describe a different -- often
    # worse -- clustering than every other section of the report.
    fit <- if (!is.null(selected_fit) && identical(as.integer(k), as.integer(selected_k))) {
      selected_fit
    } else {
      kmodes_fit(mat, k, iter_max = iter_max, nstart = nstart)
    }
    sampled_cluster <- fit$cluster[sample_idx]
    if (dplyr::n_distinct(sampled_cluster) < 2) {
      return(tibble::tibble(center = k, avg_silhouette = NA_real_,
                            min_silhouette = NA_real_, pct_negative = NA_real_))
    }
    sil <- tryCatch(cluster::silhouette(sampled_cluster, d), error = function(e) NULL)
    if (is.null(sil) || !is.matrix(sil)) {
      return(tibble::tibble(center = k, avg_silhouette = NA_real_,
                            min_silhouette = NA_real_, pct_negative = NA_real_))
    }
    widths <- as.numeric(sil[, "sil_width"])
    tibble::tibble(center = k,
                   avg_silhouette = mean(widths, na.rm = TRUE),
                   min_silhouette = min(widths, na.rm = TRUE),
                   pct_negative = sum(widths < 0, na.rm = TRUE) / length(widths))
  })
  dplyr::bind_rows(rows)
}

#' Cluster-count comparison table for the elbow method.
#'
#' The y value is the TOTAL MISMATCH COUNT, which is the function K-Modes
#' minimizes. The K-Means within-cluster sum of squares has no meaning here.
#' @param mat Integer matrix of category codes.
#' @param max_centers Largest number of clusters to try.
#' @param iter_max Maximum iterations per fit.
#' @param nstart Random starts per fit.
#' @param selected_fit The fit the report itself shows, reused at its own cluster count.
#' @param selected_k The cluster count of that fit.
#' @return A tibble with one row per candidate cluster count.
kmodes_iterate_elbow <- function(mat, max_centers, iter_max, nstart,
                                 selected_fit = NULL, selected_k = NULL) {
  upper <- min(max_centers, nrow(unique(mat)))
  if (is.na(upper) || upper < 1) {
    return(tibble::tibble(center = integer(0), total_dissimilarity = numeric(0),
                          avg_dissimilarity = numeric(0), decrease_ratio = numeric(0)))
  }
  rows <- purrr::map(seq_len(upper), function(k) {
    # Same reasoning as the silhouette table: the selected cluster count must report the
    # clustering the rest of the report describes.
    fit <- if (!is.null(selected_fit) && identical(as.integer(k), as.integer(selected_k))) {
      selected_fit
    } else {
      kmodes_fit(mat, k, iter_max = iter_max, nstart = nstart)
    }
    tibble::tibble(center = k,
                   total_dissimilarity = fit$total_dissimilarity,
                   avg_dissimilarity = fit$total_dissimilarity / nrow(mat))
  })
  dplyr::bind_rows(rows) %>%
    dplyr::mutate(decrease_ratio = (dplyr::lag(total_dissimilarity) - total_dissimilarity) /
                    dplyr::lag(total_dissimilarity))
}

#' Cramer's V between the cluster assignment and every selected variable.
#'
#' The chi-square p-value is descriptive context only: the clusters were built
#' FROM these variables, so testing them against the clusters is circular.
#' Ranking is by Cramer's V.
#' @param prepared_df Prepared (all character) data frame of the rows used.
#' @param cluster Integer cluster assignment.
#' @return A tibble sorted by Cramer's V, descending.
kmodes_variable_importance <- function(prepared_df, cluster) {
  rows <- purrr::map(names(prepared_df), function(col) {
    contingency <- table(cluster, prepared_df[[col]])
    if (nrow(contingency) < 2 || ncol(contingency) < 2) {
      return(tibble::tibble(variable = col, cramers_v = NA_real_, chi_square = NA_real_,
                            df = NA_real_, p_value = NA_real_))
    }
    test <- suppressWarnings(stats::chisq.test(contingency))
    n <- sum(contingency)
    denominator <- n * (min(dim(contingency)) - 1)
    tibble::tibble(variable = col,
                   cramers_v = if (denominator > 0) sqrt(as.numeric(test$statistic) / denominator) else NA_real_,
                   chi_square = as.numeric(test$statistic),
                   df = as.numeric(test$parameter),
                   p_value = as.numeric(test$p.value))
  })
  dplyr::bind_rows(rows) %>% dplyr::arrange(dplyr::desc(cramers_v))
}

#' Categories that are unusually common or rare inside each cluster.
#' @param prepared_df Prepared (all character) data frame of the rows used.
#' @param cluster Integer cluster assignment.
#' @param display_levels Name-keyed list from `kmodes_prepare_data()`, or NULL
#'   to fall back to `sort(unique(...))` per variable (#37936).
#' @return A tibble of one row per cluster, variable and category. `category`
#'   is a factor for compatibility with global-order consumers, while
#'   `category_order` preserves the exact per-variable order when two
#'   variables reuse labels with different orders.
kmodes_characteristic_categories <- function(prepared_df, cluster, display_levels = NULL) {
  total_n <- length(cluster)
  category_levels_by_variable <- kmodes_category_display_levels_by_variable(prepared_df, display_levels)
  cluster_sizes <- as.data.frame(table(cluster), stringsAsFactors = FALSE)
  names(cluster_sizes) <- c("cluster", "cluster_size")
  cluster_sizes$cluster <- as.integer(cluster_sizes$cluster)

  rows <- purrr::map(names(prepared_df), function(col) {
    values <- prepared_df[[col]]
    overall <- as.data.frame(table(values), stringsAsFactors = FALSE)
    names(overall) <- c("category", "overall_count")
    joint <- as.data.frame(table(cluster, values), stringsAsFactors = FALSE)
    names(joint) <- c("cluster", "category", "observed")
    joint$cluster <- as.integer(joint$cluster)
    modes_by_cluster <- tibble::tibble(cluster = cluster, category = values) %>%
      dplyr::group_by(cluster) %>%
      dplyr::summarize(mode_category = kmodes_mode_value(category), .groups = "drop")

    joint %>%
      dplyr::left_join(overall, by = "category") %>%
      dplyr::left_join(cluster_sizes, by = "cluster") %>%
      dplyr::left_join(modes_by_cluster, by = "cluster") %>%
      dplyr::mutate(
        variable = col,
        expected = cluster_size * overall_count / total_n,
        cluster_pct = observed / cluster_size,
        overall_pct = overall_count / total_n,
        # A category absent from the whole data cannot have a ratio; guard the
        # division rather than letting Inf/NaN reach the table.
        observed_expected_ratio = dplyr::if_else(overall_pct > 0, cluster_pct / overall_pct, NA_real_),
        adjusted_standardized_residual = kmodes_adjusted_residual(observed, expected,
                                                                  cluster_size / total_n,
                                                                  overall_count / total_n),
        is_mode = category == mode_category,
        category_order = match(category, category_levels_by_variable[[col]])
      ) %>%
      dplyr::select(cluster, variable, category, category_order, observed, expected, cluster_pct, overall_pct,
                    observed_expected_ratio, adjusted_standardized_residual, is_mode)
  })
  category_levels <- kmodes_category_display_levels(prepared_df, display_levels)
  dplyr::bind_rows(rows) %>%
    dplyr::mutate(category = factor(category, levels = category_levels)) %>%
    dplyr::arrange(cluster, dplyr::desc(abs(adjusted_standardized_residual)))
}

#' Adjusted standardized residual of a contingency cell.
#' @param observed Observed count.
#' @param expected Expected count.
#' @param row_prop Proportion of rows in this cluster.
#' @param col_prop Proportion of rows in this category.
#' @return The adjusted standardized residual, or NA when it is undefined.
kmodes_adjusted_residual <- function(observed, expected, row_prop, col_prop) {
  denominator <- expected * (1 - row_prop) * (1 - col_prop)
  dplyr::if_else(denominator > 0, (observed - expected) / sqrt(denominator), NA_real_)
}

#' Full category distribution of every variable within every cluster.
#' @param prepared_df Prepared (all character) data frame of the rows used.
#' @param cluster Integer cluster assignment.
#' @param importance The variable importance table, used for the default display order.
#' @param display_levels Name-keyed list from `kmodes_prepare_data()`, or NULL
#'   to fall back to `sort(unique(...))` per variable (#37936).
#' @return A tibble of one row per variable, cluster and category. `category`
#'   is a factor for compatibility with global-order consumers, while
#'   `category_order` preserves the exact per-variable order.
kmodes_category_composition <- function(prepared_df, cluster, importance, display_levels = NULL) {
  category_levels_by_variable <- kmodes_category_display_levels_by_variable(prepared_df, display_levels)
  order_lookup <- importance %>%
    dplyr::mutate(variable_order = dplyr::row_number()) %>%
    dplyr::select(variable, cramers_v, variable_order)
  # The order the variables were selected in, so the report can switch back to it.
  original_lookup <- tibble::tibble(variable = names(prepared_df),
                                    original_order = seq_along(names(prepared_df)))

  rows <- purrr::map(names(prepared_df), function(col) {
    counts <- as.data.frame(table(cluster, prepared_df[[col]]), stringsAsFactors = FALSE)
    names(counts) <- c("cluster", "category", "n")
    counts$cluster <- as.integer(counts$cluster)
    counts %>%
      dplyr::group_by(cluster) %>%
      # The denominator is the number of valid rows for this cluster and variable.
      dplyr::mutate(pct = if (sum(n) > 0) n / sum(n) else rep(NA_real_, dplyr::n())) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(variable = col,
                    category_order = match(category, category_levels_by_variable[[col]])) %>%
      dplyr::select(variable, cluster, category, category_order, n, pct)
  })
  category_levels <- kmodes_category_display_levels(prepared_df, display_levels)
  dplyr::bind_rows(rows) %>%
    dplyr::left_join(order_lookup, by = "variable") %>%
    dplyr::left_join(original_lookup, by = "variable") %>%
    dplyr::mutate(category = factor(category, levels = category_levels)) %>%
    dplyr::arrange(variable_order, cluster, category_order)
}

#' MCA coordinates for observations, categories and cluster representatives.
#'
#' MCA summarizes which categories co-occur. It is an interpretation aid, NOT a
#' picture of the mismatch distance K-Modes optimizes, and the representative
#' position is the mean coordinate of a cluster's rows -- never the Mode.
#' @param prepared_df Prepared (all character) data frame of the rows used.
#' @param cluster Integer cluster assignment.
#' @param characteristic_categories Table used to flag the categories worth labelling.
#' @param map_sample_size Maximum observations to place on the map.
#' @param map_category_top_n Number of characteristic categories to flag.
#' @return A tibble of map rows, empty (but typed) when MCA cannot be computed.
kmodes_build_mca_map <- function(prepared_df, cluster, characteristic_categories,
                                 map_sample_size = 5000, map_category_top_n = 30) {
  empty_map <- tibble::tibble(
    row_type = character(0), row_id = integer(0), cluster = integer(0),
    variable = character(0), category = character(0),
    Dim1 = numeric(0), Dim2 = numeric(0), label = character(0),
    is_representative = logical(0), is_characteristic_category = logical(0),
    dim1_pct_variance = numeric(0), dim2_pct_variance = numeric(0))

  tryCatch({
    n <- nrow(prepared_df)
    idx <- kmodes_silhouette_sample_index(n, map_sample_size)
    sampled <- prepared_df[idx, , drop = FALSE]
    sampled_cluster <- cluster[idx]
    factor_df <- as.data.frame(lapply(sampled, factor), stringsAsFactors = TRUE)
    names(factor_df) <- names(sampled)
    # A variable with a single category adds no information and makes MCA fail.
    usable <- vapply(factor_df, function(x) nlevels(x) >= 2, logical(1))
    factor_df <- factor_df[, usable, drop = FALSE]
    if (ncol(factor_df) < 2 || nrow(factor_df) < 3) {
      return(empty_map)
    }
    fit <- FactoMineR::MCA(factor_df, ncp = 2, graph = FALSE)

    eig <- as.data.frame(fit$eig)
    pct <- eig[["percentage of variance"]]
    dim1_pct <- if (length(pct) >= 1) pct[[1]] else NA_real_
    dim2_pct <- if (length(pct) >= 2) pct[[2]] else NA_real_

    ind_coord <- as.data.frame(fit$ind$coord)
    observations <- tibble::tibble(
      row_type = "observation",
      row_id = idx,
      cluster = as.integer(sampled_cluster),
      variable = NA_character_,
      category = NA_character_,
      Dim1 = as.numeric(ind_coord[[1]]),
      Dim2 = if (ncol(ind_coord) >= 2) as.numeric(ind_coord[[2]]) else 0)
    observations$label <- as.character(observations$cluster)

    var_coord <- as.data.frame(fit$var$coord)
    # FactoMineR builds the indicator matrix column by column, so the category
    # rows come back grouped by variable in the same order.
    level_counts <- vapply(factor_df, nlevels, integer(1))
    variable_of_category <- rep(names(level_counts), times = level_counts)
    category_labels <- unlist(lapply(factor_df, levels), use.names = FALSE)
    if (length(variable_of_category) != nrow(var_coord)) {
      variable_of_category <- rep(NA_character_, nrow(var_coord))
      category_labels <- rownames(var_coord)
    }
    categories <- tibble::tibble(
      row_type = "category",
      row_id = NA_integer_,
      cluster = NA_integer_,
      variable = variable_of_category,
      category = category_labels,
      Dim1 = as.numeric(var_coord[[1]]),
      Dim2 = if (ncol(var_coord) >= 2) as.numeric(var_coord[[2]]) else 0)
    categories$label <- ifelse(is.na(categories$variable), categories$category,
                               paste0(categories$variable, ": ", categories$category))

    representatives <- observations %>%
      dplyr::group_by(cluster) %>%
      dplyr::summarize(Dim1 = mean(Dim1, na.rm = TRUE), Dim2 = mean(Dim2, na.rm = TRUE),
                       .groups = "drop") %>%
      dplyr::mutate(row_type = "cluster_representative", row_id = NA_integer_,
                    variable = NA_character_, category = NA_character_,
                    label = paste0("Cluster ", cluster)) %>%
      dplyr::select(row_type, row_id, cluster, variable, category, Dim1, Dim2, label)

    top_categories <- characteristic_categories %>%
      dplyr::arrange(dplyr::desc(abs(adjusted_standardized_residual))) %>%
      dplyr::slice_head(n = max(0, map_category_top_n)) %>%
      dplyr::distinct(variable, category)

    dplyr::bind_rows(observations, categories, representatives) %>%
      dplyr::mutate(
        is_representative = row_type == "cluster_representative",
        is_characteristic_category = row_type == "category" &
          paste0(variable, "", category) %in%
            paste0(top_categories$variable, "", top_categories$category),
        dim1_pct_variance = dim1_pct,
        dim2_pct_variance = dim2_pct) %>%
      dplyr::select(row_type, row_id, cluster, variable, category, Dim1, Dim2, label,
                    is_representative, is_characteristic_category,
                    dim1_pct_variance, dim2_pct_variance)
  }, error = function(e) {
    empty_map
  })
}

#' K-Modes clustering for categorical variables.
#'
#' @param df Data frame.
#' @param ... Columns to cluster on.
#' @param centers Number of clusters.
#' @param iter.max Maximum iterations per random start.
#' @param nstart Number of random starts.
#' @param seed Random seed.
#' @param max_nrow Sample the data down to this many rows before fitting.
#' @param numeric_handling How numeric columns become categories.
#' @param numeric_bins Number of equal-width bins for numeric columns.
#' @param elbow_method_mode One of "none", "silhouette", "elbow".
#' @param max_centers Largest cluster count to evaluate.
#' @param silhouette_sample_size Rows used for the silhouette computation.
#' @param feature_top_n Characteristic categories to flag per cluster.
#' @param map_sample_size Observations placed on the cluster map.
#' @param map_category_top_n Categories labelled on the cluster map.
#' @export
exp_kmodes <- function(df, ...,
                       centers = 3,
                       iter.max = 10,
                       nstart = 5,
                       seed = 1,
                       max_nrow = NULL,
                       numeric_handling = "auto",
                       numeric_bins = 10,
                       elbow_method_mode = "silhouette",
                       max_centers = 10,
                       silhouette_sample_size = 5000,
                       feature_top_n = 10,
                       map_sample_size = 5000,
                       map_category_top_n = 30) {
  selected_cols <- tidyselect::vars_select(names(df), !!! rlang::quos(...))
  grouped_cols <- grouped_by(df)
  if (any(selected_cols %in% grouped_cols)) {
    stop("Repeat-By column cannot be used as a variable column.")
  }
  selected_cols <- setdiff(selected_cols, grouped_cols)
  if (length(selected_cols) < 2) {
    stop("K-Modes requires at least 2 variables. Select more variables.")
  }
  numeric_handling <- match.arg(as.character(numeric_handling),
                                c("auto", "as_category", "equal_width"))
  optimal_method <- match.arg(as.character(elbow_method_mode), c("none", "silhouette", "elbow"))
  numeric_bins <- as.integer(numeric_bins)
  if (is.na(numeric_bins) || numeric_bins < 2) {
    stop("Number of Bins must be 2 or larger.")
  }
  centers <- as.integer(centers)
  if (is.na(centers) || centers < 2) {
    stop("Number of Clusters must be 2 or larger.")
  }

  each_func <- function(df) {
    if (!is.null(seed)) {
      set.seed(seed)
    }
    df <- df %>% dplyr::ungroup()
    sampled_nrow <- NULL
    if (!is.null(max_nrow) && nrow(df) > max_nrow) {
      sampled_nrow <- max_nrow
      df <- df %>% sample_rows(max_nrow)
    }

    df_original <- df %>% dplyr::mutate(.kmodes_row_id = dplyr::row_number())
    prepared_all <- kmodes_prepare_data(df_original, selected_cols,
                                        numeric_handling = numeric_handling,
                                        numeric_bins = numeric_bins)
    complete_rows <- stats::complete.cases(prepared_all$prepared)
    used_row_ids <- df_original$.kmodes_row_id[complete_rows]
    excluded_nrow <- sum(!complete_rows)
    prepared_df <- prepared_all$prepared[complete_rows, , drop = FALSE]

    if (nrow(prepared_df) == 0) {
      stop("There is no row left after removing rows with missing values in the selected variables.")
    }
    distinct_patterns <- nrow(dplyr::distinct(prepared_df))
    if (centers > distinct_patterns) {
      stop(paste0("Number of Clusters (", centers, ") is larger than the number of distinct category patterns (",
                  distinct_patterns, "). Reduce the number of clusters."))
    }

    # Encode to integer codes once. Every later step -- fitting, the distance
    # matrix, the Modes -- works on these codes, and the levels map them back.
    levels_by_col <- lapply(prepared_df, function(x) sort(unique(x)))
    mat <- vapply(names(prepared_df), function(col) {
      match(prepared_df[[col]], levels_by_col[[col]])
    }, integer(nrow(prepared_df)))
    mat <- matrix(mat, nrow = nrow(prepared_df), dimnames = list(NULL, names(prepared_df)))

    fit <- kmodes_fit(mat, centers, iter_max = iter.max, nstart = nstart)
    n_used <- nrow(mat)
    n_variables <- ncol(mat)

    modes_labels <- as.data.frame(lapply(seq_len(n_variables), function(j) {
      levels_by_col[[j]][fit$modes[, j]]
    }), stringsAsFactors = FALSE)
    names(modes_labels) <- names(prepared_df)
    modes_labels$cluster <- seq_len(centers)

    sil_idx <- kmodes_silhouette_sample_index(n_used, silhouette_sample_size)
    silhouette <- kmodes_silhouette_per_row(fit$cluster, mat, sample_idx = sil_idx)

    row_metrics <- tibble::tibble(
      .kmodes_row_id = used_row_ids,
      cluster = fit$cluster,
      dissimilarity_to_mode = fit$mismatch,
      dissimilarity_rate = fit$mismatch / n_variables,
      similarity_to_mode = 1 - fit$mismatch / n_variables,
      silhouette_score = silhouette$silhouette_score,
      nearest_cluster = silhouette$nearest_cluster)

    importance <- kmodes_variable_importance(prepared_df, fit$cluster)
    characteristic <- kmodes_characteristic_categories(prepared_df, fit$cluster,
                                                       display_levels = prepared_all$display_levels)
    composition <- kmodes_category_composition(prepared_df, fit$cluster, importance,
                                                display_levels = prepared_all$display_levels)
    map <- kmodes_build_mca_map(prepared_df, fit$cluster, characteristic,
                                map_sample_size = map_sample_size,
                                map_category_top_n = map_category_top_n)

    silhouette_result <- if (optimal_method == "silhouette") {
      kmodes_iterate_silhouette(mat, max_centers, iter.max, nstart, sil_idx,
                                selected_fit = fit, selected_k = centers)
    } else {
      NULL
    }
    elbow_result <- if (optimal_method == "elbow") {
      kmodes_iterate_elbow(mat, max_centers, iter.max, nstart,
                           selected_fit = fit, selected_k = centers)
    } else {
      NULL
    }

    model <- list(
      df_original = df_original,
      selected_cols = selected_cols,
      prepared_df = prepared_df,
      levels_by_col = levels_by_col,
      numeric_conversion = prepared_all$numeric_conversion,
      cluster = fit$cluster,
      modes = modes_labels,
      row_metrics = row_metrics,
      variable_importance = importance,
      characteristic_categories = characteristic,
      category_composition = composition,
      map = map,
      silhouette_result = silhouette_result,
      elbow_result = elbow_result,
      total_dissimilarity = fit$total_dissimilarity,
      n_used = n_used,
      n_variables = n_variables,
      centers = centers,
      feature_top_n = feature_top_n,
      excluded_nrow = excluded_nrow,
      excluded_pct = if (nrow(df_original) > 0) excluded_nrow / nrow(df_original) else 0,
      sampled_nrow = sampled_nrow,
      grouped_cols = grouped_cols)
    class(model) <- c("kmodes_exploratory", class(model))
    model
  }

  do_on_each_group(df, each_func, name = "model", with_unnest = FALSE)
}

#' Per-cluster summary of a K-Modes model.
#' @param x A kmodes_exploratory model.
#' @param with_excluded_rows Append a final row for the rows excluded from the fit.
#' @return A tibble with one row per cluster.
kmodes_summary_table <- function(x, with_excluded_rows = FALSE) {
  summary_df <- x$row_metrics %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarize(
      size = dplyr::n(),
      avg_matching_rate_to_mode = mean(similarity_to_mode, na.rm = TRUE),
      avg_dissimilarity_to_mode = mean(dissimilarity_to_mode, na.rm = TRUE),
      avg_dissimilarity_rate = mean(dissimilarity_rate, na.rm = TRUE),
      avg_silhouette = if (all(is.na(silhouette_score))) NA_real_ else mean(silhouette_score, na.rm = TRUE),
      min_silhouette = if (all(is.na(silhouette_score))) NA_real_ else min(silhouette_score, na.rm = TRUE),
      pct_negative = if (all(is.na(silhouette_score))) NA_real_ else
        sum(silhouette_score < 0, na.rm = TRUE) / sum(!is.na(silhouette_score)),
      .groups = "drop") %>%
    dplyr::mutate(pct_size = size / sum(size, na.rm = TRUE)) %>%
    dplyr::select(cluster, size, pct_size, avg_matching_rate_to_mode, avg_dissimilarity_to_mode,
                  avg_dissimilarity_rate, avg_silhouette, min_silhouette, pct_negative) %>%
    dplyr::left_join(x$modes, by = "cluster") %>%
    dplyr::mutate(cluster = as.character(cluster))

  if (with_excluded_rows && x$excluded_nrow > 0) {
    summary_df <- summary_df %>%
      tibble::add_row(cluster = NA_character_, size = x$excluded_nrow,
                      pct_size = x$excluded_nrow / nrow(x$df_original))
  }
  summary_df
}

#' "Analysis Conditions and Data" table of a K-Modes model (issue #37682, the K-Modes
#' follow-up to the FA/PCA/Cronbach/Correlation summary-section standardization done in
#' tam#37638). One row per condition, Metric/Value, matching that role model's shape --
#' every value is already a plain string (a count or a comma-joined name list), so no
#' further per-column number formatting is needed on the viz side.
#' @param x A kmodes_exploratory model.
#' @return A tibble with `Metric` and `Value` columns.
kmodes_analysis_conditions_table <- function(x) {
  variable_names_display <- paste(x$selected_cols, collapse = ", ")
  tibble::tibble(
    Metric = c("Number of Variables", "Variable Names", "Row Count", "Rows Removed",
               "Number of Clusters"),
    Value = c(
      as.character(x$n_variables),
      variable_names_display,
      as.character(x$n_used),
      as.character(x$excluded_nrow),
      as.character(x$centers)
    )
  )
}

#' Row-level output of a K-Modes model.
#' @param x A kmodes_exploratory model.
#' @return The original rows with the cluster assignment and diagnostics attached.
kmodes_data_table <- function(x) {
  x$df_original %>%
    dplyr::left_join(x$row_metrics, by = ".kmodes_row_id") %>%
    dplyr::mutate(`Is Excluded` = is.na(cluster)) %>%
    dplyr::select(-.kmodes_row_id, -nearest_cluster) %>%
    dplyr::rename(`Cluster` = cluster,
                  `Dissimilarity to Mode` = dissimilarity_to_mode,
                  `Dissimilarity Rate` = dissimilarity_rate,
                  `Similarity to Mode` = similarity_to_mode,
                  `Silhouette Score` = silhouette_score) %>%
    dplyr::mutate(`Cluster` = factor(`Cluster`))
}

#' Tidy a K-Modes model.
#' @param x A kmodes_exploratory model.
#' @param type What to return. See the branches below.
#' @param with_excluded_rows Append the excluded-row count to the summary.
#' @param ... Unused.
#' @export
tidy.kmodes_exploratory <- function(x, type = "summary", with_excluded_rows = FALSE, ...) {
  if (type == "summary") {
    return(kmodes_summary_table(x, with_excluded_rows = with_excluded_rows))
  }
  if (type == "analysis_conditions") {
    return(kmodes_analysis_conditions_table(x))
  }
  if (type == "modes") {
    return(x$modes)
  }
  if (type == "silhouette") {
    return(if (is.null(x$silhouette_result)) {
      tibble::tibble(center = integer(0), avg_silhouette = numeric(0),
                     min_silhouette = numeric(0), pct_negative = numeric(0))
    } else {
      x$silhouette_result
    })
  }
  if (type == "elbow") {
    return(if (is.null(x$elbow_result)) {
      tibble::tibble(center = integer(0), total_dissimilarity = numeric(0),
                     avg_dissimilarity = numeric(0), decrease_ratio = numeric(0))
    } else {
      x$elbow_result
    })
  }
  if (type == "variable_importance") {
    return(x$variable_importance)
  }
  if (type == "characteristic_categories") {
    return(x$characteristic_categories)
  }
  if (type == "category_composition") {
    return(x$category_composition)
  }
  if (type == "cohesion") {
    return(x$row_metrics %>%
             dplyr::select(cluster, row_id = .kmodes_row_id, dissimilarity_to_mode,
                           dissimilarity_rate, similarity_to_mode))
  }
  if (type == "map") {
    return(x$map)
  }
  if (type == "data") {
    return(kmodes_data_table(x))
  }
  stop(paste0("Unknown tidy type for K-Modes: ", type))
}

#' One-row overview of a K-Modes model.
#' @param x A kmodes_exploratory model.
#' @param ... Unused.
#' @export
glance.kmodes_exploratory <- function(x, ...) {
  tibble::tibble(
    centers = x$centers,
    n_variables = x$n_variables,
    n_used = x$n_used,
    excluded_nrow = x$excluded_nrow,
    excluded_pct = x$excluded_pct,
    total_dissimilarity = x$total_dissimilarity,
    avg_dissimilarity = x$total_dissimilarity / x$n_used)
}
