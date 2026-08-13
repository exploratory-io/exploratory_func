# K-Medoids analytics implementation.

.kmedoids_safe_numeric <- function(x) {
  value <- suppressWarnings(as.numeric(x))
  if (length(value) != 1 || is.na(value) || !is.finite(value)) {
    return(NULL)
  }
  value
}

.kmedoids_pam_max_n <- 5000L
.kmedoids_map_max_n <- 5000L

.kmedoids_silhouette_components <- function(raw) {
  widths <- if (!is.null(raw$silinfo$widths)) {
    raw$silinfo$widths[, 'sil_width']
  } else {
    NULL
  }
  indices <- if (is.null(widths)) {
    integer()
  } else {
    suppressWarnings(as.integer(rownames(raw$silinfo$widths)))
  }
  if (length(indices) != length(widths) || anyNA(indices)) {
    indices <- seq_along(widths)
  }
  list(widths = widths, indices = indices)
}

.kmedoids_distance_to_medoids <- function(mat, medoid_mat, cluster_ids, metric) {
  if (nrow(mat) == 0) return(numeric())
  assigned_medoids <- medoid_mat[as.integer(cluster_ids), , drop = FALSE]
  delta <- mat - assigned_medoids
  if (metric == 'manhattan') {
    rowSums(abs(delta))
  } else {
    sqrt(rowSums(delta^2))
  }
}

.kmedoids_fit <- function(mat, centers, distance, seed = NULL,
                          algorithm = 'pam', clara_samples = 20,
                          clara_sample_size = NULL,
                          pam_max_n = .kmedoids_pam_max_n) {
  algorithm <- match.arg(algorithm, c('auto', 'pam', 'clara'))
  if (algorithm == 'auto') {
    algorithm <- if (nrow(mat) <= pam_max_n) 'pam' else 'clara'
  }
  if (algorithm == 'pam' && nrow(mat) > pam_max_n) {
    stop(
      paste0(
        'PAM is limited to ', format(pam_max_n, big.mark = ','),
        ' valid rows for performance. Select algorithm = "clara" or reduce max_nrow.'
      ),
      call. = FALSE
    )
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (algorithm == 'pam') {
    raw <- cluster::pam(
      mat, k = centers, metric = distance, stand = FALSE,
      keep.diss = FALSE, keep.data = FALSE, pamonce = 5
    )
    silhouette <- .kmedoids_silhouette_components(raw)
    silhouette_widths <- silhouette$widths
    silhouette_row_indices <- silhouette$indices
    medoid_indices <- raw$id.med
  } else {
    if (is.null(clara_sample_size)) {
      clara_sample_size <- min(nrow(mat), max(100L, 40L + 2L * centers))
    }
    clara_sample_size <- as.integer(max(
      centers + 1L,
      min(nrow(mat), pam_max_n, floor(clara_sample_size))
    ))
    raw <- cluster::clara(
      mat, k = centers, metric = distance, stand = FALSE,
      samples = max(1L, as.integer(floor(clara_samples))), sampsize = clara_sample_size,
      keep.data = FALSE, rngR = TRUE
    )
    silhouette <- .kmedoids_silhouette_components(raw)
    silhouette_widths <- silhouette$widths
    silhouette_row_indices <- silhouette$indices
    medoid_indices <- raw$i.med
  }
  list(
    raw = raw,
    algorithm = algorithm,
    is_approximate = identical(algorithm, 'clara'),
    clustering = as.integer(raw$clustering),
    medoids = raw$medoids,
    medoid_indices = as.integer(medoid_indices),
    objective = raw$objective,
    silhouette_widths = silhouette_widths,
    silhouette_row_indices = silhouette_row_indices,
    silhouette_avg = if (is.null(raw$silinfo$avg.width)) {
      NA_real_
    } else {
      as.numeric(raw$silinfo$avg.width)
    },
    clara_sample_size = if (algorithm == 'clara') clara_sample_size else NULL,
    clara_samples = if (algorithm == 'clara') max(1L, as.integer(floor(clara_samples))) else NULL
  )
}

.kmedoids_empty <- function(type) {
  switch(type,
    summary = tibble::tibble(
      cluster = integer(), size = integer(), pct_size = numeric(),
      avg_distance_to_medoid = numeric(), max_distance_to_medoid = numeric(),
      avg_silhouette = numeric(), min_silhouette = numeric(),
      pct_negative = numeric(), medoid_row_id = character()
    ),
    profile = tibble::tibble(
      variable = character(), cluster = integer(), standardized_mean = numeric(),
      effect_size = numeric(), rank = integer(), medoid = numeric(),
      cluster_median = numeric(), cluster_mean = numeric(), overall_median = numeric(),
      overall_mean = numeric()
    ),
    silhouette = tibble::tibble(
      center = integer(), avg_silhouette = numeric(), min_silhouette = numeric(),
      pct_negative = numeric()
    ),
    elbow = tibble::tibble(
      center = integer(), total_distance_to_medoids = numeric(),
      average_distance_to_medoids = numeric(), decrease_ratio = numeric()
    ),
    variable_importance = tibble::tibble(
      variable = character(), eta_squared = numeric(), test_statistic = numeric(),
      p_value = numeric()
    ),
    representative_values = tibble::tibble(
      cluster = integer(), variable = character(), medoid = numeric(),
      cluster_median = numeric(), cluster_mean = numeric(), overall_median = numeric(),
      overall_mean = numeric()
    ),
    distribution = tibble::tibble(
      cluster = integer(), variable = character(), value = numeric(), is_medoid = logical()
    ),
    cohesion = tibble::tibble(cluster = integer(), distance_to_medoid = numeric()),
    map = tibble::tibble(
      Dim1 = numeric(), Dim2 = numeric(), cluster = integer(), row_type = character(),
      label = character()
    ),
    medoid_details = tibble::tibble(cluster = integer(), row_id = character()),
    counts = tibble::tibble(
      original_nrow = integer(), analysis_nrow = integer(), excluded_nrow = integer(),
      exclusion_ratio = numeric(), requested_algorithm = character(),
      effective_algorithm = character(), is_approximate = logical(),
      fit_sample_nrow = integer(), diagnostic_nrow = integer(), map_sample_nrow = integer()
    ),
    data = tibble::tibble(),
    tibble::tibble()
  )
}

.kmedoids_valid_indices <- function(x) {
  if (!is.null(x$valid_indices)) return(x$valid_indices)
  valid <- complete.cases(x$source_data)
  if (ncol(x$source_data) > 0) {
    valid <- valid & apply(x$source_data, 1, function(row) all(is.finite(row)))
  }
  which(valid)
}

.kmedoids_original_fit_mat <- function(x) {
  if (!is.null(x$original_fit_mat)) return(x$original_fit_mat)
  x$source_data[.kmedoids_valid_indices(x), , drop = FALSE] %>% as.matrix()
}

.kmedoids_silhouette_values <- function(x) {
  values <- rep(NA_real_, x$valid_nrow)
  if (length(x$silhouette_widths) > 0 && length(x$silhouette_row_indices) > 0) {
    indices <- x$silhouette_row_indices
    keep <- indices >= 1 & indices <= x$valid_nrow
    values[indices[keep]] <- x$silhouette_widths[keep]
  }
  values
}

.kmedoids_sample_indices <- function(x, max_n, include = integer(), seed_offset = 0L) {
  n <- nrow(x$mat)
  include <- sort(unique(as.integer(include[include >= 1 & include <= n])))
  max_n <- max(1L, min(n, max(floor(max_n), length(include))))
  if (length(include) >= max_n) return(include)
  remaining <- setdiff(seq_len(n), include)
  seed <- if (is.null(x$seed)) NULL else as.integer(x$seed) + seed_offset
  if (!is.null(seed)) set.seed(seed)
  c(include, sample(remaining, max_n - length(include)))
}

.kmedoids_diagnostic_mat <- function(x, seed_offset = 0L) {
  indices <- .kmedoids_sample_indices(
    x, min(x$silhouette_sample_size, .kmedoids_pam_max_n),
    include = x$medoid_indices, seed_offset = seed_offset
  )
  list(mat = x$mat[indices, , drop = FALSE], indices = indices)
}

.kmedoids_summary <- function(x, with_excluded_rows = FALSE) {
  ids <- x$clustering
  distances <- x$distance_to_medoid
  silhouette_widths <- .kmedoids_silhouette_values(x)
  clusters <- sort(unique(ids))
  result <- purrr::map_dfr(clusters, function(cluster_id) {
    index <- which(ids == cluster_id)
    cluster_silhouette <- silhouette_widths[index]
    tibble::tibble(
      cluster = as.integer(cluster_id),
      size = length(index),
      pct_size = length(index) / x$valid_nrow,
      avg_distance_to_medoid = mean(distances[index], na.rm = TRUE),
      max_distance_to_medoid = max(distances[index], na.rm = TRUE),
      avg_silhouette = mean(cluster_silhouette, na.rm = TRUE),
      min_silhouette = min(cluster_silhouette, na.rm = TRUE),
      pct_negative = mean(cluster_silhouette < 0, na.rm = TRUE),
      medoid_row_id = x$row_ids[x$medoid_indices[[cluster_id]]]
    )
  })
  original_mat <- .kmedoids_original_fit_mat(x)
  medoid_values <- original_mat[x$medoid_indices, , drop = FALSE] %>%
    as.data.frame(check.names = FALSE) %>%
    tibble::as_tibble()
  medoid_values$cluster <- seq_len(nrow(medoid_values))
  medoid_values <- medoid_values %>% dplyr::select(cluster, dplyr::everything())
  result <- result %>% dplyr::left_join(medoid_values, by = 'cluster')
  result <- result %>% dplyr::mutate(dplyr::across(
    c(avg_distance_to_medoid, max_distance_to_medoid, avg_silhouette,
      min_silhouette, pct_negative),
    ~ ifelse(is.finite(.x), .x, NA_real_)
  ))
  if (with_excluded_rows && x$excluded_nrow > 0) {
    result <- dplyr::bind_rows(result, tibble::tibble(
      cluster = NA_integer_,
      size = x$excluded_nrow,
      pct_size = x$excluded_nrow / x$sampled_nrow,
      avg_distance_to_medoid = NA_real_,
      max_distance_to_medoid = NA_real_,
      avg_silhouette = NA_real_,
      min_silhouette = NA_real_,
      pct_negative = NA_real_,
      medoid_row_id = NA_character_
    ))
  }
  result
}

.kmedoids_profile <- function(x) {
  mat <- x$mat
  original_mat <- .kmedoids_original_fit_mat(x)
  ids <- x$clustering
  overall_mean <- colMeans(mat, na.rm = TRUE)
  overall_sd <- apply(mat, 2, stats::sd, na.rm = TRUE)
  overall_median <- apply(original_mat, 2, stats::median, na.rm = TRUE)
  original_overall_mean <- colMeans(original_mat, na.rm = TRUE)
  rows <- purrr::map_dfr(sort(unique(ids)), function(cluster_id) {
    index <- which(ids == cluster_id)
    cluster_mean <- colMeans(mat[index, , drop = FALSE], na.rm = TRUE)
    original_cluster <- original_mat[index, , drop = FALSE]
    original_cluster_mean <- colMeans(original_cluster, na.rm = TRUE)
    original_cluster_median <- apply(original_cluster, 2, stats::median, na.rm = TRUE)
    medoid_index <- x$medoid_indices[[cluster_id]]
    standardized <- ifelse(overall_sd > 0,
      (cluster_mean - overall_mean) / overall_sd,
      0
    )
    tibble::tibble(
      variable = colnames(mat),
      cluster = as.integer(cluster_id),
      standardized_mean = as.numeric(standardized),
      effect_size = abs(as.numeric(standardized)),
      medoid = as.numeric(original_mat[medoid_index, ]),
      cluster_median = as.numeric(original_cluster_median),
      cluster_mean = as.numeric(original_cluster_mean),
      overall_median = as.numeric(overall_median),
      overall_mean = as.numeric(original_overall_mean)
    )
  })
  rows %>%
    dplyr::group_by(cluster) %>%
    dplyr::mutate(rank = rank(-effect_size, ties.method = 'first')) %>%
    dplyr::ungroup() %>%
    {
      result <- .
      if (!isTRUE(x$profile_show_all)) {
        result <- dplyr::filter(result, rank <= x$profile_top_n)
      }
      if (identical(x$profile_variable_order, 'effect_size')) {
        result <- dplyr::arrange(result, cluster, rank)
      }
      result
    }
}

.kmedoids_silhouette <- function(x) {
  if (x$max_centers < 2 || nrow(x$mat) < 3) {
    return(.kmedoids_empty('silhouette'))
  }
  diagnostic <- .kmedoids_diagnostic_mat(x, seed_offset = 1000L)
  diagnostic_mat <- diagnostic$mat
  upper <- min(x$max_centers, nrow(unique(diagnostic_mat)) - 1)
  if (upper < 2) {
    return(.kmedoids_empty('silhouette'))
  }
  purrr::map_dfr(seq(2, upper), function(center) {
    fit <- tryCatch(
      .kmedoids_fit(
        diagnostic_mat, center, x$distance,
        if (is.null(x$seed)) NULL else x$seed + center,
        algorithm = 'pam'
      ),
      error = function(e) NULL
    )
    if (is.null(fit) || is.null(fit$silhouette_widths)) {
      return(tibble::tibble(
        center = center, avg_silhouette = NA_real_, min_silhouette = NA_real_,
        pct_negative = NA_real_
      ))
    }
    widths <- fit$silhouette_widths
    tibble::tibble(
      center = center,
      avg_silhouette = mean(widths, na.rm = TRUE),
      min_silhouette = min(widths, na.rm = TRUE),
      pct_negative = mean(widths < 0, na.rm = TRUE)
    )
  })
}

.kmedoids_elbow <- function(x) {
  diagnostic <- .kmedoids_diagnostic_mat(x, seed_offset = 2000L)
  diagnostic_mat <- diagnostic$mat
  upper <- min(x$max_centers, nrow(unique(diagnostic_mat)))
  if (upper < 1) {
    return(.kmedoids_empty('elbow'))
  }
  result <- purrr::map_dfr(seq_len(upper), function(center) {
    fit <- tryCatch(
      .kmedoids_fit(
        diagnostic_mat, center, x$distance,
        if (is.null(x$seed)) NULL else x$seed + center,
        algorithm = 'pam'
      ),
      error = function(e) NULL
    )
    if (is.null(fit)) {
      return(tibble::tibble(
        center = center, total_distance_to_medoids = NA_real_,
        average_distance_to_medoids = NA_real_
      ))
    }
    dist_to_medoid <- .kmedoids_distance_to_medoids(
      diagnostic_mat, fit$medoids, fit$clustering, x$distance
    )
    tibble::tibble(
      center = center,
      total_distance_to_medoids = sum(dist_to_medoid, na.rm = TRUE),
      average_distance_to_medoids = mean(dist_to_medoid, na.rm = TRUE)
    )
  })
  result %>%
    dplyr::mutate(decrease_ratio = dplyr::if_else(
      dplyr::lag(total_distance_to_medoids) > 0,
      (dplyr::lag(total_distance_to_medoids) - total_distance_to_medoids) /
        dplyr::lag(total_distance_to_medoids),
      NA_real_
    ))
}

.kmedoids_variable_importance <- function(x) {
  ids <- factor(x$clustering)
  purrr::map_dfr(seq_len(ncol(x$mat)), function(index) {
    value <- x$mat[, index]
    grand_mean <- mean(value, na.rm = TRUE)
    between <- sum(tapply(value, ids, function(group) {
      length(group) * (mean(group, na.rm = TRUE) - grand_mean)^2
    }), na.rm = TRUE)
    total <- sum((value - grand_mean)^2, na.rm = TRUE)
    eta_squared <- if (total > 0) between / total else 0
    fit <- tryCatch(stats::aov(value ~ ids), error = function(e) NULL)
    fit_table <- if (is.null(fit)) NULL else summary(fit)[[1]]
    tibble::tibble(
      variable = colnames(x$mat)[[index]],
      eta_squared = eta_squared,
      test_statistic = if (is.null(fit_table)) NA_real_ else fit_table[['F value']][[1]],
      p_value = if (is.null(fit_table)) NA_real_ else fit_table[['Pr(>F)']][[1]]
    )
  })
}

.kmedoids_representative_values <- function(x) {
  ids <- x$clustering
  original_mat <- .kmedoids_original_fit_mat(x)
  purrr::map_dfr(sort(unique(ids)), function(cluster_id) {
    index <- which(ids == cluster_id)
    medoid_index <- x$medoid_indices[[cluster_id]]
    tibble::tibble(
      cluster = as.integer(cluster_id),
      variable = colnames(original_mat),
      medoid = as.numeric(original_mat[medoid_index, ]),
      cluster_median = apply(original_mat[index, , drop = FALSE], 2, stats::median, na.rm = TRUE),
      cluster_mean = colMeans(original_mat[index, , drop = FALSE], na.rm = TRUE),
      overall_median = apply(original_mat, 2, stats::median, na.rm = TRUE),
      overall_mean = colMeans(original_mat, na.rm = TRUE)
    )
  })
}

.kmedoids_distribution <- function(x) {
  ids <- x$clustering
  medoid_indices <- x$medoid_indices
  original_mat <- .kmedoids_original_fit_mat(x)
  purrr::map_dfr(seq_len(nrow(original_mat)), function(index) {
    tibble::tibble(
      cluster = as.integer(ids[[index]]),
      variable = colnames(original_mat),
      value = as.numeric(original_mat[index, ]),
      is_medoid = index %in% medoid_indices
    )
  })
}

.kmedoids_cohesion <- function(x) {
  tibble::tibble(
    cluster = as.integer(x$clustering),
    distance_to_medoid = x$distance_to_medoid
  )
}

.kmedoids_map <- function(x) {
  map_indices <- .kmedoids_sample_indices(
    x, min(x$map_sample_size, .kmedoids_map_max_n),
    include = x$medoid_indices, seed_offset = 3000L
  )
  map_mat <- x$mat[map_indices, , drop = FALSE]
  n_dimension <- min(2, nrow(map_mat) - 1, ncol(map_mat))
  if (n_dimension < 1) {
    return(.kmedoids_empty('map'))
  }
  distance_matrix <- stats::dist(map_mat, method = x$distance)
  coordinates <- stats::cmdscale(distance_matrix, k = n_dimension, eig = TRUE, add = TRUE)
  points <- coordinates$points
  if (n_dimension == 1) points <- cbind(points, 0)
  dimension_names <- c('Dim1', 'Dim2')
  colnames(points) <- dimension_names
  row_ids <- x$row_ids[map_indices]
  result <- tibble::tibble(
    Dim1 = points[, 'Dim1'], Dim2 = points[, 'Dim2'],
    cluster = as.integer(x$clustering[map_indices]), row_type = 'observation',
    label = row_ids
  )
  medoid_positions <- match(x$medoid_indices, map_indices)
  medoid_points <- points[medoid_positions, , drop = FALSE]
  result <- dplyr::bind_rows(result, tibble::tibble(
    Dim1 = medoid_points[, 'Dim1'], Dim2 = medoid_points[, 'Dim2'],
    cluster = seq_len(nrow(medoid_points)), row_type = 'medoid',
    label = x$row_ids[x$medoid_indices]
  ))
  vector_scale <- max(abs(points), na.rm = TRUE)
  if (!is.finite(vector_scale) || vector_scale == 0) vector_scale <- 1
  loadings <- vapply(seq_len(ncol(map_mat)), function(index) {
    c(
      suppressWarnings(stats::cor(map_mat[, index], points[, 'Dim1'], use = 'complete.obs')),
      if (n_dimension >= 2) {
        suppressWarnings(stats::cor(map_mat[, index], points[, 'Dim2'], use = 'complete.obs'))
      } else 0
    )
  }, numeric(2))
  loadings[!is.finite(loadings)] <- 0
  loading_rank <- order(sqrt(colSums(loadings^2)), decreasing = TRUE)
  loading_rank <- head(loading_rank, max(1L, min(x$map_variable_n, length(loading_rank))))
  loadings <- loadings[, loading_rank, drop = FALSE]
  vector_points <- t(loadings) * (vector_scale * 0.8)
  vectors <- purrr::map_dfr(seq_len(nrow(vector_points)), function(index) {
    tibble::tibble(
      Dim1 = c(0, vector_points[index, 1]),
      Dim2 = c(0, vector_points[index, 2]),
      cluster = NA_integer_, row_type = 'vector',
      label = rep(colnames(map_mat)[[loading_rank[[index]]]], 2)
    )
  })
  result <- dplyr::bind_rows(result, vectors)
  eigenvalues <- coordinates$eig[coordinates$eig > 0]
  representation_rate <- if (length(eigenvalues) == 0) c(0, 0) else {
    cumsum(eigenvalues)[seq_len(min(2, length(eigenvalues)))] / sum(eigenvalues)
  }
  x$representation_rate <- c(representation_rate, rep(0, 2 - length(representation_rate)))
  attr(result, 'representation_rate') <- x$representation_rate
  attr(result, 'map_sample_size') <- length(map_indices)
  attr(result, 'map_sampled') <- length(map_indices) < x$valid_nrow
  result
}

.kmedoids_medoid_details <- function(x) {
  original_mat <- .kmedoids_original_fit_mat(x)
  tibble::tibble(
    cluster = seq_along(x$medoid_indices),
    row_id = x$row_ids[x$medoid_indices]
  ) %>% dplyr::bind_cols(
    original_mat[x$medoid_indices, , drop = FALSE] %>%
      as.data.frame(check.names = FALSE) %>% tibble::as_tibble()
  )
}

.kmedoids_counts <- function(x) {
  tibble::tibble(
    original_nrow = nrow(x$original_data),
    analysis_nrow = x$valid_nrow,
    excluded_nrow = x$excluded_nrow,
    exclusion_ratio = if (nrow(x$original_data) > 0) {
      x$excluded_nrow / nrow(x$original_data)
    } else 0,
    requested_algorithm = x$requested_algorithm,
    effective_algorithm = x$effective_algorithm,
    is_approximate = x$is_approximate,
    fit_sample_nrow = x$valid_nrow,
    diagnostic_nrow = min(
      x$valid_nrow, x$silhouette_sample_size, .kmedoids_pam_max_n
    ),
    map_sample_nrow = min(
      x$valid_nrow, max(x$map_sample_size, length(x$medoid_indices)),
      .kmedoids_map_max_n
    )
  )
}

#' K-Medoids clustering analytics.
#'
#' @param df A data frame.
#' @param ... Numeric columns selected with tidyselect.
#' @param centers Number of clusters.
#' @param distance Distance metric supported by `cluster::pam`.
#' @param takeSample Whether to sample rows when `max_nrow` is exceeded.
#' @param max_nrow Maximum number of rows used for fitting.
#' @param iterMax Deprecated compatibility argument. It is accepted for saved
#'   commands but is not used by `cluster::pam()` or `cluster::clara()`.
#' @param seed Random seed.
#' @param normalize_data Whether to standardize selected variables before fitting.
#' @param elbow_method_mode Which optimal-cluster diagnostics to compute.
#' @param max_centers Maximum number of centers for diagnostics.
#' @param silhouette_sample_size Maximum number of rows used for diagnostics.
#' @param profile_top_n Maximum variables shown per cluster when `profile_show_all`
#'   is `FALSE`.
#' @param profile_show_all Whether to show all profile variables.
#' @param profile_variable_order Profile variable ordering mode.
#' @param map_variable_n Maximum number of variables shown on the map.
#' @param algorithm Algorithm to use: `auto`, `pam`, or `clara`.
#' @param clara_samples Number of candidate samples used by CLARA.
#' @param clara_sample_size Number of rows in each CLARA candidate sample.
#' @param map_sample_size Maximum number of rows used for the PCoA map.
#' @return A rowwise data frame containing a K-Medoids model.
#' @export
exp_kmedoids <- function(df, ..., centers = 3, distance = 'manhattan',
                         takeSample = TRUE, max_nrow = 50000, iterMax = 100,
                         seed = 1, normalize_data = TRUE,
                         elbow_method_mode = 'silhouette', max_centers = 10,
                         silhouette_sample_size = 5000, profile_top_n = 10,
                         profile_show_all = TRUE, profile_variable_order = 'effect_size',
                         map_variable_n = 10, algorithm = 'auto',
                         clara_samples = 20, clara_sample_size = NULL,
                         map_sample_size = 2000) {
  selected_cols <- tidyselect::vars_select(names(df), !!!rlang::quos(...))
  if (length(selected_cols) == 0) {
    stop('At least one numeric variable is required for K-Medoids.', call. = FALSE)
  }
  if (!all(vapply(df[selected_cols], is.numeric, logical(1)))) {
    stop('K-Medoids requires numeric variables.', call. = FALSE)
  }
  centers <- .kmedoids_safe_numeric(centers)
  max_centers <- .kmedoids_safe_numeric(max_centers)
  iterMax <- .kmedoids_safe_numeric(iterMax)
  centers <- if (is.null(centers)) NULL else as.integer(floor(centers))
  max_centers <- if (is.null(max_centers)) NULL else as.integer(floor(max_centers))
  if (is.null(centers) || centers < 2 || is.null(iterMax) || iterMax < 1) {
    stop('centers must be at least 2 and iterMax must be positive.', call. = FALSE)
  }
  distance <- match.arg(distance, c('euclidean', 'manhattan'))
  elbow_method_mode <- match.arg(as.character(elbow_method_mode), c('none', 'silhouette', 'elbow'))
  algorithm <- match.arg(as.character(algorithm), c('auto', 'pam', 'clara'))
  clara_samples <- .kmedoids_safe_numeric(clara_samples)
  if (is.null(clara_samples) || clara_samples < 1) {
    stop('clara_samples must be a positive number.', call. = FALSE)
  }
  clara_samples <- as.integer(floor(clara_samples))
  if (!is.null(clara_sample_size)) {
    clara_sample_size <- .kmedoids_safe_numeric(clara_sample_size)
    clara_sample_size <- if (is.null(clara_sample_size)) NULL else as.integer(floor(clara_sample_size))
    if (is.null(clara_sample_size) || clara_sample_size < centers + 1) {
      stop('clara_sample_size must be greater than the number of clusters.', call. = FALSE)
    }
  }
  silhouette_sample_size <- .kmedoids_safe_numeric(silhouette_sample_size)
  silhouette_sample_size <- if (is.null(silhouette_sample_size)) {
    NULL
  } else {
    as.integer(floor(silhouette_sample_size))
  }
  if (is.null(silhouette_sample_size) || silhouette_sample_size < centers + 1) {
    stop('silhouette_sample_size must be greater than the number of clusters.', call. = FALSE)
  }
  profile_top_n <- .kmedoids_safe_numeric(profile_top_n)
  profile_top_n <- if (is.null(profile_top_n)) NULL else as.integer(floor(profile_top_n))
  if (is.null(profile_top_n) || profile_top_n < 1) {
    stop('profile_top_n must be positive.', call. = FALSE)
  }
  map_variable_n <- .kmedoids_safe_numeric(map_variable_n)
  map_variable_n <- if (is.null(map_variable_n)) NULL else as.integer(floor(map_variable_n))
  if (is.null(map_variable_n) || map_variable_n < 1) {
    stop('map_variable_n must be positive.', call. = FALSE)
  }
  map_sample_size <- .kmedoids_safe_numeric(map_sample_size)
  map_sample_size <- if (is.null(map_sample_size)) NULL else as.integer(floor(map_sample_size))
  if (is.null(map_sample_size) || map_sample_size < 1) {
    stop('map_sample_size must be positive.', call. = FALSE)
  }
  if (is.null(max_nrow) || !isTRUE(takeSample)) max_nrow <- nrow(df)
  max_nrow <- max(1, floor(.kmedoids_safe_numeric(max_nrow) %||% nrow(df)))
  original_data <- df
  original_row_ids <- as.character(seq_len(nrow(df)))
  if (nrow(df) > max_nrow) {
    set.seed(seed)
    selected_index <- sample(seq_len(nrow(df)), max_nrow)
    df <- df[selected_index, , drop = FALSE]
    original_row_ids <- original_row_ids[selected_index]
  }
  source_data <- df[selected_cols]
  source_mat <- as.matrix(source_data)
  valid <- complete.cases(source_data) & rowSums(!is.finite(source_mat)) == 0
  excluded_nrow <- sum(!valid)
  fit_data <- source_data[valid, , drop = FALSE]
  row_ids <- original_row_ids[valid]
  if (nrow(fit_data) < centers) {
    stop('The number of valid rows must be greater than or equal to the number of clusters.', call. = FALSE)
  }
  mat <- as.matrix(fit_data)
  if (isTRUE(normalize_data)) {
    mat <- scale(mat)
    mat[is.nan(mat)] <- 0
  }
  fit <- .kmedoids_fit(
    mat, centers, distance, seed, algorithm = algorithm,
    clara_samples = clara_samples, clara_sample_size = clara_sample_size
  )
  model <- list(
    mat = mat, original_fit_mat = as.matrix(fit_data),
    source_data = source_data, original_data = original_data,
    row_ids = row_ids, source_row_ids = original_row_ids, selected_cols = selected_cols,
    valid_indices = which(valid), clustering = fit$clustering,
    medoids = fit$medoids, medoid_indices = fit$medoid_indices,
    silhouette_widths = fit$silhouette_widths,
    silhouette_row_indices = fit$silhouette_row_indices,
    silhouette_avg = fit$silhouette_avg,
    requested_algorithm = algorithm, effective_algorithm = fit$algorithm,
    is_approximate = fit$is_approximate,
    clara_samples = fit$clara_samples, clara_sample_size = fit$clara_sample_size,
    valid_nrow = nrow(fit_data),
    sampled_nrow = nrow(source_data), excluded_nrow = excluded_nrow,
    distance = distance, centers = centers, iterMax = iterMax, seed = seed,
    normalize_data = normalize_data, max_centers = max_centers,
    silhouette_sample_size = silhouette_sample_size, profile_top_n = profile_top_n,
    profile_show_all = profile_show_all, profile_variable_order = profile_variable_order,
    map_variable_n = map_variable_n, map_sample_size = map_sample_size
  )
  model$distance_to_medoid <- .kmedoids_distance_to_medoids(
    mat, fit$medoids, fit$clustering, distance
  )
  model$elbow_result <- if (elbow_method_mode == 'elbow') .kmedoids_elbow(model) else NULL
  model$silhouette_result <- if (elbow_method_mode == 'silhouette') {
    .kmedoids_silhouette(model)
  } else {
    NULL
  }
  # Compute the PCoA map once at fit time and cache it, mirroring elbow_result /
  # silhouette_result above. .kmedoids_map() runs stats::dist() + stats::cmdscale()
  # over up to map_sample_size rows, which is expensive (multiple seconds at the
  # default map_sample_size). Every tidy(type='map') caller -- the Cluster Map tab,
  # the Fitted Vectors tab, and tam's set_kmedoids_analytics_params() helper (which
  # calls broom::tidy() directly for the representation_rate attribute) -- used to
  # each recompute it from scratch, tripling the cost for no benefit since the
  # result depends only on fields already present on `model` at this point.
  # A degenerate map input (e.g. every sampled row identical, so cmdscale() has
  # no positive eigenvalues to project onto) is caught here and downgraded to
  # the same empty-map sentinel .kmedoids_map() already returns for the n < 2
  # case, instead of letting it abort the whole fit -- previously this class of
  # failure only broke the Cluster Map / Fitted Vectors tabs individually.
  model$map_result <- tryCatch(
    .kmedoids_map(model),
    error = function(e) .kmedoids_empty('map')
  )
  class(model) <- c(
    'pam_exploratory',
    if (identical(fit$algorithm, 'pam')) 'pam' else 'clara',
    'partition'
  )
  tibble::tibble(model = list(model)) %>% dplyr::rowwise()
}

#' Tidy a K-Medoids model for Analytics report sections.
#' @export
tidy.pam_exploratory <- function(x, type = 'summary', with_excluded_rows = FALSE, ...) {
  switch(type,
    summary = .kmedoids_summary(x, with_excluded_rows),
    profile = .kmedoids_profile(x),
    silhouette = x$silhouette_result %||% .kmedoids_empty('silhouette'),
    elbow = x$elbow_result %||% .kmedoids_empty('elbow'),
    variable_importance = .kmedoids_variable_importance(x),
    representative_values = .kmedoids_representative_values(x),
    distribution = .kmedoids_distribution(x),
    cohesion = .kmedoids_cohesion(x),
    map = x$map_result %||% .kmedoids_map(x),
    medoid_details = .kmedoids_medoid_details(x),
    counts = .kmedoids_counts(x),
    data = {
      valid_indices <- .kmedoids_valid_indices(x)
      cluster <- rep(NA_integer_, nrow(x$source_data))
      distance <- rep(NA_real_, nrow(x$source_data))
      silhouette <- rep(NA_real_, nrow(x$source_data))
      is_medoid <- rep(FALSE, nrow(x$source_data))
      cluster[valid_indices] <- x$clustering
      distance[valid_indices] <- x$distance_to_medoid
      silhouette[valid_indices] <- .kmedoids_silhouette_values(x)
      is_medoid[valid_indices[x$medoid_indices]] <- TRUE
      dplyr::bind_cols(
        x$source_data,
        tibble::tibble(
          row_id = x$source_row_ids,
          cluster = cluster,
          is_medoid = is_medoid,
          distance_to_medoid = distance,
          silhouette_score = silhouette,
          is_excluded = is.na(cluster)
        )
      )
    },
    .kmedoids_empty(type)
  )
}

#' Glance at a K-Medoids model.
#' @export
glance.pam_exploratory <- function(x, ...) {
  tibble::tibble(
    centers = x$centers,
    total_distance_to_medoids = sum(x$distance_to_medoid, na.rm = TRUE),
    average_silhouette = x$silhouette_avg,
    requested_algorithm = x$requested_algorithm,
    effective_algorithm = x$effective_algorithm,
    is_approximate = x$is_approximate,
    fit_nrow = x$valid_nrow
  )
}

#' Augment data with K-Medoids cluster assignments.
#' @export
augment.pam_exploratory <- function(x, data = NULL, ...) {
  if (is.null(data)) data <- x$source_data
  if (nrow(data) != length(x$clustering)) {
    stop('data must have the same number of rows as the fitted K-Medoids data.', call. = FALSE)
  }
  dplyr::mutate(data, .cluster = factor(x$clustering))
}

#' @export
tidy_kmedoids <- tidy_rowwise

#' @export
glance_kmedoids <- glance_rowwise

#' @export
augment_kmedoids <- augment_rowwise
