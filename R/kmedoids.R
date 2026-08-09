# K-Medoids analytics implementation.

.kmedoids_safe_numeric <- function(x) {
  value <- suppressWarnings(as.numeric(x))
  if (length(value) != 1 || is.na(value) || !is.finite(value)) {
    return(NULL)
  }
  value
}

.kmedoids_distance_to_medoids <- function(mat, medoid_mat, cluster_ids, metric) {
  vapply(seq_len(nrow(mat)), function(i) {
    medoid <- medoid_mat[cluster_ids[[i]], , drop = FALSE]
    delta <- mat[i, , drop = FALSE] - medoid
    if (metric == 'manhattan') sum(abs(delta)) else sqrt(sum(delta^2))
  }, numeric(1))
}

.kmedoids_fit <- function(mat, centers, distance, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  cluster::pam(mat, k = centers, metric = distance, stand = FALSE, keep.diss = TRUE)
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
      effect_size = numeric(), rank = integer()
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
    data = tibble::tibble(),
    tibble::tibble()
  )
}

.kmedoids_summary <- function(x, with_excluded_rows = FALSE) {
  fit <- x$pam
  ids <- fit$clustering
  distances <- x$distance_to_medoid
  silhouette_widths <- rep(NA_real_, length(ids))
  if (!is.null(fit$silinfo$widths)) {
    silhouette_widths <- fit$silinfo$widths[, 'sil_width']
  }
  clusters <- sort(unique(ids))
  result <- purrr::map_dfr(clusters, function(cluster_id) {
    index <- which(ids == cluster_id)
    cluster_silhouette <- silhouette_widths[index]
    tibble::tibble(
      cluster = as.integer(cluster_id),
      size = length(index),
      pct_size = length(index) / x$sampled_nrow,
      avg_distance_to_medoid = mean(distances[index], na.rm = TRUE),
      max_distance_to_medoid = max(distances[index], na.rm = TRUE),
      avg_silhouette = mean(cluster_silhouette, na.rm = TRUE),
      min_silhouette = min(cluster_silhouette, na.rm = TRUE),
      pct_negative = mean(cluster_silhouette < 0, na.rm = TRUE),
      medoid_row_id = x$row_ids[fit$id.med[[cluster_id]]]
    )
  })
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
  ids <- x$pam$clustering
  overall_mean <- colMeans(mat, na.rm = TRUE)
  overall_sd <- apply(mat, 2, stats::sd, na.rm = TRUE)
  rows <- purrr::map_dfr(sort(unique(ids)), function(cluster_id) {
    index <- which(ids == cluster_id)
    cluster_mean <- colMeans(mat[index, , drop = FALSE], na.rm = TRUE)
    standardized <- ifelse(overall_sd > 0,
      (cluster_mean - overall_mean) / overall_sd,
      0
    )
    tibble::tibble(
      variable = colnames(mat),
      cluster = as.integer(cluster_id),
      standardized_mean = as.numeric(standardized),
      effect_size = abs(as.numeric(standardized))
    )
  })
  rows %>%
    dplyr::group_by(cluster) %>%
    dplyr::mutate(rank = rank(-effect_size, ties.method = 'first')) %>%
    dplyr::ungroup()
}

.kmedoids_silhouette <- function(x) {
  if (x$max_centers < 2 || nrow(x$mat) < 3) {
    return(.kmedoids_empty('silhouette'))
  }
  upper <- min(x$max_centers, nrow(unique(x$mat)) - 1)
  if (upper < 2) {
    return(.kmedoids_empty('silhouette'))
  }
  purrr::map_dfr(seq(2, upper), function(center) {
    fit <- tryCatch(
      .kmedoids_fit(x$mat, center, x$distance, x$seed),
      error = function(e) NULL
    )
    if (is.null(fit) || is.null(fit$silinfo$widths)) {
      return(tibble::tibble(
        center = center, avg_silhouette = NA_real_, min_silhouette = NA_real_,
        pct_negative = NA_real_
      ))
    }
    widths <- fit$silinfo$widths[, 'sil_width']
    tibble::tibble(
      center = center,
      avg_silhouette = mean(widths, na.rm = TRUE),
      min_silhouette = min(widths, na.rm = TRUE),
      pct_negative = mean(widths < 0, na.rm = TRUE)
    )
  })
}

.kmedoids_elbow <- function(x) {
  upper <- min(x$max_centers, nrow(unique(x$mat)))
  if (upper < 1) {
    return(.kmedoids_empty('elbow'))
  }
  result <- purrr::map_dfr(seq_len(upper), function(center) {
    fit <- tryCatch(
      .kmedoids_fit(x$mat, center, x$distance, x$seed),
      error = function(e) NULL
    )
    if (is.null(fit)) {
      return(tibble::tibble(
        center = center, total_distance_to_medoids = NA_real_,
        average_distance_to_medoids = NA_real_
      ))
    }
    dist_to_medoid <- .kmedoids_distance_to_medoids(
      x$mat, fit$medoids, fit$clustering, x$distance
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
  ids <- factor(x$pam$clustering)
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
  ids <- x$pam$clustering
  purrr::map_dfr(sort(unique(ids)), function(cluster_id) {
    index <- which(ids == cluster_id)
    medoid_index <- x$pam$id.med[[cluster_id]]
    tibble::tibble(
      cluster = as.integer(cluster_id),
      variable = colnames(x$mat),
      medoid = as.numeric(x$mat[medoid_index, ]),
      cluster_median = apply(x$mat[index, , drop = FALSE], 2, stats::median, na.rm = TRUE),
      cluster_mean = colMeans(x$mat[index, , drop = FALSE], na.rm = TRUE),
      overall_median = apply(x$mat, 2, stats::median, na.rm = TRUE),
      overall_mean = colMeans(x$mat, na.rm = TRUE)
    )
  })
}

.kmedoids_distribution <- function(x) {
  ids <- x$pam$clustering
  medoid_indices <- x$pam$id.med
  purrr::map_dfr(seq_len(nrow(x$mat)), function(index) {
    tibble::tibble(
      cluster = as.integer(ids[[index]]),
      variable = colnames(x$mat),
      value = as.numeric(x$mat[index, ]),
      is_medoid = index %in% medoid_indices
    )
  })
}

.kmedoids_cohesion <- function(x) {
  tibble::tibble(
    cluster = as.integer(x$pam$clustering),
    distance_to_medoid = x$distance_to_medoid
  )
}

.kmedoids_map <- function(x) {
  n_dimension <- min(2, nrow(x$mat) - 1, ncol(x$mat))
  if (n_dimension < 1) {
    return(.kmedoids_empty('map'))
  }
  distance_matrix <- stats::dist(x$mat, method = x$distance)
  coordinates <- stats::cmdscale(distance_matrix, k = n_dimension, eig = TRUE, add = TRUE)
  points <- coordinates$points
  if (n_dimension == 1) points <- cbind(points, 0)
  dimension_names <- c('Dim1', 'Dim2')
  colnames(points) <- dimension_names
  row_ids <- x$row_ids
  result <- tibble::tibble(
    Dim1 = points[, 'Dim1'], Dim2 = points[, 'Dim2'],
    cluster = as.integer(x$pam$clustering), row_type = 'observation',
    label = row_ids
  )
  medoid_points <- points[x$pam$id.med, , drop = FALSE]
  result <- dplyr::bind_rows(result, tibble::tibble(
    Dim1 = medoid_points[, 'Dim1'], Dim2 = medoid_points[, 'Dim2'],
    cluster = seq_len(nrow(medoid_points)), row_type = 'medoid',
    label = row_ids[x$pam$id.med]
  ))
  eigenvalues <- coordinates$eig[coordinates$eig > 0]
  representation_rate <- if (length(eigenvalues) == 0) c(0, 0) else {
    cumsum(eigenvalues)[seq_len(min(2, length(eigenvalues)))] / sum(eigenvalues)
  }
  x$representation_rate <- c(representation_rate, rep(0, 2 - length(representation_rate)))
  attr(result, 'representation_rate') <- x$representation_rate
  result
}

#' K-Medoids clustering analytics.
#'
#' @param df A data frame.
#' @param ... Numeric columns selected with tidyselect.
#' @param centers Number of clusters.
#' @param distance Distance metric supported by `cluster::pam`.
#' @param takeSample Whether to sample rows when `max_nrow` is exceeded.
#' @param max_nrow Maximum number of rows used for fitting.
#' @param iterMax Maximum PAM iterations.
#' @param seed Random seed.
#' @param normalize_data Whether to standardize selected variables before fitting.
#' @param elbow_method_mode Which optimal-cluster diagnostics to compute.
#' @param max_centers Maximum number of centers for diagnostics.
#' @param silhouette_sample_size Reserved for parity with the Analytics UI.
#' @param profile_top_n Reserved for parity with the Analytics UI.
#' @param profile_show_all Reserved for parity with the Analytics UI.
#' @param profile_variable_order Reserved for parity with the Analytics UI.
#' @param map_variable_n Reserved for parity with the Analytics UI.
#' @return A rowwise data frame containing a K-Medoids model.
#' @export
exp_kmedoids <- function(df, ..., centers = 3, distance = 'manhattan',
                         takeSample = TRUE, max_nrow = 50000, iterMax = 100,
                         seed = 1, normalize_data = TRUE,
                         elbow_method_mode = 'silhouette', max_centers = 10,
                         silhouette_sample_size = 5000, profile_top_n = 10,
                         profile_show_all = FALSE, profile_variable_order = 'effect_size',
                         map_variable_n = 10) {
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
  if (is.null(centers) || centers < 2 || is.null(iterMax) || iterMax < 1) {
    stop('centers must be at least 2 and iterMax must be positive.', call. = FALSE)
  }
  distance <- match.arg(distance, c('euclidean', 'manhattan'))
  elbow_method_mode <- match.arg(as.character(elbow_method_mode), c('none', 'silhouette', 'elbow'))
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
  valid <- complete.cases(source_data) & apply(source_data, 1, function(row) all(is.finite(row)))
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
  fit <- .kmedoids_fit(mat, centers, distance, seed)
  model <- list(
    pam = fit, mat = mat, source_data = source_data, original_data = original_data,
    row_ids = row_ids, selected_cols = selected_cols, valid_nrow = nrow(fit_data),
    sampled_nrow = nrow(source_data), excluded_nrow = excluded_nrow,
    distance = distance, centers = centers, iterMax = iterMax, seed = seed,
    normalize_data = normalize_data, max_centers = max_centers,
    silhouette_sample_size = silhouette_sample_size, profile_top_n = profile_top_n,
    profile_show_all = profile_show_all, profile_variable_order = profile_variable_order,
    map_variable_n = map_variable_n
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
  class(model) <- c('pam_exploratory', 'pam', 'partition')
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
    map = .kmedoids_map(x),
    data = dplyr::bind_cols(
      x$source_data,
      tibble::tibble(cluster = {
        values <- rep(NA_integer_, nrow(x$source_data))
        values[seq_len(nrow(x$source_data)) %in% which(complete.cases(x$source_data))] <- x$pam$clustering
        values
      })
    ),
    .kmedoids_empty(type)
  )
}

#' Glance at a K-Medoids model.
#' @export
glance.pam_exploratory <- function(x, ...) {
  tibble::tibble(
    centers = x$centers,
    total_distance_to_medoids = sum(x$distance_to_medoid, na.rm = TRUE),
    average_silhouette = if (is.null(x$pam$silinfo$avg.width)) NA_real_ else x$pam$silinfo$avg.width
  )
}

#' Augment data with K-Medoids cluster assignments.
#' @export
augment.pam_exploratory <- function(x, data = NULL, ...) {
  if (is.null(data)) data <- x$source_data
  if (nrow(data) != length(x$pam$clustering)) {
    stop('data must have the same number of rows as the fitted K-Medoids data.', call. = FALSE)
  }
  dplyr::mutate(data, .cluster = factor(x$pam$clustering))
}

#' @export
tidy_kmedoids <- tidy_rowwise

#' @export
glance_kmedoids <- glance_rowwise

#' @export
augment_kmedoids <- augment_rowwise
