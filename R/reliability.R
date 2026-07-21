# Cronbach's Alpha / reliability analysis for the Analytics View (issue #14309).
#
# exp_cronbach_alpha() wraps psych to build a reliability report per group and stores the
# fully-computed report as a model object (class "cronbach_alpha_exploratory"). The desktop
# retrieves each report section through tidy(model, type = "...") / glance(model).
#
# correlation_method:
#   "auto"       - Pearson (all continuous), Polychoric (all ordinal/binary), Mixed (mix).
#   "pearson"    - Pearson correlation -> Cronbach's Alpha.
#   "polychoric" - Polychoric/Tetrachoric correlation -> Ordinal Alpha.
#   "mixed"      - psych::mixedCor (Pearson/Polychoric/Polyserial) -> Mixed-Correlation Alpha.
#
# psych is already a hard dependency, so psych::alpha / polychoric / mixedCor add nothing new.

# ------------------------------------------------------------
# Internal helpers
# ------------------------------------------------------------

reliability_safe_number <- function(x) {
  if (length(x) == 0 || is.null(x)) {
    return(NA_real_)
  }
  x <- suppressWarnings(as.numeric(x[[1]]))
  if (!is.finite(x)) NA_real_ else x
}

reliability_classify_alpha <- function(alpha_value) {
  dplyr::case_when(
    is.na(alpha_value)  ~ "Cannot Determine",
    alpha_value >= 0.90 ~ "Excellent",
    alpha_value >= 0.80 ~ "Good",
    alpha_value >= 0.70 ~ "Acceptable",
    alpha_value >= 0.60 ~ "Questionable",
    TRUE                ~ "Poor"
  )
}

reliability_classify_item_total <- function(r) {
  dplyr::case_when(
    is.na(r)  ~ "Cannot Determine",
    r >= 0.70 ~ "Strong",
    r >= 0.50 ~ "Moderate",
    r >= 0.30 ~ "Weak",
    TRUE      ~ "Needs Attention"
  )
}

reliability_classify_alpha_change <- function(diff, tolerance = 0.01) {
  dplyr::case_when(
    is.na(diff)       ~ "Cannot Determine",
    diff >= 0.05      ~ "Improves if Dropped",
    diff > tolerance  ~ "Slightly Improves if Dropped",
    diff <= -0.10     ~ "Much Worse if Dropped",
    diff < -tolerance ~ "Worse if Dropped",
    TRUE              ~ "Little Effect"
  )
}

# Standardized alpha from a correlation matrix: k * mean_r / (1 + (k - 1) * mean_r).
reliability_alpha_from_correlation <- function(r) {
  k <- ncol(r)
  if (is.null(k) || k < 2) {
    return(NA_real_)
  }
  upper_values <- r[upper.tri(r)]
  average_r <- mean(upper_values, na.rm = TRUE)
  if (!is.finite(average_r)) {
    return(NA_real_)
  }
  k * average_r / (1 + (k - 1) * average_r)
}

reliability_is_positive_semidefinite <- function(r, tolerance = 1e-8) {
  eigenvalues <- tryCatch(
    eigen(r, symmetric = TRUE, only.values = TRUE)$values,
    error = function(e) NA_real_
  )
  if (all(is.na(eigenvalues))) {
    return(FALSE)
  }
  min(eigenvalues, na.rm = TRUE) >= -tolerance
}

reliability_make_positive_semidefinite <- function(r) {
  adjusted <- psych::cor.smooth(r)
  dimnames(adjusted) <- dimnames(r)
  adjusted
}

# ------------------------------------------------------------
# Item-type detection and correlation-method selection
# ------------------------------------------------------------

reliability_detect_item_type <- function(x) {
  non_missing <- x[!is.na(x)]
  unique_count <- length(unique(non_missing))
  if (is.logical(x)) {
    return("dichotomous")
  }
  if (is.ordered(x)) {
    if (unique_count <= 2) "dichotomous" else "ordinal"
  } else if (is.factor(x)) {
    if (unique_count <= 2) "dichotomous" else "nominal"
  } else if (is.numeric(x) || is.integer(x)) {
    if (unique_count <= 2) "dichotomous" else "continuous"
  } else {
    "unsupported"
  }
}

reliability_select_method <- function(item_types, requested_method) {
  if (requested_method != "auto") {
    return(requested_method)
  }
  unique_types <- unique(unname(item_types))
  if (all(unique_types == "continuous")) {
    return("pearson")
  }
  if (all(unique_types %in% c("ordinal", "dichotomous"))) {
    return("polychoric")
  }
  if (all(unique_types %in% c("continuous", "ordinal", "dichotomous"))) {
    return("mixed")
  }
  # Fallback: nominal/unsupported already rejected upstream; default to pearson.
  "pearson"
}

# Convert columns to integer codes usable by polychoric / mixedCor.
reliability_ordinal_to_integer <- function(x) {
  if (is.ordered(x) || is.factor(x) || is.logical(x)) {
    return(as.integer(x))
  }
  values <- sort(unique(x[!is.na(x)]))
  match(x, values)
}

reliability_dichotomous_to_integer <- function(x) {
  if (is.logical(x)) {
    return(as.integer(x))
  }
  if (is.factor(x)) {
    return(as.integer(x) - 1L)
  }
  values <- sort(unique(x[!is.na(x)]))
  match(x, values) - 1L
}

reliability_prepare_correlation_data <- function(data, item_types) {
  output <- data
  for (column_name in names(output)) {
    type <- item_types[[column_name]]
    output[[column_name]] <- switch(
      type,
      continuous = as.numeric(output[[column_name]]),
      ordinal = reliability_ordinal_to_integer(output[[column_name]]),
      dichotomous = reliability_dichotomous_to_integer(output[[column_name]]),
      as.numeric(output[[column_name]])
    )
  }
  as.data.frame(output, check.names = FALSE)
}

# ------------------------------------------------------------
# Correlation matrix builder
# ------------------------------------------------------------

reliability_build_correlation <- function(data, item_types, method,
                                          smooth = TRUE, correct = 0.5) {
  prepared_data <- reliability_prepare_correlation_data(data, item_types)
  warnings_list <- character()

  if (method == "pearson") {
    correlation_matrix <- stats::cor(prepared_data, use = "pairwise.complete.obs",
                                     method = "pearson")
  } else if (method == "polychoric") {
    obj <- suppressWarnings(psych::polychoric(prepared_data, correct = correct,
                                              smooth = FALSE, global = FALSE))
    correlation_matrix <- obj$rho
  } else if (method == "mixed") {
    continuous_index <- match(names(item_types)[item_types == "continuous"], names(prepared_data))
    polytomous_index <- match(names(item_types)[item_types == "ordinal"], names(prepared_data))
    dichotomous_index <- match(names(item_types)[item_types == "dichotomous"], names(prepared_data))
    obj <- suppressWarnings(psych::mixedCor(prepared_data,
                           c = continuous_index, p = polytomous_index, d = dichotomous_index,
                           correct = correct, global = FALSE))
    correlation_matrix <- obj$rho
  } else {
    stop("Unsupported correlation method: ", method)
  }

  correlation_matrix <- as.matrix(correlation_matrix)
  # Restore the original column order (mixedCor reorders internally).
  correlation_matrix <- correlation_matrix[names(data), names(data), drop = FALSE]
  diag(correlation_matrix) <- 1

  matrix_adjusted <- FALSE
  if (!reliability_is_positive_semidefinite(correlation_matrix)) {
    if (smooth) {
      correlation_matrix <- reliability_make_positive_semidefinite(correlation_matrix)
      matrix_adjusted <- TRUE
      warnings_list <- c(warnings_list,
        "The estimated correlation matrix was not positive semi-definite and was smoothed.")
    }
  }

  list(correlation = correlation_matrix, matrix_adjusted = matrix_adjusted,
       prepared_data = prepared_data, warnings = warnings_list)
}

# ------------------------------------------------------------
# Item-total correlations from a correlation matrix
# ------------------------------------------------------------

reliability_item_rest_correlation <- function(r) {
  items <- colnames(r)
  stats::setNames(vapply(seq_along(items), function(i) {
    other <- setdiff(seq_along(items), i)
    if (length(other) == 0) return(NA_real_)
    covariance_item_rest <- sum(r[i, other], na.rm = TRUE)
    variance_rest <- sum(r[other, other, drop = FALSE], na.rm = TRUE)
    if (!is.finite(variance_rest) || variance_rest <= 0) return(NA_real_)
    covariance_item_rest / sqrt(variance_rest)
  }, numeric(1)), items)
}

reliability_standardized_item_total <- function(r) {
  items <- colnames(r)
  stats::setNames(vapply(seq_along(items), function(i) {
    covariance_item_total <- sum(r[i, ], na.rm = TRUE)
    variance_total <- sum(r, na.rm = TRUE)
    if (!is.finite(variance_total) || variance_total <= 0) return(NA_real_)
    covariance_item_total / sqrt(variance_total)
  }, numeric(1)), items)
}

reliability_alpha_if_deleted <- function(correlation_matrix, current_alpha) {
  item_names <- colnames(correlation_matrix)
  purrr::map_dfr(item_names, function(item) {
    keep <- colnames(correlation_matrix) != item
    reduced <- correlation_matrix[keep, keep, drop = FALSE]
    deleted_alpha <- reliability_alpha_from_correlation(reduced)
    difference <- deleted_alpha - current_alpha
    tibble::tibble(
      removed_item = item,
      alpha_if_dropped = deleted_alpha,
      difference_from_current = difference,
      interpretation = reliability_classify_alpha_change(difference)
    )
  }) %>% dplyr::arrange(alpha_if_dropped)
}

reliability_numeric_response_values <- function(x) {
  # Match the Summary View histogram behavior: low-cardinality integer values
  # remain discrete, while other numeric values use ten equal-width bins.
  finite_values <- x[is.finite(x)]
  if (length(finite_values) == 0) {
    return(factor(character()))
  }

  numeric_values <- as.numeric(finite_values)
  min_value <- min(numeric_values)
  max_value <- max(numeric_values)
  integer_values <- all(numeric_values == as.integer(numeric_values))

  if (min_value == max_value || (max_value - min_value) < 13 && integer_values) {
    levels <- as.character(seq.int(floor(min_value), ceiling(max_value)))
    return(factor(as.character(numeric_values), levels = levels))
  }

  breaks <- seq(min_value, max_value, length.out = 11)
  binned_values <- cut(
    numeric_values,
    breaks = breaks,
    include.lowest = TRUE,
    right = TRUE
  )
  format_cut_output(binned_values, decimal.digits = 2, right = TRUE)
}

reliability_response_values <- function(x) {
  if (is.numeric(x)) {
    reliability_numeric_response_values(x)
  } else {
    x[!is.na(x)]
  }
}

reliability_response_distribution <- function(data) {
  purrr::map_dfr(names(data), function(column_name) {
    x <- data[[column_name]]
    response_values <- reliability_response_values(x)
    if (length(response_values) == 0) {
      return(tibble::tibble(variable = character(), response = character(),
                             count = integer(), proportion = double()))
    }
    counts <- as.data.frame(table(response_values, useNA = "no"), stringsAsFactors = FALSE)
    names(counts) <- c("response", "count")
    non_missing_n <- sum(counts$count)
    counts %>% dplyr::mutate(
      variable = column_name,
      proportion = if (non_missing_n > 0) count / non_missing_n else NA_real_
    ) %>% dplyr::select(variable, response, count, proportion)
  })
}

reliability_observed_item_statistics <- function(prepared_data) {
  purrr::map_dfr(names(prepared_data), function(column_name) {
    x <- prepared_data[[column_name]]
    tibble::tibble(
      variable = column_name,
      n = sum(!is.na(x)),
      missing_n = sum(is.na(x)),
      mean = mean(x, na.rm = TRUE),
      sd = stats::sd(x, na.rm = TRUE)
    )
  })
}

reliability_observed_item_total_correlation <- function(prepared_data) {
  raw_r_vec <- stats::setNames(rep(NA_real_, ncol(prepared_data)),
                               colnames(prepared_data))
  raw_alpha_object <- tryCatch(
    suppressWarnings(psych::alpha(prepared_data, check.keys = FALSE, warnings = FALSE)),
    error = function(e) NULL)
  if (!is.null(raw_alpha_object) && !is.null(raw_alpha_object$item.stats$raw.r)) {
    raw_stats <- raw_alpha_object$item.stats
    raw_names <- sub("-$", "", rownames(raw_stats))
    raw_r_vec[raw_names] <- raw_stats$raw.r
  }
  raw_r_vec
}

reliability_extract_key_signs <- function(alpha_object, item_names) {
  signs <- stats::setNames(rep(1, length(item_names)), item_names)
  keys <- if (!is.null(alpha_object)) alpha_object$keys else NULL
  keys <- unlist(keys, use.names = FALSE)
  if (length(keys) == 0) {
    return(signs)
  }
  for (key in keys) {
    item_name <- sub("^-", "", key)
    if (item_name %in% item_names) {
      signs[[item_name]] <- if (startsWith(key, "-")) -1 else 1
    }
  }
  signs
}

reliability_apply_key_signs <- function(data, key_signs) {
  for (item_name in names(key_signs)[key_signs < 0]) {
    values <- data[[item_name]]
    observed <- values[!is.na(values)]
    if (length(observed) > 0) {
      data[[item_name]] <- max(observed) + min(observed) - values
    }
  }
  data
}

reliability_scale_candidates <- function(current_alpha, alpha_if_deleted, item_count) {
  base <- tibble::tibble(
    candidate = "Use all items",
    item_count = item_count,
    alpha = current_alpha,
    recommendation = dplyr::if_else(current_alpha >= 0.70, "Usable", "Reconsider")
  )
  improvements <- alpha_if_deleted %>%
    dplyr::filter(difference_from_current >= 0.02) %>%
    dplyr::transmute(
      candidate = paste0("Drop \"", removed_item, "\""),
      item_count = item_count - 1L,
      alpha = alpha_if_dropped,
      recommendation = dplyr::if_else(difference_from_current >= 0.05,
                                      "Recommended", "Consider")
    )
  dplyr::bind_rows(base, improvements) %>% dplyr::arrange(dplyr::desc(alpha))
}

# ------------------------------------------------------------
# Public: exp_cronbach_alpha
# ------------------------------------------------------------

#' Cronbach's Alpha reliability analysis for the Analytics View.
#' @export
exp_cronbach_alpha <- function(df, ..., correlation_method = "auto", check_keys = FALSE,
                               smooth = TRUE, correct = 0.5, max_nrow = NULL, seed = 1) {
  correlation_method <- match.arg(correlation_method,
                                  c("auto", "pearson", "polychoric", "mixed"))

  selected_cols <- tidyselect::vars_select(names(df), !!! rlang::quos(...))
  grouped_cols <- grouped_by(df)
  selected_cols <- setdiff(selected_cols, grouped_cols)

  if (length(selected_cols) < 2) {
    stop("EXP-ANA-5 :: [] :: Cronbach's Alpha requires at least 2 variables.")
  }

  # Remove types that break downstream binding, mirroring exp_factanal.
  df <- df %>% dplyr::select(-where(is.list),
                             -where(lubridate::is.difftime),
                             -where(lubridate::is.duration),
                             -where(lubridate::is.interval),
                             -where(lubridate::is.period))

  if (!is.null(seed)) {
    set.seed(seed)
  }

  each_func <- function(df) {
    sampled_nrow <- NULL
    if (!is.null(max_nrow) && nrow(df) > max_nrow) {
      sampled_nrow <- max_nrow
      df <- df %>% sample_rows(max_nrow)
    }

    # Base-R subsetting handles spaces / multibyte column names.
    cleaned_df <- df[, colnames(df) %in% selected_cols, drop = FALSE]

    # Drop constant columns (single unique value) which break correlation.
    for (col in colnames(cleaned_df)) {
      if (length(unique(cleaned_df[[col]][!is.na(cleaned_df[[col]])])) < 2) {
        cleaned_df <- cleaned_df[colnames(cleaned_df) != col]
      }
    }
    if (ncol(cleaned_df) < 2) {
      if (length(grouped_cols) < 1) {
        stop("There are not enough columns after removing columns with only NA or a single value.")
      } else {
        return(NULL)
      }
    }

    item_types <- vapply(cleaned_df, reliability_detect_item_type, character(1))
    names(item_types) <- colnames(cleaned_df)

    unsupported <- names(item_types)[item_types %in% c("nominal", "unsupported")]
    if (length(unsupported) > 0) {
      stop("These variables cannot be used for reliability analysis (nominal/unsupported): ",
           paste(unsupported, collapse = ", "))
    }

    selected_method <- reliability_select_method(item_types, correlation_method)

    correlation_result <- reliability_build_correlation(
      cleaned_df, item_types, selected_method, smooth = smooth, correct = correct)
    r <- correlation_result$correlation

    complete_n <- sum(stats::complete.cases(cleaned_df))
    total_n <- nrow(cleaned_df)
    total_missing_n <- sum(is.na(cleaned_df))
    rows_with_missing_n <- sum(!stats::complete.cases(cleaned_df))

    standardized_alpha <- reliability_alpha_from_correlation(r)
    average_r <- mean(r[upper.tri(r)], na.rm = TRUE)
    median_r <- stats::median(r[upper.tri(r)], na.rm = TRUE)

    coefficient_name <- switch(selected_method,
      pearson = "Cronbach's Alpha",
      polychoric = "Ordinal Alpha",
      mixed = "Mixed-Correlation Alpha")

    # psych::alpha on the correlation matrix -> G6, std.r, r.cor, r.drop.
    matrix_alpha_object <- tryCatch(
      suppressWarnings(psych::alpha(r, n.obs = complete_n, check.keys = check_keys,
                                    warnings = FALSE)),
      error = function(e) NULL)

    # psych determines reverse keys from the first principal component. Apply
    # the same keys to every matrix-based statistic, not just alpha itself.
    key_signs <- reliability_extract_key_signs(matrix_alpha_object, colnames(r))
    if (isTRUE(check_keys) && any(key_signs < 0)) {
      key_matrix <- diag(key_signs)
      r <- key_matrix %*% r %*% key_matrix
      dimnames(r) <- dimnames(correlation_result$correlation)
      matrix_alpha_object <- tryCatch(
        suppressWarnings(psych::alpha(r, n.obs = complete_n, check.keys = FALSE,
                                      warnings = FALSE)),
        error = function(e) NULL)
    }

    g6 <- if (!is.null(matrix_alpha_object)) {
      reliability_safe_number(matrix_alpha_object$total[["G6(smc)"]])
    } else NA_real_

    # Pearson uses raw alpha and the Feldt/Duhachek interval from the data.
    # Polychoric/mixed methods use the matrix for standardized alpha only;
    # their raw-data CI is hidden by specification.
    raw_alpha <- NA_real_
    raw_alpha_object <- NULL
    confidence_interval <- tibble::tibble(method = character(), lower = numeric(),
                                          estimate = numeric(), upper = numeric())
    if (selected_method == "pearson") {
      raw_alpha_object <- tryCatch(
        suppressWarnings(psych::alpha(cleaned_df, check.keys = check_keys, warnings = FALSE)),
        error = function(e) NULL)
      if (!is.null(raw_alpha_object)) {
        raw_alpha <- reliability_safe_number(raw_alpha_object$total$raw_alpha)
      }
    }

    # psych::alpha()$feldt is a list of 1-row data frames
    # (lower.ci/alpha/upper.ci), each holding a raw_alpha column. The
    # specification exposes this interval for Pearson alpha only.
    ci_alpha_object <- raw_alpha_object
    if (!is.null(ci_alpha_object)) {
      feldt <- ci_alpha_object$feldt
      ci_lower <- reliability_safe_number(feldt$lower.ci$raw_alpha)
      ci_est <- reliability_safe_number(feldt$alpha$raw_alpha)
      ci_upper <- reliability_safe_number(feldt$upper.ci$raw_alpha)
      if (!is.na(ci_lower) && !is.na(ci_upper)) {
        confidence_interval <- tibble::tibble(
          method = "Feldt", lower = ci_lower, estimate = ci_est, upper = ci_upper)
      }
    }

    display_alpha <- if (selected_method == "pearson") raw_alpha else standardized_alpha

    # Item statistics.
    prepared_data <- correlation_result$prepared_data
    if (isTRUE(check_keys) && any(key_signs < 0)) {
      prepared_data <- reliability_apply_key_signs(prepared_data, key_signs)
    }
    item_rest <- reliability_item_rest_correlation(r)
    std_item_total <- reliability_standardized_item_total(r)
    observed_stats <- reliability_observed_item_statistics(prepared_data)

    # r.cor from the matrix-based psych::alpha (item.stats columns: r, r.cor, r.drop).
    r_cor_vec <- stats::setNames(rep(NA_real_, length(colnames(r))), colnames(r))
    if (!is.null(matrix_alpha_object) && !is.null(matrix_alpha_object$item.stats$r.cor)) {
      mstats <- matrix_alpha_object$item.stats
      r_cor_vec[rownames(mstats)] <- mstats$r.cor
    }
    # raw.r is the observed-data item-total correlation. It is available for
    # every correlation method, including mixed and polychoric analyses; the
    # matrix-based statistics above still use the selected correlation method.
    raw_r_vec <- reliability_observed_item_total_correlation(prepared_data)

    item_statistics <- observed_stats %>%
      dplyr::mutate(
        raw.r = raw_r_vec[variable],
        std.r = std_item_total[variable],
        r.drop = item_rest[variable],
        r.cor = r_cor_vec[variable],
        interpretation = reliability_classify_item_total(item_rest[variable])) %>%
      dplyr::select(variable, r.drop, raw.r, std.r, r.cor, n, missing_n, mean, sd,
                    interpretation)

    alpha_if_deleted <- reliability_alpha_if_deleted(r, standardized_alpha)

    response_distribution <- reliability_response_distribution(cleaned_df)

    # Flagged item: the item whose removal most improves alpha.
    flagged <- alpha_if_deleted %>%
      dplyr::filter(difference_from_current > 0) %>%
      dplyr::arrange(dplyr::desc(difference_from_current)) %>%
      dplyr::slice_head(n = 1) %>% dplyr::pull(removed_item)
    flagged_item <- if (length(flagged) == 0) NA_character_ else flagged

    ci_text <- if (nrow(confidence_interval) > 0) {
      paste0(round(confidence_interval$lower[[1]], 3), " - ",
             round(confidence_interval$upper[[1]], 3))
    } else NA_character_

    summary_table <- tibble::tibble(
      Metric = c(coefficient_name, "Standardized Alpha", "95% CI", "Number of Variables",
                 "Complete Responses", "Responses with Missing", "Total Missing Cells",
                 "Average Inter-item Correlation", "Flagged Item"),
      Value = c(as.character(round(display_alpha, 3)),
                as.character(round(standardized_alpha, 3)),
                ci_text,
                as.character(ncol(cleaned_df)),
                as.character(complete_n),
                as.character(rows_with_missing_n),
                as.character(total_missing_n),
                as.character(round(average_r, 3)),
                flagged_item),
      Interpretation = c(
        reliability_classify_alpha(display_alpha),
        "Standardized consistency",
        if (nrow(confidence_interval) > 0) "Feldt/Duhachek interval" else NA_character_,
        paste0(ncol(cleaned_df), " items used"),
        "Rows with no missing values",
        "Rows with 1+ missing value",
        "Missing cells across all items",
        dplyr::case_when(
          average_r >= 0.50 ~ "High relatedness",
          average_r >= 0.30 ~ "Moderate relatedness",
          average_r >= 0.15 ~ "Weak relatedness",
          TRUE ~ "Low relatedness"),
        dplyr::if_else(is.na(flagged_item), "None", "Alpha rises if dropped")))
    if (nrow(confidence_interval) == 0) {
      summary_table <- summary_table %>% dplyr::filter(Metric != "95% CI")
    }

    scale_candidates <- reliability_scale_candidates(standardized_alpha, alpha_if_deleted,
                                                     ncol(cleaned_df))

    correlation_long <- tibble::as_tibble(as.data.frame(r), rownames = "variable_1") %>%
      tidyr::pivot_longer(cols = -variable_1, names_to = "variable_2",
                          values_to = "correlation")

    warnings_all <- unique(correlation_result$warnings)

    fit <- list(
      selected_method = selected_method,
      coefficient_name = coefficient_name,
      requested_method = correlation_method,
      item_types = item_types,
      alpha = display_alpha,
      raw_alpha = raw_alpha,
      standardized_alpha = standardized_alpha,
      average_r = average_r,
      median_r = median_r,
      g6 = g6,
      confidence_interval = confidence_interval,
      summary_table = summary_table,
      alpha_if_deleted = alpha_if_deleted,
      item_statistics = item_statistics,
      correlation_matrix = r,
      correlation_long = correlation_long,
      response_distribution = response_distribution,
      scale_candidates = scale_candidates,
      complete_n = complete_n,
      total_n = total_n,
      total_missing_n = total_missing_n,
      rows_with_missing_n = rows_with_missing_n,
      flagged_item = flagged_item,
      matrix_adjusted = correlation_result$matrix_adjusted,
      warnings = warnings_all,
      df = df,
      grouped_cols = grouped_cols,
      sampled_nrow = sampled_nrow
    )
    class(fit) <- c("cronbach_alpha_exploratory")
    fit
  }

  do_on_each_group(df, each_func, name = "model", with_unnest = FALSE)
}

# ------------------------------------------------------------
# glance / tidy
# ------------------------------------------------------------

#' @export
glance.cronbach_alpha_exploratory <- function(x, pretty.name = FALSE, ...) {
  ci_lower <- if (nrow(x$confidence_interval) > 0) x$confidence_interval$lower[[1]] else NA_real_
  ci_upper <- if (nrow(x$confidence_interval) > 0) x$confidence_interval$upper[[1]] else NA_real_
  res <- tibble::tibble(
    `Coefficient` = x$coefficient_name,
    `Alpha` = x$alpha,
    `Standardized Alpha` = x$standardized_alpha,
    `CI Lower` = ci_lower,
    `CI Upper` = ci_upper,
    `G6` = x$g6,
    `Number of Variables` = length(x$item_types),
    `Complete Responses` = x$complete_n,
    `Responses with Missing` = x$rows_with_missing_n,
    `Average Inter-item Correlation` = x$average_r,
    `Correlation Type` = x$selected_method,
    `Interpretation` = reliability_classify_alpha(x$alpha)
  )
  res
}

#' @export
tidy.cronbach_alpha_exploratory <- function(x, type = "summary", pretty.name = FALSE, ...) {
  if (type == "summary") {
    res <- x$summary_table
  } else if (type == "alpha_if_dropped") {
    res <- x$alpha_if_deleted %>%
      dplyr::select(`Dropped Item` = removed_item,
                    `Alpha if Dropped` = alpha_if_dropped,
                    `Difference from Current` = difference_from_current,
                    `Interpretation` = interpretation)
  } else if (type == "item_stats") {
    res <- x$item_statistics %>%
      dplyr::select(`Variable` = variable,
                    `Item-Rest Correlation` = r.drop,
                    `Standardized Item-Total` = std.r,
                    `Corrected Correlation` = r.cor,
                    `Rows` = n,
                    `Missing` = missing_n,
                    `Mean` = mean,
                    `Standard Deviation` = sd,
                    `Interpretation` = interpretation)
    if ("raw.r" %in% names(x$item_statistics)) {
      res <- res %>% dplyr::mutate(`Item-Total Correlation` = x$item_statistics$raw.r) %>%
        dplyr::select(`Variable`, `Item-Rest Correlation`, `Item-Total Correlation`,
                      `Standardized Item-Total`, `Corrected Correlation`, `Rows`,
                      `Missing`, `Mean`, `Standard Deviation`, `Interpretation`)
    }
  } else if (type == "correlation") {
    res <- x$correlation_long
  } else if (type == "response_distribution") {
    res <- x$response_distribution %>%
      dplyr::select(`Variable` = variable, `Response` = response,
                    `Count` = count, `Proportion` = proportion)
  } else if (type == "scale_candidates") {
    res <- x$scale_candidates %>%
      dplyr::select(`Candidate` = candidate, `Number of Items` = item_count,
                    `Alpha` = alpha, `Recommendation` = recommendation)
  } else {
    # data - original filtered df
    res <- x$df
  }
  res
}
