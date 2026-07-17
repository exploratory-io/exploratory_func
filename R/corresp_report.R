# Correspondence Analysis report helpers (#37086)
#
# Fit-time computation for the redesigned Correspondence Analysis report.
# Adopted from the report spec by Kan (issue #37086). These builders produce
# language-neutral results (status enums, numeric metrics); localized sentences
# and judgement labels are produced on the tam/JS side.
#
# Two public builders:
#   build_pairwise_association_results()  -> sections 3 (variable-pair chi-square) and 4 (residual cells / featured combinations)
#   build_section5_from_factominer()      -> section 5 (per-dimension category coordinates / contribution / cos2)

# ============================================================
# Section 3 / 4: helpers
# ============================================================

# Category display order: factor -> defined levels, character -> first appearance.
ca_get_category_levels <- function(x) {
  if (is.factor(x)) {
    levels(droplevels(x))
  } else {
    unique(as.character(x[!is.na(x)]))
  }
}

# Cramer's V
ca_calculate_cramers_v <- function(contingency_table, chi_square_statistic) {
  n <- sum(contingency_table)
  d <- min(nrow(contingency_table) - 1, ncol(contingency_table) - 1)
  if (n <= 0 || d <= 0 || is.na(chi_square_statistic)) {
    return(NA_real_)
  }
  sqrt(chi_square_statistic / (n * d))
}

# Cramer's V strength classification (size-aware), returns language-neutral enum.
ca_classify_cramers_v <- function(cramers_v, number_of_rows, number_of_columns) {
  d <- min(number_of_rows - 1, number_of_columns - 1)
  if (is.na(cramers_v) || d <= 0) {
    return("undeterminable")
  }
  small_threshold <- 0.10 / sqrt(d)
  medium_threshold <- 0.30 / sqrt(d)
  large_threshold <- 0.50 / sqrt(d)
  dplyr::case_when(
    cramers_v < small_threshold ~ "very_weak",
    cramers_v < medium_threshold ~ "weak",
    cramers_v < large_threshold ~ "moderate",
    TRUE ~ "strong"
  )
}

# Expected-count diagnostics for the chi-square approximation.
ca_evaluate_expected_counts <- function(expected_counts) {
  number_of_cells <- length(expected_counts)
  number_under_1 <- sum(expected_counts < 1)
  number_under_5 <- sum(expected_counts < 5)
  proportion_under_5 <- number_under_5 / number_of_cells
  approximation_ok <- (number_under_1 == 0 && proportion_under_5 <= 0.20)
  list(
    approximation_ok = approximation_ok,
    minimum_expected_count = min(expected_counts),
    number_under_1 = number_under_1,
    number_under_5 = number_under_5,
    proportion_under_5 = proportion_under_5
  )
}

ca_create_significance_marker <- function(adjusted_p_value) {
  dplyr::case_when(
    is.na(adjusted_p_value) ~ "",
    adjusted_p_value < 0.001 ~ "***",
    adjusted_p_value < 0.01 ~ "**",
    adjusted_p_value < 0.05 ~ "*",
    TRUE ~ ""
  )
}

# ------------------------------------------------------------
# Analyze one variable pair
# ------------------------------------------------------------
ca_analyze_one_variable_pair <- function(
  data, variable_1, variable_2, pair_index,
  cell_adjust_method, alpha, simulation_count, seed,
  suppress_cell_p_when_sparse,
  practical_ratio_upper, practical_ratio_lower, practical_minimum_difference
) {
  pair_data <- data %>%
    dplyr::transmute(
      row_category = .data[[variable_1]],
      column_category = .data[[variable_2]]
    ) %>%
    tidyr::drop_na()

  pair_id <- paste(variable_1, variable_2, sep = " × ")

  empty_pair_result <- function(n, row_cat_count, column_cat_count) {
    tibble::tibble(
      pair_index = pair_index, pair_id = pair_id,
      variable_1 = variable_1, variable_2 = variable_2,
      n = n, row_category_count = row_cat_count, column_category_count = column_cat_count,
      chi_square = NA_real_, df = NA_real_, p_value = NA_real_,
      cramers_v = NA_real_, association_strength = "undeterminable",
      test_method = "undeterminable", expected_count_warning = TRUE,
      minimum_expected_count = NA_real_, expected_under_1_count = NA_integer_,
      expected_under_5_count = NA_integer_, expected_under_5_rate = NA_real_
    )
  }

  if (nrow(pair_data) == 0) {
    return(list(pair_result = empty_pair_result(0, 0, 0), cell_results = tibble::tibble()))
  }

  row_levels <- ca_get_category_levels(pair_data$row_category)
  column_levels <- ca_get_category_levels(pair_data$column_category)

  pair_data <- pair_data %>%
    dplyr::mutate(
      row_category = factor(row_category, levels = row_levels),
      column_category = factor(column_category, levels = column_levels)
    )

  contingency_table <- table(pair_data$row_category, pair_data$column_category)
  contingency_table <- contingency_table[
    rowSums(contingency_table) > 0, colSums(contingency_table) > 0, drop = FALSE
  ]

  if (nrow(contingency_table) < 2 || ncol(contingency_table) < 2) {
    return(list(
      pair_result = empty_pair_result(sum(contingency_table), nrow(contingency_table), ncol(contingency_table)),
      cell_results = tibble::tibble()
    ))
  }

  pearson_test <- suppressWarnings(chisq.test(contingency_table, correct = FALSE))
  expected_diagnostics <- ca_evaluate_expected_counts(pearson_test$expected)
  asymptotic_approximation_ok <- expected_diagnostics$approximation_ok

  if (asymptotic_approximation_ok) {
    overall_p_value <- pearson_test$p.value
    test_method <- "pearson"
  } else if (nrow(contingency_table) == 2 && ncol(contingency_table) == 2) {
    fisher_result <- fisher.test(contingency_table)
    overall_p_value <- fisher_result$p.value
    test_method <- "fisher"
  } else {
    set.seed(seed + pair_index)
    monte_carlo_result <- chisq.test(
      contingency_table, correct = FALSE,
      simulate.p.value = TRUE, B = simulation_count
    )
    overall_p_value <- monte_carlo_result$p.value
    test_method <- paste0("monte_carlo:", simulation_count)
  }

  chi_square_statistic <- as.numeric(pearson_test$statistic)
  degrees_of_freedom <- (nrow(contingency_table) - 1) * (ncol(contingency_table) - 1)
  cramers_v <- ca_calculate_cramers_v(contingency_table, chi_square_statistic)
  association_strength <- ca_classify_cramers_v(cramers_v, nrow(contingency_table), ncol(contingency_table))

  cell_results <- as.data.frame(as.table(contingency_table), stringsAsFactors = FALSE)
  names(cell_results) <- c("row_category", "column_category", "observed_count")
  n <- sum(contingency_table)

  minimum_difference <- if (is.null(practical_minimum_difference)) {
    max(5, 0.005 * n)
  } else {
    practical_minimum_difference
  }

  cell_results <- cell_results %>%
    dplyr::mutate(
      expected_count = as.vector(pearson_test$expected),
      count_difference = observed_count - expected_count,
      absolute_count_difference = abs(count_difference),
      observed_expected_ratio = dplyr::if_else(expected_count > 0, observed_count / expected_count, NA_real_),
      pearson_residual = as.vector(pearson_test$residuals),
      adjusted_standardized_residual = as.vector(pearson_test$stdres),
      row_order = match(row_category, rownames(contingency_table)),
      column_order = match(column_category, colnames(contingency_table))
    ) %>%
    dplyr::mutate(
      cell_p_value = 2 * pnorm(abs(adjusted_standardized_residual), lower.tail = FALSE)
    )

  if (!asymptotic_approximation_ok && suppress_cell_p_when_sparse) {
    cell_results <- cell_results %>%
      dplyr::mutate(cell_p_value = NA_real_, cell_adjusted_p_value = NA_real_)
  } else {
    cell_results <- cell_results %>%
      dplyr::mutate(cell_adjusted_p_value = p.adjust(cell_p_value, method = cell_adjust_method))
  }

  cell_results <- cell_results %>%
    dplyr::mutate(
      cell_significant = !is.na(cell_adjusted_p_value) & cell_adjusted_p_value < alpha,
      residual_direction = dplyr::case_when(
        adjusted_standardized_residual > 0 ~ "more_than_expected",
        adjusted_standardized_residual < 0 ~ "less_than_expected",
        TRUE ~ "as_expected"
      ),
      practical_difference = (
        (observed_expected_ratio >= practical_ratio_upper | observed_expected_ratio <= practical_ratio_lower) &
          absolute_count_difference >= minimum_difference
      ),
      significance_marker = ca_create_significance_marker(cell_adjusted_p_value),
      cell_label = paste0(sprintf("%.2f", adjusted_standardized_residual), significance_marker)
    ) %>%
    dplyr::mutate(
      pair_index = pair_index, pair_id = pair_id,
      variable_1 = variable_1, variable_2 = variable_2,
      n = n, test_method = test_method,
      expected_count_warning = !asymptotic_approximation_ok,
      cell_adjust_method = cell_adjust_method,
      practical_minimum_difference = minimum_difference,
      .before = 1
    )

  pair_result <- tibble::tibble(
    pair_index = pair_index, pair_id = pair_id,
    variable_1 = variable_1, variable_2 = variable_2,
    n = n, row_category_count = nrow(contingency_table),
    column_category_count = ncol(contingency_table),
    chi_square = chi_square_statistic, df = degrees_of_freedom,
    p_value = overall_p_value, cramers_v = cramers_v,
    association_strength = association_strength, test_method = test_method,
    expected_count_warning = !asymptotic_approximation_ok,
    minimum_expected_count = expected_diagnostics$minimum_expected_count,
    expected_under_1_count = expected_diagnostics$number_under_1,
    expected_under_5_count = expected_diagnostics$number_under_5,
    expected_under_5_rate = expected_diagnostics$proportion_under_5
  )

  list(pair_result = pair_result, cell_results = cell_results)
}

# ------------------------------------------------------------
# Main builder: sections 3 and 4
# ------------------------------------------------------------
#' @export
build_pairwise_association_results <- function(
  data, variables,
  overall_adjust_method = "holm",
  cell_adjust_method = "holm",
  alpha = 0.05,
  missing_method = c("listwise", "pairwise"),
  simulation_count = 20000,
  seed = 123,
  suppress_cell_p_when_sparse = TRUE,
  featured_rule = c("statistical", "statistical_and_practical"),
  practical_ratio_upper = 1.20,
  practical_ratio_lower = 0.80,
  practical_minimum_difference = NULL,
  require_overall_significance = TRUE
) {
  missing_method <- match.arg(missing_method)
  featured_rule <- match.arg(featured_rule)

  supported_adjust_methods <- c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
  if (!overall_adjust_method %in% supported_adjust_methods) stop("overall_adjust_method is invalid.")
  if (!cell_adjust_method %in% supported_adjust_methods) stop("cell_adjust_method is invalid.")
  if (length(variables) < 2) stop("Specify two or more categorical variables.")

  missing_variables <- setdiff(variables, names(data))
  if (length(missing_variables) > 0) {
    stop("Variables not found in data: ", paste(missing_variables, collapse = ", "))
  }

  if (missing_method == "listwise") {
    analysis_data <- data %>% dplyr::filter(dplyr::if_all(dplyr::all_of(variables), ~ !is.na(.x)))
  } else {
    analysis_data <- data
  }

  variable_pairs <- combn(variables, 2, simplify = FALSE)

  pair_analysis_results <- purrr::map2(
    variable_pairs, seq_along(variable_pairs),
    function(pair, pair_index) {
      ca_analyze_one_variable_pair(
        data = analysis_data, variable_1 = pair[[1]], variable_2 = pair[[2]],
        pair_index = pair_index, cell_adjust_method = cell_adjust_method,
        alpha = alpha, simulation_count = simulation_count, seed = seed,
        suppress_cell_p_when_sparse = suppress_cell_p_when_sparse,
        practical_ratio_upper = practical_ratio_upper,
        practical_ratio_lower = practical_ratio_lower,
        practical_minimum_difference = practical_minimum_difference
      )
    }
  )

  variable_pair_results <- purrr::map_dfr(pair_analysis_results, "pair_result")

  valid_p_value <- !is.na(variable_pair_results$p_value) & is.finite(variable_pair_results$p_value)
  variable_pair_results$adjusted_p_value <- NA_real_
  variable_pair_results$adjusted_p_value[valid_p_value] <- p.adjust(
    variable_pair_results$p_value[valid_p_value], method = overall_adjust_method
  )
  number_of_valid_tests <- sum(valid_p_value)

  variable_pair_results <- variable_pair_results %>%
    dplyr::mutate(
      overall_adjust_method = overall_adjust_method,
      number_of_tests = number_of_valid_tests,
      pair_significant = !is.na(adjusted_p_value) & adjusted_p_value < alpha,
      # language-neutral judgement enum: significant_moderate_strong | significant_weak | not_significant | undeterminable
      judgement = dplyr::case_when(
        is.na(adjusted_p_value) ~ "undeterminable",
        pair_significant & association_strength %in% c("moderate", "strong") ~ "significant_notable",
        pair_significant ~ "significant_weak",
        TRUE ~ "not_significant"
      )
    ) %>%
    dplyr::arrange(adjusted_p_value, dplyr::desc(cramers_v))

  residual_heatmap_data <- purrr::map_dfr(pair_analysis_results, "cell_results")

  settings <- list(
    variables = variables, analysis_n = nrow(analysis_data),
    overall_adjust_method = overall_adjust_method, cell_adjust_method = cell_adjust_method,
    alpha = alpha, missing_method = missing_method, simulation_count = simulation_count,
    suppress_cell_p_when_sparse = suppress_cell_p_when_sparse, featured_rule = featured_rule,
    practical_ratio_upper = practical_ratio_upper, practical_ratio_lower = practical_ratio_lower,
    practical_minimum_difference = practical_minimum_difference,
    require_overall_significance = require_overall_significance
  )

  if (nrow(residual_heatmap_data) == 0) {
    return(list(
      variable_pair_results = variable_pair_results,
      residual_heatmap_data = tibble::tibble(),
      featured_combinations = tibble::tibble(),
      settings = settings
    ))
  }

  residual_heatmap_data <- residual_heatmap_data %>%
    dplyr::left_join(
      variable_pair_results %>%
        dplyr::select(pair_index, p_value, adjusted_p_value, pair_significant, cramers_v, association_strength, judgement) %>%
        dplyr::rename(
          overall_p_value = p_value,
          overall_adjusted_p_value = adjusted_p_value,
          overall_judgement = judgement
        ),
      by = "pair_index"
    ) %>%
    dplyr::mutate(
      statistically_featured = (
        cell_significant & !expected_count_warning &
          (!require_overall_significance | pair_significant)
      ),
      # featured_rule is a scalar: "statistical" -> statistically_featured;
      # "statistical_and_practical" -> also require practical_difference.
      featured_combination = statistically_featured &
        (featured_rule == "statistical" | practical_difference),
      # language-neutral final judgement enum
      final_judgement = dplyr::case_when(
        expected_count_warning & suppress_cell_p_when_sparse ~ "sparse_suppressed",
        require_overall_significance & !pair_significant ~ "exploratory_pair_not_significant",
        !cell_significant ~ "no_clear_difference",
        adjusted_standardized_residual > 0 & practical_difference ~ "significantly_more",
        adjusted_standardized_residual < 0 & practical_difference ~ "significantly_less",
        adjusted_standardized_residual > 0 ~ "more_but_small",
        adjusted_standardized_residual < 0 ~ "less_but_small",
        TRUE ~ "no_clear_difference"
      ),
      heatmap_fill_value = adjusted_standardized_residual,
      heatmap_label = cell_label,
      tooltip_category_pair = paste0(row_category, " × ", column_category)
    ) %>%
    dplyr::arrange(pair_index, row_order, column_order)

  featured_combinations <- residual_heatmap_data %>%
    dplyr::filter(featured_combination) %>%
    dplyr::mutate(
      absolute_residual = abs(adjusted_standardized_residual),
      category_pair = paste0(row_category, " × ", column_category)
    ) %>%
    dplyr::arrange(pair_index, cell_adjusted_p_value, dplyr::desc(absolute_residual)) %>%
    dplyr::group_by(pair_id) %>%
    dplyr::mutate(rank_within_pair = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      pair_index, pair_id, variable_1, variable_2, rank_within_pair, category_pair,
      row_category, column_category, observed_count, expected_count, count_difference,
      observed_expected_ratio, adjusted_standardized_residual, cell_p_value,
      cell_adjusted_p_value, practical_difference, final_judgement, n, cramers_v, association_strength
    )

  list(
    variable_pair_results = variable_pair_results,
    residual_heatmap_data = residual_heatmap_data,
    featured_combinations = featured_combinations,
    settings = settings
  )
}

# ============================================================
# Section 5: helpers
# ============================================================

ca_matrix_to_long <- function(x, value_name) {
  if (is.null(x)) stop("A matrix-form analysis result is required.")
  # A single-dimension CA/MCA result is returned as a named numeric vector
  # (the "Dim 1" column name is dropped). Coerce it back to a 1-column matrix.
  if (!is.matrix(x)) {
    nm <- names(x)
    x <- matrix(as.numeric(x), ncol = 1, dimnames = list(nm, "Dim 1"))
  }
  as.data.frame(x, check.names = FALSE) %>%
    tibble::rownames_to_column("category_id") %>%
    tidyr::pivot_longer(cols = -category_id, names_to = "dimension_name", values_to = value_name) %>%
    dplyr::mutate(dimension = as.integer(stringr::str_extract(dimension_name, "\\d+"))) %>%
    dplyr::select(-dimension_name)
}

ca_extract_eigenvalues <- function(result) {
  eig <- as.data.frame(result$eig)
  if (ncol(eig) < 2) stop("Cannot obtain eigenvalues and explained rates.")
  explained_pct <- as.numeric(eig[[2]])
  cumulative_pct <- if (ncol(eig) >= 3) as.numeric(eig[[3]]) else cumsum(explained_pct)
  tibble::tibble(
    dimension = seq_len(nrow(eig)),
    eigenvalue = as.numeric(eig[[1]]),
    explained_pct = explained_pct,
    cumulative_pct = cumulative_pct
  )
}

# language-neutral cos2 quality enum
ca_classify_cos2 <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x >= 0.70 ~ "well_represented",
    x >= 0.40 ~ "moderately_represented",
    x >= 0.20 ~ "limited_representation",
    TRUE ~ "interpret_with_caution"
  )
}

# Category counts / shares for MCA
ca_calculate_mca_category_counts <- function(source_data, variables) {
  purrr::map_dfr(
    seq_along(variables),
    function(variable_index) {
      variable_name <- variables[[variable_index]]
      source_data %>%
        dplyr::transmute(category = as.character(.data[[variable_name]])) %>%
        dplyr::filter(!is.na(category)) %>%
        dplyr::count(category, name = "count") %>%
        dplyr::mutate(
          variable = variable_name,
          variable_total = sum(count),
          share = count / variable_total,
          variable_order = variable_index
        )
    }
  )
}

# Category counts / shares for CA (from the contingency table)
ca_calculate_ca_category_counts <- function(contingency_table, row_variable_name, column_variable_name) {
  contingency_table <- as.matrix(contingency_table)
  total_n <- sum(contingency_table)
  row_counts <- tibble::tibble(
    variable = row_variable_name, category = rownames(contingency_table),
    count = as.numeric(rowSums(contingency_table)), variable_total = total_n,
    share = count / variable_total, variable_order = 1L,
    category_order = seq_len(nrow(contingency_table))
  )
  column_counts <- tibble::tibble(
    variable = column_variable_name, category = colnames(contingency_table),
    count = as.numeric(colSums(contingency_table)), variable_total = total_n,
    share = count / variable_total, variable_order = 2L,
    category_order = seq_len(ncol(contingency_table))
  )
  dplyr::bind_rows(row_counts, column_counts)
}

# MCA: FactoMineR result -> tidy metrics (category_lookup supplied by caller via V<i>: prefix)
ca_extract_factominer_mca_metrics <- function(result, category_lookup, source_data = NULL, variables = NULL) {
  if (is.null(result$var$coord) || is.null(result$var$contrib) || is.null(result$var$cos2)) {
    stop("MCA result is missing coord, contrib, or cos2.")
  }
  coordinates <- ca_matrix_to_long(result$var$coord, "coordinate")
  contributions <- ca_matrix_to_long(result$var$contrib, "contribution_pct")
  cos2 <- ca_matrix_to_long(result$var$cos2, "cos2")
  eigenvalues <- ca_extract_eigenvalues(result)

  metrics <- coordinates %>%
    dplyr::left_join(contributions, by = c("category_id", "dimension")) %>%
    dplyr::left_join(cos2, by = c("category_id", "dimension")) %>%
    dplyr::left_join(category_lookup, by = "category_id") %>%
    dplyr::left_join(eigenvalues, by = "dimension") %>%
    dplyr::mutate(contribution_group = "all")

  if (!is.null(source_data) && !is.null(variables)) {
    counts <- ca_calculate_mca_category_counts(source_data, variables)
    metrics <- metrics %>%
      dplyr::left_join(counts %>% dplyr::select(variable, category, count, variable_total, share),
                       by = c("variable", "category"))
  } else {
    metrics <- metrics %>% dplyr::mutate(count = NA_real_, variable_total = NA_real_, share = NA_real_)
  }
  metrics
}

# CA: FactoMineR result -> tidy metrics (row + column parts)
ca_extract_factominer_ca_metrics <- function(result, row_variable_name, column_variable_name, contingency_table = NULL) {
  if (is.null(result$row) || is.null(result$col)) stop("CA result is missing row or col results.")
  eigenvalues <- ca_extract_eigenvalues(result)

  create_part <- function(part, variable_name, contribution_group, variable_order) {
    coordinates <- ca_matrix_to_long(part$coord, "coordinate")
    contributions <- ca_matrix_to_long(part$contrib, "contribution_pct")
    cos2 <- ca_matrix_to_long(part$cos2, "cos2")
    coordinates %>%
      dplyr::left_join(contributions, by = c("category_id", "dimension")) %>%
      dplyr::left_join(cos2, by = c("category_id", "dimension")) %>%
      dplyr::mutate(
        variable = variable_name, category = category_id,
        contribution_group = contribution_group, variable_order = variable_order,
        category_order = match(category_id, unique(category_id))
      )
  }

  row_metrics <- create_part(result$row, row_variable_name, "row", 1L)
  column_metrics <- create_part(result$col, column_variable_name, "column", 2L)
  metrics <- dplyr::bind_rows(row_metrics, column_metrics) %>%
    dplyr::left_join(eigenvalues, by = "dimension")

  if (!is.null(contingency_table)) {
    counts <- ca_calculate_ca_category_counts(contingency_table, row_variable_name, column_variable_name)
    metrics <- metrics %>%
      dplyr::left_join(counts %>% dplyr::select(variable, category, count, variable_total, share),
                       by = c("variable", "category"))
  } else {
    metrics <- metrics %>% dplyr::mutate(count = NA_real_, variable_total = NA_real_, share = NA_real_)
  }
  metrics
}

# Common derived columns + judgement enums
ca_prepare_section5_metrics <- function(metrics) {
  required_columns <- c("dimension", "variable", "category", "coordinate",
                        "contribution_pct", "cos2", "contribution_group",
                        "explained_pct", "cumulative_pct")
  missing_columns <- setdiff(required_columns, names(metrics))
  if (length(missing_columns) > 0) stop("Missing required columns: ", paste(missing_columns, collapse = ", "))

  metrics %>%
    dplyr::group_by(dimension, contribution_group) %>%
    dplyr::mutate(
      expected_contribution_pct = 100 / dplyr::n(),
      contribution_above_average = contribution_pct >= expected_contribution_pct
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dimension) %>%
    dplyr::mutate(contribution_rank = dplyr::min_rank(dplyr::desc(contribution_pct))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      category_key = paste(variable, category, sep = ""),
      side = dplyr::case_when(coordinate > 0 ~ "positive", coordinate < 0 ~ "negative", TRUE ~ "origin"),
      cos2_quality = ca_classify_cos2(cos2),
      interpretation_score = contribution_pct * cos2,
      low_frequency_status = dplyr::case_when(
        is.na(count) | is.na(share) ~ NA_character_,
        count < 5 | share < 0.002 ~ "strong_warning",
        count < pmax(10, variable_total * 0.005) ~ "caution",
        TRUE ~ "ok"
      ),
      cos2_warning = cos2 < 0.20,
      dimension_label = paste0("Dimension ", dimension)
    )
}

# 5-2 default category selection
ca_select_default_category_ids <- function(dimension_data, top_n = 10, minimum_per_side = 2) {
  base <- dimension_data %>%
    dplyr::filter(coordinate != 0, contribution_above_average | contribution_rank <= top_n)
  for (side_name in c("positive", "negative")) {
    current_count <- sum(base$side == side_name)
    if (current_count < minimum_per_side) {
      additional <- dimension_data %>%
        dplyr::filter(side == side_name, coordinate != 0, !category_key %in% base$category_key) %>%
        dplyr::arrange(dplyr::desc(interpretation_score), dplyr::desc(contribution_pct)) %>%
        dplyr::slice_head(n = minimum_per_side - current_count)
      base <- dplyr::bind_rows(base, additional)
    }
  }
  unique(base$category_key)
}

ca_add_default_display_flag <- function(metrics, max_dimensions = 5, top_n = 10, minimum_per_side = 2) {
  selected_ids <- metrics %>%
    dplyr::filter(dimension <= max_dimensions) %>%
    dplyr::group_split(dimension) %>%
    purrr::map(ca_select_default_category_ids, top_n = top_n, minimum_per_side = minimum_per_side) %>%
    unlist(use.names = FALSE) %>%
    unique()
  metrics %>% dplyr::mutate(default_display = category_key %in% selected_ids)
}

# 5-1 summary category selection (returns rows, language-neutral)
ca_select_summary_categories <- function(dimension_data, side_name, maximum_categories = 3, minimum_cos2 = 0.20) {
  candidates <- dimension_data %>%
    dplyr::filter(side == side_name, coordinate != 0,
                  is.na(low_frequency_status) | low_frequency_status != "strong_warning")
  primary <- candidates %>% dplyr::filter(contribution_above_average, cos2 >= minimum_cos2)
  if (nrow(primary) == 0) primary <- candidates %>% dplyr::filter(cos2 >= minimum_cos2)
  if (nrow(primary) == 0) primary <- candidates
  primary %>%
    dplyr::arrange(dplyr::desc(interpretation_score), dplyr::desc(contribution_pct)) %>%
    dplyr::slice_head(n = maximum_categories)
}

# 5-1 dimension summary (language-neutral; positive/negative categories joined as readable text)
ca_build_dimension_summary <- function(metrics, max_dimensions = 5, analysis_type = "MCA") {
  dimensions <- sort(unique(metrics$dimension))
  dimensions <- dimensions[dimensions <= max_dimensions]

  join_labels <- function(x) {
    if (nrow(x) == 0) return("")
    paste(paste0(x$variable, ": ", x$category), collapse = ", ")
  }

  purrr::map_dfr(dimensions, function(current_dimension) {
    current <- metrics %>% dplyr::filter(dimension == current_dimension)
    positive <- ca_select_summary_categories(current, "positive")
    negative <- ca_select_summary_categories(current, "negative")

    variable_contributions <- current %>%
      dplyr::group_by(variable) %>%
      dplyr::summarise(variable_contribution = sum(contribution_pct, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(variable_contribution))

    main_variables <- if (analysis_type == "CA") {
      paste(variable_contributions$variable, collapse = ", ")
    } else {
      paste(head(variable_contributions$variable, 2), collapse = ", ")
    }

    pattern_status <- dplyr::case_when(
      nrow(positive) > 0 && nrow(negative) > 0 ~ "contrast",
      nrow(positive) > 0 ~ "positive_only",
      nrow(negative) > 0 ~ "negative_only",
      TRUE ~ "none"
    )

    tibble::tibble(
      dimension = current_dimension,
      explained_pct = dplyr::first(current$explained_pct),
      cumulative_pct = dplyr::first(current$cumulative_pct),
      positive_categories = join_labels(positive),
      negative_categories = join_labels(negative),
      main_variables = main_variables,
      pattern_status = pattern_status
    )
  })
}

# 5-2 matrix (long)
ca_build_dimension_matrix_long <- function(metrics, max_dimensions = 5, only_default_categories = TRUE) {
  selected <- metrics %>% dplyr::filter(dimension <= max_dimensions)
  if (only_default_categories) selected <- selected %>% dplyr::filter(default_display)
  selected %>%
    dplyr::transmute(
      variable_order, category_order,
      variable, category, low_frequency_status, dimension,
      dimension_header = sprintf("Dimension %d (%.1f%%)", dimension, explained_pct),
      coordinate = round(coordinate, 2),
      contribution_pct = round(contribution_pct, 1),
      cos2 = round(cos2, 2),
      expected_contribution_pct = round(expected_contribution_pct, 1),
      contribution_above_average, cos2_quality, count, share
    ) %>%
    dplyr::arrange(variable_order, category_order, dimension)
}

# 5-3 details
ca_build_category_details <- function(metrics) {
  metrics %>%
    dplyr::transmute(
      dimension, explained_pct, cumulative_pct, variable, category, side,
      coordinate, contribution_pct, expected_contribution_pct, contribution_above_average,
      contribution_rank, cos2, cos2_quality, count, share, low_frequency_status, default_display
    ) %>%
    dplyr::arrange(dimension, variable, category)
}

# Public: build all section-5 outputs from a FactoMineR result
#' @export
build_section5_from_factominer <- function(
  result, analysis_type = c("CA", "MCA"),
  category_lookup = NULL, source_data = NULL, variables = NULL,
  row_variable_name = "row_variable", column_variable_name = "column_variable",
  contingency_table = NULL,
  max_dimensions = 5, top_n = 10, minimum_per_side = 2
) {
  analysis_type <- match.arg(analysis_type)

  metrics <- if (analysis_type == "CA") {
    ca_extract_factominer_ca_metrics(result, row_variable_name, column_variable_name, contingency_table)
  } else {
    ca_extract_factominer_mca_metrics(result, category_lookup, source_data, variables)
  }

  prepared <- ca_prepare_section5_metrics(metrics) %>%
    ca_add_default_display_flag(max_dimensions = max_dimensions, top_n = top_n, minimum_per_side = minimum_per_side)

  list(
    dimension_summary = ca_build_dimension_summary(prepared, max_dimensions, analysis_type),
    dimension_matrix_long = ca_build_dimension_matrix_long(prepared, max_dimensions, only_default_categories = TRUE),
    category_details = ca_build_category_details(prepared),
    all_metrics = prepared
  )
}
