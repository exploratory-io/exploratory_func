#' Function for Correspondence Analysis Analytics View
#'
#' 2 selected variables -> FactoMineR::CA on the contingency table (class ca_exploratory).
#' 3 or more            -> FactoMineR::MCA (class mca_exploratory).
#' Fit-time computation (pairwise chi-square associations + per-dimension metrics)
#' is stored on the model object; tidy() extracts it. See R/corresp_report.R.
#' @export
exp_mca <- function(df, ..., max_nrow = NULL, allow_single_column = FALSE, ncp = 5,
                    quanti_sups = NULL, seed = 1,
                    overall_adjust_method = "holm", cell_adjust_method = "holm", alpha = 0.05,
                    missing_method = "listwise",
                    simulation_count = 20000) {
  all_cols <- colnames(df)
  selected_cols <- tidyselect::vars_select(names(df), !!! rlang::quos(...))
  grouped_cols <- grouped_by(df)
  selected_cols <- setdiff(selected_cols, grouped_cols)
  quanti_sups <- setdiff(quanti_sups, grouped_cols)

  if (any(selected_cols %in% grouped_cols)) {
    stop("Repeat-By column cannot be used as a variable column.")
  }

  df <- df %>%
    dplyr::select(-where(is.list),
                  -where(lubridate::is.difftime),
                  -where(lubridate::is.duration),
                  -where(lubridate::is.interval),
                  -where(lubridate::is.period))

  if(!is.null(seed)) { # Set seed before starting to call sample_n.
    set.seed(seed)
  }

  each_func <- function(df) {
    sampled_nrow <- NULL
    original_nrow <- nrow(df)
    if (!is.null(max_nrow) && nrow(df) > max_nrow) {
      sampled_nrow <- max_nrow
      df <- df %>% sample_rows(max_nrow)
    }

    cleaned_df <- df[, colnames(df) %in% c(selected_cols, quanti_sups), drop = FALSE]

    for (col in selected_cols) {
      unique_val <- unique(cleaned_df[[col]])
      if (length(unique_val) == 1) {
        cleaned_df <- cleaned_df[colnames(cleaned_df) != col]
      }
    }
    if (allow_single_column) {
      min_ncol <- 1
    } else {
      min_ncol <- 2
    }
    if (sum(selected_cols %in% colnames(cleaned_df)) < min_ncol) {
      if (length(grouped_cols) < 1) {
        stop("There are not enough columns after removing the columns with only NA or a single value.")
      } else {
        return(NULL)
      }
    }

    # Effective (non-dropped) selected variables, in selection order.
    effective_vars <- selected_cols[selected_cols %in% colnames(cleaned_df)]

    # Rows used by the CA fit and by listwise association reporting.
    report_source <- df[, effective_vars, drop = FALSE]
    complete_rows <- stats::complete.cases(report_source)
    n_used <- if (missing_method == "listwise") sum(complete_rows) else nrow(df)
    n_excluded <- original_nrow - n_used

    is_ca <- length(effective_vars) == 2

    if (is_ca) {
      row_var <- effective_vars[[1]]
      col_var <- effective_vars[[2]]
      ct_data <- report_source[complete_rows, , drop = FALSE]
      contingency_table <- table(
        factor(as.character(ct_data[[row_var]]), levels = ca_get_category_levels(ct_data[[row_var]])),
        factor(as.character(ct_data[[col_var]]), levels = ca_get_category_levels(ct_data[[col_var]]))
      )
      fit <- FactoMineR::CA(contingency_table, ncp = ncp, graph = FALSE)
      fit$analysis_type <- "CA"
      fit$row_variable_name <- row_var
      fit$column_variable_name <- col_var
      fit$contingency_table <- contingency_table
      fit$section5 <- build_section5_from_factominer(
        fit, analysis_type = "CA",
        row_variable_name = row_var, column_variable_name = col_var,
        contingency_table = contingency_table, max_dimensions = ncp
      )
      class(fit) <- c("ca_exploratory", "mca_exploratory", class(fit))
    } else {
      var_names_map <- colnames(cleaned_df)
      names(var_names_map) <- paste0("V", 1:length(var_names_map))
      # Prefix category values with the column index so they are unique across columns.
      for (i in 1:length(cleaned_df)) {
        if (colnames(cleaned_df)[i] %in% selected_cols) {
          cleaned_df[i] <- as.factor(paste0("V", i, ":", cleaned_df[[i]]))
        }
      }
      quanti_sup_idx <- which(colnames(cleaned_df) %in% quanti_sups)
      if (length(quanti_sup_idx) == 0) quanti_sup_idx <- NULL
      fit <- FactoMineR::MCA(cleaned_df, ncp = ncp, graph = FALSE, quanti.sup = quanti_sup_idx)
      fit$analysis_type <- "MCA"
      fit$var_names_map <- var_names_map

      # category_lookup for section 5 (real variable/category from the V<i>: id).
      category_ids <- rownames(fit$var$coord)
      fit$category_lookup <- .mca_build_category_lookup(category_ids, var_names_map, effective_vars, df)
      fit$section5 <- build_section5_from_factominer(
        fit, analysis_type = "MCA",
        category_lookup = fit$category_lookup, source_data = df, variables = effective_vars,
        max_dimensions = ncp
      )
      class(fit) <- c("mca_exploratory", class(fit))
    }

    # Sections 3 & 4: pairwise associations (works for both CA and MCA).
    # With one selected variable there are no pairs, but the legacy
    # allow_single_column option still returns the fitted MCA model.
    if (length(effective_vars) >= 2) {
      association_data <- if (missing_method == "listwise") report_source else df
      fit$association <- build_pairwise_association_results(
        data = association_data, variables = effective_vars,
        overall_adjust_method = overall_adjust_method, cell_adjust_method = cell_adjust_method,
        alpha = alpha, missing_method = missing_method, simulation_count = simulation_count,
        seed = seed
      )
    } else {
      fit$association <- list(
        variable_pair_results = tibble::tibble(),
        residual_heatmap_data = tibble::tibble(),
        featured_combinations = tibble::tibble(),
        settings = list(variables = effective_vars, analysis_n = n_used,
                        missing_method = missing_method)
      )
    }

    fit$effective_vars <- effective_vars
    fit$n_used <- n_used
    fit$n_excluded <- n_excluded
    fit$category_total <- nrow(fit$section5$all_metrics %>% dplyr::distinct(variable, category))
    fit$n_dims <- nrow(fit$eig)
    fit$df <- df
    fit$grouped_cols <- grouped_cols
    fit$sampled_nrow <- sampled_nrow
    fit
  }

  do_on_each_group(df, each_func, name = "model", with_unnest = FALSE)
}

#' Correspondence Analysis for already-aggregated (cross tab) input.
#'
#' Takes a wide cross tabulation: one row category column whose values are the row
#' categories, plus two or more numeric columns whose NAMES are the column categories
#' and whose values are the cell counts. The contingency table is handed to
#' FactoMineR::CA directly instead of being counted from raw rows, and the resulting
#' model object is assembled to be identical in shape to the 2-variable branch of
#' exp_mca() - so every tidy() type and the whole report pipeline work unchanged.
#'
#' @param row_category Column holding the row categories.
#' @param ... Numeric columns holding the aggregated counts. Their names become the
#'   column categories.
#' @param column_variable_name Display name of the variable the count columns represent.
#'   The aggregated input has no column carrying it, so it is supplied as text.
#' @export
exp_mca_aggregated <- function(df, row_category, ...,
                               column_variable_name = "Column",
                               ncp = 5, seed = 1,
                               overall_adjust_method = "holm", cell_adjust_method = "holm",
                               alpha = 0.05,
                               simulation_count = 20000) {
  row_col <- col_name(substitute(row_category))
  value_cols <- tidyselect::vars_select(names(df), !!! rlang::quos(...))
  grouped_cols <- grouped_by(df)
  value_cols <- setdiff(value_cols, c(grouped_cols, row_col))

  if (!row_col %in% colnames(df)) {
    stop(paste0("The row category column is not found: ", row_col))
  }
  if (row_col %in% grouped_cols) {
    stop("Repeat-By column cannot be used as the row category column.")
  }
  if (length(value_cols) < 2) {
    stop("Select two or more columns that hold the aggregated counts.")
  }
  non_numeric_cols <- value_cols[!purrr::map_lgl(value_cols, function(col) is.numeric(df[[col]]))]
  if (length(non_numeric_cols) > 0) {
    stop(paste0("The aggregated count columns must be numeric: ", paste(non_numeric_cols, collapse = ", ")))
  }
  if (is.null(column_variable_name) || is.na(column_variable_name) || column_variable_name == "") {
    column_variable_name <- "Column"
  }
  # The row and column variable names label separate category sets in the report
  # tables and the category map. Sharing one name would merge those sets.
  if (column_variable_name == row_col) {
    stop("The column variable name must be different from the row category column name.")
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  each_func <- function(df) {
    counts_df <- df[, c(row_col, value_cols), drop = FALSE]

    # An absent cell in a cross tab means "no observations", which is a zero count.
    counts_df <- counts_df %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(value_cols),
                                  ~ dplyr::if_else(is.na(.x), 0, as.numeric(.x))))

    count_values <- as.matrix(counts_df[, value_cols, drop = FALSE])
    .ca_validate_aggregated_counts(count_values)

    # Rows sharing a category label are summed rather than rejected.
    row_labels <- as.character(counts_df[[row_col]])
    row_labels[is.na(row_labels)] <- "NA"
    row_levels <- unique(row_labels[order(match(row_labels,
                                                ca_get_category_levels(counts_df[[row_col]])))])
    contingency_table <- rowsum(round(count_values), group = factor(row_labels, levels = row_levels),
                                reorder = FALSE)
    dimnames(contingency_table) <- list(rownames(contingency_table), value_cols)
    storage.mode(contingency_table) <- "integer"
    # Match the object the raw 2-variable path hands to FactoMineR::CA exactly:
    # a `table` with unnamed dimnames, not a bare matrix.
    names(dimnames(contingency_table)) <- c("", "")
    class(contingency_table) <- "table"

    .ca_fit_from_contingency_table(
      contingency_table = contingency_table,
      row_variable_name = row_col, column_variable_name = column_variable_name,
      ncp = ncp, seed = seed,
      overall_adjust_method = overall_adjust_method, cell_adjust_method = cell_adjust_method,
      alpha = alpha, simulation_count = simulation_count,
      df = df, grouped_cols = grouped_cols
    )
  }

  do_on_each_group(df, each_func, name = "model", with_unnest = FALSE)
}

# Shared assembly for both aggregated entry points (wide and long). Takes an
# already-built contingency table and returns the ca_exploratory model object,
# identical in shape to the 2-variable branch of exp_mca().
# Returns NULL for a grouped input whose table collapses below 2x2, so that
# Repeat By drops the group instead of aborting the whole analysis.
.ca_fit_from_contingency_table <- function(contingency_table, row_variable_name,
                                           column_variable_name, ncp, seed,
                                           overall_adjust_method, cell_adjust_method,
                                           alpha, simulation_count, df, grouped_cols) {
  contingency_table <- contingency_table[
    rowSums(contingency_table) > 0, colSums(contingency_table) > 0, drop = FALSE
  ]
  if (nrow(contingency_table) < 2 || ncol(contingency_table) < 2) {
    if (length(grouped_cols) < 1) {
      stop("There are not enough categories after removing rows and columns with no counts.")
    } else {
      return(NULL)
    }
  }

  # A table with k rows and m columns supports at most min(k, m) - 1 dimensions.
  effective_ncp <- min(ncp, min(dim(contingency_table)) - 1)

  fit <- FactoMineR::CA(contingency_table, ncp = effective_ncp, graph = FALSE)
  fit$analysis_type <- "CA"
  fit$row_variable_name <- row_variable_name
  fit$column_variable_name <- column_variable_name
  fit$contingency_table <- contingency_table
  fit$section5 <- build_section5_from_factominer(
    fit, analysis_type = "CA",
    row_variable_name = row_variable_name, column_variable_name = column_variable_name,
    contingency_table = contingency_table, max_dimensions = effective_ncp
  )
  class(fit) <- c("ca_exploratory", "mca_exploratory", class(fit))

  fit$association <- build_pairwise_association_results_from_counts(
    contingency_table = contingency_table,
    row_variable_name = row_variable_name, column_variable_name = column_variable_name,
    overall_adjust_method = overall_adjust_method, cell_adjust_method = cell_adjust_method,
    alpha = alpha, simulation_count = simulation_count, seed = seed
  )

  fit$effective_vars <- c(row_variable_name, column_variable_name)
  fit$n_used <- sum(contingency_table)
  fit$n_excluded <- 0
  fit$category_total <- nrow(fit$section5$all_metrics %>% dplyr::distinct(variable, category))
  fit$n_dims <- nrow(fit$eig)
  fit$df <- df
  fit$grouped_cols <- grouped_cols
  fit$sampled_nrow <- NULL
  fit
}

# Shared count validation for both aggregated entry points.
.ca_validate_aggregated_counts <- function(count_values) {
  if (any(!is.finite(count_values))) {
    stop("The aggregated counts must be finite numbers.")
  }
  if (any(count_values < 0)) {
    stop("The aggregated counts must not be negative.")
  }
  # Chi-square residuals, the Fisher exact test and the Monte Carlo p-value all
  # require whole counts. Weighted (fractional) counts are not supported.
  if (any(abs(count_values - round(count_values)) > 1e-8)) {
    stop("The aggregated counts must be whole numbers.")
  }
  invisible(TRUE)
}

#' Correspondence Analysis for already-aggregated input in LONG format.
#'
#' Takes three columns - the row category, the column category, and the count for
#' that combination - and builds the contingency table from them. The resulting
#' model object is identical to the one exp_mca_aggregated() produces from the
#' equivalent wide cross tab, so every tidy() type and the whole report pipeline
#' work unchanged.
#'
#' Unlike the wide form there is no column_variable_name argument: the column
#' variable's name IS the column category column's name.
#'
#' @param row_category Column holding the row categories.
#' @param column_category Column holding the column categories.
#' @param count Numeric column holding the count for each row/column combination.
#' @export
exp_mca_aggregated_long <- function(df, row_category, column_category, count,
                                    ncp = 5, seed = 1,
                                    overall_adjust_method = "holm", cell_adjust_method = "holm",
                                    alpha = 0.05,
                                    simulation_count = 20000) {
  row_col <- col_name(substitute(row_category))
  col_col <- col_name(substitute(column_category))
  count_col <- col_name(substitute(count))
  grouped_cols <- grouped_by(df)

  for (nm in c(row_col, col_col, count_col)) {
    if (!nm %in% colnames(df)) {
      stop(paste0("The column is not found: ", nm))
    }
  }
  if (length(unique(c(row_col, col_col, count_col))) < 3) {
    stop("The row category, column category and count columns must be different columns.")
  }
  if (any(c(row_col, col_col, count_col) %in% grouped_cols)) {
    stop("Repeat-By column cannot be used as the row category, column category or count column.")
  }
  if (!is.numeric(df[[count_col]])) {
    stop("The aggregated count column must be numeric.")
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  each_func <- function(df) {
    long_df <- df[, c(row_col, col_col, count_col), drop = FALSE]
    # An absent cell means "no observations", which is a zero count.
    counts <- as.numeric(long_df[[count_col]])
    counts[is.na(counts)] <- 0
    .ca_validate_aggregated_counts(counts)

    row_labels <- as.character(long_df[[row_col]])
    col_labels <- as.character(long_df[[col_col]])
    row_labels[is.na(row_labels)] <- "NA"
    col_labels[is.na(col_labels)] <- "NA"

    # Keep the declared category order (factor levels, else first appearance),
    # matching how the raw and wide paths order their categories.
    row_levels <- unique(row_labels[order(match(row_labels,
                                                ca_get_category_levels(long_df[[row_col]])))])
    col_levels <- unique(col_labels[order(match(col_labels,
                                                ca_get_category_levels(long_df[[col_col]])))])

    # xtabs sums duplicated row/column combinations rather than rejecting them,
    # mirroring how the wide path sums repeated row labels.
    contingency_table <- xtabs(
      counts ~ factor(row_labels, levels = row_levels) + factor(col_labels, levels = col_levels)
    )
    contingency_table <- round(contingency_table)
    dimnames(contingency_table) <- list(row_levels, col_levels)
    storage.mode(contingency_table) <- "integer"
    names(dimnames(contingency_table)) <- c("", "")
    class(contingency_table) <- "table"

    .ca_fit_from_contingency_table(
      contingency_table = contingency_table,
      row_variable_name = row_col, column_variable_name = col_col,
      ncp = ncp, seed = seed,
      overall_adjust_method = overall_adjust_method, cell_adjust_method = cell_adjust_method,
      alpha = alpha, simulation_count = simulation_count,
      df = df, grouped_cols = grouped_cols
    )
  }

  do_on_each_group(df, each_func, name = "model", with_unnest = FALSE)
}

# Build a category_id -> (variable, category, orders) lookup from MCA V<i>: ids.
.mca_build_category_lookup <- function(category_ids, var_names_map, variables, source_data) {
  # category_id like "V2:Some Value" (value itself may contain ":").
  prefix <- stringr::str_extract(category_ids, "^V\\d+")
  value <- stringr::str_replace(category_ids, "^V\\d+:", "")
  variable <- unname(var_names_map[prefix])
  variable_order <- match(variable, variables)
  lookup <- tibble::tibble(
    category_id = category_ids,
    variable = variable,
    category = value,
    variable_order = variable_order
  )
  lookup %>%
    dplyr::group_by(variable) %>%
    dplyr::mutate(category_order = dplyr::row_number()) %>%
    dplyr::ungroup()
}

# Coerce a CA part matrix/vector (coord or contrib) to a wide tibble with
# category + Dimension 1..k columns, guaranteeing Dimension 1 and Dimension 2 exist.
.ca_part_coord_wide <- function(mat, variable_name) {
  if (!is.matrix(mat)) {
    mat <- matrix(as.numeric(mat), ncol = 1, dimnames = list(names(mat), "Dim 1"))
  }
  res <- tibble::rownames_to_column(as.data.frame(mat, check.names = FALSE), var = "category")
  res <- res %>% dplyr::rename_with(~gsub("^Dim ", "Dimension ", .), dplyr::starts_with("Dim "))
  if (!"Dimension 1" %in% names(res)) res$`Dimension 1` <- 0
  if (!"Dimension 2" %in% names(res)) res$`Dimension 2` <- 0
  res$variable <- variable_name
  res
}

#' Extracts results from correspondence analysis result object in a dataframe column.
#' @export
tidy.mca_exploratory <- function(x, type = "categories", ...) {
  .tidy_corresp_impl(x, type)
}

#' @export
tidy.ca_exploratory <- function(x, type = "categories", ...) {
  .tidy_corresp_impl(x, type)
}

.tidy_corresp_impl <- function(x, type = "categories") {
  analysis_type <- if (!is.null(x$analysis_type)) x$analysis_type else "MCA"

  # ---------- legacy types (kept byte-compatible for MCA) ----------
  if (type == "categories") {
    if (analysis_type == "CA") {
      res <- dplyr::bind_rows(
        .ca_part_coord_wide(x$row$coord, x$row_variable_name),
        .ca_part_coord_wide(x$col$coord, x$column_variable_name)
      )
      return(res %>% dplyr::select(variable, category, `Dimension 1`, `Dimension 2`))
    }
    res <- tibble::rownames_to_column(as.data.frame(x$var$coord), var = "category")
    res <- res %>% dplyr::select(category, `Dim 1`, `Dim 2`)
    res <- res %>% tidyr::separate(col = category, into = c("variable", "category"), sep = ":", extra = "merge")
    res <- res %>% dplyr::mutate(variable = x$var_names_map[variable])
    res <- res %>% dplyr::rename_with(~gsub("Dim ", "Dimension ", .), dplyr::starts_with("Dim "))
    return(res)
  }
  else if (type == "variables") {
    if (analysis_type == "CA") {
      return(tibble::tibble(variable = character(0), `Dimension 1` = numeric(0), `Dimension 2` = numeric(0)))
    }
    res <- tibble::rownames_to_column(as.data.frame(x$var$eta2), var = "variable")
    res <- res %>% dplyr::select(variable, `Dim 1`, `Dim 2`)
    res <- res %>% dplyr::rename_with(~gsub("Dim ", "Dimension ", .), dplyr::starts_with("Dim "))
    return(res)
  }
  else if (type == "quanti_sup") {
    if (analysis_type == "CA" || is.null(x$quanti.sup)) {
      return(tibble::tibble(variable = character(0), `Dimension 1` = numeric(0), `Dimension 2` = numeric(0)))
    }
    res <- tibble::rownames_to_column(as.data.frame(x$quanti.sup$coord), var = "variable")
    res <- res %>% dplyr::select(variable, `Dim 1`, `Dim 2`)
    res <- res %>% dplyr::rename_with(~gsub("Dim ", "Dimension ", .), dplyr::starts_with("Dim "))
    return(res)
  }
  else if (type == "contrib") {
    if (analysis_type == "CA") {
      res <- dplyr::bind_rows(
        .ca_part_coord_wide(x$row$contrib, x$row_variable_name),
        .ca_part_coord_wide(x$col$contrib, x$column_variable_name)
      )
      res <- res %>% dplyr::select(variable, category, dplyr::starts_with("Dimension "))
      res <- res %>% tidyr::unite(Category, variable, category, sep = " - ")
      res <- res %>% tidyr::pivot_longer(cols = dplyr::starts_with("Dimension "), names_to = "Dimension", values_to = "Value")
      return(res)
    } else {
      res <- tibble::rownames_to_column(as.data.frame(x$var$contrib), var = "category")
      res <- res %>% tidyr::separate(col = category, into = c("variable", "category"), sep = ":", extra = "merge")
      res <- res %>% dplyr::mutate(variable = x$var_names_map[variable])
    }
    res <- res %>% dplyr::select(variable, category, dplyr::starts_with("Dim "))
    res <- res %>% tidyr::unite(Category, variable, category, sep = " - ")
    res <- res %>% tidyr::pivot_longer(cols = dplyr::starts_with("Dim "), names_to = "Dimension", values_to = "Value")
    res <- res %>% dplyr::mutate(Dimension = stringr::str_replace(Dimension, "Dim ", "Dimension "))
    return(res)
  }
  else if (type == "variance") {
    res <- as.data.frame(x$eig) %>% dplyr::mutate(dim = 1:dplyr::n())
    res <- res %>% dplyr::filter(`percentage of variance` > 1e-15)
    return(res)
  }
  else if (type == "data") {
    if (analysis_type == "CA") {
      # Join each observation's row-variable category coordinates (dimension scores).
      coord <- tibble::rownames_to_column(as.data.frame(x$row$coord), var = ".__row_cat")
      coord <- coord %>% dplyr::rename_with(~gsub("Dim ", "Dimension ", .), dplyr::starts_with("Dim "))
      res <- x$df
      res$.__row_cat <- as.character(res[[x$row_variable_name]])
      res <- res %>% dplyr::left_join(coord, by = ".__row_cat") %>% dplyr::select(-.__row_cat)
      return(res)
    }
    res <- as.data.frame(x$ind$coord)
    res <- x$df %>% dplyr::bind_cols(res)
    res <- res %>% dplyr::rename_with(~gsub("Dim ", "Dimension ", .), dplyr::starts_with("Dim "))
    return(res)
  }

  # ---------- new report types (#37086) ----------
  else if (type == "analysis_summary") {
    return(.tidy_analysis_summary(x, analysis_type))
  }
  else if (type == "category_map") {
    return(.tidy_category_map(x))
  }
  else if (type == "pairwise_association") {
    return(x$association$variable_pair_results)
  }
  else if (type == "residual_cells") {
    return(x$association$residual_heatmap_data)
  }
  else if (type == "featured_combinations") {
    return(x$association$featured_combinations)
  }
  else if (type == "dimension_summary") {
    return(x$section5$dimension_summary)
  }
  else if (type == "dimension_matrix") {
    return(x$section5$dimension_matrix_long)
  }
  else if (type == "category_details") {
    return(x$section5$category_details)
  }
  else {
    stop(paste0("Unknown tidy type for correspondence analysis: ", type))
  }
}

# §1 Analysis summary — Item / Value rows + hidden status columns.
.tidy_analysis_summary <- function(x, analysis_type) {
  eig <- as.data.frame(x$eig)
  cum12 <- if (nrow(eig) >= 2) eig[[3]][2] else if (nrow(eig) >= 1) eig[[3]][1] else NA_real_
  main_vars <- x$section5$dimension_summary %>%
    dplyr::filter(dimension <= 2) %>% dplyr::pull(main_variables)
  main_vars <- unique(trimws(unlist(strsplit(main_vars, ",\\s*"))))
  main_vars <- paste(main_vars[nzchar(main_vars)], collapse = ", ")

  tibble::tibble(
    Item = c("method", "variables", "n_used", "n_excluded", "category_total", "n_dims", "cumulative_1_2", "main_variables"),
    Value = c(
      analysis_type,
      paste(x$effective_vars, collapse = ", "),
      as.character(x$n_used),
      as.character(x$n_excluded),
      as.character(x$category_total),
      as.character(x$n_dims),
      sprintf("%.1f%%", cum12),
      main_vars
    ),
    status = c("method", "variables", "count", "count", "count", "count", "cumulative", "variables")
  )
}

# §2 Category map — coordinates + count/share/contrib/cos2 for the scatter + tooltip.
.tidy_category_map <- function(x) {
  m <- x$section5$all_metrics
  wide <- m %>%
    dplyr::filter(dimension %in% c(1, 2)) %>%
    dplyr::select(variable, category, dimension, coordinate, contribution_pct, cos2, count, share) %>%
    tidyr::pivot_wider(
      names_from = dimension,
      values_from = c(coordinate, contribution_pct, cos2),
      names_glue = "{.value}_{dimension}"
    )
  # Guarantee dimension-2 columns even when the analysis has only 1 dimension.
  for (col in c("coordinate_1", "coordinate_2", "contribution_pct_1", "contribution_pct_2", "cos2_1", "cos2_2")) {
    if (!col %in% names(wide)) wide[[col]] <- NA_real_
  }
  wide %>%
    dplyr::transmute(
      Variable = variable,
      Category = category,
      `Dimension 1` = coordinate_1,
      `Dimension 2` = coordinate_2,
      Count = count,
      Share = share,
      `Contribution 1` = contribution_pct_1,
      `Contribution 2` = contribution_pct_2,
      `Cos2 1` = cos2_1,
      `Cos2 2` = cos2_2
    )
}
