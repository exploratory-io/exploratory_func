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
                    missing_method = "listwise", featured_rule = "statistical",
                    simulation_count = 20000, suppress_cell_p_when_sparse = TRUE,
                    practical_ratio_upper = 1.20, practical_ratio_lower = 0.80,
                    practical_minimum_difference = NULL, require_overall_significance = TRUE) {
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
        seed = seed, suppress_cell_p_when_sparse = suppress_cell_p_when_sparse,
        featured_rule = featured_rule, practical_ratio_upper = practical_ratio_upper,
        practical_ratio_lower = practical_ratio_lower,
        practical_minimum_difference = practical_minimum_difference,
        require_overall_significance = require_overall_significance
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
