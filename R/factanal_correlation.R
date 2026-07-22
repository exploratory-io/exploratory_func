# =============================================================================
# Correlation type selection / construction for Factor Analysis (issue #26623)
#
# Automatic selection rules (spec):
#
#   all numeric                       -> Pearson
#   all binary                        -> Tetrachoric
#   all ordinal, at most 4 categories -> Polychoric
#   all ordinal, 5 categories         -> decided from the response distribution
#   all ordinal, 6+ categories        -> Pearson by default
#   numeric / binary / ordinal mix    -> Mixed correlation
#   any nominal category              -> Unsupported
#
# The correlation matrix built here is the single matrix every downstream
# computation (fa, KMO, Bartlett, eigenvalues, parallel analysis, factor
# scores) has to use, so that the report never mixes Pearson and Polychoric.
# =============================================================================

# -----------------------------------------------------------------------------
# Normalize a declared variable type
# -----------------------------------------------------------------------------
normalize_factor_variable_type <- function(x) {
  aliases <- c(
    "auto" = "auto",
    "numeric" = "numeric", "continuous" = "numeric", "number" = "numeric",
    "binary" = "binary", "dichotomous" = "binary", "logical" = "binary", "boolean" = "binary",
    "ordinal" = "ordinal", "ordered" = "ordinal", "ordered_factor" = "ordinal",
    "nominal" = "nominal", "categorical" = "nominal", "unordered" = "nominal"
  )
  key <- tolower(trimws(as.character(x)))
  if (!key %in% names(aliases)) {
    stop(sprintf("Unknown variable type: '%s'. Allowed values are auto, numeric, binary, ordinal, and nominal.", x),
         call. = FALSE)
  }
  unname(aliases[[key]])
}

# -----------------------------------------------------------------------------
# Defined category levels of a variable.
# Priority: supplied metadata -> factor levels -> observed values.
# -----------------------------------------------------------------------------
get_factor_category_levels <- function(x, supplied_levels = NULL) {
  if (!is.null(supplied_levels)) {
    return(supplied_levels)
  }
  if (is.factor(x)) {
    return(levels(x))
  }
  observed <- x[!is.na(x)]
  if (is.logical(observed)) {
    return(c(FALSE, TRUE))
  }
  if (is.numeric(observed)) {
    return(sort(unique(observed)))
  }
  sort(unique(as.character(observed)))
}

# -----------------------------------------------------------------------------
# Is a numeric column really a rating scale (1-5 survey answer) rather than a
# continuous measurement?
#
# The spec's literal rule calls every numeric column with 3+ distinct values
# "numeric", which would mean a 1-to-5 survey item imported as an integer -- the
# exact data the polychoric request is about -- never reaches the ordinal branch.
# So numeric columns are treated as ordinal only when they look like a rating
# scale: consecutive integers, 3 to `max_categories` of them, starting at 0 or 1.
# A measurement like cyl (4, 6, 8) or a count is not consecutive-from-0/1 and
# stays numeric, so existing all-numeric analyses keep using Pearson.
# -----------------------------------------------------------------------------
is_rating_scale_numeric <- function(observed, max_categories = 7) {
  observed <- observed[is.finite(observed)]
  if (length(observed) == 0) return(FALSE)
  if (any(observed != floor(observed))) return(FALSE)
  values <- sort(unique(observed))
  n_values <- length(values)
  if (n_values < 3 || n_values > max_categories) return(FALSE)
  if (!(min(values) %in% c(0, 1))) return(FALSE)
  # Consecutive integers only: 1,2,3,4,5 yes; 1,2,5 no.
  identical(as.numeric(max(values) - min(values) + 1), as.numeric(n_values))
}

# -----------------------------------------------------------------------------
# Detect the type of one variable.
# -----------------------------------------------------------------------------
inspect_factor_variable <- function(variable_name, x, declared_type = "auto", supplied_levels = NULL) {
  declared_type <- normalize_factor_variable_type(declared_type)

  observed <- x[!is.na(x)]
  n_observed_categories <- length(unique(observed))
  warnings <- character()

  if (n_observed_categories < 2) {
    return(list(
      variable = variable_name, declared_type = declared_type, detected_type = "invalid",
      n_non_missing = length(observed), n_observed_categories = n_observed_categories,
      n_defined_categories = n_observed_categories,
      category_levels = get_factor_category_levels(x, supplied_levels),
      order_source = NA_character_,
      warnings = sprintf("Variable '%s' has fewer than two observed values.", variable_name)
    ))
  }

  if (declared_type != "auto") {
    detected_type <- declared_type
  } else if (!is.null(supplied_levels)) {
    # Supplied levels are treated as ordered-category metadata.
    detected_type <- if (length(supplied_levels) == 2) "binary" else "ordinal"
  } else if (is.logical(x)) {
    detected_type <- "binary"
  } else if (is.ordered(x)) {
    detected_type <- if (nlevels(x) == 2) "binary" else "ordinal"
  } else if (is.factor(x)) {
    detected_type <- if (nlevels(x) == 2) "binary" else "nominal"
  } else if (is.numeric(x)) {
    detected_type <- if (n_observed_categories == 2) "binary"
      else if (is_rating_scale_numeric(observed)) "ordinal"
      else "numeric"
  } else if (is.character(x)) {
    detected_type <- if (n_observed_categories == 2) "binary" else "nominal"
  } else {
    detected_type <- "nominal"
  }

  is_categorical <- detected_type %in% c("binary", "ordinal", "nominal")
  if (is_categorical) {
    category_levels <- get_factor_category_levels(x = x, supplied_levels = supplied_levels)
    n_defined_categories <- length(category_levels)
    order_source <- if (!is.null(supplied_levels)) "supplied_metadata"
      else if (is.ordered(x)) "ordered_factor_levels"
      else if (is.factor(x)) "factor_levels"
      else if (is.numeric(x)) "sorted_numeric_values"
      else "sorted_character_values"
  } else {
    category_levels <- get_factor_category_levels(x = x, supplied_levels = supplied_levels)
    n_defined_categories <- NA_integer_
    order_source <- NA_character_
  }

  if (detected_type == "numeric" && !is.numeric(x)) {
    detected_type <- "invalid"
    warnings <- c(warnings, sprintf("Variable '%s' is declared numeric but its R data type is not numeric.", variable_name))
  }
  if (detected_type == "binary" && !is.na(n_defined_categories) && n_defined_categories != 2) {
    detected_type <- "invalid"
    warnings <- c(warnings, sprintf("Variable '%s' is binary but has %d defined categories.", variable_name, n_defined_categories))
  }
  if (detected_type == "ordinal" && !is.na(n_defined_categories) && n_defined_categories == 2) {
    detected_type <- "binary"
  }
  if (detected_type == "ordinal" && !is.na(n_defined_categories) && n_defined_categories < 2) {
    detected_type <- "invalid"
    warnings <- c(warnings, sprintf("Variable '%s' does not have enough ordered categories.", variable_name))
  }
  if (detected_type == "ordinal" && is.character(x) && is.null(supplied_levels)) {
    warnings <- c(warnings, sprintf(
      "Variable '%s' is ordinal character data, but no category order was supplied. Alphabetical order will be used.",
      variable_name))
  }

  list(
    variable = variable_name, declared_type = declared_type, detected_type = detected_type,
    n_non_missing = length(observed), n_observed_categories = n_observed_categories,
    n_defined_categories = n_defined_categories, category_levels = category_levels,
    order_source = order_source, warnings = warnings
  )
}

# -----------------------------------------------------------------------------
# Ordered categories -> 1, 2, 3, ... codes
# -----------------------------------------------------------------------------
encode_ordered_variable <- function(x, category_levels) {
  match(as.character(x), as.character(category_levels))
}

# -----------------------------------------------------------------------------
# Skewness (standardized 3rd moment, no extra package dependency)
# -----------------------------------------------------------------------------
calculate_ordinal_skewness <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3) return(NA_real_)
  center <- mean(x)
  second_moment <- mean((x - center)^2)
  if (!is.finite(second_moment) || second_moment <= 0) return(NA_real_)
  mean((x - center)^3) / second_moment^(3 / 2)
}

# -----------------------------------------------------------------------------
# Diagnose the distribution of a 5-category variable.
# Polychoric is recommended when any of these holds:
#   - the modal category takes 50% or more
#   - the lowest or highest category takes 40% or more
#   - |skewness| is 1 or more
# A category below 5% only produces a warning by default, because sparse
# categories can also destabilize the polychoric estimation.
# -----------------------------------------------------------------------------
assess_five_category_distribution <- function(variable_name, x, category_levels,
                                              modal_prop_cutoff = 0.50,
                                              extreme_prop_cutoff = 0.40,
                                              skewness_cutoff = 1.00,
                                              rare_category_prop_cutoff = 0.05,
                                              rare_category_triggers_poly = FALSE) {
  if (length(category_levels) != 5) {
    stop(sprintf("Variable '%s' does not have exactly five defined categories.", variable_name), call. = FALSE)
  }
  encoded <- encode_ordered_variable(x = x, category_levels = category_levels)
  valid <- !is.na(encoded)
  n_valid <- sum(valid)
  counts <- tabulate(encoded[valid], nbins = 5)
  proportions <- if (n_valid > 0) counts / n_valid else rep(NA_real_, 5)

  modal_prop <- max(proportions, na.rm = TRUE)
  lower_extreme_prop <- proportions[[1]]
  upper_extreme_prop <- proportions[[5]]
  max_extreme_prop <- max(lower_extreme_prop, upper_extreme_prop, na.rm = TRUE)
  min_category_prop <- min(proportions, na.rm = TRUE)
  skewness <- calculate_ordinal_skewness(encoded[valid])

  dominant_category <- is.finite(modal_prop) && modal_prop >= modal_prop_cutoff
  extreme_concentration <- is.finite(max_extreme_prop) && max_extreme_prop >= extreme_prop_cutoff
  strong_skewness <- is.finite(skewness) && abs(skewness) >= skewness_cutoff
  rare_category <- is.finite(min_category_prop) && min_category_prop < rare_category_prop_cutoff

  trigger_ids <- character()
  if (dominant_category) trigger_ids <- c(trigger_ids, "dominant_category")
  if (extreme_concentration) trigger_ids <- c(trigger_ids, "extreme_category_concentration")
  if (strong_skewness) trigger_ids <- c(trigger_ids, "strong_skewness")
  if (rare_category && rare_category_triggers_poly) trigger_ids <- c(trigger_ids, "rare_category")

  data.frame(
    variable = variable_name, n = n_valid,
    modal_category_prop = modal_prop,
    lower_extreme_prop = lower_extreme_prop, upper_extreme_prop = upper_extreme_prop,
    max_extreme_prop = max_extreme_prop, minimum_category_prop = min_category_prop,
    skewness = skewness,
    dominant_category = dominant_category, extreme_concentration = extreme_concentration,
    strong_skewness = strong_skewness, rare_category = rare_category,
    polychoric_trigger = length(trigger_ids) > 0,
    trigger_reason = if (length(trigger_ids) == 0) NA_character_ else paste(trigger_ids, collapse = ", "),
    stringsAsFactors = FALSE
  )
}

# -----------------------------------------------------------------------------
# Main automatic-selection entry point.
# -----------------------------------------------------------------------------
select_factor_correlation_type <- function(data, variables = names(data),
                                           variable_types = NULL, category_levels = NULL,
                                           modal_prop_cutoff = 0.50, extreme_prop_cutoff = 0.40,
                                           skewness_cutoff = 1.00, rare_category_prop_cutoff = 0.05,
                                           rare_category_triggers_poly = FALSE) {
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }
  missing_variables <- setdiff(variables, names(data))
  if (length(missing_variables) > 0) {
    stop(sprintf("The following variables do not exist: %s", paste(missing_variables, collapse = ", ")), call. = FALSE)
  }
  if (length(variables) < 2) {
    stop("At least two variables are required.", call. = FALSE)
  }

  declared_types <- rep("auto", length(variables))
  names(declared_types) <- variables
  if (!is.null(variable_types)) {
    if (is.null(names(variable_types))) {
      stop("variable_types must be a named vector.", call. = FALSE)
    }
    unknown_type_variables <- setdiff(names(variable_types), variables)
    if (length(unknown_type_variables) > 0) {
      stop(sprintf("variable_types contains unknown variables: %s", paste(unknown_type_variables, collapse = ", ")), call. = FALSE)
    }
    declared_types[names(variable_types)] <- vapply(variable_types, normalize_factor_variable_type, character(1))
  }
  if (is.null(category_levels)) {
    category_levels <- list()
  }

  inspections <- lapply(variables, function(variable_name) {
    inspect_factor_variable(variable_name = variable_name, x = data[[variable_name]],
                            declared_type = declared_types[[variable_name]],
                            supplied_levels = category_levels[[variable_name]])
  })
  names(inspections) <- variables

  variable_summary <- do.call(rbind, lapply(inspections, function(result) {
    data.frame(variable = result$variable, declared_type = result$declared_type,
               detected_type = result$detected_type, n_non_missing = result$n_non_missing,
               n_observed_categories = result$n_observed_categories,
               n_defined_categories = result$n_defined_categories,
               order_source = result$order_source, stringsAsFactors = FALSE)
  }))
  rownames(variable_summary) <- NULL

  detected_types <- variable_summary$detected_type
  detected_category_levels <- lapply(inspections, function(result) result$category_levels)
  all_warnings <- unlist(lapply(inspections, function(result) result$warnings), use.names = FALSE)
  five_category_diagnostics <- data.frame()

  build_result <- function(selected_method, selected_label, psych_fa_cor, available_options, reason, warnings) {
    structure(list(
      selected_method = selected_method, selected_label = selected_label,
      psych_fa_cor = psych_fa_cor, available_options = available_options, reason = reason,
      variables = variables, variable_summary = variable_summary,
      category_levels = detected_category_levels,
      five_category_diagnostics = five_category_diagnostics,
      warnings = unique(warnings),
      thresholds = list(modal_prop_cutoff = modal_prop_cutoff, extreme_prop_cutoff = extreme_prop_cutoff,
                        skewness_cutoff = skewness_cutoff, rare_category_prop_cutoff = rare_category_prop_cutoff,
                        rare_category_triggers_poly = rare_category_triggers_poly)
    ), class = "factor_correlation_selection")
  }

  # Invalid variables
  if (any(detected_types == "invalid")) {
    invalid_variables <- variable_summary$variable[detected_types == "invalid"]
    return(build_result("unsupported", "Unsupported", NA_character_, character(),
                        sprintf("Invalid variables were detected: %s", paste(invalid_variables, collapse = ", ")),
                        all_warnings))
  }

  # Nominal categories
  if (any(detected_types == "nominal")) {
    nominal_variables <- variable_summary$variable[detected_types == "nominal"]
    warning_message <- sprintf(
      "Nominal categorical variables cannot be used directly in this factor analysis: %s",
      paste(nominal_variables, collapse = ", "))
    return(build_result("unsupported", "Unsupported", NA_character_, character(),
                        warning_message, c(all_warnings, warning_message)))
  }

  if (all(detected_types == "numeric")) {
    selected_method <- "pearson"; selected_label <- "Pearson"; psych_fa_cor <- "cor"
    available_options <- c("pearson")
    reason <- "All selected variables are numeric. Pearson correlation was selected."

  } else if (all(detected_types == "binary")) {
    selected_method <- "tetrachoric"; selected_label <- "Tetrachoric"; psych_fa_cor <- "tet"
    available_options <- c("tetrachoric", "pearson")
    reason <- "All selected variables are binary. Tetrachoric correlation was selected."

  } else if (all(detected_types == "ordinal")) {
    max_categories <- max(variable_summary$n_defined_categories, na.rm = TRUE)

    if (max_categories <= 4) {
      selected_method <- "polychoric"; selected_label <- "Polychoric"; psych_fa_cor <- "poly"
      available_options <- c("polychoric", "pearson")
      reason <- paste0("All selected variables are ordinal and have no more than four categories. ",
                       "Polychoric correlation was selected.")

    } else if (max_categories == 5) {
      five_category_variables <- variable_summary$variable[variable_summary$n_defined_categories == 5]
      five_category_diagnostics <- do.call(rbind, lapply(five_category_variables, function(variable_name) {
        assess_five_category_distribution(
          variable_name = variable_name, x = data[[variable_name]],
          category_levels = detected_category_levels[[variable_name]],
          modal_prop_cutoff = modal_prop_cutoff, extreme_prop_cutoff = extreme_prop_cutoff,
          skewness_cutoff = skewness_cutoff, rare_category_prop_cutoff = rare_category_prop_cutoff,
          rare_category_triggers_poly = rare_category_triggers_poly)
      }))
      use_polychoric <- any(five_category_diagnostics$polychoric_trigger, na.rm = TRUE)
      sparse_variables <- five_category_diagnostics$variable[five_category_diagnostics$rare_category]
      if (length(sparse_variables) > 0) {
        all_warnings <- c(all_warnings, sprintf(
          "Some categories contain less than %.1f%% of responses: %s. Polychoric correlations may become unstable when categories are sparse.",
          rare_category_prop_cutoff * 100, paste(sparse_variables, collapse = ", ")))
      }
      if (use_polychoric) {
        selected_method <- "polychoric"; selected_label <- "Polychoric"; psych_fa_cor <- "poly"
        reason <- paste0("All selected variables are ordinal with no more than five categories. ",
                         "At least one five-category variable has a concentrated or strongly skewed distribution. ",
                         "Polychoric correlation was selected.")
      } else {
        selected_method <- "pearson"; selected_label <- "Pearson"; psych_fa_cor <- "cor"
        reason <- paste0("All selected variables are ordinal with no more than five categories, ",
                         "but their distributions are relatively balanced. Pearson correlation was selected.")
      }
      available_options <- c(selected_method, setdiff(c("pearson", "polychoric"), selected_method))

    } else {
      selected_method <- "pearson"; selected_label <- "Pearson"; psych_fa_cor <- "cor"
      available_options <- c("pearson", "polychoric")
      reason <- paste0("All selected variables are ordinal and at least one variable has six or more categories. ",
                       "Pearson correlation was selected by default. ",
                       "Polychoric correlation remains available as a manual option.")
    }

  } else if (all(detected_types %in% c("numeric", "binary", "ordinal"))) {
    selected_method <- "mixed"; selected_label <- "Mixed correlation"; psych_fa_cor <- "mixed"
    available_options <- c("mixed", "pearson")
    reason <- sprintf("The selected variables contain multiple supported types: %s. Mixed correlation was selected.",
                      paste(unique(detected_types), collapse = ", "))

  } else {
    return(build_result("unsupported", "Unsupported", NA_character_, character(),
                        "The selected variable combination is not supported.", all_warnings))
  }

  build_result(selected_method, selected_label, psych_fa_cor, available_options, reason, all_warnings)
}

# -----------------------------------------------------------------------------
# Encode the analysis data as a numeric data frame using the detected category
# order, so every correlation method sees the same coding.
# -----------------------------------------------------------------------------
encode_factanal_data <- function(data, selection) {
  data <- as.data.frame(data)
  for (col in colnames(data)) {
    x <- data[[col]]
    if (is.numeric(x)) next
    levels_for_col <- selection$category_levels[[col]]
    if (is.null(levels_for_col)) {
      levels_for_col <- get_factor_category_levels(x)
    }
    data[[col]] <- as.numeric(encode_ordered_variable(x, levels_for_col))
  }
  data
}

# -----------------------------------------------------------------------------
# Build the single correlation matrix used across the whole analysis.
# Returns the matrix, the thresholds (for the polychoric family), whether the
# matrix had to be smoothed, and any estimation warnings.
# -----------------------------------------------------------------------------
build_factor_correlation <- function(data, correlation_type = c("pearson", "polychoric", "tetrachoric", "mixed"),
                                     use = "pairwise.complete.obs", correct = 0.5) {
  correlation_type <- match.arg(correlation_type)
  data <- as.data.frame(data)

  if (correlation_type == "pearson") {
    return(list(correlation = stats::cor(data, use = use), thresholds = NULL,
                type = "pearson", smoothed = FALSE, warnings = character(), failed = FALSE))
  }

  captured <- character()
  result <- withCallingHandlers(
    tryCatch({
      switch(correlation_type,
        polychoric = psych::polychoric(data, correct = correct),
        tetrachoric = psych::tetrachoric(data, correct = correct),
        mixed = psych::mixedCor(data))
    }, error = function(e) {
      captured <<- c(captured, conditionMessage(e))
      NULL
    }),
    warning = function(w) {
      captured <<- c(captured, conditionMessage(w))
      invokeRestart("muffleWarning")
    })

  if (is.null(result) || is.null(result$rho)) {
    # Degrade to Pearson rather than aborting the whole analysis.
    return(list(correlation = stats::cor(data, use = use), thresholds = NULL,
                type = "pearson", smoothed = FALSE, warnings = captured, failed = TRUE))
  }

  list(correlation = result$rho,
       thresholds = if (!is.null(result$tau)) result$tau else NULL,
       type = correlation_type,
       smoothed = any(grepl("smooth", captured, ignore.case = TRUE)),
       warnings = captured, failed = FALSE)
}

# -----------------------------------------------------------------------------
# Resolve the correlation type actually used from the requested UI value
# ("auto" / "pearson" / "polychoric") and the automatic selection result.
# -----------------------------------------------------------------------------
resolve_factanal_correlation_type <- function(requested, selection) {
  if (is.null(requested) || length(requested) != 1L || is.na(requested)) {
    requested <- "auto"
  }
  requested <- tolower(trimws(as.character(requested)))
  if (!requested %in% c("auto", "pearson", "polychoric", "tetrachoric", "mixed")) {
    stop(sprintf("Unknown correlation type: '%s'. Allowed values are auto, pearson, and polychoric.", requested),
         call. = FALSE)
  }
  if (requested != "auto") {
    return(list(type = requested, auto = FALSE,
                reason = sprintf("%s was selected manually.",
                                 factanal_correlation_label(requested))))
  }
  list(type = selection$selected_method, auto = TRUE, reason = selection$reason)
}

# English-canonical label. The client translates it (VizUtil message tables).
factanal_correlation_label <- function(type) {
  switch(as.character(type),
         pearson = "Pearson Correlation",
         polychoric = "Polychoric Correlation",
         tetrachoric = "Tetrachoric Correlation",
         mixed = "Mixed Correlation",
         "Unsupported")
}

# The "Type of Scores" property values are psych::fa() `scores` values. When the fit is built from
# a correlation matrix the scores come from psych::factor.scores(), whose `method` uses a different
# spelling for the default: fa()'s "regression" is factor.scores()'s "Thurstone". Everything else
# passes through, and anything unknown falls back to the default rather than erroring.
factanal_score_method <- function(scores) {
  scores <- if (is.null(scores) || length(scores) != 1L || is.na(scores)) "regression" else as.character(scores)
  switch(scores,
         regression = "Thurstone",
         Thurstone = "Thurstone",
         Bartlett = "Bartlett",
         tenBerge = "tenBerge",
         Anderson = "Anderson",
         "Thurstone")
}

# Rotation names for the report's Analysis Method table. A generic title-case would turn the
# camel-cased psych names into non-words ("bentlerT" -> "Bentlert"), so they are mapped explicitly.
factanal_rotation_label <- function(rotate) {
  rotate <- if (is.null(rotate) || length(rotate) != 1L || is.na(rotate)) "none" else as.character(rotate)
  switch(rotate,
         none = "None",
         varimax = "Varimax",
         Promax = "Promax",
         promax = "Promax with Kaiser Normalization",
         oblimin = "Oblimin",
         quartimax = "Quartimax",
         bentlerT = "Bentler (Orthogonal)",
         bentlerQ = "Bentler (Oblique)",
         equamax = "Equamax",
         geominT = "Geomin (Orthogonal)",
         geomin = "Geomin (Oblique)",
         simplimax = "Simplimax",
         cluster = "Cluster (Oblique)",
         rotate)
}

factanal_extraction_method_label <- function(fm) {
  switch(as.character(fm),
         minres = "Minimum Residual",
         ml = "Maximum Likelihood",
         pa = "Principal Axis",
         ols = "Ordinary Least Squares",
         wls = "Weighted Least Squares",
         gls = "Generalized Least Squares",
         minchi = "Minimum Chi-Square",
         minrank = "Minimum Rank",
         alpha = "Alpha Factoring",
         as.character(fm))
}

# -----------------------------------------------------------------------------
# Diagnostics for a polychoric-family correlation matrix (issue #26623, report
# section "Data Suitability Diagnostics"). Values are English-canonical strings
# the client translates; `status` is a language-neutral token.
# -----------------------------------------------------------------------------
compute_polychoric_diagnostics <- function(data, cor_result, selection,
                                           rare_category_prop_cutoff = 0.05) {
  data <- as.data.frame(data)
  # Category-shape diagnostics only make sense for the CATEGORICAL variables. In a mixed analysis
  # a continuous column would otherwise report one "category" per distinct value, flag every one
  # of them as sparse, and make every pair look like it has empty combinations. (issue #26623)
  categorical_variables <- if (!is.null(selection) && !is.null(selection$variable_summary)) {
    summary_df <- selection$variable_summary
    summary_df$variable[summary_df$detected_type %in% c("binary", "ordinal")]
  } else {
    colnames(data)
  }
  variables <- intersect(colnames(data), categorical_variables)
  if (length(variables) == 0) {
    variables <- colnames(data)
  }

  # 1. Number of categories
  category_counts <- vapply(variables, function(v) {
    length(unique(data[[v]][!is.na(data[[v]])]))
  }, numeric(1))
  if (length(unique(category_counts)) == 1) {
    category_value <- sprintf("All variables have %d categories", as.integer(category_counts[[1]]))
    category_status <- "uniform"
  } else {
    category_value <- sprintf("%d to %d categories", as.integer(min(category_counts)), as.integer(max(category_counts)))
    category_status <- "mixed"
  }

  # 2. Sparse categories (a category holding less than the cutoff share of responses)
  sparse_variables <- variables[vapply(variables, function(v) {
    x <- data[[v]][!is.na(data[[v]])]
    if (length(x) == 0) return(FALSE)
    props <- as.numeric(table(x)) / length(x)
    any(props < rare_category_prop_cutoff)
  }, logical(1))]
  sparse_value <- if (length(sparse_variables) == 0) "None" else sprintf("Detected in %d variables", length(sparse_variables))
  sparse_status <- if (length(sparse_variables) == 0) "ok" else "caution"

  # 3. Empty category combinations (a zero cell in a pair's cross tabulation)
  empty_pairs <- 0L
  if (length(variables) >= 2) {
    for (i in seq_len(length(variables) - 1)) {
      for (j in seq(i + 1, length(variables))) {
        tab <- table(data[[variables[[i]]]], data[[variables[[j]]]])
        if (any(tab == 0)) empty_pairs <- empty_pairs + 1L
      }
    }
  }
  empty_value <- if (empty_pairs == 0) "None" else sprintf("Detected in %d variable pairs", empty_pairs)
  empty_status <- if (empty_pairs == 0) "ok" else "caution"

  # 4. Correlation estimation failures (NA in the off-diagonal of the matrix)
  R <- cor_result$correlation
  failed_cells <- if (is.null(R)) NA_integer_ else {
    off_diagonal <- R
    diag(off_diagonal) <- 0
    as.integer(sum(is.na(off_diagonal)) / 2)
  }
  if (isTRUE(cor_result$failed)) {
    failure_value <- "Estimation failed"
    failure_status <- "failed"
  } else if (is.na(failed_cells) || failed_cells == 0) {
    failure_value <- "None"
    failure_status <- "ok"
  } else {
    failure_value <- sprintf("Detected in %d variable pairs", failed_cells)
    failure_status <- "caution"
  }

  # 5. Positive definiteness of the correlation matrix
  min_eigen <- tryCatch({
    if (is.null(R) || anyNA(R)) NA_real_ else min(eigen(R, symmetric = TRUE, only.values = TRUE)$values)
  }, error = function(e) NA_real_)
  if (is.na(min_eigen)) {
    definite_value <- "Not Available"; definite_status <- "na"
  } else if (min_eigen > 1e-8) {
    definite_value <- "No problem"; definite_status <- "ok"
  } else {
    definite_value <- "Not positive definite"; definite_status <- "caution"
  }

  # 6. Whether the matrix had to be smoothed
  smoothed_value <- if (isTRUE(cor_result$smoothed)) "Applied" else "None"
  smoothed_status <- if (isTRUE(cor_result$smoothed)) "caution" else "ok"

  tibble::tibble(
    Diagnostic = c("Number of Categories", "Sparse Categories", "Empty Category Combinations",
                   "Correlation Estimation Failures", "Positive Definiteness of the Correlation Matrix",
                   "Smoothing Applied"),
    Judgement = c(category_value, sparse_value, empty_value, failure_value, definite_value, smoothed_value),
    Description = c(
      "Number of categories in the selected variables.",
      "Variables that have a category holding less than 5% of the responses. Polychoric correlations may become unstable when categories are sparse.",
      "Variable pairs that have a category combination with no observations.",
      "Variable pairs whose correlation could not be estimated.",
      "Whether the correlation matrix is positive definite, which factor analysis assumes.",
      "Whether the correlation matrix had to be smoothed to become positive definite."
    ),
    status = c(category_status, sparse_status, empty_status, failure_status, definite_status, smoothed_status)
  )
}
