# ANCOVA Calculation V2 (Phase 1 -- R computation layer redesign).
# tam#38385 / exploratory_func Phase 1 of a 3-phase project.
#
# This file is ADDITIVE: it does not modify or remove the existing exp_anova()
# ANCOVA path in R/test_wrapper.R, which keeps working until a later phase
# wires the desktop app up to run_ancova_v2(). See the PR description for the
# full rationale.
#
# Design summary (see the tam#38385 issue body for the full spec):
#   - Formally supports P >= 1 covariates.
#   - Slope-homogeneity (the ANCOVA assumption) is decided by ONE global
#     nested F-test: additive model (Y ~ G + X1c + ... + XPc) vs. interaction
#     model (additive + G:X1c + ... + G:XPc). This single test is the ONLY
#     thing that selects the final model -- individual per-covariate
#     interaction tests are diagnostic-only (Holm-adjusted) and never drive
#     model selection (no stepwise shrinkage).
#   - "Interaction model could not be estimated" (rank-deficient) is a
#     DIFFERENT state from "interaction not significant" and is never
#     conflated with it.
#   - Every downstream computation (ANCOVA table, effect sizes, adjusted
#     means, pairwise comparisons) is derived from the ONE final model that
#     was selected -- nothing downstream re-fits its own lm().
#   - User-supplied column names are NEVER pasted into a formula string.
#     Columns are renamed to safe internal identifiers
#     (.ancova_y / .ancova_factor / .ancova_x_<j> / .ancova_xc_<j>) and every
#     formula is built with stats::reformulate() on those safe names only.
#
# Result shape: run_ancova_v2() returns a plain, deeply-nested R list (never
# a JSON string) that mirrors the "ancova_result" tree from the spec 1:1, so
# the caller can serialize it with jsonlite::toJSON(result, auto_unbox = TRUE,
# na = "null") -- see lessons/global/r-integration.md#jsonlite-auto-unbox-scalarizes-length-1-vector.
# Tabular sections are tibbles (jsonlite turns those into arrays of row
# objects automatically); every other leaf is a length-1 scalar or a plain
# list, which auto_unbox handles correctly.

# ------------------------------------------------------------
# Error signalling
# ------------------------------------------------------------

#' Signal a structured ANCOVA V2 error that run_ancova_v2() turns into
#' analysis_status = "error" with the given error_code, instead of a
#' generic, unclassified R error.
#' @param error_code Machine-readable error code (e.g. "ANCOVA_RANK_DEFICIENT").
#' @param message Human-readable message.
#' @noRd
ancova_stop <- function(error_code, message) {
  cond <- structure(
    class = c("ancova_error", "error", "condition"),
    list(message = message, call = sys.call(-1), error_code = error_code)
  )
  stop(cond)
}

# ------------------------------------------------------------
# 1. prepare_ancova_data() -- safe renaming + complete-case sample
# ------------------------------------------------------------

#' Map user-supplied outcome/factor/covariate column names to safe internal
#' names and compute the ONE complete-case analysis sample every downstream
#' computation must reuse (Rule 1/2).
#' @param data Source data frame.
#' @param outcome Outcome column name (string).
#' @param factor_col Factor (group) column name (string).
#' @param covariates Character vector of 1+ covariate column names.
#' @noRd
prepare_ancova_data <- function(data, outcome, factor_col, covariates) {
  if (!is.data.frame(data)) {
    ancova_stop("ANCOVA_INVALID_INPUT", "`data` must be a data frame.")
  }
  if (!is.character(outcome) || length(outcome) != 1) {
    ancova_stop("ANCOVA_INVALID_INPUT", "`outcome` must be a single column name.")
  }
  if (!is.character(factor_col) || length(factor_col) != 1) {
    ancova_stop("ANCOVA_INVALID_INPUT", "`factor` must be a single column name.")
  }
  if (!is.character(covariates) || length(covariates) < 1) {
    ancova_stop("ANCOVA_INVALID_INPUT", "`covariates` must be a character vector with at least 1 name.")
  }

  requested_cols <- c(outcome, factor_col, covariates)
  missing_cols <- setdiff(requested_cols, names(data))
  if (length(missing_cols) > 0) {
    ancova_stop("ANCOVA_COLUMN_NOT_FOUND",
                paste0("Column(s) not found in data: ", paste(missing_cols, collapse = ", ")))
  }

  n_covariates <- length(covariates)
  safe_y <- ".ancova_y"
  safe_factor <- ".ancova_factor"
  safe_x <- paste0(".ancova_x_", seq_len(n_covariates))

  # Base-R subsetting/renaming handles spaces, multibyte characters and
  # symbols in column names without ever touching a formula string.
  raw <- data[, requested_cols, drop = FALSE]
  names(raw) <- c(safe_y, safe_factor, safe_x)

  n_original <- nrow(data)

  # Preserve the ORIGINAL factor level order (Rule: factor levels keep their
  # original order; only unused levels are dropped). If the source column is
  # not already a factor, fall back to sorted-unique order.
  original_factor_col <- data[[factor_col]]
  original_levels <- if (is.factor(original_factor_col)) {
    levels(original_factor_col)
  } else {
    sort(unique(as.character(original_factor_col[!is.na(original_factor_col)])))
  }
  raw[[safe_factor]] <- factor(as.character(raw[[safe_factor]]), levels = original_levels)

  # Complete-case sample determined ONCE from outcome + factor + all
  # covariates (conceptually data |> select(...) |> tidyr::drop_na()).
  analysis_data <- raw %>% tidyr::drop_na(dplyr::all_of(c(safe_y, safe_factor, safe_x)))
  analysis_data[[safe_factor]] <- droplevels(analysis_data[[safe_factor]])

  n_used <- nrow(analysis_data)
  n_removed <- n_original - n_used
  factor_levels <- levels(analysis_data[[safe_factor]])

  list(
    analysis_data = analysis_data,
    n_original = n_original,
    n_used = n_used,
    n_removed = n_removed,
    factor_levels = factor_levels,
    safe_y = safe_y,
    safe_factor = safe_factor,
    safe_x = safe_x,
    outcome_name = outcome,
    factor_name = factor_col,
    covariate_names = covariates
  )
}

# ------------------------------------------------------------
# 2. validate_ancova_data() -- pre-fit validation
# ------------------------------------------------------------

#' Validate the complete-case analysis sample before any model is fit.
#' Stops with a structured ancova_error() on failure.
#' @param prep Result of prepare_ancova_data().
#' @noRd
validate_ancova_data <- function(prep) {
  analysis_data <- prep$analysis_data
  y <- analysis_data[[prep$safe_y]]

  if (!is.numeric(y)) {
    ancova_stop("ANCOVA_INVALID_OUTCOME", "The outcome column must be numeric.")
  }
  finite_y <- y[is.finite(y)]
  if (length(finite_y) == 0) {
    ancova_stop("ANCOVA_INVALID_OUTCOME", "The outcome column has no finite values.")
  }
  if (stats::var(finite_y) <= 0 || !is.finite(stats::var(finite_y))) {
    ancova_stop("ANCOVA_INVALID_OUTCOME", "The outcome column has zero variance.")
  }

  if (length(prep$factor_levels) < 2) {
    ancova_stop("ANCOVA_INVALID_FACTOR",
                "The factor column must have at least 2 levels after removing missing values.")
  }

  for (j in seq_along(prep$safe_x)) {
    x <- analysis_data[[prep$safe_x[j]]]
    covariate_label <- prep$covariate_names[j]
    if (!is.numeric(x)) {
      ancova_stop("ANCOVA_NON_NUMERIC_COVARIATE",
                  paste0("Covariate \"", covariate_label, "\" must be numeric."))
    }
    if (!all(is.finite(x))) {
      ancova_stop("ANCOVA_NON_NUMERIC_COVARIATE",
                  paste0("Covariate \"", covariate_label, "\" has non-finite values."))
    }
    if (stats::var(x) <= 0 || !is.finite(stats::var(x))) {
      ancova_stop("ANCOVA_CONSTANT_COVARIATE",
                  paste0("Covariate \"", covariate_label, "\" is constant and cannot be used."))
    }
  }

  invisible(TRUE)
}

# ------------------------------------------------------------
# 3. center_ancova_covariates() -- grand-mean centering (Rule 2)
# ------------------------------------------------------------

#' Mean-center every covariate using the mean computed strictly from the
#' complete-case analysis sample. Never mutates the original data.
#' @param prep Result of prepare_ancova_data().
#' @noRd
center_ancova_covariates <- function(prep) {
  analysis_data <- prep$analysis_data
  n_covariates <- length(prep$safe_x)
  safe_xc <- paste0(".ancova_xc_", seq_len(n_covariates))

  covariate_summary <- purrr::map_dfr(seq_len(n_covariates), function(j) {
    x <- analysis_data[[prep$safe_x[j]]]
    mean_j <- mean(x, na.rm = TRUE)
    unique_count <- length(unique(x))
    analysis_data[[safe_xc[j]]] <<- x - mean_j
    tibble::tibble(
      name = prep$covariate_names[j],
      mean = mean_j,
      sd = stats::sd(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE),
      unique_count = unique_count,
      low_cardinality = unique_count <= 5,
      # The reference point every adjusted statistic is computed at, on the
      # covariate's OWN raw scale -- the grand mean, because covariates are
      # grand-mean centered. Charts that draw a reference line on a raw-scale
      # x axis need this; `centered_reference_value` below is the same point
      # expressed in centered space, where it is always 0 (tam#38389 Q-7).
      reference_value = mean_j,
      centered_reference_value = 0
    )
  })

  list(
    analysis_data = analysis_data,
    safe_xc = safe_xc,
    covariate_summary = covariate_summary,
    covariate_means = stats::setNames(covariate_summary$mean, prep$covariate_names)
  )
}

# ------------------------------------------------------------
# 4. build_ancova_formulas() -- safe reformulate()-based construction
# ------------------------------------------------------------

#' Build the additive and interaction model formulas from safe internal
#' names ONLY, via stats::reformulate() -- never string-pasting a raw user
#' column name.
#' @param safe_y Safe outcome name.
#' @param safe_factor Safe factor name.
#' @param safe_xc Character vector of safe centered-covariate names.
#' @noRd
build_ancova_formulas <- function(safe_y, safe_factor, safe_xc) {
  interaction_terms <- paste0(safe_factor, ":", safe_xc)
  additive_terms <- c(safe_factor, safe_xc)
  interaction_all_terms <- c(additive_terms, interaction_terms)

  list(
    additive = stats::reformulate(additive_terms, response = safe_y),
    interaction = stats::reformulate(interaction_all_terms, response = safe_y),
    additive_terms = additive_terms,
    interaction_terms = interaction_terms,
    interaction_all_terms = interaction_all_terms
  )
}

# ------------------------------------------------------------
# 5. fit_ancova_models()
# ------------------------------------------------------------

#' Fit the additive and interaction lm() models. Fitting failures are
#' captured (not thrown) so validate_ancova_models() can classify them.
#' @noRd
fit_ancova_models <- function(data, formulas) {
  fit_one <- function(formula) {
    tryCatch(stats::lm(formula, data = data), error = function(e) e)
  }
  list(
    additive = fit_one(formulas$additive),
    interaction = fit_one(formulas$interaction)
  )
}

# ------------------------------------------------------------
# 6. validate_ancova_models() -- rank checks, distinguishing
#    "not significant" from "not estimable" (Rule: rank checks)
# ------------------------------------------------------------

#' Check qr(model.matrix())$rank == ncol(model.matrix()) for both models.
#' Stops with ANCOVA_RANK_DEFICIENT when the ADDITIVE model is deficient
#' (this also covers perfect covariate-covariate collinearity, Test H).
#' Interaction-only deficiency is NOT an error here -- it is returned so the
#' caller can set slope_homogeneity_status = "not_estimable" and keep the
#' additive model's results (Test I).
#' @noRd
validate_ancova_models <- function(models) {
  rank_check <- function(model) {
    if (inherits(model, "error")) {
      return(list(rank = NA_integer_, ncol = NA_integer_, deficient = TRUE, fit_failed = TRUE))
    }
    mm <- stats::model.matrix(model)
    r <- if (nrow(mm) == 0) 0L else qr(mm)$rank
    list(rank = r, ncol = ncol(mm), deficient = r < ncol(mm), fit_failed = FALSE)
  }

  additive_check <- rank_check(models$additive)
  if (isTRUE(additive_check$fit_failed)) {
    ancova_stop("ANCOVA_MODEL_FIT_FAILED",
                paste0("The additive model could not be fit: ", conditionMessage(models$additive)))
  }
  if (isTRUE(additive_check$deficient)) {
    ancova_stop("ANCOVA_RANK_DEFICIENT",
                "The additive model is rank-deficient (perfectly collinear covariates/factor). Cannot proceed.")
  }

  interaction_check <- rank_check(models$interaction)

  list(additive = additive_check, interaction = interaction_check)
}

# ------------------------------------------------------------
# 7. test_global_slope_homogeneity() -- THE model-selection decision (Rule 3)
# ------------------------------------------------------------

#' Nested F-test: additive vs. interaction model. This single test decides
#' model selection for the whole analysis.
#' @param interaction_estimable Whether the interaction model was full rank.
#' @noRd
test_global_slope_homogeneity <- function(model_additive, model_interaction,
                                           interaction_estimable, alpha) {
  if (!interaction_estimable) {
    return(list(
      estimable = FALSE,
      rss_additive = NA_real_, rss_interaction = NA_real_,
      ss_interaction_global = NA_real_, df_interaction_global = NA_integer_,
      df_residual_interaction = NA_integer_,
      F = NA_real_, p_value = NA_real_, alpha = alpha, significant = NA
    ))
  }

  comparison <- stats::anova(model_additive, model_interaction)
  # Row 1 = additive (reduced), Row 2 = interaction (full).
  list(
    estimable = TRUE,
    rss_additive = comparison$RSS[1],
    rss_interaction = comparison$RSS[2],
    ss_interaction_global = comparison$RSS[1] - comparison$RSS[2],
    df_interaction_global = comparison$Df[2],
    df_residual_interaction = model_interaction$df.residual,
    F = comparison$F[2],
    p_value = comparison$`Pr(>F)`[2],
    alpha = alpha,
    # p_value == alpha counts as NON-significant (strict <).
    significant = !is.na(comparison$`Pr(>F)`[2]) && comparison$`Pr(>F)`[2] < alpha
  )
}

# ------------------------------------------------------------
# 8. test_covariate_interactions() -- diagnostic-only (Rule 7)
# ------------------------------------------------------------

#' Per-covariate interaction test: full interaction model vs. a reduced
#' model with only that ONE G:Xj term removed (all other interaction terms
#' kept). Holm-adjusted across covariates. NEVER used for model selection.
#' @noRd
test_covariate_interactions <- function(model_interaction, data, formulas, safe_y,
                                         covariate_names, safe_xc,
                                         interaction_estimable, alpha) {
  empty <- tibble::tibble(
    covariate = character(), SS = double(), df1 = integer(), df2 = integer(),
    F = double(), p_raw = double(), p_holm = double(),
    significant_raw = logical(), significant_adjusted = logical()
  )
  if (!interaction_estimable) {
    return(empty)
  }

  interaction_terms <- formulas$interaction_terms
  raw_rows <- purrr::map_dfr(seq_along(safe_xc), function(j) {
    term_j <- interaction_terms[j]
    reduced_terms <- setdiff(formulas$interaction_all_terms, term_j)
    reduced_formula <- stats::reformulate(reduced_terms, response = safe_y)
    reduced_model <- tryCatch(stats::lm(reduced_formula, data = data), error = function(e) e)
    if (inherits(reduced_model, "error")) {
      return(tibble::tibble(covariate = covariate_names[j], SS = NA_real_,
                             df1 = NA_integer_, df2 = NA_integer_, F = NA_real_,
                             p_raw = NA_real_))
    }
    comparison <- stats::anova(reduced_model, model_interaction)
    tibble::tibble(
      covariate = covariate_names[j],
      SS = comparison$RSS[1] - comparison$RSS[2],
      df1 = comparison$Df[2],
      df2 = model_interaction$df.residual,
      F = comparison$F[2],
      p_raw = comparison$`Pr(>F)`[2]
    )
  })

  raw_rows %>%
    dplyr::mutate(
      p_holm = stats::p.adjust(p_raw, method = "holm"),
      significant_raw = !is.na(p_raw) & p_raw < alpha,
      significant_adjusted = !is.na(p_holm) & p_holm < alpha
    )
}

# ------------------------------------------------------------
# 9. select_ancova_model() -- Rule 6, no stepwise shrinkage (Rule 7)
# ------------------------------------------------------------

#' Decide the final model purely from the GLOBAL slope-homogeneity test.
#' Never auto-shrinks to a partial interaction model.
#' @noRd
select_ancova_model <- function(homogeneity) {
  if (!homogeneity$estimable) {
    status <- "not_estimable"
    final_model_type <- "additive"
    standard_ancova_valid <- FALSE
  } else if (isTRUE(homogeneity$significant)) {
    # "detected"/"not_detected", never "violated"/"homogeneous": P >= alpha does
    # NOT establish that the slopes are equal, only that no difference was
    # detected, and a status word that claims otherwise gets read as proof of
    # the assumption (tam#38389 s7).
    status <- "detected"
    final_model_type <- "interaction"
    standard_ancova_valid <- FALSE
  } else {
    status <- "not_detected"
    final_model_type <- "additive"
    standard_ancova_valid <- TRUE
  }
  list(
    status = status,
    final_model_type = final_model_type,
    standard_ancova_valid = standard_ancova_valid,
    source_model = final_model_type
  )
}

# ------------------------------------------------------------
# 10. compute_ancova_table() -- Type II SS + 4 effect sizes
# ------------------------------------------------------------

#' Build the ANCOVA table (Type II SS via car::Anova) with eta-squared,
#' partial eta-squared, Cohen's f (partial) and omega-squared per term.
#' Only meaningful when final_model_type == "additive".
#' @noRd
compute_ancova_table <- function(final_model, analysis_data, safe_y, safe_factor,
                                  safe_xc, covariate_names) {
  car_anova <- car::Anova(final_model, type = 2)
  term_rows <- rownames(car_anova)
  ss_col <- car_anova[["Sum Sq"]]
  df_col <- car_anova[["Df"]]
  f_col <- car_anova[["F value"]]
  p_col <- car_anova[["Pr(>F)"]]
  names(ss_col) <- term_rows
  names(df_col) <- term_rows
  names(f_col) <- term_rows
  names(p_col) <- term_rows

  SS_error <- ss_col[["Residuals"]]
  df_error <- df_col[["Residuals"]]
  MS_error <- SS_error / df_error

  y <- analysis_data[[safe_y]]
  SS_total <- sum((y - mean(y)) ^ 2)
  df_total <- length(y) - 1

  effect_terms <- setdiff(term_rows, "Residuals")
  covariate_label_map <- stats::setNames(covariate_names, safe_xc)

  terms_tbl <- purrr::map_dfr(effect_terms, function(term) {
    term_type <- if (term == safe_factor) "factor" else "covariate"
    term_name <- if (term == safe_factor) "Factor" else unname(covariate_label_map[[term]])
    SS_effect <- ss_col[[term]]
    df_effect <- df_col[[term]]
    partial_eta_sq <- SS_effect / (SS_effect + SS_error)
    tibble::tibble(
      term_type = term_type,
      term_name = term_name,
      sum_squares = SS_effect,
      df = df_effect,
      mean_square = SS_effect / df_effect,
      F = f_col[[term]],
      p_value = p_col[[term]],
      eta_squared = SS_effect / SS_total,
      partial_eta_squared = partial_eta_sq,
      cohens_f_partial = sqrt(partial_eta_sq / (1 - partial_eta_sq)),
      # Kept as-is when negative -- never truncated to 0.
      omega_squared = (SS_effect - df_effect * MS_error) / (SS_total + MS_error),
      # Legacy "squared ratio" (2乗比) alias: numerically identical to
      # eta_squared. Exposed only as a deprecated alias, never computed
      # independently, per the spec.
      squared_ratio_deprecated_alias = SS_effect / SS_total,
      source_model = "additive"
    )
  })

  residual <- list(
    term_type = "residual", term_name = "Residual",
    sum_squares = SS_error, df = df_error, mean_square = MS_error,
    source_model = "additive"
  )
  corrected_total <- list(
    term_type = "total", term_name = "Corrected Total",
    sum_squares = SS_total, df = df_total,
    source_model = "additive"
  )

  list(terms = terms_tbl, residual = residual, corrected_total = corrected_total, ss_type = "II")
}

# ------------------------------------------------------------
# Shared emmeans helper for adjusted means / conditional means
# ------------------------------------------------------------

#' Build the emmGrid used for BOTH adjusted/conditional means and the
#' pairwise comparisons derived from it (never a fresh emmeans() call for
#' pairwise, per spec).
#' @noRd
ancova_build_emm <- function(model, safe_factor, safe_xc) {
  at_list <- stats::setNames(as.list(rep(0, length(safe_xc))), safe_xc)
  emmeans::emmeans(model, stats::reformulate(safe_factor), at = at_list)
}

#' Tidy an emmGrid of estimated marginal means into the
#' group/estimate/standard_error/df/confidence_lower/confidence_upper shape.
#' @noRd
ancova_tidy_emm <- function(emm, safe_factor, confidence_level, source_model) {
  summ <- as.data.frame(summary(emm, level = confidence_level, infer = c(TRUE, FALSE)))
  tibble::tibble(
    group = as.character(summ[[safe_factor]]),
    estimate = summ$emmean,
    standard_error = summ$SE,
    df = summ$df,
    confidence_lower = summ$asymp.LCL %||% summ$lower.CL,
    confidence_upper = summ$asymp.UCL %||% summ$upper.CL,
    source_model = source_model
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Split an emmeans pairwise contrast label ("group1 - group2") back into
#' group1/group2 by validating candidate split points against the KNOWN
#' factor levels, instead of assuming " - " never occurs inside a level's
#' own text (a level like "Q1 - Q2" would otherwise be mis-split).
#' @noRd
ancova_split_pair_label <- function(label, levels) {
  positions <- gregexpr(" - ", label, fixed = TRUE)[[1]]
  if (positions[1] == -1) {
    return(c(NA_character_, NA_character_))
  }
  for (pos in positions) {
    lhs <- substr(label, 1, pos - 1)
    rhs <- substr(label, pos + 3, nchar(label))
    if (lhs %in% levels && rhs %in% levels) {
      return(c(lhs, rhs))
    }
  }
  # Fall back to the first split point if no exact match was found (should
  # not happen for a well-formed emmeans contrast label).
  pos <- positions[1]
  c(substr(label, 1, pos - 1), substr(label, pos + 3, nchar(label)))
}

#' Tidy an emmeans::pairs() contrast object into
#' group1/group2/<estimate col>/... using level-validated label splitting.
#' @noRd
ancova_tidy_pairs <- function(pw, levels, confidence_level, estimate_col_out,
                               extra_cols = list(), source_model) {
  summ <- as.data.frame(summary(pw, level = confidence_level, infer = c(TRUE, TRUE)))
  # Defensive: as.data.frame() can hand back the contrast label as a factor
  # depending on R/emmeans version defaults; vapply() over a factor would
  # silently iterate its integer codes instead of the label text.
  contrast_labels <- as.character(summ$contrast)
  pairs_split <- t(vapply(contrast_labels, ancova_split_pair_label, character(2), levels = levels))
  out <- tibble::tibble(
    group1 = pairs_split[, 1],
    group2 = pairs_split[, 2],
    !!estimate_col_out := summ$estimate,
    standard_error = summ$SE,
    df = summ$df,
    t_value = summ$t.ratio,
    confidence_lower = summ$asymp.LCL %||% summ$lower.CL,
    confidence_upper = summ$asymp.UCL %||% summ$upper.CL,
    p_value = summ$p.value,
    adjustment = "tukey",
    source_model = source_model
  )
  for (nm in names(extra_cols)) {
    out[[nm]] <- extra_cols[[nm]]
  }
  out
}

# ------------------------------------------------------------
# 11. compute_ancova_adjusted_means() (additive case) /
#     compute_ancova_conditional_means() (interaction case)
# ------------------------------------------------------------

#' Estimated marginal means at all covariates fixed at their analysis-sample
#' grand mean (centered == 0). Reused, unchanged, for BOTH the additive
#' "adjusted means" case and the interaction "conditional means at
#' reference" case -- only the source_model label and the caller-facing
#' field name differ.
#' @noRd
compute_ancova_adjusted_means <- function(model, safe_factor, safe_xc,
                                           covariate_means, confidence_level,
                                           source_model) {
  emm <- ancova_build_emm(model, safe_factor, safe_xc)
  means <- ancova_tidy_emm(emm, safe_factor, confidence_level, source_model)
  list(
    means = means,
    reference_covariates = as.list(covariate_means),
    emm = emm
  )
}

# ------------------------------------------------------------
# 12. compute_ancova_pairwise() -- from the SAME emmGrid (additive and
#     interaction-conditional cases)
# ------------------------------------------------------------

#' Pairwise comparisons from the exact emmGrid object used for the means
#' above (never a fresh emmeans() call).
#' @noRd
compute_ancova_pairwise <- function(emm, factor_levels, confidence_level, source_model) {
  pw <- emmeans::contrast(emm, method = "pairwise", adjust = "tukey")
  ancova_tidy_pairs(pw, factor_levels, confidence_level,
                     estimate_col_out = "adjusted_difference", source_model = source_model)
}

# ------------------------------------------------------------
# 13. compute_ancova_slopes() -- interaction case only (Rule 4)
# ------------------------------------------------------------

#' Per-covariate group-specific slopes (emtrends) and their pairwise
#' comparisons, computed from the interaction model. Only meaningful when
#' final_model_type == "interaction".
#' @noRd
compute_ancova_slopes <- function(model_interaction, safe_factor, safe_xc,
                                   covariate_names, factor_levels, confidence_level) {
  purrr::map(seq_along(safe_xc), function(j) {
    other_xc <- setdiff(safe_xc, safe_xc[j])
    at_list <- if (length(other_xc) > 0) {
      stats::setNames(as.list(rep(0, length(other_xc))), other_xc)
    } else {
      list()
    }
    trend_emm <- emmeans::emtrends(model_interaction, stats::reformulate(safe_factor),
                                   var = safe_xc[j], at = at_list)
    trend_col <- paste0(safe_xc[j], ".trend")
    summ <- as.data.frame(summary(trend_emm, level = confidence_level, infer = c(TRUE, FALSE)))
    slopes <- tibble::tibble(
      covariate = covariate_names[j],
      group = as.character(summ[[safe_factor]]),
      slope = summ[[trend_col]],
      standard_error = summ$SE,
      df = summ$df,
      confidence_lower = summ$asymp.LCL %||% summ$lower.CL,
      confidence_upper = summ$asymp.UCL %||% summ$upper.CL,
      source_model = "interaction"
    )

    pw <- emmeans::contrast(trend_emm, method = "pairwise", adjust = "tukey")
    slope_comparisons <- ancova_tidy_pairs(
      pw, factor_levels, confidence_level, estimate_col_out = "slope_difference",
      extra_cols = list(covariate = covariate_names[j]), source_model = "interaction"
    ) %>% dplyr::select(covariate, group1, group2, slope_difference, standard_error, df,
                        t_value, p_value, confidence_lower, confidence_upper, adjustment,
                        source_model)

    list(covariate = covariate_names[j], slopes = slopes, slope_comparisons = slope_comparisons)
  })
}

# ------------------------------------------------------------
# 14. compute_ancova_raw_statistics() -- descriptive, model-free
# ------------------------------------------------------------

#' Per-factor-level descriptive statistics on the RAW (uncentered) outcome,
#' with df = n_group - 1 (never the model residual df).
#' @noRd
compute_ancova_raw_statistics <- function(analysis_data, safe_y, safe_factor,
                                           factor_levels, alpha) {
  purrr::map_dfr(factor_levels, function(g) {
    vals <- analysis_data[[safe_y]][analysis_data[[safe_factor]] == g]
    n <- length(vals)
    mean_g <- mean(vals, na.rm = TRUE)
    sd_g <- stats::sd(vals, na.rm = TRUE)
    se_g <- sd_g / sqrt(n)
    df_g <- n - 1
    tcrit <- if (df_g > 0) stats::qt(1 - alpha / 2, df_g) else NA_real_
    tibble::tibble(
      group = g, n = n, mean = mean_g, sd = sd_g, se = se_g, df = df_g,
      ci_lower = if (!is.na(tcrit)) mean_g - tcrit * se_g else NA_real_,
      ci_upper = if (!is.na(tcrit)) mean_g + tcrit * se_g else NA_real_,
      min = min(vals, na.rm = TRUE), max = max(vals, na.rm = TRUE),
      source_model = "raw_data"
    )
  })
}

# ------------------------------------------------------------
# 15. assemble_ancova_result()
# ------------------------------------------------------------

#' Assemble the final nested ancova_result list from every computed piece.
#' @noRd
assemble_ancova_result <- function(prep, covariate_summary, homogeneity,
                                    covariate_tests, selection, ancova_table,
                                    adjusted_means, pairwise_comparisons,
                                    conditional_means, conditional_pairwise,
                                    interaction_details, raw_statistics,
                                    alpha, warnings_list) {
  list(
    calculation_version = 2,
    analysis_status = "ok",
    variables = list(
      outcome = prep$outcome_name,
      factor = prep$factor_name,
      covariates = as.list(prep$covariate_names)
    ),
    analysis_sample = list(
      n_original = prep$n_original,
      n_used = prep$n_used,
      n_removed = prep$n_removed,
      factor_levels = as.list(prep$factor_levels)
    ),
    covariate_summary = covariate_summary,
    slope_homogeneity = list(
      estimable = homogeneity$estimable,
      status = selection$status,
      global_test = list(
        SS = homogeneity$ss_interaction_global,
        df1 = homogeneity$df_interaction_global,
        df2 = homogeneity$df_residual_interaction,
        F = homogeneity$F,
        p_value = homogeneity$p_value,
        alpha = homogeneity$alpha,
        significant = homogeneity$significant
      ),
      covariate_tests = covariate_tests
    ),
    model_selection = list(
      final_model_type = selection$final_model_type,
      standard_ancova_valid = selection$standard_ancova_valid,
      source_model = selection$source_model
    ),
    ancova_table = ancova_table,
    adjusted_means = adjusted_means,
    pairwise_comparisons = pairwise_comparisons,
    conditional_means_at_reference = conditional_means,
    conditional_pairwise_at_reference = conditional_pairwise,
    interaction_details = interaction_details,
    raw_statistics = raw_statistics,
    metadata = list(
      alpha = alpha,
      confidence_level = 1 - alpha,
      ss_type = "II",
      covariate_centering = "grand_mean",
      interaction_global_test = "nested_model_f_test",
      individual_interaction_test = "nested_model_f_test",
      individual_interaction_p_adjustment = "holm",
      pairwise_adjustment = "tukey"
    ),
    warnings = as.list(warnings_list)
  )
}

# ------------------------------------------------------------
# Top-level entry point
# ------------------------------------------------------------

#' ANCOVA Calculation V2 -- R computation layer for the Analytics View
#' (tam#38385, Phase 1).
#'
#' Supports multiple covariates, tests the homogeneity-of-regression-slopes
#' assumption with a single global nested F-test (additive vs. interaction
#' model), and branches to a distinct, clearly-labeled result set when that
#' assumption is violated instead of reporting standard ANCOVA output that
#' would no longer be valid. See the file header of R/ancova_v2.R for the
#' full design summary.
#'
#' @param data A data frame.
#' @param outcome Name of the numeric outcome column (string).
#' @param factor Name of the categorical factor/group column (string, >= 2
#'   levels after NA removal).
#' @param covariates Character vector of 1+ numeric covariate column names.
#' @param alpha Significance threshold (default 0.05). confidence_level is
#'   `1 - alpha`.
#' @param keep_internals When TRUE, the returned list carries an extra
#'   `internals` element holding the fitted `lm` objects, the emmGrid the
#'   adjusted/conditional means came from, and the centered analysis data.
#'   These are R objects, NOT part of the serializable result contract --
#'   `internals` must never be passed to `jsonlite::toJSON()`. It exists so
#'   an in-process caller (`exp_ancova()`, and Phase 2's diagnostics) can
#'   `predict()` / `rstandard()` off the SAME fits the reported statistics
#'   came from instead of refitting a second, possibly divergent model.
#' @return A nested R list mirroring the "ancova_result" shape from the
#'   tam#38385 spec. On failure, a short list with
#'   `analysis_status = "error"` and an `error_code`.
#' @export
run_ancova_v2 <- function(data, outcome, factor, covariates, alpha = 0.05,
                          keep_internals = FALSE) {
  tryCatch({
    if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
      ancova_stop("ANCOVA_INVALID_INPUT", "`alpha` must be a single number strictly between 0 and 1.")
    }

    warnings_list <- character()

    prep <- prepare_ancova_data(data, outcome, factor, covariates)
    validate_ancova_data(prep)

    centered <- center_ancova_covariates(prep)
    formulas <- build_ancova_formulas(prep$safe_y, prep$safe_factor, centered$safe_xc)
    models <- fit_ancova_models(centered$analysis_data, formulas)
    model_checks <- validate_ancova_models(models)

    homogeneity <- test_global_slope_homogeneity(
      models$additive, models$interaction, !model_checks$interaction$deficient, alpha)
    covariate_tests <- test_covariate_interactions(
      models$interaction, centered$analysis_data, formulas, prep$safe_y, prep$covariate_names,
      centered$safe_xc, homogeneity$estimable, alpha)
    selection <- select_ancova_model(homogeneity)

    if (selection$status == "not_estimable") {
      warnings_list <- c(warnings_list,
        "The interaction model could not be estimated (rank-deficient), so the homogeneity-of-slopes assumption could not be tested. Standard ANCOVA results are shown but should be interpreted with caution.")
    }

    confidence_level <- 1 - alpha
    final_model <- if (selection$final_model_type == "interaction") models$interaction else models$additive

    ancova_table <- NULL
    reported_emm <- NULL
    adjusted_means <- NULL
    pairwise_comparisons <- NULL
    conditional_means <- NULL
    conditional_pairwise <- NULL
    interaction_details <- list()

    if (selection$final_model_type == "additive") {
      ancova_table <- compute_ancova_table(
        final_model, centered$analysis_data, prep$safe_y, prep$safe_factor,
        centered$safe_xc, prep$covariate_names)
      am <- compute_ancova_adjusted_means(
        final_model, prep$safe_factor, centered$safe_xc, centered$covariate_means,
        confidence_level, source_model = "additive")
      adjusted_means <- list(means = am$means, reference_covariates = am$reference_covariates)
      reported_emm <- am$emm
      pairwise_comparisons <- compute_ancova_pairwise(
        am$emm, prep$factor_levels, confidence_level, source_model = "additive")
    } else {
      cm <- compute_ancova_adjusted_means(
        final_model, prep$safe_factor, centered$safe_xc, centered$covariate_means,
        confidence_level, source_model = "interaction")
      conditional_means <- list(means = cm$means, reference_covariates = cm$reference_covariates)
      reported_emm <- cm$emm
      conditional_pairwise <- compute_ancova_pairwise(
        cm$emm, prep$factor_levels, confidence_level, source_model = "interaction")
      interaction_details <- compute_ancova_slopes(
        final_model, prep$safe_factor, centered$safe_xc, prep$covariate_names,
        prep$factor_levels, confidence_level)
      warnings_list <- c(warnings_list,
        "The homogeneity-of-regression-slopes assumption was violated: standard ANCOVA adjusted means and pairwise comparisons are not reported. Conditional means at the covariate reference point and group-specific slopes are reported instead.")
    }

    low_card <- centered$covariate_summary$name[centered$covariate_summary$low_cardinality]
    if (length(low_card) > 0) {
      warnings_list <- c(warnings_list, paste0(
        "Low-cardinality covariate(s) (<= 5 unique values): ", paste(low_card, collapse = ", "), "."))
    }

    raw_statistics <- compute_ancova_raw_statistics(
      centered$analysis_data, prep$safe_y, prep$safe_factor, prep$factor_levels, alpha)

    result <- assemble_ancova_result(
      prep, centered$covariate_summary, homogeneity, covariate_tests, selection,
      ancova_table, adjusted_means, pairwise_comparisons, conditional_means,
      conditional_pairwise, interaction_details, raw_statistics, alpha, warnings_list)

    if (isTRUE(keep_internals)) {
      # NOT part of the serializable contract -- see the @param note. Everything
      # here is a live R object; the reported statistics above were all derived
      # from these exact fits, so a consumer that re-derives anything from them
      # (predict(), rstandard(), a re-adjusted pairwise contrast) cannot drift
      # from what the report shows.
      result$internals <- list(
        model_additive = models$additive,
        model_interaction = models$interaction,
        final_model = final_model,
        final_model_type = selection$final_model_type,
        reported_emm = reported_emm,
        analysis_data = centered$analysis_data,
        safe_y = prep$safe_y,
        safe_factor = prep$safe_factor,
        safe_x = prep$safe_x,
        safe_xc = centered$safe_xc,
        covariate_means = centered$covariate_means,
        formulas = formulas
      )
    }
    result
  },
  ancova_error = function(e) {
    list(calculation_version = 2, analysis_status = "error",
         error_code = e$error_code, message = conditionMessage(e))
  },
  error = function(e) {
    list(calculation_version = 2, analysis_status = "error",
         error_code = "ANCOVA_UNEXPECTED_ERROR", message = conditionMessage(e))
  })
}
