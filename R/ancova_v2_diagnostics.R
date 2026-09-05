# ANCOVA Calculation V2 -- Phase 2 diagnostics (tam#38389).
#
# Phase 1 (R/ancova_v2.R) decides everything: which rows are in the analysis,
# which model is final, what the slope-homogeneity test concluded. This file
# adds NOTHING to that. It is a presentation layer that turns the fits Phase 1
# already produced into the data three diagnostic charts need, and it obeys two
# rules the spec states outright:
#
#   Relationship chart -> ALWAYS the full-interaction model. The chart exists to
#   let a reader judge whether the slopes really are parallel; drawing it from
#   the additive model, which forces them parallel by construction, would be a
#   circular diagnostic (spec sections 15 and 82).
#
#   Residual diagnostics -> ALWAYS the FINAL model, whichever that is. These ask
#   whether the model being reported describes the data (sections 34, 82).
#
# Nothing here refits: every fit comes from run_ancova_v2's own `internals`, so
# a diagnostic cannot drift from the numbers the report shows. predict() and
# rstandard() on those fits are the only model calls (section 65).
#
# Serialization contract, inherited from R/ancova_v2.R's header: every leaf is a
# tibble, a length-1 scalar, or a plain list. No model objects, no emmGrid.

# ------------------------------------------------------------
# Constants (spec sections 19, 48, 49, 50, 51)
# ------------------------------------------------------------

#' Prediction-grid resolution per factor level for a regression line.
ANCOVA_RELATIONSHIP_GRID_SIZE <- 100L

#' Scatter points a single chart may carry.
ANCOVA_MAX_POINTS_PER_CHART <- 5000L

#' Total scatter points the relationship charts may carry ACROSS every
#' covariate. With many covariates a per-chart cap alone would multiply into a
#' payload nothing can render; this bounds the whole set.
ANCOVA_DIAGNOSTIC_POINT_BUDGET <- 50000L

#' Floor for the per-covariate share of that budget. Below this a scatter stops
#' conveying a distribution at all, so a very wide covariate list gets fewer
#' USABLE charts rather than many useless ones.
ANCOVA_MIN_POINTS_PER_COVARIATE <- 500L

#' Q-Q points. Sampled as evenly spaced order statistics, never at random --
#' random sampling drops exactly the tails a Q-Q plot is read for (section 51).
ANCOVA_QQ_MAX_POINTS <- 2000L

#' Factor levels beyond which the relationship chart's points are omitted by
#' default and only the regression lines are drawn (section 63).
ANCOVA_MAX_LEVELS_WITH_POINTS <- 12L

# ------------------------------------------------------------
# Sampling helpers
# ------------------------------------------------------------

#' Factor-stratified sample of row indices.
#'
#' Plain random sampling can erase a small group entirely, which is the one
#' thing a group-comparison chart must not do. Each level keeps at least its
#' proportional share, and a level smaller than that share is kept whole
#' (section 48).
#' @noRd
sample_ancova_scatter_points <- function(factor_values, max_points) {
  n <- length(factor_values)
  if (n <= max_points) {
    return(seq_len(n))
  }
  idx_by_level <- split(seq_len(n), factor_values)
  idx_by_level <- idx_by_level[vapply(idx_by_level, length, integer(1)) > 0]
  sizes <- vapply(idx_by_level, length, integer(1))

  # Largest-remainder allocation, so the kept total is exactly max_points
  # instead of drifting with rounding across many levels.
  exact <- sizes / n * max_points
  keep <- pmin(sizes, floor(exact))
  remainder <- max_points - sum(keep)
  if (remainder > 0) {
    room <- sizes - keep
    order_by_frac <- order(exact - floor(exact), decreasing = TRUE)
    for (i in order_by_frac) {
      if (remainder <= 0) break
      take <- min(room[[i]], remainder)
      keep[[i]] <- keep[[i]] + take
      remainder <- remainder - take
    }
  }

  picked <- unlist(lapply(seq_along(idx_by_level), function(i) {
    ids <- idx_by_level[[i]]
    if (keep[[i]] >= length(ids)) ids else sort(sample(ids, keep[[i]]))
  }), use.names = FALSE)
  sort(picked)
}

#' Per-covariate point allocation under the shared budget (section 49).
#' @noRd
ancova_points_per_covariate <- function(n_covariates) {
  if (n_covariates <= 0) return(ANCOVA_MAX_POINTS_PER_CHART)
  share <- floor(ANCOVA_DIAGNOSTIC_POINT_BUDGET / n_covariates)
  max(ANCOVA_MIN_POINTS_PER_COVARIATE, min(ANCOVA_MAX_POINTS_PER_CHART, share))
}

# ------------------------------------------------------------
# Relationship chart (sections 11-32)
# ------------------------------------------------------------

#' Regression lines for one covariate, from the INTERACTION model.
#'
#' One line per factor level, spanning only that level's own observed range of
#' the covariate -- a line drawn past where a group has data invites reading a
#' comparison the data cannot support (section 18). Every other covariate is
#' held at its grand mean, which in centered space is 0 (section 16).
#' @noRd
compute_ancova_relationship_lines <- function(model_interaction, analysis_data, safe_factor,
                                              safe_xc_target, safe_xc_all, factor_levels,
                                              covariate_mean, alpha) {
  conf_level <- 1 - alpha
  purrr::map_dfr(factor_levels, function(lv) {
    in_level <- analysis_data[[safe_factor]] == lv
    xs <- analysis_data[[safe_xc_target]][in_level]
    xs <- xs[is.finite(xs)]
    if (length(xs) < 2 || length(unique(xs)) < 2) {
      # A group with no spread has no line to draw; the other groups still do.
      return(tibble::tibble())
    }
    grid <- seq(min(xs), max(xs), length.out = ANCOVA_RELATIONSHIP_GRID_SIZE)
    newdata <- as.data.frame(stats::setNames(
      lapply(safe_xc_all, function(nm) if (identical(nm, safe_xc_target)) grid else rep(0, length(grid))),
      safe_xc_all), stringsAsFactors = FALSE)
    newdata[[safe_factor]] <- factor(lv, levels = factor_levels)

    pred <- stats::predict(model_interaction, newdata = newdata, se.fit = TRUE)
    tcrit <- stats::qt(1 - alpha / 2, df = pred$df)
    tibble::tibble(
      factor_level = as.character(lv),
      # Reported on the covariate's own raw scale -- the chart's x axis is the
      # column the user picked, not a centered internal.
      x = grid + covariate_mean,
      predicted_y = as.numeric(pred$fit),
      standard_error = as.numeric(pred$se.fit),
      ci_lower = as.numeric(pred$fit) - tcrit * as.numeric(pred$se.fit),
      ci_upper = as.numeric(pred$fit) + tcrit * as.numeric(pred$se.fit),
      confidence_level = conf_level
    )
  })
}

#' Observation points for one covariate, adjusted for the other covariates.
#'
#' Plotting the raw outcome against one covariate while the LINE holds the
#' others at their means shows a cloud and a line that answer different
#' questions; the scatter looks far noisier than the model it is drawn beside.
#' So each point is moved to where it would sit if that row's other covariates
#' were at their means, keeping its own residual (sections 22-25):
#'
#'   adjusted_y = predict(interaction, row with other covariates centered at 0)
#'                + (y - predict(interaction, row as observed))
#'
#' With a single covariate the reference row IS the observed row, so
#' adjusted_y == raw_y exactly and no special case is needed (section 26).
#' @noRd
compute_ancova_relationship_points <- function(model_interaction, analysis_data, safe_y, safe_factor,
                                               safe_xc_target, safe_xc_all, factor_levels,
                                               covariate_mean, low_cardinality, max_points) {
  n <- nrow(analysis_data)
  fitted_observed <- as.numeric(stats::predict(model_interaction, newdata = analysis_data))
  residual <- analysis_data[[safe_y]] - fitted_observed

  reference_data <- analysis_data
  for (nm in safe_xc_all) {
    if (!identical(nm, safe_xc_target)) reference_data[[nm]] <- 0
  }
  fitted_reference <- as.numeric(stats::predict(model_interaction, newdata = reference_data))
  adjusted_y <- fitted_reference + residual

  raw_x <- analysis_data[[safe_xc_target]] + covariate_mean

  # Jitter is a DISPLAY concession for a covariate with a handful of distinct
  # values, where every point would otherwise land on the same few verticals.
  # It never touches the model, the prediction, or the reported value: display_x
  # is a separate column and raw x is what a tooltip reads (sections 31-32).
  display_x <- raw_x
  if (isTRUE(low_cardinality)) {
    spread <- diff(range(raw_x, na.rm = TRUE))
    width <- if (is.finite(spread) && spread > 0) spread / 60 else 0.05
    display_x <- raw_x + stats::runif(n, -width, width)
  }

  keep <- sample_ancova_scatter_points(analysis_data[[safe_factor]], max_points)
  list(
    points = tibble::tibble(
      row_id = keep,
      factor_level = as.character(analysis_data[[safe_factor]])[keep],
      x = raw_x[keep],
      display_x = display_x[keep],
      raw_y = analysis_data[[safe_y]][keep],
      adjusted_y = adjusted_y[keep]
    ),
    n_total = n,
    n_displayed = length(keep),
    sampled = length(keep) < n
  )
}

#' Everything the relationship chart needs, one entry per covariate.
#' @noRd
compute_ancova_relationships <- function(model_interaction, analysis_data, prep, centered,
                                         covariate_tests, interaction_estimable, alpha) {
  covariate_names <- prep$covariate_names
  n_cov <- length(covariate_names)
  max_points <- ancova_points_per_covariate(n_cov)
  factor_levels <- prep$factor_levels
  slopes_by_covariate <- NULL

  purrr::map(seq_len(n_cov), function(j) {
    cov_name <- covariate_names[[j]]
    base <- list(
      covariate = cov_name,
      reference_value = unname(centered$covariate_means[[cov_name]]),
      interaction_test = ancova_covariate_test_row(covariate_tests, cov_name)
    )

    if (!isTRUE(interaction_estimable)) {
      # Better to say the chart cannot be drawn than to draw the additive
      # model's parallel lines and let them be read as evidence of parallelism.
      return(c(base, list(
        available = FALSE,
        reason = "interaction_model_not_estimable",
        points = tibble::tibble(), lines = tibble::tibble(), slopes = tibble::tibble(),
        metadata = list(source_model = "interaction", n_total = nrow(analysis_data),
                        n_displayed = 0L, sampled = FALSE)
      )))
    }

    safe_xc_target <- centered$safe_xc[[j]]
    if (length(unique(analysis_data[[safe_xc_target]])) < 2) {
      return(c(base, list(
        available = FALSE, reason = "insufficient_unique_x",
        points = tibble::tibble(), lines = tibble::tibble(), slopes = tibble::tibble(),
        metadata = list(source_model = "interaction", n_total = nrow(analysis_data),
                        n_displayed = 0L, sampled = FALSE)
      )))
    }

    lines <- compute_ancova_relationship_lines(
      model_interaction, analysis_data, prep$safe_factor, safe_xc_target,
      centered$safe_xc, factor_levels, base$reference_value, alpha)

    if (nrow(lines) == 0 || any(!is.finite(lines$predicted_y))) {
      return(c(base, list(
        available = FALSE, reason = "non_finite_predictions",
        points = tibble::tibble(), lines = tibble::tibble(), slopes = tibble::tibble(),
        metadata = list(source_model = "interaction", n_total = nrow(analysis_data),
                        n_displayed = 0L, sampled = FALSE)
      )))
    }

    show_points <- length(factor_levels) <= ANCOVA_MAX_LEVELS_WITH_POINTS
    pts <- if (show_points) {
      compute_ancova_relationship_points(
        model_interaction, analysis_data, prep$safe_y, prep$safe_factor,
        safe_xc_target, centered$safe_xc, factor_levels, base$reference_value,
        centered$covariate_summary$low_cardinality[[j]], max_points)
    } else {
      list(points = tibble::tibble(), n_total = nrow(analysis_data),
           n_displayed = 0L, sampled = FALSE)
    }

    if (is.null(slopes_by_covariate)) {
      # emtrends once for every covariate, not once per covariate.
      slopes_by_covariate <<- tryCatch(
        compute_ancova_slopes(model_interaction, prep$safe_factor, centered$safe_xc,
                              covariate_names, factor_levels, 1 - alpha),
        error = function(e) list())
    }
    slopes <- ancova_slopes_for_covariate(slopes_by_covariate, cov_name)

    c(base, list(
      available = TRUE,
      reason = NA_character_,
      points = pts$points,
      lines = lines,
      slopes = slopes,
      metadata = list(
        source_model = "interaction",
        other_covariates_reference = if (n_cov > 1) "grand_mean" else "not_applicable",
        points_shown_by_default = show_points,
        low_cardinality_jitter = isTRUE(centered$covariate_summary$low_cardinality[[j]]),
        n_total = pts$n_total,
        n_displayed = pts$n_displayed,
        sampled = pts$sampled
      )
    ))
  })
}

#' @noRd
ancova_covariate_test_row <- function(covariate_tests, cov_name) {
  if (is.null(covariate_tests) || nrow(covariate_tests) == 0) return(list())
  row <- covariate_tests[covariate_tests$covariate == cov_name, , drop = FALSE]
  if (nrow(row) == 0) return(list())
  as.list(row[1, , drop = TRUE])
}

#' @noRd
ancova_slopes_for_covariate <- function(interaction_details, cov_name) {
  if (length(interaction_details) == 0) return(tibble::tibble())
  for (entry in interaction_details) {
    if (identical(entry$covariate, cov_name)) return(entry$slopes)
  }
  tibble::tibble()
}

# ------------------------------------------------------------
# Residual diagnostics (sections 33-45)
# ------------------------------------------------------------

#' @noRd
compute_residual_fitted_data <- function(final_model, analysis_data, safe_factor, max_points) {
  std_resid <- stats::rstandard(final_model)
  fitted <- stats::fitted(final_model)
  raw_resid <- stats::residuals(final_model)
  n <- length(std_resid)
  keep <- sample_ancova_scatter_points(analysis_data[[safe_factor]][seq_len(n)], max_points)
  list(
    points = tibble::tibble(
      fitted = as.numeric(fitted)[keep],
      residual = as.numeric(raw_resid)[keep],
      standardized_residual = as.numeric(std_resid)[keep],
      factor_level = as.character(analysis_data[[safe_factor]])[keep]
    ),
    n_total = n,
    n_displayed = length(keep),
    sampled = length(keep) < n,
    full_fitted = as.numeric(fitted),
    full_std_resid = as.numeric(std_resid)
  )
}

#' Lowess smoother over the FULL data, not the sampled points.
#'
#' The smoother is what a reader actually judges the residual pattern from, so
#' it must describe every row -- computing it from the scatter sample would let
#' the picture change with the sampling (section 50).
#' @noRd
compute_residual_smoother <- function(fitted, standardized_residual) {
  ok <- is.finite(fitted) & is.finite(standardized_residual)
  if (sum(ok) < 3) return(tibble::tibble())
  sm <- stats::lowess(fitted[ok], standardized_residual[ok], f = 2 / 3, iter = 3)
  tibble::tibble(fitted = sm$x, smoothed_residual = sm$y)
}

#' Q-Q data, thinned by evenly spaced ORDER STATISTICS.
#'
#' Sorting first and then taking every k-th value keeps both tails, which is
#' the part of a Q-Q plot that carries the information; a random subsample
#' would preferentially drop them (section 51).
#' @noRd
compute_qq_data <- function(standardized_residual) {
  z <- sort(standardized_residual[is.finite(standardized_residual)])
  n <- length(z)
  if (n < 3) {
    return(list(points = tibble::tibble(), reference_line = list(intercept = NA_real_, slope = NA_real_),
                n_total = n, n_displayed = 0L, sampled = FALSE))
  }
  theoretical <- stats::qnorm(stats::ppoints(n))

  idx <- seq_len(n)
  if (n > ANCOVA_QQ_MAX_POINTS) {
    idx <- unique(round(seq(1, n, length.out = ANCOVA_QQ_MAX_POINTS)))
  }

  # Reference line through the first and third quartiles -- the conventional
  # Q-Q line, computed from ALL residuals even when the points are thinned.
  xq <- stats::qnorm(c(0.25, 0.75))
  yq <- stats::quantile(z, c(0.25, 0.75), names = FALSE)
  slope <- diff(yq) / diff(xq)
  intercept <- yq[[1]] - slope * xq[[1]]

  list(
    points = tibble::tibble(theoretical = theoretical[idx], observed = z[idx]),
    reference_line = list(intercept = intercept, slope = slope),
    n_total = n,
    n_displayed = length(idx),
    sampled = length(idx) < n
  )
}

#' @noRd
compute_ancova_residual_diagnostics <- function(final_model, analysis_data, safe_factor,
                                                final_model_type) {
  rf <- compute_residual_fitted_data(final_model, analysis_data, safe_factor,
                                     ANCOVA_MAX_POINTS_PER_CHART)
  smoother <- compute_residual_smoother(rf$full_fitted, rf$full_std_resid)
  qq <- compute_qq_data(rf$full_std_resid)

  list(
    available = TRUE,
    source_model = final_model_type,
    model_type = final_model_type,
    n = rf$n_total,
    residual_vs_fitted = list(
      points = rf$points,
      smoother = smoother,
      reference_y = 0,
      n_total = rf$n_total,
      n_displayed = rf$n_displayed,
      sampled = rf$sampled
    ),
    qq = list(
      points = qq$points,
      reference_line = qq$reference_line,
      n_total = qq$n_total,
      n_displayed = qq$n_displayed,
      sampled = qq$sampled
    )
  )
}

# ------------------------------------------------------------
# Assembly (sections 52, 56, 57, 77, 79)
# ------------------------------------------------------------

#' @noRd
assemble_ancova_diagnostics <- function(relationships, residuals, selection, homogeneity,
                                        covariate_tests, alpha) {
  detected <- character(0)
  if (!is.null(covariate_tests) && nrow(covariate_tests) > 0) {
    detected <- as.character(covariate_tests$covariate[isTRUE_vec(covariate_tests$significant_adjusted)])
  }
  list(
    diagnostics_version = 1,
    relationships = relationships,
    residuals = residuals,
    # Phase 3 reads these rather than re-deriving them from the tables
    # (section 79).
    homogeneity_status = selection$status,
    standard_ancova_valid = selection$standard_ancova_valid,
    interaction_detected_covariates = as.list(detected),
    diagnostic_model_type = selection$final_model_type,
    diagnostics_available = isTRUE(residuals$available) ||
      any(vapply(relationships, function(r) isTRUE(r$available), logical(1))),
    metadata = list(
      relationship_model = "full_interaction",
      residual_model = "final_model",
      relationship_reference = "other_covariates_at_grand_mean",
      relationship_points = "partial_adjusted_observations",
      residual_type = "standardized",
      qq_distribution = "normal",
      scatter_sampling = "factor_stratified",
      # Not testable from the data alone, so it is stated rather than judged
      # (section 4).
      independence_tested = FALSE,
      alpha = alpha,
      grid_size = ANCOVA_RELATIONSHIP_GRID_SIZE,
      max_points_per_chart = ANCOVA_MAX_POINTS_PER_CHART,
      diagnostic_point_budget = ANCOVA_DIAGNOSTIC_POINT_BUDGET,
      qq_max_points = ANCOVA_QQ_MAX_POINTS
    )
  )
}

#' @noRd
isTRUE_vec <- function(x) {
  if (is.null(x)) return(logical(0))
  !is.na(x) & x
}

#' Top-level Phase 2 entry point, called from within run_ancova_v2().
#'
#' Guarded per chart family: one diagnostic that cannot be produced must not
#' cost the report the rest of them, or the ANCOVA result itself (section 64).
#' @noRd
compute_ancova_diagnostics <- function(models, final_model, analysis_data, prep, centered,
                                       selection, homogeneity, covariate_tests, alpha) {
  interaction_estimable <- isTRUE(homogeneity$estimable) && inherits(models$interaction, "lm")

  relationships <- tryCatch(
    compute_ancova_relationships(models$interaction, analysis_data, prep, centered,
                                 covariate_tests, interaction_estimable, alpha),
    error = function(e) list())

  residuals <- tryCatch(
    compute_ancova_residual_diagnostics(final_model, analysis_data, prep$safe_factor,
                                        selection$final_model_type),
    error = function(e) list(available = FALSE, reason = "residuals_not_available",
                             source_model = selection$final_model_type))

  assemble_ancova_diagnostics(relationships, residuals, selection, homogeneity,
                              covariate_tests, alpha)
}
