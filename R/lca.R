#' Fit latent class analysis models for categorical variables.
#'
#' Fits every requested class count with poLCA, selects the model with the
#' smallest BIC, and retains the fitted conditional probabilities and row-level
#' posterior membership probabilities for the analytics report.
#'
#' @param df Data frame.
#' @param ... Categorical indicator columns.
#' @param min_nclass Smallest class count to evaluate.
#' @param max_nclass Largest class count to evaluate.
#' @param nrep Number of random starts for each candidate model.
#' @param maxiter Maximum EM iterations per random start.
#' @param seed Random seed.
#' @param relationship_column Optional categorical column for class-by-category
#'   composition. It is not used as an indicator.
#' @param feature_top_n Number of top characteristic categories retained per
#'   class for the report.
#' @param max_nrow Optional cap on the number of rows used to fit the model. When
#'   set and the (group's) data has more rows, a random sample of this size is
#'   used for fitting AND for the row-level Output Data table (tam#38399), the
#'   same "Sample Data" behavior K-Means/K-Modes/K-Medoids already have. NULL
#'   means use every row.
#' @export
exp_lca <- function(df, ...,
                    min_nclass = 2,
                    max_nclass = 6,
                    nrep = 20,
                    maxiter = 5000,
                    seed = 1,
                    relationship_column = NULL,
                    feature_top_n = 10,
                    max_nrow = NULL) {
  if (!requireNamespace("poLCA", quietly = TRUE)) {
    stop("Latent Class Analysis requires the poLCA package.")
  }

  selected_cols <- tidyselect::vars_select(names(df), !!!rlang::quos(...))
  grouped_cols <- grouped_by(df)
  if (any(selected_cols %in% grouped_cols)) {
    stop("Repeat-By column cannot be used as a variable column.")
  }
  selected_cols <- setdiff(selected_cols, grouped_cols)
  if (length(selected_cols) < 2) {
    stop("Latent Class Analysis requires at least 2 categorical variables. Select more variables.")
  }

  relationship_expr <- substitute(relationship_column)
  relationship_col <- if (is.null(relationship_expr)) NULL else rlang::as_name(relationship_expr)
  if (!is.null(relationship_col)) {
    if (!relationship_col %in% names(df)) {
      stop("The relationship variable is not present in the data.")
    }
    if (relationship_col %in% selected_cols) {
      stop("The relationship variable must be different from the indicator variables.")
    }
    if (relationship_col %in% grouped_cols) {
      stop("Repeat-By column cannot be used as a relationship variable.")
    }
  }

  min_nclass <- as.integer(min_nclass)
  max_nclass <- as.integer(max_nclass)
  nrep <- as.integer(nrep)
  maxiter <- as.integer(maxiter)
  feature_top_n <- as.integer(feature_top_n)
  if (is.na(min_nclass) || min_nclass < 2 || is.na(max_nclass) || max_nclass < min_nclass) {
    stop("Class counts must be integers with Maximum Number of Classes at least 2 and no smaller than Minimum Number of Classes.")
  }
  if (is.na(nrep) || nrep < 1 || is.na(maxiter) || maxiter < 1 || is.na(feature_top_n) || feature_top_n < 1) {
    stop("Trial Times, Max Iteration Times, and Number of Characteristics must be 1 or larger.")
  }

  unsupported <- selected_cols[!vapply(df[selected_cols], function(x) {
    is.character(x) || is.factor(x) || is.logical(x)
  }, logical(1))]
  if (length(unsupported)) {
    stop(paste0("Latent Class Analysis supports character, factor, ordered, and logical variables only. Unsupported: ",
                paste(unsupported, collapse = ", "), "."))
  }

  each_func <- function(group_df) {
    group_df <- dplyr::ungroup(group_df)
    # tam#38399: cap the data BEFORE fitting, same "Sample Data" behavior as
    # exp_kmeans/exp_kmodes/exp_kmedoids -- the Output Data table (tidy(..., type="data"))
    # is built from this same (possibly sampled) data, so max_nrow is exactly what
    # caps how much data that table shows.
    if (!is.null(seed)) set.seed(seed)
    sampled_nrow <- NULL
    if (!is.null(max_nrow) && nrow(group_df) > max_nrow) {
      sampled_nrow <- max_nrow
      group_df <- group_df %>% sample_rows(max_nrow)
    }
    original <- group_df %>% dplyr::mutate(.lca_row_id = dplyr::row_number())
    indicators <- lca_encode_indicators(original, selected_cols)
    complete_rows <- stats::complete.cases(indicators)
    used <- indicators[complete_rows, , drop = FALSE]
    used_row_ids <- original$.lca_row_id[complete_rows]
    excluded_nrow <- sum(!complete_rows)
    if (!nrow(used)) {
      stop("There is no row left after removing rows with missing values in the selected variables.")
    }
    if (any(vapply(used, function(x) length(unique(x)) < 2, logical(1)))) {
      stop("Each selected variable must contain at least 2 categories after rows with missing values are removed.")
    }
    distinct_patterns <- nrow(dplyr::distinct(used))
    max_candidate_nclass <- min(max_nclass, distinct_patterns, nrow(used))
    if (max_candidate_nclass < min_nclass) {
      stop("There are not enough distinct complete rows to fit at least 2 latent classes.")
    }
    requested_counts <- seq.int(min_nclass, max_candidate_nclass)
    # tam#38352: a 1-class fit is always computed as an internal baseline,
    # regardless of the user-configured explore range (which starts at 2 in
    # the UI). It is a legitimate LCA reference model -- if it turns out to
    # have the smallest BIC among converged candidates, that is evidence no
    # latent class structure beyond a single group is supported by the data,
    # which the report surfaces explicitly rather than silently picking a
    # multi-class split anyway.
    candidate_counts <- sort(unique(c(1L, requested_counts)))

    formula <- stats::as.formula(paste0("cbind(", paste(vapply(names(used), lca_quote_name, character(1)), collapse = ", "), ") ~ 1"))
    candidates <- lapply(candidate_counts, function(k) {
      attempt <- lca_fit_adaptive(formula, used, k, nrep, maxiter, seed)
      if (inherits(attempt$fit, "error")) {
        return(list(nclass = k, fit = NULL, error = conditionMessage(attempt$fit),
                    random_starts = attempt$random_starts, best_reproductions = NA_integer_))
      }
      list(nclass = k, fit = attempt$fit, error = NULL,
           random_starts = attempt$random_starts,
           best_reproductions = attempt$best_reproductions)
    })
    successful <- Filter(function(x) !is.null(x$fit), candidates)
    if (!length(successful)) {
      errors <- vapply(candidates, function(x) paste0(x$nclass, ": ", x$error), character(1))
      stop(paste0("Latent Class Analysis could not fit any requested class count. ", paste(errors, collapse = " | ")))
    }
    # tam#38352: a non-converged fit is not a candidate for the recommended
    # model -- its BIC describes a solution the optimizer never actually
    # settled into. Recommend by BIC only among CONVERGED fits; fall back to
    # every successful fit only when none converged at all (so the analysis
    # still returns a result rather than erroring out).
    converged_candidates <- Filter(function(x) lca_fit_converged(x$fit), successful)
    selection_pool <- if (length(converged_candidates)) converged_candidates else successful
    used_unconverged_fallback <- length(converged_candidates) == 0
    best_index <- which.min(vapply(selection_pool, function(x) x$fit$bic, numeric(1)))
    selected <- selection_pool[[best_index]]$fit
    normalized <- lca_normalize_class_order(selected)
    selected <- normalized$fit

    class_selection <- lca_class_selection_table(candidates)
    profiles <- lca_profile_table(selected, original[complete_rows, selected_cols, drop = FALSE], selected_cols)
    discrimination <- profiles %>%
      dplyr::group_by(variable, category) %>%
      dplyr::summarise(max_minus_min_probability = max(probability) - min(probability), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(max_minus_min_probability), variable, category)
    characteristics <- profiles %>%
      dplyr::group_by(class) %>%
      dplyr::arrange(dplyr::desc(abs_difference), variable, category, .by_group = TRUE) %>%
      dplyr::mutate(rank = dplyr::row_number()) %>%
      dplyr::filter(rank <= feature_top_n) %>%
      dplyr::ungroup()
    row_assignments <- lca_assignment_table(original, used_row_ids, selected)
    relationship <- lca_relationship_table(original, used_row_ids, selected$predclass, relationship_col)

    model <- list(
      selected_cols = selected_cols,
      relationship_col = relationship_col,
      df_original = original,
      n_used = nrow(used),
      excluded_nrow = excluded_nrow,
      min_nclass = min_nclass,
      max_nclass = max_nclass,
      used_unconverged_fallback = used_unconverged_fallback,
      nrep = nrep,
      maxiter = maxiter,
      feature_top_n = feature_top_n,
      selected_fit = selected,
      candidates = candidates,
      class_selection = class_selection,
      profiles = profiles,
      characteristics = characteristics,
      discrimination = discrimination,
      row_assignments = row_assignments,
      relationship = relationship,
      sampled_nrow = sampled_nrow,
      grouped_cols = grouped_cols
    )
    class(model) <- c("lca_exploratory", class(model))
    model
  }
  do_on_each_group(df, each_func, name = "model", with_unnest = FALSE)
}

lca_quote_name <- function(name) {
  paste0("`", gsub("`", "\\\\`", name, fixed = TRUE), "`")
}

# tam#38381 follow-up: the category order an indicator is presented in.
#
# A FACTOR carries a declared level order, and for an ordered factor that order is the
# whole point of the type -- 6ヶ月未満 < 1年未満 < 1年 - 3年 < 3年以上 is meaningful in a
# way its alphabetical order is not. Sorting the labels as text throws that away and the
# report then lists categories in an order the user never chose.
#
# Only levels actually present are kept: poLCA needs every category to have observations,
# and a declared-but-unused level would create an empty category. Any value somehow absent
# from the declared levels is appended in text order rather than dropped.
#
# Character and logical indicators have no declared order, so they keep the text sort
# (which is also the natural one for logical: FALSE, TRUE).
#
# SHARED so the encoding and the report agree by construction. These were two separate
# copies of the same sort, which is exactly how the two drift apart.
lca_indicator_levels <- function(x) {
  present <- unique(as.character(x[!is.na(x)]))
  if (is.factor(x)) {
    declared <- levels(x)
    kept <- declared[declared %in% present]
    return(c(kept, sort(setdiff(present, kept), method = "radix")))
  }
  sort(present, method = "radix")
}

lca_encode_indicators <- function(df, cols) {
  out <- lapply(cols, function(col) {
    x <- df[[col]]
    levels <- lca_indicator_levels(x)
    as.integer(match(as.character(x), levels))
  })
  out <- as.data.frame(out, check.names = FALSE)
  names(out) <- cols
  out
}

lca_normalize_class_order <- function(fit) {
  # poLCA class IDs depend on random starting values.  Display classes in a
  # stable, interpretable order: largest estimated class first, then original ID.
  order_idx <- order(-fit$P, seq_along(fit$P))
  fit$P <- fit$P[order_idx]
  fit$posterior <- fit$posterior[, order_idx, drop = FALSE]
  fit$probs <- lapply(fit$probs, function(x) x[order_idx, , drop = FALSE])
  remap <- integer(length(order_idx))
  remap[order_idx] <- seq_along(order_idx)
  fit$predclass <- remap[fit$predclass]
  list(fit = fit, order = order_idx)
}

lca_class_selection_table <- function(candidates) {
  dplyr::bind_rows(lapply(candidates, function(candidate) {
    if (is.null(candidate$fit)) {
      return(tibble::tibble(
        number_of_classes = candidate$nclass, log_likelihood = NA_real_, aic = NA_real_, bic = NA_real_,
        entropy = NA_real_,
        minimum_class_share = NA_real_, mean_maximum_membership_probability = NA_real_,
        pct_low_confidence = NA_real_,
        random_starts = NA_integer_, best_solution_reproductions = NA_integer_,
        converged = FALSE, error = candidate$error
      ))
    }
    fit <- candidate$fit
    max_posterior <- apply(fit$posterior, 1, max)
    converged <- lca_fit_converged(fit)
    tibble::tibble(
      number_of_classes = candidate$nclass,
      log_likelihood = fit$llik,
      aic = fit$aic,
      bic = fit$bic,
      entropy = lca_entropy(fit),
      minimum_class_share = min(fit$P),
      mean_maximum_membership_probability = mean(max_posterior),
      pct_low_confidence = mean(max_posterior < 0.6),
      # tam#38380: the number of random starts this candidate actually used (it escalates
      # on its own when the best solution is not reproduced) and how many of those starts
      # reached it. Read together: 2 of 50 is a much weaker result than 2 of 2.
      random_starts = if (is.null(candidate$random_starts)) NA_integer_ else as.integer(candidate$random_starts),
      best_solution_reproductions = if (is.null(candidate$best_reproductions)) NA_integer_ else as.integer(candidate$best_reproductions),
      # poLCA's eflag records numerical/start errors, not iteration-limit
      # termination. A fit is known to have converged only when it stopped
      # before maxiter and did not encounter such an error.
      converged = converged,
      # tam#38352: a fit that returned successfully but did not converge is
      # not "no error" -- the Error column previously left it blank, reading
      # as if nothing was wrong. Report why the optimizer stopped instead.
      error = if (converged) NA_character_ else lca_stop_reason(fit)
    )
  }))
}

# tam#38380: how many of poLCA's random starts landed on the BEST solution.
#
# poLCA runs `nrep` independent random starts internally and returns only the winner,
# but it also records every start's log-likelihood in fit$attempts. Local optima are the
# main practical hazard in LCA, so "the best solution was reached N of M times" is the
# diagnostic that tells a user whether to trust the reported model at all -- a best
# log-likelihood hit exactly once is a warning sign, not a result.
#
# Tolerance: EM runs that converge to the SAME optimum agree to many digits, while
# genuinely different optima are far apart (fractions of a log-likelihood unit at least).
# A relative tolerance keeps that separation at any data scale -- an absolute epsilon that
# works for llik = -700 is far too strict at llik = -70000.
LCA_REPRODUCTION_RELATIVE_TOLERANCE <- 1e-6

lca_best_reproduction_count <- function(fit) {
  attempts <- fit$attempts
  if (is.null(attempts) || !length(attempts)) return(NA_integer_)
  attempts <- attempts[is.finite(attempts)]
  if (!length(attempts)) return(NA_integer_)
  best <- max(attempts)
  tol <- LCA_REPRODUCTION_RELATIVE_TOLERANCE * max(1, abs(best))
  sum(abs(attempts - best) <= tol)
}

# tam#38380: adaptive random starts. Rather than making everyone pay for 100 starts on
# every candidate, start at the configured nrep and escalate only when the best solution
# was not reproduced enough times to trust it. Escalation is per class count, because each
# candidate has its own optimization landscape -- a 2-class model can be trivially stable
# while a 6-class one is not.
#
# The escalated run REPLACES the previous one rather than pooling with it: pooling would
# report a reproduction count drawn from a different number of starts than the one shown
# next to it, which is exactly the ratio the user is being asked to judge.
#
# The 1-class baseline is exempt. poLCA solves it directly with no EM loop (numiter = 1),
# so every start is identical and escalating would burn time to re-derive the same number.
LCA_ADAPTIVE_START_SCHEDULE <- c(50L, 100L)
LCA_MIN_BEST_REPRODUCTIONS <- 2L

lca_fit_adaptive <- function(formula, used, k, nrep, maxiter, seed) {
  schedule <- as.integer(nrep)
  if (k > 1L) {
    schedule <- c(schedule, LCA_ADAPTIVE_START_SCHEDULE[LCA_ADAPTIVE_START_SCHEDULE > nrep])
  }
  last <- NULL
  for (i in seq_along(schedule)) {
    starts <- schedule[[i]]
    if (!is.null(seed)) set.seed(seed + k)
    fit <- tryCatch(
      poLCA::poLCA(formula, data = used, nclass = k, nrep = starts, maxiter = maxiter,
                   verbose = FALSE, calc.se = FALSE),
      error = function(e) e
    )
    if (inherits(fit, "error")) {
      # Escalation is a best-effort reliability check. If an earlier schedule
      # entry produced a usable fit, do not discard it just because an
      # optional larger run failed.
      if (!is.null(last)) return(last)
      return(list(fit = fit, random_starts = starts, best_reproductions = NA_integer_))
    }
    reproductions <- lca_best_reproduction_count(fit)
    last <- list(fit = fit, random_starts = starts, best_reproductions = reproductions)
    if (is.na(reproductions) || reproductions >= LCA_MIN_BEST_REPRODUCTIONS) {
      return(last)
    }
  }
  last
}

# tam#38383: normalized entropy (the "entropy R-squared" / relative entropy
# criterion, Ramaswamy et al. 1993) -- how cleanly the posterior assigns rows
# to classes, on 0..1 where 1 means every row belongs to exactly one class.
#
#   E = 1 - ( -sum_i sum_k p_ik * log(p_ik) ) / (n * log(K))
#
# It is a SEPARATION measure, not a fit criterion: it says nothing about
# whether K classes are warranted, so it must never be used on its own to pick
# the class count (the report's own explanation says the same).
#
# Two guards on the arithmetic:
#  - 0 * log(0) is 0 in the limit but NaN in floating point, so the zero
#    posteriors are dropped rather than multiplied.
#  - log(K) is 0 for the 1-class baseline, which would divide by zero. Entropy
#    is undefined for a single class (there is nothing to separate), so that
#    row reports NA rather than a garbage value or a spurious 1.
lca_entropy <- function(fit) {
  posterior <- fit$posterior
  if (is.null(posterior) || length(posterior) == 0) return(NA_real_)
  posterior <- as.matrix(posterior)
  nclass <- ncol(posterior)
  nobs <- nrow(posterior)
  if (is.na(nclass) || nclass < 2L || nobs < 1L) return(NA_real_)
  p <- posterior[is.finite(posterior) & posterior > 0]
  if (length(p) == 0) return(NA_real_)
  1 - (-sum(p * log(p))) / (nobs * log(nclass))
}

lca_fit_converged <- function(fit) {
  # poLCA computes the one-class baseline directly without an EM loop and
  # reports numiter = 1. It is therefore converged even when maxiter = 1.
  if (length(fit$P) == 1L) {
    return(!isTRUE(fit$eflag))
  }
  !isTRUE(fit$eflag) &&
    !is.null(fit$numiter) &&
    !is.null(fit$maxiter) &&
    isTRUE(fit$numiter < fit$maxiter)
}

# tam#38352: names the reason a successful (non-erroring) fit still failed
# lca_fit_converged(), for the class_selection table's Error column. Mirrors
# lca_fit_converged()'s own two conditions in the same order.
lca_stop_reason <- function(fit) {
  if (isTRUE(fit$eflag)) {
    "Numerical issue during estimation"
  } else if (!is.null(fit$numiter) && !is.null(fit$maxiter) && isTRUE(fit$numiter >= fit$maxiter)) {
    "Reached maximum iterations"
  } else {
    "Did not converge"
  }
}

lca_profile_table <- function(fit, observed, cols) {
  dplyr::bind_rows(lapply(cols, function(col) {
    levels <- lca_indicator_levels(observed[[col]])
    probabilities <- fit$probs[[col]]
    tibble::tibble(
      variable = col,
      category = factor(rep(levels, each = nrow(probabilities)), levels = levels),
      class = rep(seq_len(nrow(probabilities)), times = length(levels)),
      probability = as.vector(probabilities),
      overall_probability = rep(prop.table(table(factor(as.character(observed[[col]]), levels = levels))), each = nrow(probabilities))
    )
  })) %>%
    dplyr::mutate(
      difference = probability - overall_probability,
      abs_difference = abs(difference),
      class = factor(paste("Class", class), levels = paste("Class", seq_along(fit$P)))
    )
}

lca_assignment_table <- function(original, used_row_ids, fit) {
  class_probabilities <- tibble::as_tibble(fit$posterior, .name_repair = "minimal")
  names(class_probabilities) <- paste("Class", seq_len(ncol(fit$posterior)), "Probability")
  assigned <- dplyr::bind_cols(
    tibble::tibble(
      .lca_row_id = used_row_ids,
      `Latent Class` = factor(paste("Class", fit$predclass), levels = paste("Class", seq_along(fit$P))),
      `Assignment Confidence` = apply(fit$posterior, 1, max)
    ),
    class_probabilities,
    tibble::tibble(`Is Excluded` = FALSE)
  )
  original %>%
    dplyr::left_join(assigned, by = ".lca_row_id") %>%
    dplyr::mutate(`Is Excluded` = dplyr::coalesce(`Is Excluded`, TRUE)) %>%
    dplyr::select(-.lca_row_id)
}

lca_relationship_table <- function(original, used_row_ids, predclass, relationship_col) {
  if (is.null(relationship_col)) {
    return(tibble::tibble(relationship = character(), class = character(), rows = integer()))
  }
  memberships <- tibble::tibble(.lca_row_id = used_row_ids, class = paste("Class", predclass))
  values <- original %>%
    dplyr::select(.lca_row_id, relationship = dplyr::all_of(relationship_col)) %>%
    dplyr::inner_join(memberships, by = ".lca_row_id") %>%
    dplyr::filter(!is.na(relationship)) %>%
    dplyr::mutate(relationship = as.character(relationship))
  if (!nrow(values)) return(tibble::tibble(relationship = character(), class = character(), rows = integer()))
  tidyr::complete(values %>% dplyr::count(relationship, class, name = "rows"),
                  relationship, class, fill = list(rows = 0L))
}

#' Tidy a latent class analysis model for analytics visualizations.
#' @export
tidy.lca_exploratory <- function(x, type = "summary", ...) {
  fit <- x$selected_fit
  if (type == "analysis_conditions") {
    # tam#38352: a 1-class baseline is always fit internally in addition to
    # the user-configured explore range (which starts at 2 in the UI) -- see
    # exp_lca()'s candidate_counts. Report that explicitly rather than
    # letting "Class Counts Compared" under-describe what class_selection
    # actually shows a row for.
    class_counts <- vapply(x$candidates, function(candidate) candidate$nclass, integer(1))
    class_counts_text <- if (length(class_counts) == 1L) {
      as.character(class_counts)
    } else if (class_counts[[1]] == 1L) {
      paste0("1 (baseline), ", min(class_counts[-1L]), " to ", max(class_counts[-1L]))
    } else {
      paste0(min(class_counts), " to ", max(class_counts))
    }
    return(tibble::tibble(
      Metric = c("Number of Variables", "Variable Names", "Rows Used", "Rows Removed",
                 "Class Counts Compared", "Random Starts", "Maximum Iterations", "Selected Number of Classes"),
      Value = c(length(x$selected_cols), paste(x$selected_cols, collapse = ", "), x$n_used, x$excluded_nrow,
                class_counts_text, x$nrep, x$maxiter, length(fit$P))
    ))
  }
  if (type == "class_selection") return(x$class_selection)
  if (type == "summary") {
    max_posterior <- apply(fit$posterior, 1, max)
    return(tibble::tibble(
      class = factor(paste("Class", seq_along(fit$P)), levels = paste("Class", seq_along(fit$P))),
      share = fit$P,
      rows = as.integer(tabulate(fit$predclass, nbins = length(fit$P))),
      mean_membership_probability = vapply(seq_along(fit$P), function(k) mean(fit$posterior[fit$predclass == k, k]), numeric(1)),
      model_bic = fit$bic,
      mean_maximum_membership_probability = mean(max_posterior),
      pct_low_confidence = mean(max_posterior < 0.6)
    ))
  }
  if (type == "profiles") return(x$profiles)
  if (type == "characteristics") return(x$characteristics)
  if (type == "discrimination") return(x$discrimination)
  if (type == "assignment_confidence") {
    return(x$row_assignments %>%
      dplyr::filter(!`Is Excluded`) %>%
      dplyr::mutate(confidence_band = cut(`Assignment Confidence`, breaks = c(-Inf, .6, .8, .9, Inf),
                                          labels = c("Below 60%", "60% to under 80%", "80% to under 90%", "90% or higher"), right = FALSE)) %>%
      dplyr::count(confidence_band, name = "rows"))
  }
  if (type == "relationship") return(x$relationship)
  if (type == "data") return(x$row_assignments)
  stop(paste0("Unknown tidy type for Latent Class Analysis: ", type))
}

#' One-row overview of a latent class analysis model.
#' @export
glance.lca_exploratory <- function(x, ...) {
  tibble::tibble(
    selected_classes = length(x$selected_fit$P),
    bic = x$selected_fit$bic,
    aic = x$selected_fit$aic,
    log_likelihood = x$selected_fit$llik,
    n_used = x$n_used,
    excluded_nrow = x$excluded_nrow
  )
}
