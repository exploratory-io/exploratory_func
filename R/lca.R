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
#' @export
exp_lca <- function(df, ...,
                    min_nclass = 2,
                    max_nclass = 6,
                    nrep = 20,
                    maxiter = 5000,
                    seed = 1,
                    relationship_column = NULL,
                    feature_top_n = 10) {
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
    class_distribution <- lca_class_distribution_table(original, selected_cols, used_row_ids,
                                                       selected$predclass, length(selected$P))
    variable_discrimination <- calculate_lca_variable_discrimination(profiles)
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
      variable_discrimination = variable_discrimination,
      class_distribution = class_distribution,
      row_assignments = row_assignments,
      relationship = relationship,
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
        reproduction_rate = NA_real_, solution_stability = NA_character_,
        converged = FALSE, error = candidate$error
      ))
    }
    fit <- candidate$fit
    max_posterior <- apply(fit$posterior, 1, max)
    converged <- lca_fit_converged(fit)
    # tam#38417: with a single class every row belongs to it with probability 1 by
    # construction, so "mean maximum membership probability = 100%" and "0% below 60%
    # confidence" are not measurements of anything -- they are restatements of the class
    # count. Reported blank so the column reads as a comparison across the candidates
    # that actually have something to separate. minimum_class_share is left at its 100%:
    # that one IS the size of the only class, which is a fact about the model.
    is_baseline <- candidate$nclass <= 1L
    random_starts <- if (is.null(candidate$random_starts)) NA_integer_ else as.integer(candidate$random_starts)
    best_reproductions <- if (is.null(candidate$best_reproductions)) NA_integer_ else as.integer(candidate$best_reproductions)
    tibble::tibble(
      number_of_classes = candidate$nclass,
      log_likelihood = fit$llik,
      aic = fit$aic,
      bic = fit$bic,
      entropy = lca_entropy(fit),
      minimum_class_share = min(fit$P),
      mean_maximum_membership_probability = if (is_baseline) NA_real_ else mean(max_posterior),
      pct_low_confidence = if (is_baseline) NA_real_ else mean(max_posterior < 0.6),
      # tam#38380: the number of random starts this candidate actually used (it escalates
      # on its own when the best solution is not reproduced) and how many of those starts
      # reached it. Read together: 2 of 50 is a much weaker result than 2 of 2.
      random_starts = random_starts,
      best_solution_reproductions = best_reproductions,
      # tam#38417: how OFTEN the best solution was reached, as a share of the starts that
      # were actually run. The raw reproduction count alone is not comparable across rows
      # once the adaptive schedule escalates a candidate to 50 or 100 starts -- 2 of 2 and
      # 2 of 100 are very different results printed as the same number.
      reproduction_rate = lca_reproduction_rate(best_reproductions, random_starts),
      solution_stability = lca_solution_stability(best_reproductions, random_starts),
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
# The 1-class baseline is exempt, and since tam#38417 does not run random starts at all:
# poLCA solves it directly with no EM loop (numiter = 1), so every start is identical.
LCA_ADAPTIVE_START_SCHEDULE <- c(50L, 100L)
LCA_MIN_BEST_REPRODUCTIONS <- 2L

lca_fit_adaptive <- function(formula, used, k, nrep, maxiter, seed) {
  # tam#38417: the 1-class baseline does not search a class assignment at all, so there
  # are no local optima for extra starts to escape -- poLCA solves it directly. Run it
  # ONCE and report no random-start count, rather than paying for nrep identical fits and
  # then printing a number that reads as a reliability diagnostic beside the multi-class
  # rows, where it means something entirely different.
  if (k <= 1L) {
    if (!is.null(seed)) set.seed(seed + k)
    fit <- tryCatch(
      poLCA::poLCA(formula, data = used, nclass = k, nrep = 1L, maxiter = maxiter,
                   verbose = FALSE, calc.se = FALSE),
      error = function(e) e
    )
    return(list(fit = fit, random_starts = NA_integer_, best_reproductions = NA_integer_))
  }
  schedule <- as.integer(nrep)
  schedule <- c(schedule, LCA_ADAPTIVE_START_SCHEDULE[LCA_ADAPTIVE_START_SCHEDULE > nrep])
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

# tam#38417: reproduction rate and the stability verdict derived from it.
#
# The rule table comes from the issue and is deliberately a JOINT condition on the count
# and the rate, not either alone:
#
#   Stable    best solution reached 5+ times AND rate >= 10%
#   Caution   reached 2-4 times, OR 5+ times but with rate < 10%
#   Unstable  reached at most once, even after the schedule escalated to 100 starts
#
# The count alone would call "2 of 2" and "2 of 100" the same; the rate alone would call
# "1 of 1" (100%) the most stable result in the table when it is the least informative.
LCA_STABLE_MIN_REPRODUCTIONS <- 5L
LCA_STABLE_MIN_RATE <- 0.10

lca_reproduction_rate <- function(reproductions, starts) {
  if (is.null(reproductions) || is.null(starts)) return(NA_real_)
  if (is.na(reproductions) || is.na(starts) || starts <= 0L) return(NA_real_)
  as.numeric(reproductions) / as.numeric(starts)
}

lca_solution_stability <- function(reproductions, starts) {
  rate <- lca_reproduction_rate(reproductions, starts)
  if (is.na(rate) || is.na(reproductions)) return(NA_character_)
  if (reproductions <= 1L) return("Unstable")
  if (reproductions >= LCA_STABLE_MIN_REPRODUCTIONS && rate >= LCA_STABLE_MIN_RATE) return("Stable")
  "Caution"
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
  # tam#38417: the report compares a class-conditional probability against an overall one,
  # so both sides have to come from the same model or the difference is not a quantity the
  # model ever asserted. The observed share (prop.table over the complete cases) answers a
  # different question -- what the sample did -- and mixing it with P(Y=r|C=c) produced
  # differences that do not reconcile with the fitted probabilities at all.
  #
  #   model_overall_probability: P(Y=r) = sum_c P(C=c) * P(Y=r|C=c)
  #
  # overall_probability (the observed share) is retained so nothing downstream that reads
  # it silently changes shape, and so the two can be compared when diagnosing model fit.
  class_shares <- fit$P
  dplyr::bind_rows(lapply(cols, function(col) {
    levels <- lca_indicator_levels(observed[[col]])
    probabilities <- fit$probs[[col]]
    # probabilities is class x category; the model-implied marginal is the class-share
    # weighted column mean, recycled back across the classes for the long layout.
    model_overall <- as.vector(class_shares %*% probabilities)
    tibble::tibble(
      variable = col,
      category = factor(rep(levels, each = nrow(probabilities)), levels = levels),
      class = rep(seq_len(nrow(probabilities)), times = length(levels)),
      probability = as.vector(probabilities),
      overall_probability = rep(prop.table(table(factor(as.character(observed[[col]]), levels = levels))), each = nrow(probabilities)),
      model_overall_probability = rep(model_overall, each = nrow(probabilities))
    )
  })) %>%
    dplyr::mutate(
      difference = probability - model_overall_probability,
      observed_difference = probability - overall_probability,
      abs_difference = abs(difference),
      class = factor(paste("Class", class), levels = paste("Class", seq_along(fit$P)))
    )
}

#' Discrimination power of each indicator variable in a latent class model.
#'
#' Scores how differently each variable's response distribution behaves ACROSS the latent
#' classes, on 0..1, as the mean Total Variation Distance over every class pair:
#'
#'   D_j(c1, c2) = 0.5 * sum_r | P(Y_j = r | C = c1) - P(Y_j = r | C = c2) |
#'   D_j         = mean over all c1 < c2
#'
#' The measure is deliberately NOT a significance test, a predictive importance, or a
#' selection criterion -- it is a distance between the model's own estimated conditional
#' response probabilities. Consequences worth stating, because each one is a property the
#' report relies on (tam#38418):
#'
#'  - Range is 0..1 regardless of how many categories the variable has, so a 2-category
#'    variable and a 10-category one are directly comparable. (A raw sum of absolute
#'    differences would not be: it grows with the number of categories.)
#'  - Invariant to class LABELS and to class ORDER, because every unordered pair is
#'    visited exactly once and the distance is symmetric.
#'  - Invariant to category ORDER, because the sum runs over the whole category set.
#'  - Class pairs are averaged with EQUAL weight; class sizes are deliberately not used.
#'    The question is how different the response patterns are, not how many rows sit in
#'    each pattern -- that is what the class-size columns elsewhere in the report answer.
#'
#' @param probabilities Long data frame of class-conditional response probabilities.
#' @param variable_col,category_col,class_col,probability_col Column names.
#' @param tolerance Allowed deviation from 1 when checking each variable x class sum.
#' @export
calculate_lca_variable_discrimination <- function(probabilities,
                                                  variable_col = "variable",
                                                  category_col = "category",
                                                  class_col = "class",
                                                  probability_col = "probability",
                                                  tolerance = 1e-6) {
  df <- as.data.frame(probabilities, stringsAsFactors = FALSE)
  required <- c(variable_col, category_col, class_col, probability_col)
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols)) {
    stop(paste0("Variable discrimination input is missing column(s): ", paste(missing_cols, collapse = ", "), "."))
  }
  values <- df[[probability_col]]
  if (!is.numeric(values) || any(!is.finite(values))) {
    stop("Conditional response probabilities must be finite numbers. NA, NaN and Inf are not allowed.")
  }
  # Zero is legitimate -- a class can genuinely never give an answer -- so only values
  # outside [0, 1] are rejected.
  if (any(values < 0 | values > 1)) {
    stop("Conditional response probabilities must be between 0 and 1.")
  }

  variables <- unique(as.character(df[[variable_col]]))
  categories <- as.character(df[[category_col]])
  classes <- as.character(df[[class_col]])

  pair_rows <- list()
  scores <- vector("list", length(variables))
  for (i in seq_along(variables)) {
    variable_name <- variables[[i]]
    keep <- as.character(df[[variable_col]]) == variable_name
    v_categories <- categories[keep]
    v_classes <- classes[keep]
    v_values <- values[keep]

    class_levels <- unique(v_classes)
    # Every class must carry the SAME category set. Filling a missing category with 0
    # would silently turn a data-shaping bug (a category/class misalignment upstream --
    # the exact failure this check exists to catch) into a plausible-looking score.
    per_class <- lapply(class_levels, function(cl) {
      idx <- v_classes == cl
      stats::setNames(v_values[idx], v_categories[idx])
    })
    category_sets <- lapply(per_class, function(x) sort(names(x)))
    if (length(category_sets) > 1 && !all(vapply(category_sets[-1], function(x) identical(x, category_sets[[1]]), logical(1)))) {
      stop(paste0("Category mismatch detected for variable: ", variable_name))
    }
    if (any(vapply(per_class, function(x) any(duplicated(names(x))), logical(1)))) {
      stop(paste0("Duplicated categories detected for variable: ", variable_name))
    }
    bad <- vapply(per_class, function(x) abs(sum(x) - 1) > tolerance, logical(1))
    if (any(bad)) {
      stop(paste0("Conditional response probabilities do not sum to 1 for variable: ", variable_name,
                  ", class: ", paste(class_levels[bad], collapse = ", "), "."))
    }

    if (length(class_levels) < 2L) {
      # A single class has nothing to be distinguished FROM. NA, never 0: 0 already means
      # "several classes whose response distributions are identical", which is a real and
      # very different finding.
      scores[[i]] <- tibble::tibble(
        variable = variable_name, discrimination_score = NA_real_,
        max_pairwise_score = NA_real_, min_pairwise_score = NA_real_,
        number_of_class_pairs = 0L
      )
      next
    }

    pairs <- utils::combn(seq_along(class_levels), 2, simplify = FALSE)
    pair_scores <- vapply(pairs, function(pair) {
      p1 <- per_class[[pair[[1]]]]
      p2 <- per_class[[pair[[2]]]][names(p1)]
      0.5 * sum(abs(p1 - p2))
    }, numeric(1))
    pair_rows[[length(pair_rows) + 1L]] <- tibble::tibble(
      variable = variable_name,
      class_1 = class_levels[vapply(pairs, function(pair) pair[[1]], integer(1))],
      class_2 = class_levels[vapply(pairs, function(pair) pair[[2]], integer(1))],
      pairwise_discrimination = pair_scores
    )
    scores[[i]] <- tibble::tibble(
      variable = variable_name,
      discrimination_score = mean(pair_scores),
      max_pairwise_score = max(pair_scores),
      min_pairwise_score = min(pair_scores),
      number_of_class_pairs = length(pair_scores)
    )
  }

  result <- dplyr::bind_rows(scores)
  # Ties keep the original variable-selection order so the chart's bar order is stable
  # across runs rather than depending on whatever order the sort happened to produce.
  result$.selection_order <- seq_len(nrow(result))
  result <- result[order(-result$discrimination_score, result$.selection_order), , drop = FALSE]
  # Rank is computed AFTER the sort, so tied scores share a rank and the row order the
  # chart draws is the row order the ranks describe.
  result$rank <- rank(-result$discrimination_score, na.last = "keep", ties.method = "min")
  result$.selection_order <- NULL
  result <- tibble::as_tibble(result)
  attr(result, "pairwise") <- if (length(pair_rows)) dplyr::bind_rows(pair_rows) else
    tibble::tibble(variable = character(), class_1 = character(), class_2 = character(),
                   pairwise_discrimination = numeric())
  result
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

# tam#38419: how the assigned classes are distributed WITHIN each answer category, for
# every indicator variable at once. The report renders it as a 100% stacked bar faceted by
# variable, so what matters here is the count per (variable, category, class) cell -- the
# ratio is a chart-side window function, not something to precompute (see the spec loop's
# "Ratio + Total: window function, never a static R column" rule).
#
# Rows EXCLUDED from the estimation are kept, with class NA. They are a real part of the
# picture: an answer category answered only by rows that were dropped for missingness
# elsewhere shows up as a full-height (NA) band rather than silently disappearing, which
# is what the spec's reference chart shows.
#
# tidyr::complete() fills unobserved (category, class) combinations with 0 so a missing
# cell is drawn as absent rather than collapsing the stack.
lca_class_distribution_table <- function(original, cols, used_row_ids, predclass, nclass) {
  class_levels <- paste("Class", seq_len(nclass))
  assigned <- tibble::tibble(.lca_row_id = used_row_ids, class = paste("Class", predclass))
  base <- original %>%
    dplyr::select(dplyr::all_of(c(".lca_row_id", cols))) %>%
    dplyr::left_join(assigned, by = ".lca_row_id")
  dplyr::bind_rows(lapply(cols, function(col) {
    levels <- lca_indicator_levels(original[[col]])
    counted <- base %>%
      dplyr::transmute(
        category = factor(as.character(.data[[col]]), levels = levels),
        class = factor(class, levels = class_levels)
      ) %>%
      dplyr::count(category, class, name = "rows")
    tidyr::complete(counted, category, class, fill = list(rows = 0L)) %>%
      dplyr::mutate(variable = col) %>%
      dplyr::select(variable, category, class, rows)
  }))
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
    # tam#38417: the excluded-row count is only interpretable against the size of the
    # data it was taken out of -- 279 rows is a rounding error in a million and a quarter
    # of the sample in a thousand. The share is appended to that row only (the used-row
    # count is the denominator's other half and reads fine on its own).
    total_rows <- x$n_used + x$excluded_nrow
    excluded_text <- if (total_rows > 0) {
      paste0(x$excluded_nrow, " (", formatC(100 * x$excluded_nrow / total_rows, format = "f", digits = 1), "%)")
    } else {
      as.character(x$excluded_nrow)
    }
    return(tibble::tibble(
      Metric = c("Number of Variables", "Variable Names", "Rows Used", "Rows Removed",
                 "Class Counts Compared", "Random Starts", "Maximum Iterations", "Selected Number of Classes"),
      Value = c(length(x$selected_cols), paste(x$selected_cols, collapse = ", "), x$n_used, excluded_text,
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
  # tam#38418: per-VARIABLE discrimination (mean pairwise TVD). Distinct from the
  # "discrimination" type above, which is per variable AND category (max - min response
  # probability) and feeds the "Class-Characterizing Categories" chart.
  if (type == "variable_discrimination") return(x$variable_discrimination)
  if (type == "variable_discrimination_pairs") {
    pairs <- attr(x$variable_discrimination, "pairwise")
    return(if (is.null(pairs)) tibble::tibble(variable = character(), class_1 = character(),
                                              class_2 = character(),
                                              pairwise_discrimination = numeric()) else pairs)
  }
  if (type == "class_distribution") return(x$class_distribution)
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
