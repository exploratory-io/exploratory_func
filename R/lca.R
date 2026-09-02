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
    candidate_counts <- seq.int(min_nclass, max_candidate_nclass)

    formula <- stats::as.formula(paste0("cbind(", paste(vapply(names(used), lca_quote_name, character(1)), collapse = ", "), ") ~ 1"))
    candidates <- lapply(candidate_counts, function(k) {
      if (!is.null(seed)) set.seed(seed + k)
      fit <- tryCatch(
        poLCA::poLCA(formula, data = used, nclass = k, nrep = nrep, maxiter = maxiter,
                     verbose = FALSE, calc.se = FALSE),
        error = function(e) e
      )
      if (inherits(fit, "error")) {
        return(list(nclass = k, fit = NULL, error = conditionMessage(fit)))
      }
      list(nclass = k, fit = fit, error = NULL)
    })
    successful <- Filter(function(x) !is.null(x$fit), candidates)
    if (!length(successful)) {
      errors <- vapply(candidates, function(x) paste0(x$nclass, ": ", x$error), character(1))
      stop(paste0("Latent Class Analysis could not fit any requested class count. ", paste(errors, collapse = " | ")))
    }
    best_index <- which.min(vapply(successful, function(x) x$fit$bic, numeric(1)))
    selected <- successful[[best_index]]$fit
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

lca_encode_indicators <- function(df, cols) {
  out <- lapply(cols, function(col) {
    x <- df[[col]]
    levels <- sort(unique(as.character(x[!is.na(x)])), method = "radix")
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
        minimum_class_share = NA_real_, mean_maximum_membership_probability = NA_real_,
        pct_low_confidence = NA_real_, converged = FALSE, error = candidate$error
      ))
    }
    fit <- candidate$fit
    max_posterior <- apply(fit$posterior, 1, max)
    tibble::tibble(
      number_of_classes = candidate$nclass,
      log_likelihood = fit$llik,
      aic = fit$aic,
      bic = fit$bic,
      minimum_class_share = min(fit$P),
      mean_maximum_membership_probability = mean(max_posterior),
      pct_low_confidence = mean(max_posterior < 0.6),
      # poLCA's eflag records numerical/start errors, not iteration-limit
      # termination. A fit is known to have converged only when it stopped
      # before maxiter and did not encounter such an error.
      converged = lca_fit_converged(fit),
      error = NA_character_
    )
  }))
}

lca_fit_converged <- function(fit) {
  !isTRUE(fit$eflag) &&
    !is.null(fit$numiter) &&
    !is.null(fit$maxiter) &&
    isTRUE(fit$numiter < fit$maxiter)
}

lca_profile_table <- function(fit, observed, cols) {
  dplyr::bind_rows(lapply(cols, function(col) {
    levels <- sort(unique(as.character(observed[[col]])), method = "radix")
    probabilities <- fit$probs[[col]]
    tibble::tibble(
      variable = col,
      category = rep(levels, each = nrow(probabilities)),
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
    return(tibble::tibble(
      Metric = c("Number of Variables", "Variable Names", "Rows Used", "Rows Removed",
                 "Class Counts Compared", "Random Starts", "Maximum Iterations", "Selected Number of Classes"),
      Value = c(length(x$selected_cols), paste(x$selected_cols, collapse = ", "), x$n_used, x$excluded_nrow,
                paste0(x$min_nclass, " to ", x$max_nclass), x$nrep, x$maxiter, length(fit$P))
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
