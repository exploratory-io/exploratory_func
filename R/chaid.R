#' Fit a classification CHAID tree.
#'
#' @param data A data frame containing the target and predictors.
#' @param target Name of the categorical target column.
#' @param predictors Optional character vector of predictor names.
#' @param weight Reserved for future weighted CHAID support.
#' @param alpha_split Significance threshold for selecting a split.
#' @param alpha_merge Significance threshold for merging categories.
#' @param max_depth Maximum tree depth.
#' @param min_split Minimum number of rows required to consider a split.
#' @param min_bucket Minimum number of rows allowed in a child node.
#' @param min_node_proportion Optional minimum child-node proportion.
#' @param numeric_binning Binning method for numeric predictors.
#' @param numeric_bins Number of numeric bins.
#' @param missing Missing-value handling method.
#' @param chi_square Chi-square statistic to use.
#' @param bonferroni Whether to apply Bonferroni correction.
#' @param ordinal_predictors Predictors to treat as ordered.
#' @param max_categories Maximum number of predictor categories.
#' @param verbose Whether to emit diagnostic messages.
#' @return An object with class `exploratory_chaid`.
#' @export
chaid_fit <- function(data,
                      target,
                      predictors = NULL,
                      weight = NULL,
                      alpha_split = 0.05,
                      alpha_merge = 0.05,
                      max_depth = 3,
                      min_split = 50,
                      min_bucket = 20,
                      min_node_proportion = NULL,
                      numeric_binning = c('quantile', 'equal_width', 'none'),
                      numeric_bins = 10,
                      missing = c('as_category', 'exclude'),
                      chi_square = c('pearson', 'likelihood_ratio'),
                      bonferroni = TRUE,
                      ordinal_predictors = NULL,
                      max_categories = 50,
                      verbose = FALSE) {
  validation <- validate_chaid_inputs(
    data = data,
    target = target,
    predictors = predictors,
    weight = weight,
    alpha_split = alpha_split,
    alpha_merge = alpha_merge,
    max_depth = max_depth,
    min_split = min_split,
    min_bucket = min_bucket,
    min_node_proportion = min_node_proportion,
    numeric_binning = numeric_binning,
    numeric_bins = numeric_bins,
    missing = missing,
    chi_square = chi_square,
    bonferroni = bonferroni,
    ordinal_predictors = ordinal_predictors,
    max_categories = max_categories,
    verbose = verbose
  )

  prepared <- prepare_chaid_data(
    data = data,
    target = target,
    predictors = validation$predictors,
    parameters = validation$parameters
  )
  target.factor <- prepared$data[[target]]
  class.levels <- levels(target.factor)
  class.counts <- tabulate(target.factor, nbins = length(class.levels))
  predicted.class <- class.levels[which.max(class.counts)]
  class.distribution <- if (sum(class.counts) == 0) {
    rep(0, length(class.levels))
  } else {
    class.counts / sum(class.counts)
  }

  nodes <- data.frame(
    node_id = 1L,
    parent_id = NA_integer_,
    depth = 0L,
    is_terminal = TRUE,
    n = nrow(prepared$data),
    weighted_n = nrow(prepared$data),
    predicted_class = predicted.class,
    split_variable = NA_character_,
    p_value = NA_real_,
    adjusted_p_value = NA_real_,
    split_statistic = NA_real_,
    split_df = NA_real_,
    rule = 'Root',
    stringsAsFactors = FALSE
  )
  nodes$class_distribution <- list(setNames(class.distribution, class.levels))

  model <- list(
    nodes = nodes,
    edges = data.frame(
      parent_id = integer(),
      child_id = integer(),
      split_variable = character(),
      label = character(),
      split_value = character(),
      original_categories = character(),
      stringsAsFactors = FALSE
    ),
    .node_metadata = list(`1` = list(split_groups = NULL)),
    rules = nodes[, c('node_id', 'rule', 'predicted_class', 'n')],
    category_merge_map = data.frame(
      node_id = integer(),
      variable = character(),
      merged_group = character(),
      original_categories = character(),
      merge_p_value = numeric(),
      stringsAsFactors = FALSE
    ),
    numeric_binning_map = prepared$numeric_binning_map,
    class_levels = class.levels,
    target = target,
    target_type = if (is.logical(data[[target]])) {
      'logical'
    } else if (is.factor(data[[target]])) {
      'factor'
    } else {
      'character'
    },
    predictors = prepared$predictors,
    parameters = validation$parameters,
    training_metadata = list(
      n_rows = nrow(prepared$data),
      original_n_rows = nrow(data)
    ),
    prepared_levels = prepared$prepared_levels,
    predictor_info = prepared$predictor_info
  )
  low.expected.split <- FALSE
  grew.tree <- FALSE
  if (length(prepared$predictors) > 0 && nrow(prepared$data) > 0 &&
      length(class.levels) > 1) {
    grew.tree <- TRUE
    tree <- grow_chaid_tree(
      data = prepared$data,
      target = target,
      predictors = prepared$predictors,
      parameters = validation$parameters,
      predictor_info = prepared$predictor_info,
      class_levels = class.levels
    )
    model$nodes <- tree$nodes
    model$edges <- tree$edges
    model$.node_metadata <- tree$node_metadata
    model$category_merge_map <- tree$category_merge_map
    model$rules <- tree$nodes[tree$nodes$is_terminal,
                              c('node_id', 'rule', 'predicted_class', 'n')]
    low.expected.split <- isTRUE(tree$low_expected)
  }
  class(model) <- c('exploratory_chaid', 'list')

  emit_chaid_warnings(
    numeric_binned = names(Filter(function(m) isTRUE(!m$excluded) && m$method != 'none',
                                  prepared$numeric_binning_map)),
    root_only = grew.tree && nrow(model$nodes) == 1L,
    low_expected = low.expected.split
  )
  model
}

#' Emit the diagnostic warnings called for by the CHAID spec.
#'
#' Emitted once per fit (not per node) so a large tree does not produce a
#' warning storm.
#'
#' @param numeric_binned Names of numeric predictors that were binned.
#' @param root_only TRUE when a splittable problem produced only the root node.
#' @param low_expected TRUE when an accepted split relied on a table with many
#'   small expected cell counts.
#' @return Invisibly NULL.
emit_chaid_warnings <- function(numeric_binned, root_only, low_expected) {
  if (length(numeric_binned) > 0) {
    warning(paste0(
      'Numeric predictor(s) ', paste(numeric_binned, collapse = ', '),
      ' were binned into categories; results depend on the binning method.'
    ), call. = FALSE)
  }
  if (isTRUE(root_only)) {
    warning('No significant split was found; the tree contains only the root node.',
            call. = FALSE)
  }
  if (isTRUE(low_expected)) {
    warning(paste0(
      'Some chi-square tests had low expected cell frequencies; ',
      'p-values may be unreliable.'
    ), call. = FALSE)
  }
  invisible(NULL)
}

#' Prepare target and predictors for CHAID fitting.
#'
#' @param data A data frame.
#' @param target Target column name.
#' @param predictors Predictor column names.
#' @param parameters Validated CHAID parameters.
#' @return A list containing prepared data and transformation metadata.
prepare_chaid_data <- function(data, target, predictors, parameters) {
  target.raw <- data[[target]]
  row.keep <- !is.na(target.raw)
  predictor.values <- list()
  predictor.info <- list()
  numeric.binning.map <- list()
  prepared.levels <- list()
  predictors.kept <- character()

  for (variable in predictors) {
    value <- data[[variable]]
    ordered.variable <- is.ordered(value) ||
      variable %in% parameters$ordinal_predictors

    if (is.numeric(value)) {
      if (parameters$numeric_binning == 'none') {
        numeric.binning.map[[variable]] <- list(
          method = 'none',
          breaks = numeric(),
          labels = character(),
          excluded = TRUE
        )
        next
      }
      binning <- create_numeric_bins(
        value = value,
        method = parameters$numeric_binning,
        n_bins = parameters$numeric_bins
      )
      transformed <- binning$values
      numeric.binning.map[[variable]] <- binning$metadata
      # Binned numerics are ordinal: bins have a natural order, so CHAID may only
      # merge ADJACENT bins. This keeps numeric split rules contiguous (a single
      # range chain) rather than lumping, say, the lowest and highest bins.
      ordered.variable <- TRUE
      ordered.levels <- binning$metadata$labels
      if (parameters$missing == 'as_category') {
        ordered.levels <- c(ordered.levels, 'Missing')
      }
      predictor.info[[variable]] <- list(
        ordered = TRUE,
        levels = ordered.levels
      )
    } else {
      transformed <- as.character(value)
      if (is.factor(value)) {
        category.levels <- levels(value)
      } else {
        category.levels <- unique(transformed[!is.na(transformed)])
      }
      if (parameters$missing == 'as_category' && anyNA(transformed)) {
        category.levels <- unique(c(category.levels, 'Missing'))
      }
      predictor.info[[variable]] <- list(
        ordered = ordered.variable,
        levels = category.levels
      )
    }

    if (parameters$missing == 'exclude') {
      row.keep <- row.keep & !is.na(transformed)
    } else {
      transformed[is.na(transformed)] <- 'Missing'
    }

    predictor.values[[variable]] <- transformed
    predictors.kept <- c(predictors.kept, variable)
    prepared.levels[[variable]] <- unique(c(
      predictor.info[[variable]]$levels %||% character(),
      transformed[!is.na(transformed)]
    ))
  }

  target.levels <- if (is.factor(target.raw)) {
    levels(target.raw)
  } else if (is.logical(target.raw)) {
    # Stable, deterministic order with the positive class (TRUE) first, so
    # class_levels[1] is the positive class for binary/logical targets.
    c('TRUE', 'FALSE')
  } else {
    unique(as.character(target.raw[!is.na(target.raw)]))
  }
  target.factor <- factor(as.character(target.raw[row.keep]), levels = target.levels)
  prepared.data <- data.frame(row.names = seq_len(sum(row.keep)))
  prepared.data[[target]] <- target.factor
  for (variable in predictors.kept) {
    prepared.data[[variable]] <- predictor.values[[variable]][row.keep]
    prepared.data[[variable]] <- as.character(prepared.data[[variable]])
  }

  list(
    data = prepared.data,
    predictors = predictors.kept,
    predictor_info = predictor.info[predictors.kept],
    numeric_binning_map = numeric.binning.map,
    prepared_levels = prepared.levels[predictors.kept]
  )
}

#' Format numeric break values compactly for bin labels.
#'
#' @param v A numeric value.
#' @return A compact string without scientific notation.
format_chaid_break <- function(v) {
  trimws(formatC(signif(v, 6), format = 'fg', digits = 15))
}

#' Build human-readable range labels for numeric bins.
#'
#' Given break points `c(-Inf, b1, ..., bk, Inf)` used with right-closed cuts,
#' produce interpretable labels such as `<= b1`, `(b1, b2]`, `> bk`, so rules
#' and tree edges read as ranges rather than opaque `Bin1`..`BinN` codes.
#'
#' @param breaks Numeric break points, ascending, first `-Inf` and last `Inf`.
#' @return A character vector of `length(breaks) - 1` labels, in break order.
format_chaid_bin_labels <- function(breaks) {
  n <- length(breaks) - 1L
  if (n <= 0) {
    return(character())
  }
  vapply(seq_len(n), function(i) {
    lo <- breaks[i]
    hi <- breaks[i + 1L]
    lo.inf <- is.infinite(lo) && lo < 0
    hi.inf <- is.infinite(hi) && hi > 0
    if (lo.inf && hi.inf) {
      'All values'
    } else if (lo.inf) {
      paste0('<= ', format_chaid_break(hi))
    } else if (hi.inf) {
      paste0('> ', format_chaid_break(lo))
    } else {
      paste0('(', format_chaid_break(lo), ', ', format_chaid_break(hi), ']')
    }
  }, character(1))
}

#' Create deterministic bins for a numeric predictor.
#'
#' @param value Numeric predictor values.
#' @param method Binning method.
#' @param n_bins Requested number of bins.
#' @return Binned values and saved binning metadata.
create_numeric_bins <- function(value, method, n_bins) {
  finite.value <- value[!is.na(value) & is.finite(value)]
  if (length(finite.value) == 0 || length(unique(finite.value)) <= 1) {
    breaks <- c(-Inf, Inf)
  } else if (method == 'quantile') {
    breaks <- unique(as.numeric(stats::quantile(
      finite.value,
      probs = seq(0, 1, length.out = n_bins + 1),
      na.rm = TRUE,
      names = FALSE
    )))
    breaks[1] <- -Inf
    breaks[length(breaks)] <- Inf
  } else {
    value.range <- range(finite.value)
    breaks <- unique(seq(value.range[1], value.range[2], length.out = n_bins + 1))
    breaks[1] <- -Inf
    breaks[length(breaks)] <- Inf
  }

  labels <- format_chaid_bin_labels(breaks)
  # Cut to bin indices, then map to labels. Indexing (rather than passing
  # labels= to cut) keeps this robust even if two labels round to the same
  # display string, which would otherwise be a duplicated-factor-level error.
  bin.index <- cut(
    value,
    breaks = breaks,
    labels = FALSE,
    include.lowest = TRUE,
    right = TRUE
  )
  binned <- labels[bin.index]
  list(
    values = binned,
    metadata = list(
      method = method,
      breaks = breaks,
      labels = labels,
      excluded = FALSE
    )
  )
}

#' Compute a chi-square association test from a contingency matrix.
#'
#' This is the hot-path entry point: category merging computes the
#' target-by-category table once per predictor per node, then evaluates every
#' pairwise and overall test as sub-matrices of it, avoiding a raw-data pass per
#' test. Pearson matches `stats::chisq.test(correct = FALSE)`.
#'
#' @param observed Integer matrix, rows = target classes, cols = categories.
#' @param method Pearson or likelihood-ratio chi-square.
#' @return A list with statistic, p-value, degrees of freedom, the (trimmed)
#'   observed and expected matrices, and `low_expected` (share of expected cells
#'   below 5).
compute_chisq_from_counts <- function(observed, method = 'pearson') {
  observed <- observed[rowSums(observed) > 0, colSums(observed) > 0, drop = FALSE]
  if (nrow(observed) < 2 || ncol(observed) < 2) {
    return(list(
      statistic = NA_real_,
      p_value = NA_real_,
      df = 0,
      observed = observed,
      expected = matrix(numeric(), nrow = 0, ncol = 0),
      low_expected = NA_real_
    ))
  }
  expected <- outer(rowSums(observed), colSums(observed)) / sum(observed)
  low.expected <- mean(expected < 5)
  if (method == 'likelihood_ratio') {
    positive <- observed > 0 & expected > 0
    statistic <- 2 * sum(observed[positive] *
      log(observed[positive] / expected[positive]))
  } else {
    statistic <- sum((observed - expected)^2 / expected)
  }
  df <- (nrow(observed) - 1) * (ncol(observed) - 1)
  p.value <- stats::pchisq(statistic, df = df, lower.tail = FALSE)
  list(
    statistic = as.numeric(statistic),
    p_value = as.numeric(p.value),
    df = as.numeric(df),
    observed = observed,
    expected = expected,
    low_expected = as.numeric(low.expected)
  )
}

#' Compute a chi-square association test for two categorical vectors.
#'
#' Thin wrapper over [compute_chisq_from_counts()] kept for the raw-vector
#' callers and for equivalence testing against the counts path.
#'
#' @param x Predictor categories.
#' @param y Target categories.
#' @param method Pearson or likelihood-ratio chi-square.
#' @return A list containing the test statistic, p-value, degrees of freedom,
#'   observed counts, expected counts, and low-expected share.
compute_chisq_test <- function(x, y, method = 'pearson') {
  valid <- !is.na(x) & !is.na(y)
  observed <- table(as.character(y[valid]), as.character(x[valid]), useNA = 'no')
  compute_chisq_from_counts(observed, method = method)
}

#' Apply Bonferroni correction to a vector of p-values.
#'
#' @param p_values Raw p-values.
#' @param bonferroni Whether to apply the correction.
#' @param n_tests Number of tests in the correction family.
#' @return Adjusted p-values.
adjust_p_value_bonferroni <- function(p_values, bonferroni = TRUE,
                                      n_tests = length(p_values)) {
  p_values[is.na(p_values)] <- 1
  if (!bonferroni || n_tests <= 1) {
    return(pmin(1, p_values))
  }
  pmin(1, p_values * n_tests)
}

#' Merge statistically similar predictor categories.
#'
#' @param values Predictor categories within one node.
#' @param target Target categories within one node.
#' @param ordered Whether only adjacent groups may be compared.
#' @param ordered_levels Ordered category levels.
#' @param alpha_merge Merge significance threshold.
#' @param bonferroni Whether to correct pairwise p-values.
#' @param variable Predictor name.
#' @param node_id Current node ID.
#' @param chi_square Chi-square statistic to use.
#' @return Category groups and merge/test metadata.
merge_categories <- function(values,
                             target,
                             ordered = FALSE,
                             ordered_levels = NULL,
                             alpha_merge = 0.05,
                             bonferroni = TRUE,
                             variable = '',
                             node_id = 1L,
                             chi_square = 'pearson') {
  values <- as.character(values)
  target <- as.character(target)
  valid <- !is.na(values) & !is.na(target)
  values <- values[valid]
  target <- target[valid]
  categories <- if (ordered && !is.null(ordered_levels)) {
    ordered_levels[ordered_levels %in% unique(values)]
  } else {
    unique(values)
  }

  # Build the target-by-category contingency table ONCE. Every pairwise merge
  # test and the overall test are then column extractions / sums of this matrix,
  # so no per-test pass over the raw vectors is needed (the performance fix).
  target.levels <- unique(target)
  contingency <- unclass(table(
    factor(target, levels = target.levels),
    factor(values, levels = categories)
  ))

  # Each group is a set of column indices into `categories`.
  groups <- as.list(seq_along(categories))
  merge.history <- list()

  col_of_group <- function(group.indices) {
    if (length(group.indices) == 1L) {
      contingency[, group.indices]
    } else {
      rowSums(contingency[, group.indices, drop = FALSE])
    }
  }

  repeat {
    if (length(groups) < 2) {
      break
    }
    candidates <- list()
    candidate.index <- 0L
    for (i in seq_len(length(groups) - 1L)) {
      for (j in seq.int(i + 1L, length(groups))) {
        if (ordered && j != i + 1L) {
          next
        }
        pair.observed <- cbind(col_of_group(groups[[i]]), col_of_group(groups[[j]]))
        test <- compute_chisq_from_counts(pair.observed, method = chi_square)
        candidate.index <- candidate.index + 1L
        candidates[[candidate.index]] <- list(
          i = i,
          j = j,
          p_value = test$p_value
        )
      }
    }
    if (length(candidates) == 0) {
      break
    }
    raw.p.values <- vapply(candidates, function(candidate) candidate$p_value, numeric(1))
    adjusted.p.values <- adjust_p_value_bonferroni(
      raw.p.values,
      bonferroni = bonferroni,
      n_tests = length(raw.p.values)
    )
    best <- which.max(adjusted.p.values)
    if (is.na(adjusted.p.values[best]) || adjusted.p.values[best] <= alpha_merge) {
      break
    }
    selected <- candidates[[best]]
    original.categories <- categories[c(groups[[selected$i]], groups[[selected$j]])]
    merge.history[[length(merge.history) + 1L]] <- list(
      node_id = node_id,
      variable = variable,
      merged_group = paste(original.categories, collapse = ' + '),
      original_categories = original.categories,
      merge_p_value = raw.p.values[best],
      adjusted_p_value = adjusted.p.values[best]
    )
    groups[[selected$i]] <- c(groups[[selected$i]], groups[[selected$j]])
    groups[[selected$j]] <- NULL
  }

  group.category.lists <- lapply(groups, function(group.indices) categories[group.indices])
  group.labels <- vapply(group.category.lists, paste, character(1), collapse = ' + ')
  overall.observed <- vapply(groups, col_of_group, numeric(length(target.levels)))
  overall.test <- compute_chisq_from_counts(overall.observed, method = chi_square)
  list(
    groups = group.category.lists,
    group_labels = group.labels,
    merge_history = merge.history,
    p_value = overall.test$p_value,
    adjusted_p_value = overall.test$p_value,
    test = overall.test,
    low_expected = isTRUE(overall.test$low_expected >= 0.2)
  )
}

#' Evaluate one predictor at a CHAID node.
#'
#' @param data Node data.
#' @param target Target column name.
#' @param variable Predictor name.
#' @param parameters Validated CHAID parameters.
#' @param predictor_info Predictor metadata.
#' @param node_id Current node ID.
#' @return Predictor evaluation or NULL when no split is possible.
evaluate_predictor <- function(data, target, variable, parameters,
                               predictor_info, node_id) {
  values <- as.character(data[[variable]])
  if (length(unique(values)) < 2) {
    return(NULL)
  }
  info <- predictor_info[[variable]] %||% list(ordered = FALSE, levels = unique(values))
  merge.result <- merge_categories(
    values = values,
    target = data[[target]],
    ordered = isTRUE(info$ordered),
    ordered_levels = info$levels,
    alpha_merge = parameters$alpha_merge,
    bonferroni = parameters$bonferroni,
    variable = variable,
    node_id = node_id,
    chi_square = parameters$chi_square
  )
  if (length(merge.result$groups) < 2 || is.na(merge.result$p_value)) {
    return(NULL)
  }
  merge.result$variable <- variable
  merge.result$values <- values
  merge.result
}

#' Grow a recursive multiway CHAID tree.
#'
#' @param data Prepared training data.
#' @param target Target column name.
#' @param predictors Prepared predictors.
#' @param parameters Validated CHAID parameters.
#' @param predictor_info Predictor metadata.
#' @param class_levels Target class levels.
#' @return Tree tables and traversal metadata.
grow_chaid_tree <- function(data, target, predictors, parameters,
                            predictor_info, class_levels) {
  # An environment (reference semantics) rather than a list: grow_node recurses
  # and mutates this shared state, and nested list-`<<-` updates are prone to
  # lost updates. With an environment, `state$field <- value` mutates in place.
  state <- new.env(parent = emptyenv())
  state$node_records <- list()
  state$edges <- list()
  state$node_metadata <- list()
  state$merge_history <- list()
  state$next_id <- 1L
  state$low_expected <- FALSE

  grow_node <- function(node.data, node.id, parent.id, depth, rule) {
    target.factor <- factor(as.character(node.data[[target]]), levels = class_levels)
    counts <- tabulate(target.factor, nbins = length(class_levels))
    distribution <- if (sum(counts) == 0) {
      rep(0, length(class_levels))
    } else {
      counts / sum(counts)
    }
    record <- list(
      node_id = as.integer(node.id),
      parent_id = if (is.null(parent.id)) NA_integer_ else as.integer(parent.id),
      depth = as.integer(depth),
      is_terminal = TRUE,
      n = nrow(node.data),
      weighted_n = nrow(node.data),
      predicted_class = class_levels[which.max(counts)],
      class_distribution = setNames(distribution, class_levels),
      split_variable = NA_character_,
      p_value = NA_real_,
      adjusted_p_value = NA_real_,
      split_statistic = NA_real_,
      split_df = NA_real_,
      rule = rule
    )
    state$node_records[[as.character(node.id)]] <- record
    state$node_metadata[[as.character(node.id)]] <- list(
      split_variable = NULL,
      split_groups = NULL,
      group_labels = NULL
    )

    pure <- sum(counts > 0) <= 1
    if (pure || depth >= parameters$max_depth ||
        nrow(node.data) < parameters$min_split || length(predictors) == 0) {
      return(invisible(NULL))
    }

    evaluations <- lapply(predictors, function(variable) {
      values <- as.character(node.data[[variable]])
      category.count <- length(unique(values[!is.na(values)]))
      if (category.count > parameters$max_categories) {
        warning(paste0(
          'Predictor ', variable, ' was skipped because it has more than ',
          parameters$max_categories, ' categories'
        ), call. = FALSE)
        return(NULL)
      }
      evaluate_predictor(
        data = node.data,
        target = target,
        variable = variable,
        parameters = parameters,
        predictor_info = predictor_info,
        node_id = node.id
      )
    })
    evaluations <- Filter(Negate(is.null), evaluations)
    if (length(evaluations) == 0) {
      return(invisible(NULL))
    }
    raw.p.values <- vapply(evaluations, function(evaluation) evaluation$p_value, numeric(1))
    adjusted.p.values <- adjust_p_value_bonferroni(
      raw.p.values,
      bonferroni = parameters$bonferroni,
      n_tests = length(raw.p.values)
    )
    best.index <- which.min(adjusted.p.values)
    best <- evaluations[[best.index]]
    best$adjusted_p_value <- adjusted.p.values[best.index]
    current.record <- state$node_records[[as.character(node.id)]]
    current.record$p_value <- best$p_value
    current.record$adjusted_p_value <- best$adjusted_p_value
    current.record$split_statistic <- best$test$statistic
    current.record$split_df <- best$test$df
    state$node_records[[as.character(node.id)]] <- current.record

    if (is.na(best$p_value) || best$adjusted_p_value > parameters$alpha_split) {
      return(invisible(NULL))
    }
    child.counts <- vapply(best$groups, function(group) {
      sum(best$values %in% group)
    }, integer(1))
    if (any(child.counts < parameters$min_bucket) ||
        (!is.null(parameters$min_node_proportion) &&
         any(child.counts / nrow(node.data) < parameters$min_node_proportion))) {
      return(invisible(NULL))
    }

    if (isTRUE(best$low_expected)) {
      state$low_expected <- TRUE
    }
    current.record$is_terminal <- FALSE
    current.record$split_variable <- best$variable
    state$node_records[[as.character(node.id)]] <- current.record
    state$node_metadata[[as.character(node.id)]] <- list(
      split_variable = best$variable,
      split_groups = best$groups,
      group_labels = best$group_labels
    )
    if (length(best$merge_history) > 0) {
      state$merge_history <- c(state$merge_history, best$merge_history)
    }

    for (i in seq_along(best$groups)) {
      state$next_id <- state$next_id + 1L
      child.id <- state$next_id
      group <- best$groups[[i]]
      child.data <- node.data[best$values %in% group, , drop = FALSE]
      label <- best$group_labels[i]
      state$edges[[length(state$edges) + 1L]] <- list(
        parent_id = as.integer(node.id),
        child_id = as.integer(child.id),
        split_variable = best$variable,
        label = label,
        split_value = label,
        original_categories = paste(group, collapse = ' | ')
      )
      child.rule <- paste0(rule, ' & ', best$variable, ' in {', label, '}')
      grow_node(child.data, child.id, node.id, depth + 1L, child.rule)
    }
    invisible(NULL)
  }

  grow_node(data, 1L, NULL, 0L, 'Root')
  record.names <- names(state$node_records)
  record.list <- state$node_records[order(as.integer(record.names))]
  nodes <- do.call(rbind, lapply(record.list, function(record) {
    data.frame(
      node_id = record$node_id,
      parent_id = record$parent_id,
      depth = record$depth,
      is_terminal = record$is_terminal,
      n = record$n,
      weighted_n = record$weighted_n,
      predicted_class = record$predicted_class,
      split_variable = record$split_variable,
      p_value = record$p_value,
      adjusted_p_value = record$adjusted_p_value,
      split_statistic = record$split_statistic,
      split_df = record$split_df,
      rule = record$rule,
      stringsAsFactors = FALSE
    )
  }))
  nodes$class_distribution <- lapply(record.list, function(record) {
    record$class_distribution
  })
  row.names(nodes) <- NULL
  edges <- if (length(state$edges) == 0) {
    data.frame(
      parent_id = integer(), child_id = integer(), split_variable = character(),
      label = character(), split_value = character(),
      original_categories = character(), stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, lapply(state$edges, as.data.frame, stringsAsFactors = FALSE))
  }
  row.names(edges) <- NULL
  list(
    nodes = nodes,
    edges = edges,
    node_metadata = state$node_metadata,
    category_merge_map = merge_history_to_data(state$merge_history),
    low_expected = state$low_expected
  )
}

#' Convert merge-history records to a stable data frame.
#'
#' @param merge_history List of category merge records.
#' @return Category merge data frame.
merge_history_to_data <- function(merge_history) {
  if (length(merge_history) == 0) {
    return(data.frame(
      node_id = integer(), variable = character(), merged_group = character(),
      original_categories = character(), merge_p_value = numeric(),
      adjusted_p_value = numeric(), stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, lapply(merge_history, function(record) {
    data.frame(
      node_id = record$node_id,
      variable = record$variable,
      merged_group = record$merged_group,
      original_categories = paste(record$original_categories, collapse = ' | '),
      merge_p_value = record$merge_p_value,
      adjusted_p_value = record$adjusted_p_value,
      stringsAsFactors = FALSE
    )
  }))
}

#' Transform new data using a fitted CHAID model's preprocessing metadata.
#'
#' @param new_data New predictor data.
#' @param model A fitted `exploratory_chaid` model.
#' @return A data frame containing transformed predictors.
prepare_chaid_new_data <- function(new_data, model) {
  if (!is.data.frame(new_data)) {
    stop('new_data must be a data frame')
  }
  missing.predictors <- setdiff(model$predictors, names(new_data))
  if (length(missing.predictors) > 0) {
    stop(paste0(
      'new_data is missing predictor columns: ',
      paste(missing.predictors, collapse = ', ')
    ))
  }
  prepared.data <- data.frame(row.names = seq_len(nrow(new_data)))
  for (variable in model$predictors) {
    value <- new_data[[variable]]
    numeric.map <- model$numeric_binning_map[[variable]]
    if (!is.null(numeric.map) && !isTRUE(numeric.map$excluded) &&
        numeric.map$method != 'none') {
      transformed <- apply_numeric_bins(value, numeric.map)
    } else {
      transformed <- as.character(value)
    }
    if (model$parameters$missing == 'as_category') {
      transformed[is.na(transformed)] <- 'Missing'
    }
    prepared.data[[variable]] <- transformed
  }
  prepared.data
}

#' Apply saved numeric binning metadata to new values.
#'
#' @param value Numeric values.
#' @param metadata Saved binning metadata.
#' @return Character bin labels.
apply_numeric_bins <- function(value, metadata) {
  if (!is.numeric(value)) {
    value <- suppressWarnings(as.numeric(as.character(value)))
  }
  bin.index <- cut(
    value,
    breaks = metadata$breaks,
    labels = FALSE,
    include.lowest = TRUE,
    right = TRUE
  )
  metadata$labels[bin.index]
}

#' Traverse one row through a fitted CHAID tree.
#'
#' @param row One transformed predictor row.
#' @param model A fitted `exploratory_chaid` model.
#' @return The final node ID.
traverse_chaid_tree <- function(row, model) {
  node.id <- 1L
  repeat {
    node.row <- match(node.id, model$nodes$node_id)
    if (is.na(node.row)) {
      return(1L)
    }
    node <- model$nodes[node.row, , drop = FALSE]
    metadata <- model$.node_metadata[[as.character(node.id)]]
    if (isTRUE(node$is_terminal) || is.null(metadata) ||
        is.null(metadata$split_variable)) {
      return(as.integer(node.id))
    }
    variable <- metadata$split_variable
    value <- row[[variable]]
    if (length(value) != 1 || is.na(value)) {
      return(as.integer(node.id))
    }
    group.index <- which(vapply(metadata$split_groups, function(group) {
      value %in% group
    }, logical(1)))
    if (length(group.index) != 1) {
      return(as.integer(node.id))
    }
    edge.rows <- model$edges$parent_id == node.id &
      model$edges$label == metadata$group_labels[group.index]
    if (sum(edge.rows) != 1) {
      return(as.integer(node.id))
    }
    node.id <- as.integer(model$edges$child_id[edge.rows])
  }
}

#' Predict from a fitted classification CHAID tree.
#'
#' @param model A fitted `exploratory_chaid` model.
#' @param new_data New predictor data.
#' @param type Prediction output type: `class`, `prob`, `node`, or `all`.
#' @return A vector or data frame of predictions.
#' @export
chaid_predict <- function(model, new_data, type = c('class', 'prob', 'node', 'all')) {
  if (!inherits(model, 'exploratory_chaid')) {
    stop('model must be an exploratory_chaid object')
  }
  prediction.type <- match.arg(type, choices = c('class', 'prob', 'node', 'all'))
  prepared.data <- prepare_chaid_new_data(new_data, model)
  node.ids <- if (nrow(prepared.data) == 0) {
    integer()
  } else {
    vapply(seq_len(nrow(prepared.data)), function(index) {
      traverse_chaid_tree(prepared.data[index, , drop = FALSE], model)
    }, integer(1))
  }
  node.rows <- match(node.ids, model$nodes$node_id)
  predicted.classes <- as.character(model$nodes$predicted_class[node.rows])
  probability.matrix <- if (length(node.ids) == 0) {
    matrix(numeric(), nrow = 0, ncol = length(model$class_levels),
           dimnames = list(NULL, paste0('.pred_prob_', model$class_levels)))
  } else {
    do.call(rbind, model$nodes$class_distribution[node.rows])
  }
  colnames(probability.matrix) <- paste0('.pred_prob_', model$class_levels)

  if (prediction.type == 'class') {
    return(predicted.classes)
  }
  if (prediction.type == 'node') {
    return(as.integer(node.ids))
  }
  probability.data <- as.data.frame(probability.matrix, stringsAsFactors = FALSE)
  if (prediction.type == 'prob') {
    return(probability.data)
  }
  data.frame(
    .chaid_node_id = as.integer(node.ids),
    .pred_class = predicted.classes,
    .chaid_rule = model$nodes$rule[node.rows],
    probability.data,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' Predict using the standard S3 prediction interface.
#'
#' @param object A fitted `exploratory_chaid` model.
#' @param newdata New predictor data.
#' @param type Prediction output type.
#' @param ... Additional arguments ignored.
#' @return Predictions from `chaid_predict()`.
#' @export
predict.exploratory_chaid <- function(object, newdata,
                                      type = c('class', 'prob', 'node', 'all'), ...) {
  chaid_predict(object, newdata, type = type)
}

#' Return a node summary table for a CHAID model.
#'
#' @param model A fitted `exploratory_chaid` model.
#' @return A node summary data frame.
#' @export
chaid_node_summary <- function(model) {
  validate_chaid_model(model)
  root.n <- model$nodes$n[model$nodes$node_id == 1L]
  data.frame(
    Node = model$nodes$node_id,
    Rule = model$nodes$rule,
    Rows = model$nodes$n,
    `%` = model$nodes$n / root.n * 100,
    `Predicted Class` = model$nodes$predicted_class,
    `Target Distribution` = vapply(
      model$nodes$class_distribution,
      format_chaid_distribution,
      character(1)
    ),
    `Split Variable` = model$nodes$split_variable,
    `p-value` = model$nodes$p_value,
    `Adjusted p-value` = model$nodes$adjusted_p_value,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' Return terminal-node rules for a CHAID model.
#'
#' @param model A fitted `exploratory_chaid` model.
#' @return A terminal rule data frame.
#' @export
chaid_rule_table <- function(model) {
  validate_chaid_model(model)
  terminal <- model$nodes$is_terminal
  data.frame(
    Node = model$nodes$node_id[terminal],
    Rule = model$nodes$rule[terminal],
    Prediction = model$nodes$predicted_class[terminal],
    Probability = vapply(
      model$nodes$class_distribution[terminal],
      max,
      numeric(1)
    ),
    Rows = model$nodes$n[terminal],
    stringsAsFactors = FALSE
  )
}

#' Return category merge history for a CHAID model.
#'
#' @param model A fitted `exploratory_chaid` model.
#' @return A category merge data frame.
#' @export
chaid_category_merge_table <- function(model) {
  validate_chaid_model(model)
  merge.data <- model$category_merge_map
  data.frame(
    Node = merge.data$node_id,
    Variable = merge.data$variable,
    `Merged Category` = merge.data$merged_group,
    `Original Categories` = merge.data$original_categories,
    `Merge p-value` = merge.data$merge_p_value,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' Return split-level summary data for a CHAID model.
#'
#' @param model A fitted `exploratory_chaid` model.
#' @return A split summary data frame.
#' @export
chaid_split_summary <- function(model) {
  validate_chaid_model(model)
  split.rows <- !model$nodes$is_terminal
  node.ids <- model$nodes$node_id[split.rows]
  data.frame(
    Depth = model$nodes$depth[split.rows],
    Node = node.ids,
    `Split Variable` = model$nodes$split_variable[split.rows],
    `p-value` = model$nodes$p_value[split.rows],
    `Adjusted p-value` = model$nodes$adjusted_p_value[split.rows],
    `Number of Children` = vapply(node.ids, function(node.id) {
      sum(model$edges$parent_id == node.id)
    }, integer(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' Return renderer-friendly tree nodes and edges.
#'
#' @param model A fitted `exploratory_chaid` model.
#' @return A list with `nodes` and `edges` data frames.
#' @export
chaid_tree_data <- function(model) {
  validate_chaid_model(model)
  root.n <- model$nodes$n[model$nodes$node_id == 1L]
  tree.nodes <- data.frame(
    node_id = model$nodes$node_id,
    label = paste0('Node ', model$nodes$node_id, ': ', model$nodes$predicted_class),
    depth = model$nodes$depth,
    n = model$nodes$n,
    percent = model$nodes$n / root.n * 100,
    predicted_class = model$nodes$predicted_class,
    target_distribution = vapply(
      model$nodes$class_distribution,
      format_chaid_distribution,
      character(1)
    ),
    is_terminal = model$nodes$is_terminal,
    stringsAsFactors = FALSE
  )
  tree.edges <- model$edges[, c(
    'parent_id', 'child_id', 'label', 'split_variable', 'split_value'
  ), drop = FALSE]
  list(nodes = tree.nodes, edges = tree.edges)
}

#' Validate the basic shape of an exploratory CHAID model.
#'
#' @param model Model to validate.
#' @return Invisibly TRUE.
validate_chaid_model <- function(model) {
  if (!inherits(model, 'exploratory_chaid') ||
      is.null(model$nodes) || is.null(model$class_levels)) {
    stop('model must be an exploratory_chaid object')
  }
  invisible(TRUE)
}

#' Format a named target distribution for report output.
#'
#' @param distribution Named probability vector.
#' @return A compact distribution string.
format_chaid_distribution <- function(distribution) {
  paste(
    paste0(names(distribution), ': ', round(as.numeric(distribution) * 100, 2), '%'),
    collapse = ', '
  )
}

#' Validate CHAID input data and parameters.
#'
#' @param data A data frame.
#' @param target Target column name.
#' @param predictors Predictor column names.
#' @param weight Optional weight column name.
#' @param alpha_split Split significance threshold.
#' @param alpha_merge Merge significance threshold.
#' @param max_depth Maximum tree depth.
#' @param min_split Minimum node size for split consideration.
#' @param min_bucket Minimum child size.
#' @param min_node_proportion Optional child proportion threshold.
#' @param numeric_binning Numeric binning method.
#' @param numeric_bins Number of bins.
#' @param missing Missing-value handling method.
#' @param chi_square Chi-square statistic.
#' @param bonferroni Bonferroni correction flag.
#' @param ordinal_predictors Ordered predictor names.
#' @param max_categories Maximum category count.
#' @param verbose Diagnostic flag.
#' @return A list containing validated predictors and parameters.
validate_chaid_inputs <- function(data,
                                  target,
                                  predictors,
                                  weight,
                                  alpha_split,
                                  alpha_merge,
                                  max_depth,
                                  min_split,
                                  min_bucket,
                                  min_node_proportion,
                                  numeric_binning,
                                  numeric_bins,
                                  missing,
                                  chi_square,
                                  bonferroni,
                                  ordinal_predictors,
                                  max_categories,
                                  verbose) {
  if (!is.data.frame(data)) {
    stop('data must be a data frame')
  }
  if (length(target) != 1 || !is.character(target) ||
      !target %in% names(data)) {
    stop('target must name an existing column')
  }
  if (!is.factor(data[[target]]) && !is.character(data[[target]]) &&
      !is.logical(data[[target]])) {
    stop('target must be character, factor, or logical')
  }
  if (all(is.na(data[[target]]))) {
    stop('target must contain at least one non-missing value')
  }
  if (!is.null(weight)) {
    stop('weighted CHAID is not supported in this version')
  }
  if (is.null(predictors)) {
    predictors <- setdiff(names(data), target)
  }
  if (length(predictors) > 0 &&
      (!is.character(predictors) || any(!predictors %in% names(data)))) {
    stop('predictor names must identify existing columns')
  }
  if (target %in% predictors) {
    stop('target cannot also be a predictor')
  }
  if (!is.numeric(alpha_split) || length(alpha_split) != 1 ||
      is.na(alpha_split) || alpha_split <= 0 || alpha_split > 1) {
    stop('alpha_split must be greater than 0 and at most 1')
  }
  if (!is.numeric(alpha_merge) || length(alpha_merge) != 1 ||
      is.na(alpha_merge) || alpha_merge <= 0 || alpha_merge > 1) {
    stop('alpha_merge must be greater than 0 and at most 1')
  }
  if (length(max_depth) != 1 || is.na(max_depth) || max_depth < 0 ||
      max_depth != as.integer(max_depth)) {
    stop('max_depth must be a non-negative integer')
  }
  if (length(min_split) != 1 || is.na(min_split) || min_split < 1 ||
      min_split != as.integer(min_split)) {
    stop('min_split must be a positive integer')
  }
  if (length(min_bucket) != 1 || is.na(min_bucket) || min_bucket < 1 ||
      min_bucket != as.integer(min_bucket)) {
    stop('min_bucket must be a positive integer')
  }
  if (min_split < min_bucket) {
    stop('min_split must be greater than or equal to min_bucket')
  }
  if (!is.null(min_node_proportion) &&
      (length(min_node_proportion) != 1 || is.na(min_node_proportion) ||
       min_node_proportion <= 0 || min_node_proportion > 1)) {
    stop('min_node_proportion must be greater than 0 and at most 1')
  }
  numeric.binning <- match.arg(
    numeric_binning,
    choices = c('quantile', 'equal_width', 'none')
  )
  missing.method <- match.arg(
    missing,
    choices = c('as_category', 'exclude')
  )
  chi.square <- match.arg(
    chi_square,
    choices = c('pearson', 'likelihood_ratio')
  )
  if (length(numeric_bins) != 1 || is.na(numeric_bins) || numeric_bins < 2 ||
      numeric_bins != as.integer(numeric_bins)) {
    stop('numeric_bins must be an integer greater than or equal to 2')
  }
  if (!is.logical(bonferroni) || length(bonferroni) != 1 || is.na(bonferroni)) {
    stop('bonferroni must be TRUE or FALSE')
  }
  if (!is.null(ordinal_predictors) &&
      any(!ordinal_predictors %in% predictors)) {
    stop('ordinal_predictors must be included in predictors')
  }
  if (length(max_categories) != 1 || is.na(max_categories) ||
      max_categories < 2 || max_categories != as.integer(max_categories)) {
    stop('max_categories must be an integer greater than or equal to 2')
  }

  list(
    predictors = predictors,
    parameters = list(
      alpha_split = alpha_split,
      alpha_merge = alpha_merge,
      max_depth = as.integer(max_depth),
      min_split = as.integer(min_split),
      min_bucket = as.integer(min_bucket),
      min_node_proportion = min_node_proportion,
      numeric_binning = numeric.binning,
      numeric_bins = as.integer(numeric_bins),
      missing = missing.method,
      chi_square = chi.square,
      bonferroni = bonferroni,
      ordinal_predictors = ordinal_predictors,
      max_categories = as.integer(max_categories),
      verbose = verbose
    )
  )
}
