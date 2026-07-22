# Report-formatting helpers for CHAID analytics report tables (tam #37177).
#
# These are display-only transformations of strings that the CHAID engine
# already produces (node rules, merged category-group labels, child-edge
# interval labels). They never change how the tree is fitted.
#
# Two primitives cover five separate spec bullets:
#   P1 chaid_readable_condition()  - "Root & X in {(3, 5] + (5, 6.7]}" -> "3 < X <= 6.7"
#   P2 chaid_collapse_intervals()  - c("(3, 5]", "(5, 6.7]")           -> "(3, 6.7]"
#
# Numeric bin labels produced by create_numeric_bins() have exactly three
# shapes: "<= b1", "(b1, b2]", "> bk". Anything else (a category name, the
# "Missing" level) is passed through untouched and acts as a barrier that
# collapsing never crosses.

CHAID_GROUP_SEPARATOR <- ' + '
CHAID_CONDITION_SEPARATOR <- ' & '
CHAID_ROOT_LABEL <- 'Root'
# Emitted for the root row once the "Root & " prefix is stripped (tam #37177).
# Localized on the tam side via the ANALYTICS.UI pass-through namespace.
CHAID_ALL_LABEL <- 'All'

#' Parse a numeric bin label into an interval.
#'
#' @param label A single bin label, e.g. `"<= 3"`, `"(3, 5]"`, `"> 23"`.
#' @return A list with `lower` / `upper` (character bound text, `NA` when the
#'   side is unbounded) and `lower_value` / `upper_value` (numeric, `-Inf` /
#'   `Inf` when unbounded), or `NULL` when `label` is not an interval.
chaid_parse_interval <- function(label) {
  if (length(label) != 1 || is.na(label)) {
    return(NULL)
  }
  label <- trimws(label)
  m <- regmatches(label, regexec('^<=[[:space:]]*(.+)$', label))[[1]]
  if (length(m) == 2) {
    upper <- trimws(m[2])
    upper.value <- suppressWarnings(as.numeric(upper))
    if (is.na(upper.value)) {
      return(NULL)
    }
    return(list(lower = NA_character_, upper = upper,
                lower_value = -Inf, upper_value = upper.value))
  }
  m <- regmatches(label, regexec('^>[[:space:]]*(.+)$', label))[[1]]
  if (length(m) == 2) {
    lower <- trimws(m[2])
    lower.value <- suppressWarnings(as.numeric(lower))
    if (is.na(lower.value)) {
      return(NULL)
    }
    return(list(lower = lower, upper = NA_character_,
                lower_value = lower.value, upper_value = Inf))
  }
  m <- regmatches(label, regexec('^\\([[:space:]]*([^,]+),[[:space:]]*(.+)\\]$', label))[[1]]
  if (length(m) == 3) {
    lower <- trimws(m[2])
    upper <- trimws(m[3])
    lower.value <- suppressWarnings(as.numeric(lower))
    upper.value <- suppressWarnings(as.numeric(upper))
    if (is.na(lower.value) || is.na(upper.value)) {
      return(NULL)
    }
    return(list(lower = lower, upper = upper,
                lower_value = lower.value, upper_value = upper.value))
  }
  NULL
}

#' Render an interval back to a bin label.
#'
#' @param interval A list as returned by [chaid_parse_interval()].
#' @return A single label string.
chaid_format_interval <- function(interval) {
  if (is.na(interval$lower) && is.na(interval$upper)) {
    return(CHAID_ALL_LABEL)
  }
  if (is.na(interval$lower)) {
    return(paste0('<= ', interval$upper))
  }
  if (is.na(interval$upper)) {
    return(paste0('> ', interval$lower))
  }
  paste0('(', interval$lower, ', ', interval$upper, ']')
}

#' Collapse a run of adjacent numeric bin labels into single ranges.
#'
#' Only CONTIGUOUS labels are collapsed: the upper bound of one must equal the
#' lower bound of the next. A gap keeps the pieces enumerated (tam #37177 —
#' collapsing across a gap would claim values the branch does not contain).
#' Non-interval entries (category names, `"Missing"`) pass through in place.
#'
#' @param labels Character vector of bin labels, in bin order.
#' @return Character vector, same order, with adjacent intervals merged.
chaid_collapse_intervals <- function(labels) {
  labels <- as.character(labels)
  if (length(labels) <= 1) {
    return(labels)
  }
  out <- character()
  pending <- NULL
  flush <- function() {
    if (!is.null(pending)) {
      out <<- c(out, chaid_format_interval(pending))
      pending <<- NULL
    }
  }
  for (label in labels) {
    interval <- chaid_parse_interval(label)
    if (is.null(interval)) {
      flush()
      out <- c(out, label)
      next
    }
    if (is.null(pending)) {
      pending <- interval
      next
    }
    contiguous <- is.finite(pending$upper_value) &&
      is.finite(interval$lower_value) &&
      isTRUE(all.equal(pending$upper_value, interval$lower_value))
    if (contiguous) {
      pending$upper <- interval$upper
      pending$upper_value <- interval$upper_value
    } else {
      flush()
      pending <- interval
    }
  }
  flush()
  out
}

#' Split a rule string into its top-level conditions.
#'
#' Splitting on `" & "` alone is unsafe because a category value may itself
#' contain `" & "` (e.g. `"R & D"`). Fragments are re-joined until the
#' `{...}` group of the condition is balanced.
#'
#' @param rule A rule string.
#' @return Character vector of conditions.
chaid_split_conditions <- function(rule) {
  fragments <- strsplit(rule, CHAID_CONDITION_SEPARATOR, fixed = TRUE)[[1]]
  out <- character()
  buffer <- NULL
  for (fragment in fragments) {
    buffer <- if (is.null(buffer)) fragment else paste0(buffer, CHAID_CONDITION_SEPARATOR, fragment)
    opens <- lengths(regmatches(buffer, gregexpr('\\{', buffer)))
    closes <- lengths(regmatches(buffer, gregexpr('\\}', buffer)))
    if (opens <= closes) {
      out <- c(out, buffer)
      buffer <- NULL
    }
  }
  if (!is.null(buffer)) {
    out <- c(out, buffer)
  }
  out
}

#' Rewrite one `<variable> in {<group>}` condition in readable form.
#'
#' @param condition A single condition string.
#' @return A readable condition string.
chaid_readable_one_condition <- function(condition) {
  m <- regmatches(condition, regexec('^(.*) in \\{(.*)\\}$', condition))[[1]]
  if (length(m) != 3) {
    return(condition)
  }
  variable <- m[2]
  parts <- strsplit(m[3], CHAID_GROUP_SEPARATOR, fixed = TRUE)[[1]]
  collapsed <- chaid_collapse_intervals(parts)
  if (length(collapsed) == 1) {
    interval <- chaid_parse_interval(collapsed)
    if (!is.null(interval)) {
      if (is.na(interval$lower)) {
        return(paste0(variable, ' <= ', interval$upper))
      }
      if (is.na(interval$upper)) {
        return(paste0(variable, ' > ', interval$lower))
      }
      return(paste0(interval$lower, ' < ', variable, ' <= ', interval$upper))
    }
  }
  paste0(variable, ' in (', paste(collapsed, collapse = CHAID_GROUP_SEPARATOR), ')')
}

#' Rewrite a CHAID node rule in readable form.
#'
#' Drops the leading `Root` term, collapses contiguous numeric bin groups into
#' a single inequality, and renders categorical groups as `X in (a + b)`.
#' The root node's own rule becomes [CHAID_ALL_LABEL].
#'
#' @param rule Character vector of rule strings.
#' @return Character vector of readable rules.
chaid_readable_condition <- function(rule) {
  vapply(rule, function(one) {
    if (is.na(one)) {
      return(NA_character_)
    }
    one <- trimws(one)
    if (!nzchar(one)) {
      return(CHAID_ALL_LABEL)
    }
    conditions <- chaid_split_conditions(one)
    conditions <- conditions[trimws(conditions) != CHAID_ROOT_LABEL]
    if (length(conditions) == 0) {
      return(CHAID_ALL_LABEL)
    }
    paste(vapply(conditions, chaid_readable_one_condition, character(1)),
          collapse = CHAID_CONDITION_SEPARATOR)
  }, character(1), USE.NAMES = FALSE)
}

#' Order the members of a category group.
#'
#' Factor (and binned-numeric / ordinal) predictors keep their original level
#' order; every other predictor is sorted alphabetically, so the same merged
#' group always reads the same way (tam #37177 — `離婚 | 既婚` vs `既婚 | 離婚`).
#'
#' @param parts Character vector of category names.
#' @param levels Original level order, or `NULL` for alphabetical.
#' @return Reordered character vector.
chaid_order_group_parts <- function(parts, levels = NULL) {
  parts <- as.character(parts)
  if (length(parts) <= 1) {
    return(parts)
  }
  if (is.null(levels) || length(levels) == 0) {
    return(sort(parts))
  }
  position <- match(parts, levels)
  # Anything not in the declared levels keeps its relative order at the end.
  position[is.na(position)] <- length(levels) + seq_len(sum(is.na(position)))
  parts[order(position)]
}

#' Level order to use when ordering a predictor's category group.
#'
#' @param model A fitted `exploratory_chaid` model.
#' @param variable Predictor name.
#' @return Character vector of levels, or `NULL` when alphabetical order applies.
chaid_group_level_order <- function(model, variable) {
  # A predictor that was a factor in the USER's data frame keeps its declared
  # level order. This has to come from the pre-cleanup frame: cleanup_df() turns
  # every character predictor into a factor whose levels are merely
  # data-appearance order, so `predictor_info` cannot tell the two apart.
  declared <- model$original_factor_levels[[variable]]
  if (!is.null(declared) && length(declared) > 0) {
    return(declared)
  }
  info <- model$predictor_info[[variable]]
  # Binned numerics and ordinal predictors carry a real order too; anything
  # else (a plain character column) is displayed alphabetically.
  if (!is.null(info) && isTRUE(info$ordered)) {
    return(info$levels)
  }
  NULL
}

#' Normalize a merged category-group label for display.
#'
#' Orders the members ([chaid_order_group_parts()]) and then collapses
#' contiguous numeric intervals ([chaid_collapse_intervals()]).
#'
#' @param label A `" + "`-joined group label.
#' @param levels Original level order, or `NULL`.
#' @param collapse Whether to collapse contiguous intervals.
#' @param separator Separator used by `label`.
#' @return A normalized label string.
chaid_normalize_group_label <- function(label, levels = NULL, collapse = TRUE,
                                        separator = CHAID_GROUP_SEPARATOR) {
  if (is.na(label)) {
    return(label)
  }
  parts <- strsplit(label, separator, fixed = TRUE)[[1]]
  parts <- chaid_order_group_parts(parts, levels)
  if (isTRUE(collapse)) {
    parts <- chaid_collapse_intervals(parts)
  }
  paste(parts, collapse = separator)
}

#' Keep only the final merge row for each (node, variable) merge chain.
#'
#' CHAID records every intermediate step of a merge, so the same group appears
#' repeatedly as it grows. A row is dropped when a LATER row for the same node
#' and variable covers a strict superset of its original categories — leaving
#' exactly the groups that survived to the split (tam #37177).
#'
#' @param merges Category-merge data frame.
#' @param node_col,variable_col,categories_col Column names to key on.
#' @param separator Separator used by `categories_col`.
#' @return The filtered data frame.
chaid_keep_final_merges <- function(merges, node_col = 'node_id',
                                    variable_col = 'variable',
                                    categories_col = 'original_categories',
                                    separator = ' | ') {
  if (is.null(merges) || nrow(merges) == 0) {
    return(merges)
  }
  category.sets <- lapply(merges[[categories_col]], function(value) {
    if (is.na(value)) character() else trimws(strsplit(value, separator, fixed = TRUE)[[1]])
  })
  keep <- rep(TRUE, nrow(merges))
  for (i in seq_len(nrow(merges))) {
    later <- which(seq_len(nrow(merges)) > i &
                     merges[[node_col]] == merges[[node_col]][i] &
                     merges[[variable_col]] == merges[[variable_col]][i])
    for (j in later) {
      if (length(category.sets[[i]]) < length(category.sets[[j]]) &&
          all(category.sets[[i]] %in% category.sets[[j]])) {
        keep[i] <- FALSE
        break
      }
    }
  }
  merges[keep, , drop = FALSE]
}
