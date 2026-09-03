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
#
# tam #37691: the CHAID report table columns (Merged Category, Final Intervals)
# display the unbounded-above shape as "bk <" (symbol after the number) instead
# of "> bk". This is DISPLAY-ONLY, applied by chaid_display_symbol_after_number()
# strictly AFTER chaid_collapse_intervals()/chaid_format_interval() -- those stay
# on the original "> bk" shape, because chaid_collapse_intervals() ALSO produces
# the machine-readable `cond_value` build_chaid.R stores for the interactive
# tree's Show Detail drill-down (DTreeGenerator.parseNumericBinLabel on the tam
# side parses exactly "<=" / ">" / "(a, b]", not "N <"). Do not fold the display
# flip into chaid_format_interval() itself -- it silently breaks Show Detail for
# any unbounded-above branch (caught live via the exploratory_func CI suite,
# test_build_chaid.R's "tree_nodes edge labels collapse contiguous numeric bins").

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

#' Rewrite a collapsed bin-label string for REPORT TABLE display (tam #37691).
#'
#' The Merged Category and Final Intervals report columns show an
#' unbounded-above bin as `"N <"` (symbol after the number) instead of the
#' engine's own `"> N"`. This is applied AFTER collapsing/formatting, as a
#' separate, later step -- never inside [chaid_format_interval()] itself,
#' because that function's output also becomes `cond_value`
#' (`build_chaid.R`'s `tree_nodes` tidy type), the machine-readable label the
#' interactive tree's Show Detail drill-down parses
#' (`DTreeGenerator.parseNumericBinLabel` on the tam side expects exactly
#' `"<="` / `">"` / `"(a, b]"`). Folding the flip into `chaid_format_interval()`
#' silently breaks Show Detail for any unbounded-above branch.
#'
#' @param label A `" + "`-joined collapsed label string (as returned by
#'   [chaid_normalize_group_label()]), or `NA`.
#' @return The same string with every `"> N"` part rewritten to `"N <"`;
#'   non-interval parts (category names, `"Missing"`, `"All"`) pass through
#'   unchanged.
chaid_display_symbol_after_number <- function(label) {
  if (length(label) != 1 || is.na(label)) {
    return(label)
  }
  parts <- strsplit(label, CHAID_GROUP_SEPARATOR, fixed = TRUE)[[1]]
  parts <- vapply(parts, function(part) {
    interval <- chaid_parse_interval(part)
    if (!is.null(interval) && is.na(interval$upper)) {
      return(paste0(interval$lower, ' <'))
    }
    part
  }, character(1), USE.NAMES = FALSE)
  paste(parts, collapse = CHAID_GROUP_SEPARATOR)
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
    # tam #37177: a single non-interval member reads as an equality, not a
    # one-element set -- `残業 = TRUE` / `職種 = 営業` instead of `in (TRUE)`.
    return(paste0(variable, ' = ', collapsed))
  }
  paste0(variable, ' in (', paste(collapsed, collapse = CHAID_GROUP_SEPARATOR), ')')
}

#' Display order of a classification target's class labels.
#'
#' For a 2-class logical / Yes-No target the POSITIVE class is listed first, so a
#' FALSE-predicted node still shows its TRUE proportion at the top (SPSS parity).
#' Every other target keeps its level order. This is the order the tree chart's
#' class table uses, so it is also the order sibling nodes are ranked by when
#' nothing else separates them (tam #38372).
#'
#' @param class_levels Character vector of the fitted target's class levels.
#' @return `class_levels`, reordered for display.
chaid_display_class_order <- function(class_levels) {
  class_levels <- as.character(class_levels)
  k <- length(class_levels)
  if (k != 2) {
    return(class_levels)
  }
  up <- toupper(class_levels)
  positive.idx <- if (setequal(up, c('FALSE', 'TRUE'))) {
    which(up == 'TRUE')
  } else if (setequal(up, c('NO', 'YES'))) {
    which(up == 'YES')
  } else {
    NA_integer_
  }
  if (is.na(positive.idx)) {
    return(class_levels)
  }
  class_levels[c(positive.idx, setdiff(seq_len(k), positive.idx))]
}

#' Order one node's children the way the tree chart draws them, left to right.
#'
#' MUST stay in lockstep with `DTreeGenerator._orderChildrenTrueLeft` (tam
#' `src/js/viz/DTreeGenerator.js`) and its report-side mirror
#' `dtree_renumber_tree_nodes_true_left` (tam `public/lib/library.r`). For a
#' logical target (TRUE/FALSE or Yes/No), the child TRUE rate is the primary key,
#' followed by the condition side rank and child id (tam #38357). For every other
#' target, tie-breaks are applied in this order and the FIRST one that separates a
#' pair wins:
#'
#'   1. TRUE/Yes first, FALSE/No last (tam #37252).
#'   2. A binned-numeric run's lowest bound, ascending, so a numeric axis reads
#'      low -> high left -> right. This MUST outrank the class tie-break below --
#'      a non-monotone predictor/target relationship would otherwise scramble the
#'      bins.
#'   3. The predicted class, in the target's own display order
#'      ([chaid_display_class_order()]).
#'   4. The child's own id, so the result is always deterministic.
#'
#' Before tam #38372 CHAID emitted ids in group-CREATION order and left the
#' reordering to the chart, which desynchronised every report tab's Node column
#' from the chart's chip numbers. Ordering here instead makes the chart's own
#' reorder a no-op and every consumer agree.
#'
#' @param kids Integer vector of child node ids.
#' @param edges The tree's edge frame (`parent_id`, `child_id`, `original_categories`).
#' @param nodes The tree's node frame (`node_id`, `predicted_class`,
#'   `class_distribution`).
#' @param class_order Target class labels in display order, or `NULL` for a
#'   numeric target (no class tie-break).
#' @return `kids`, reordered left to right.
chaid_order_children_for_display <- function(kids, edges, nodes, class_order = NULL) {
  kids <- as.integer(kids)
  if (length(kids) < 2) {
    return(kids)
  }
  values.of <- function(child) {
    row <- which(edges$child_id == child)
    if (length(row) != 1) {
      return(character(0))
    }
    raw <- edges$original_categories[row]
    if (is.na(raw)) {
      return(character(0))
    }
    trimws(strsplit(as.character(raw), ' | ', fixed = TRUE)[[1]])
  }
  side.rank <- function(child) {
    values <- toupper(values.of(child))
    true.side <- any(values %in% c('TRUE', 'YES'))
    false.side <- any(values %in% c('FALSE', 'NO'))
    if (true.side && !false.side) return(0L)
    if (false.side && !true.side) return(2L)
    1L
  }
  # Lowest numeric bound of a binned-numeric run; NA for a nominal split. Reuses
  # chaid_parse_interval() rather than adding another bin-label regex.
  ordered.key <- function(child) {
    values <- values.of(child)
    if (length(values) == 0) return(NA_real_)
    lo <- NA_real_
    for (label in values) {
      parsed <- chaid_parse_interval(label)
      if (is.null(parsed)) return(NA_real_)
      v <- parsed$lower_value
      if (!is.finite(v) && v > 0) return(NA_real_)   # defensive; lower is never +Inf
      if (is.na(lo) || v < lo) lo <- v
    }
    lo
  }
  class.rank <- function(child) {
    if (is.null(class_order) || length(class_order) == 0) return(NA_real_)
    row <- which(nodes$node_id == child)
    if (length(row) != 1) return(NA_real_)
    hit <- match(as.character(nodes$predicted_class[row]), as.character(class_order))
    if (is.na(hit)) NA_real_ else as.numeric(hit)
  }
  class.labels <- toupper(as.character(class_order))
  is.logical.target <- length(class.labels) == 2 &&
    (setequal(class.labels, c('FALSE', 'TRUE')) ||
       setequal(class.labels, c('NO', 'YES')))
  true.rate <- function(child) {
    if (!is.logical.target || !('class_distribution' %in% names(nodes))) return(0)
    row <- which(nodes$node_id == child)
    if (length(row) != 1) return(0)
    distribution <- nodes$class_distribution[[row]]
    if (is.null(distribution) || length(distribution) == 0) return(0)
    hit <- which(toupper(names(distribution)) %in% c('TRUE', 'YES'))
    if (length(hit) == 0) return(0)
    rate <- suppressWarnings(as.numeric(distribution[hit[1]]))
    if (is.na(rate) || !is.finite(rate)) 0 else rate
  }

  ranks <- vapply(kids, side.rank, integer(1))
  if (is.logical.target) {
    # Match DTreeGenerator._orderChildrenTrueLeft: a logical target's more
    # TRUE-heavy branch is always leftmost; condition rank only breaks a tie.
    rates <- -vapply(kids, true.rate, numeric(1))
    return(kids[order(rates, ranks, kids)])
  }
  keys <- vapply(kids, ordered.key, numeric(1))
  classes <- vapply(kids, class.rank, numeric(1))
  # A nominal split has no numeric key on ANY child; only then does class order
  # get a say (matching the JS, which requires both keys to be absent).
  if (any(!is.na(keys))) {
    classes <- rep(NA_real_, length(kids))
  }
  kids[order(ranks,
             ifelse(is.na(keys), 0, keys),
             ifelse(is.na(classes), 0, classes),
             kids)]
}

#' Branch label for the interactive tree chart, in CART's form.
#'
#' `build_rpart_tree_nodes()` writes a categorical branch as `X = a, b, c`;
#' CHAID's report convention is `X in (a + b + c)`. Both feed the SAME
#' Characteristic Groups table (`dtree_report_characteristic_groups()` joins
#' `edge_label` with `" AND "`), which therefore printed two formats depending
#' on the algorithm (tam #38372). The tree chart itself already renders
#' `a, b, c` from `cond_value`, so the CART form is also what the diagram shows.
#'
#' A single member (`X = a`) and a collapsed numeric run (`X <= 6`,
#' `2 < X <= 5`) already come out CART-shaped from
#' [chaid_readable_one_condition()] and are delegated to it. A multi-member
#' group that still contains an interval label — a non-contiguous bin run that
#' would not collapse — also stays with `in (...)`, because `X = <= 2, (2.8, 5]`
#' is unreadable.
#'
#' @param variable Display (original) column name.
#' @param categories Character vector of category / bin labels, already ordered
#'   and interval-collapsed.
#' @return A single label string.
chaid_tree_edge_label <- function(variable, categories) {
  categories <- as.character(categories)
  in_form <- function() {
    chaid_readable_one_condition(
      paste0(variable, ' in {',
             paste(categories, collapse = CHAID_GROUP_SEPARATOR), '}'))
  }
  if (length(categories) <= 1) {
    return(in_form())
  }
  has_interval <- any(vapply(categories,
                             function(one) !is.null(chaid_parse_interval(one)),
                             logical(1)))
  if (has_interval) {
    return(in_form())
  }
  paste0(variable, ' = ', paste(categories, collapse = ', '))
}

#' Reorder every category group inside composite rule strings.
#'
#' A rule is a `" & "`-joined list of `<var> in {a + b}` conditions built in
#' CHAID's MERGE order. This gives each condition's members the predictor's
#' declared order (see [chaid_group_level_order()] / [chaid_order_group_parts()]),
#' so the Node Summary / Rules tabs agree with the tree chart and the Category
#' Merges table (tam #38372). Variable names are left in whatever name space the
#' caller passed — apply this BEFORE [chaid_map_display_names_in_text()], while
#' the names are still CLEAN, since the level lookup is keyed on the clean name.
#'
#' @param text Character vector of rule strings.
#' @param model A fitted `exploratory_chaid` model.
#' @return `text`, with each condition's members reordered.
chaid_normalize_condition_groups <- function(text, model) {
  text <- as.character(text)
  if (length(text) == 0) {
    return(text)
  }
  level_cache <- new.env(parent = emptyenv())
  levels_for <- function(variable) {
    key <- paste0('v', variable)   # avoid clashing with env internals
    if (!exists(key, envir = level_cache, inherits = FALSE)) {
      assign(key, list(chaid_group_level_order(model, variable)), envir = level_cache)
    }
    get(key, envir = level_cache, inherits = FALSE)[[1]]
  }
  map_condition <- function(condition) {
    m <- regmatches(condition, regexec('^(.*) in \\{(.*)\\}$', condition))[[1]]
    if (length(m) != 3) {
      return(condition)
    }
    variable <- m[2]
    parts <- strsplit(m[3], CHAID_GROUP_SEPARATOR, fixed = TRUE)[[1]]
    parts <- chaid_order_group_parts(parts, levels_for(variable))
    paste0(variable, ' in {', paste(parts, collapse = CHAID_GROUP_SEPARATOR), '}')
  }
  vapply(text, function(rule) {
    if (is.na(rule)) {
      return(NA_character_)
    }
    conditions <- chaid_split_conditions(rule)
    paste(vapply(conditions, map_condition, character(1)),
          collapse = CHAID_CONDITION_SEPARATOR)
  }, character(1), USE.NAMES = FALSE)
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

#' Map a clean (fit-time) predictor/column name back to its original name.
#'
#' `cleanup_df()`'s `map_name = FALSE` mode (used by `exp_chaid()`) replaces
#' commas with periods in column names -- `mmpf::marginalPrediction` does not
#' handle commas well -- before `chaid_fit()` ever runs. Everything `chaid_fit()`
#' builds (`model$nodes$split_variable`, `model$nodes$rule`,
#' `model$category_merge_map$variable`) is therefore keyed/embedded in this
#' CLEANED name space, not the column's real name. `model$terms_mapping`
#' (clean -> original) is computed by `exp_chaid()` only AFTER `chaid_fit()`
#' returns (`build_chaid.R`), so report functions resolve it back through this
#' helper at DISPLAY time.
#'
#' This is display-only. Internal tree traversal for scoring
#' (`traverse_chaid_tree()` / `chaid_assign_nodes()`) reads split variable
#' names from `model$.node_metadata`, a separate structure this never touches,
#' and stays in clean-name space; `model$numeric_binning_map` and
#' `model$predictor_info` are also clean-keyed and must be looked up with the
#' CLEAN name before being mapped for display (see `chaid_numeric_intervals()`
#' / `chaid_category_merge_table()` in `chaid.R`).
#'
#' @param name A clean column name (or character vector of them). `NA` passes through.
#' @param terms_mapping `model$terms_mapping` (named character vector, clean -> original), or `NULL`.
#' @return `name`, with any entry found in `terms_mapping` replaced by its original.
chaid_map_display_name <- function(name, terms_mapping) {
  name <- as.character(name)
  if (is.null(terms_mapping) || length(terms_mapping) == 0 || length(name) == 0) {
    return(name)
  }
  hit <- !is.na(name) & name %in% names(terms_mapping)
  name[hit] <- unname(terms_mapping[name[hit]])
  name
}

#' Rewrite clean column names in the variable side of composite rule strings
#' back to their original names (display-only -- see
#' [chaid_map_display_name()]). Category values are deliberately left alone.
#'
#' @param text A character vector of rule strings (each may join multiple
#'   `" & "`-separated conditions, one variable name per condition).
#' @param terms_mapping `model$terms_mapping` (named character vector, clean -> original), or `NULL`.
#' @return `text`, with condition variable names replaced by their originals.
chaid_map_display_names_in_text <- function(text, terms_mapping) {
  text <- as.character(text)
  if (is.null(terms_mapping) || length(terms_mapping) == 0 || length(text) == 0) {
    return(text)
  }
  map_condition <- function(condition) {
    marker <- regexpr(" in \\{", condition, perl = TRUE)[[1]]
    if (marker < 1) {
      return(condition)
    }
    variable <- substr(condition, 1, marker - 1)
    suffix <- substr(condition, marker, nchar(condition))
    paste0(chaid_map_display_name(variable, terms_mapping), suffix)
  }
  vapply(text, function(rule) {
    if (is.na(rule)) {
      return(NA_character_)
    }
    conditions <- chaid_split_conditions(rule)
    paste(vapply(conditions, map_condition, character(1)),
          collapse = CHAID_CONDITION_SEPARATOR)
  }, character(1), USE.NAMES = FALSE)
}

#' Level order to use when ordering a predictor's category group.
#'
#' @param model A fitted `exploratory_chaid` model.
#' @param variable Predictor name, in CLEAN (fit-time) name space -- matching
#'   `model$predictor_info`'s keys, exactly as every existing caller already
#'   passes it.
#' @return Character vector of levels, or `NULL` when alphabetical order applies.
chaid_group_level_order <- function(model, variable) {
  # A predictor that was a factor in the USER's data frame keeps its declared
  # level order. This has to come from the pre-cleanup frame: cleanup_df() turns
  # every character predictor into a factor whose levels are merely
  # data-appearance order, so `predictor_info` cannot tell the two apart.
  # `original_factor_levels` is captured BEFORE cleanup (build_chaid.R), so it
  # is keyed by the ORIGINAL column name -- resolve `variable` (clean) through
  # terms_mapping first, or this silently misses every renamed (e.g.
  # comma-containing) column and falls through to the alphabetical default.
  declared_name <- chaid_map_display_name(variable, model$terms_mapping)
  declared <- model$original_factor_levels[[declared_name]]
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
