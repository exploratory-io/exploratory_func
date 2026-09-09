# how to run this test:
# devtools::test(filter="tidy_type_dispatch")
context("tidy()/glance() type dispatch must not fall through to the model's own input data")

# tam#38607. tidy.cor_exploratory dispatched on `type` and ended in a catch-all
#
#   else { x$data }
#
# so asking for a type it did not implement returned the model's SOURCE DATA. tam's Correlation
# report asked for type="analysis_conditions", fell through, and rendered every input row and
# column as its "Analysis Conditions and Data" table. Nothing errored, nothing warned, and the
# table looked like a real result.
#
# This is deliberately NOT "every type dispatch must end in stop()". Plenty of these methods have
# a meaningful default -- tidy.chisq_exploratory builds chi-square density data, tidy.shapiro_
# exploratory returns the model summary, tidy.clm_exploratory_0 returns the coefficient table --
# and a static rule cannot tell a purpose-built default from a wrong one. What it CAN tell, and
# what actually bit us, is a default that hands back a field holding the model's own input.
#
# The population is discovered from the package source rather than listed here, so a method added
# tomorrow is covered without anyone remembering to add it (this repo's guard tests fail OPEN
# otherwise). ALLOWED records the ones that are correct on purpose; it may shrink, never grow,
# without a reason written next to the entry.

# Fields that hold the model's own input rows. A default branch returning one of these is the
# tam#38607 shape: the caller asked for a derived table and got the raw input instead.
INPUT_DATA_FIELDS <- c("data", "df", "source_data", "raw_data", "source.data", "input_data")

# Methods whose input-returning default is correct, with the reason. Each entry is a promise that
# someone checked; none may be added without one.
ALLOWED <- list()

#' Every top-level `tidy.*`/`glance.*` definition in R/ that dispatches on a `type` argument.
#' @return list of list(name, file, body)
collect_type_dispatch_methods <- function() {
  r_dir <- normalizePath(file.path(rprojroot::find_package_root_file(), "R"), mustWork = TRUE)
  files <- list.files(r_dir, pattern = "[.][Rr]$", full.names = TRUE)
  out <- list()
  for (f in files) {
    exprs <- tryCatch(parse(f, keep.source = FALSE), error = function(e) NULL)
    if (is.null(exprs)) next
    for (e in exprs) {
      if (!is.call(e) || length(e) < 3) next
      op <- as.character(e[[1]])
      if (!(length(op) == 1 && op %in% c("<-", "="))) next
      nm <- tryCatch(as.character(e[[2]]), error = function(err) character(0))
      if (length(nm) != 1) next
      if (!grepl("^(tidy|glance)[.]", nm)) next
      fn <- e[[3]]
      if (!is.call(fn) || as.character(fn[[1]])[[1]] != "function") next
      if (!("type" %in% names(fn[[2]]))) next
      out[[length(out) + 1]] <- list(name = nm, file = basename(f), body = fn[[3]])
    }
  }
  out
}

#' The expression a `type` dispatch falls through to: the terminal `else` of the OUTERMOST
#' if-chain that tests `type`, or the unnamed default of `switch(type, ...)`. NULL when the
#' dispatch has no fall-through at all.
#'
#' Outermost, not last-seen: an earlier draft assigned on every `if` it walked past, so a nested
#' `if` deeper in the tree overwrote the real terminal else and the guard silently missed
#' tidy.one_sample_t_test_exploratory. A guard that can fail open is worse than none.
mentions_type <- function(e) {
  if (is.name(e)) return(identical(as.character(e), "type"))
  if (!is.call(e)) return(FALSE)
  for (i in seq_along(e)) {
    if (!is.null(e[[i]]) && isTRUE(try(mentions_type(e[[i]]), silent = TRUE))) return(TRUE)
  }
  FALSE
}

#' Walk an if/else-if chain to its final `else` branch.
end_of_chain <- function(e) {
  while (is.call(e) && is.name(e[[1]]) && as.character(e[[1]]) == "if" && length(e) == 4) {
    e <- e[[4]]
  }
  e
}

terminal_default <- function(body_expr) {
  found <- NULL
  done <- FALSE
  walk <- function(e) {
    if (done || !is.call(e)) return(invisible(NULL))
    head_name <- if (is.name(e[[1]])) as.character(e[[1]]) else ""

    if (head_name == "switch" && length(e) >= 3 && mentions_type(e[[2]])) {
      args <- as.list(e)[-(1:2)]
      nms <- names(args)
      if (is.null(nms)) nms <- rep("", length(args))
      # A switch default is the LAST argument with no name.
      if (nms[[length(nms)]] == "") { found <<- args[[length(args)]]; done <<- TRUE }
      return(invisible(NULL))
    }

    if (head_name == "if" && length(e) == 4 && mentions_type(e[[2]])) {
      tail_expr <- end_of_chain(e)
      # `if (type == ...) A else B` with no further chain: B is the fall-through. When the chain
      # ends in another `if` with no else, there is no fall-through at all.
      if (!(is.call(tail_expr) && is.name(tail_expr[[1]]) &&
            as.character(tail_expr[[1]]) == "if")) {
        found <<- tail_expr
      }
      done <<- TRUE
      return(invisible(NULL))
    }

    for (i in seq_along(e)) {
      if (done) break
      if (!is.null(e[[i]])) try(walk(e[[i]]), silent = TRUE)
    }
    invisible(NULL)
  }
  walk(body_expr)
  found
}

#' The `$` field a fall-through branch returns, if that is all it does. `{ x$data }` and `x$data`
#' both count; anything that computes something does not.
returned_input_field <- function(expr) {
  if (is.null(expr)) return(NA_character_)
  # Unwrap a `{ ... }` block down to its single, last statement.
  while (is.call(expr) && is.name(expr[[1]]) && as.character(expr[[1]]) == "{") {
    if (length(expr) != 2) return(NA_character_)
    expr <- expr[[2]]
  }
  if (!(is.call(expr) && is.name(expr[[1]]) && as.character(expr[[1]]) == "$")) return(NA_character_)
  field <- tryCatch(as.character(expr[[3]]), error = function(e) character(0))
  if (length(field) != 1) return(NA_character_)
  field
}

test_that("the package's own source is where the method list comes from", {
  skip_if_not_installed("rprojroot")
  methods <- collect_type_dispatch_methods()
  # Discovering the population instead of listing it is the whole point; if discovery breaks, this
  # guard would pass while checking nothing.
  expect_gt(length(methods), 30)
  expect_true("tidy.cor_exploratory" %in% vapply(methods, function(m) m$name, character(1)))
})

test_that("no type dispatch falls through to the model's own input data (tam#38607)", {
  skip_if_not_installed("rprojroot")
  offenders <- character(0)
  for (m in collect_type_dispatch_methods()) {
    field <- returned_input_field(terminal_default(m$body))
    if (is.na(field) || !(field %in% INPUT_DATA_FIELDS)) next
    if (m$name %in% names(ALLOWED)) next
    offenders <- c(offenders, paste0(m$name, " (", m$file, ") falls through to x$", field))
  }
  expect_equal(offenders, character(0),
    info = paste0(
      "A `type` dispatch whose default returns the model's own input hands a caller asking for a ",
      "derived table the RAW DATA instead, with no error -- tam#38607, where the Correlation ",
      "report rendered its whole input as the analysis-conditions table. Name the types you ",
      "support and stop() on the rest. Offenders:\n  - ",
      paste(offenders, collapse = "\n  - ")))
})

test_that("the guard actually fires on the shape it was written for", {
  # Sabotage check: the rule is worthless if it cannot recognize the original bug. This is the
  # exact body tidy.cor_exploratory had before tam#38607.
  original <- quote({
    if (type == "cor") {
      x$cor
    }
    else {
      x$data
    }
  })
  expect_equal(returned_input_field(terminal_default(original)), "data")

  # ...and does not fire on a default that computes something.
  computed <- quote({
    if (type == "observed") { x$observed } else { generate_density_data(x$statistic) }
  })
  expect_true(is.na(returned_input_field(terminal_default(computed))))

  # ...nor on one that errors, which is what the fix looks like.
  fixed <- quote({
    if (type == "cor") { x$cor }
    else if (type == "data") { x$data }
    else { stop("unsupported") }
  })
  expect_true(is.na(returned_input_field(terminal_default(fixed))))

  # A switch default is the same hazard reached by a different syntax.
  switched <- quote(switch(type, summary = x$summary, x$data))
  expect_equal(returned_input_field(terminal_default(switched)), "data")
})
