# Tests for exploratory::round(), whose tie-breaking rule comes from
# getOption("exploratory.round_mode"): "half_up" (default), "half_down", "half_even".

with_mode <- function(mode, code) {
  withr::with_options(list(exploratory.round_mode = mode), code)
}

round_in_mode <- function(mode, x, ...) {
  with_mode(mode, exploratory::round(x, ...))
}

test_that("tie table: half_up / half_down / half_even", {
  # Each row: x, digits, expected under half_up, half_down, half_even.
  cases <- list(
    list(x = 2062.5, digits = 0, up = 2063, down = 2062, even = 2062),
    list(x = 2063.5, digits = 0, up = 2064, down = 2063, even = 2064),
    list(x = -2.5, digits = 0, up = -3, down = -2, even = -2),
    list(x = -3.5, digits = 0, up = -4, down = -3, even = -4),
    list(x = 3.125, digits = 2, up = 3.13, down = 3.12, even = 3.12),
    list(x = 2.675, digits = 2, up = 2.68, down = 2.67, even = 2.67),
    list(x = 1250, digits = -2, up = 1300, down = 1200, even = 1200),
    list(x = 2.4, digits = 0, up = 2, down = 2, even = 2),
    list(x = 2.6, digits = 0, up = 3, down = 3, even = 3)
  )
  for (case in cases) {
    label <- paste0("round(", case$x, ", ", case$digits, ")")
    expect_equal(round_in_mode("half_up", case$x, digits = case$digits), case$up, info = paste(label, "half_up"))
    expect_equal(round_in_mode("half_down", case$x, digits = case$digits), case$down, info = paste(label, "half_down"))
    expect_equal(round_in_mode("half_even", case$x, digits = case$digits), case$even, info = paste(label, "half_even"))
  }
})

test_that("half_even is exactly base::round", {
  x <- c(-3.5, -2.5, -0.5, 0.5, 1.5, 2.5, 3.5, 2.675, 3.125, 1e10 + 0.5, NA, NaN, Inf, -Inf)
  for (digits in c(0, 1, 2, -1)) {
    expect_identical(round_in_mode("half_even", x, digits = digits), base::round(x, digits))
  }
})

test_that("negatives are symmetric by magnitude in every mode", {
  x <- c(-2.5, -3.5, -3.125, -0.5, -1250)
  expect_equal(round_in_mode("half_up", x), c(-3, -4, -3, -1, -1250))
  expect_equal(round_in_mode("half_down", x), c(-2, -3, -3, 0, -1250))
  expect_equal(round_in_mode("half_even", x), c(-2, -4, -3, 0, -1250))
  expect_equal(round_in_mode("half_up", -3.125, digits = 2), -3.13)
  expect_equal(round_in_mode("half_down", -3.125, digits = 2), -3.12)
  # The mirror property: round(-x) == -round(x) in all modes.
  pos <- c(0.5, 1.5, 2.5, 2.675, 1234.5, 0.125)
  for (mode in c("half_up", "half_down", "half_even")) {
    expect_equal(round_in_mode(mode, -pos, digits = 2), -round_in_mode(mode, pos, digits = 2), info = mode)
    expect_equal(round_in_mode(mode, -pos), -round_in_mode(mode, pos), info = mode)
  }
})

test_that("vector and negative digits", {
  expect_equal(round_in_mode("half_up", c(1250, 1350, 1249), digits = -2), c(1300, 1400, 1200))
  expect_equal(round_in_mode("half_down", c(1250, 1350, 1251), digits = -2), c(1200, 1300, 1300))
  expect_equal(round_in_mode("half_up", 1250, digits = -2), 1300)
  # digits as a vector is recycled like base::round.
  expect_equal(round_in_mode("half_up", c(2.5, 2.25, 2.125), digits = c(0, 1, 2)), c(3, 2.3, 2.13))
  expect_equal(round_in_mode("half_down", c(2.5, 2.25, 2.125), digits = c(0, 1, 2)), c(2, 2.2, 2.12))
  # digits longer than x recycles x.
  expect_equal(round_in_mode("half_up", 2.5, digits = c(0, 1)), c(3, 2.5))
  # Extreme digits behave like base::round (no NaN from Inf * 0, no overflow).
  for (mode in c("half_up", "half_down", "half_even")) {
    expect_identical(round_in_mode(mode, c(123.456, 1e300), digits = 400), c(123.456, 1e300), info = mode)
    expect_identical(round_in_mode(mode, c(123.456, 1e300), digits = -400), c(0, 0), info = mode)
  }
  expect_true(is.na(round_in_mode("half_up", 2.5, digits = NA_real_)))
})

test_that("NA / NaN / Inf pass through in every mode", {
  x <- c(1.5, NA, NaN, Inf, -Inf)
  for (mode in c("half_up", "half_down", "half_even")) {
    res <- round_in_mode(mode, x)
    expect_equal(res[2:5], x[2:5], info = mode)
    expect_true(is.na(res[2]) && !is.nan(res[2]), info = mode)
    expect_true(is.nan(res[3]), info = mode)
  }
  expect_equal(round_in_mode("half_up", NA_real_), NA_real_)
  expect_identical(round_in_mode("half_up", numeric(0)), numeric(0))
})

test_that("names, dim and dimnames are kept", {
  x <- c(a = 0.5, b = 1.5, c = 2.5)
  for (mode in c("half_up", "half_down", "half_even")) {
    expect_identical(names(round_in_mode(mode, x)), c("a", "b", "c"))
  }
  expect_equal(round_in_mode("half_up", x), c(a = 1, b = 2, c = 3))
  expect_equal(round_in_mode("half_down", x), c(a = 0, b = 1, c = 2))

  m <- matrix(c(0.5, 1.5, 2.5, 3.5), nrow = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  res <- round_in_mode("half_up", m)
  expect_identical(dim(res), dim(m))
  expect_identical(dimnames(res), dimnames(m))
  expect_equal(unname(res), matrix(c(1, 2, 3, 4), nrow = 2))
})

test_that("integer, Date, POSIXct, difftime are identical to base::round", {
  d <- as.Date("2026-01-15")
  ct <- as.POSIXct("2026-01-15 12:34:56.789", tz = "UTC")
  dt <- as.difftime(c(1.5, 2.5, 3.5), units = "hours")
  for (mode in c("half_up", "half_down", "half_even")) {
    expect_identical(round_in_mode(mode, 5L), base::round(5L), info = mode)
    expect_identical(round_in_mode(mode, c(1L, NA, -3L)), base::round(c(1L, NA, -3L)), info = mode)
    expect_identical(round_in_mode(mode, d), base::round(d), info = mode)
    expect_identical(round_in_mode(mode, ct), base::round(ct), info = mode)
    expect_identical(round_in_mode(mode, ct, "mins"), base::round(ct, "mins"), info = mode)
    expect_identical(round_in_mode(mode, ct, units = "mins"), base::round(ct, units = "mins"), info = mode)
    expect_identical(round_in_mode(mode, dt, 1), base::round(dt, 1), info = mode)
    expect_identical(round_in_mode(mode, dt), base::round(dt), info = mode)
    expect_identical(round_in_mode(mode, factor(c("a", "b")) == "a"), base::round(c(TRUE, FALSE)), info = mode)
  }
})

test_that("digit= partial matching works (the chart translator emits round(col, digit=2))", {
  with_mode("half_up", expect_equal(exploratory::round(2.675, digit = 2), 2.68))
  with_mode("half_down", expect_equal(exploratory::round(2.675, digit = 2), 2.67))
  with_mode("half_up", expect_equal(exploratory::round(2.25, digit = 1), 2.3))
})

test_that("option fallback: unset / bogus / NA / length != 1 / NULL all behave as half_up", {
  # Unset.
  withr::with_options(list(exploratory.round_mode = NULL), {
    expect_equal(exploratory::round(2.5), 3)
    expect_equal(exploratory::round(-2.5), -3)
  })
  for (bad in list("bogus", NA, NA_character_, c("half_down", "half_even"), character(0), 5, TRUE, "")) {
    withr::with_options(list(exploratory.round_mode = bad), {
      expect_equal(exploratory::round(2.5), 3, info = paste(deparse(bad), collapse = ""))
      expect_equal(exploratory::round(2.675, digits = 2), 2.68, info = paste(deparse(bad), collapse = ""))
    })
  }
})

test_that("half_up matches janitor::round_half_up on ordinary values", {
  skip_if_not_installed("janitor")
  set.seed(1)
  x <- c(round(stats::rnorm(500, sd = 1000), 3), seq(-5, 5, by = 0.125))
  for (digits in c(0, 1, 2, 3)) {
    expect_equal(round_in_mode("half_up", x, digits = digits), janitor::round_half_up(x, digits), info = digits)
  }
})

test_that("large magnitudes: exact halves are not corrupted by float spacing", {
  # Above ~2^27 the sqrt(eps) fudge cannot be added to a scaled value without
  # losing it to float spacing, so the implementation must not rely on that.
  expect_equal(round_in_mode("half_down", 200000000.5), 200000000)
  expect_equal(round_in_mode("half_up", 200000000.5), 200000001)
  expect_equal(round_in_mode("half_down", -200000000.5), -200000000)
  expect_equal(round_in_mode("half_up", -200000000.5), -200000001)
  # At and above 2^52 every double is an integer: unchanged in every mode.
  big <- c(2^52 + 1, 2^53, 1e15 + 1, 1e300, -(2^52 + 1))
  for (mode in c("half_up", "half_down", "half_even")) {
    expect_identical(round_in_mode(mode, big), big, info = mode)
  }
})

test_that("dplyr::mutate in the global environment resolves round to the mask", {
  skip_if_not("package:exploratory" %in% search())
  df <- data.frame(x = c(0.5, 1.5, 2.5, -2.5))
  run <- function(mode) {
    withr::with_options(list(exploratory.round_mode = mode), {
      eval(quote(dplyr::mutate(df, y = round(x))), envir = list2env(list(df = df), parent = globalenv()))$y
    })
  }
  expect_equal(run("half_up"), c(1, 2, 3, -3))
  expect_equal(run("half_down"), c(0, 1, 2, -2))
  expect_equal(run("half_even"), c(0, 2, 2, -2))
})

# ---------------------------------------------------------------------------
# Guard: discovery-based. Every bare round( in R/*.R must be a known DISPLAY site.
# Computation sites must call base::round( so the user's rounding setting can
# never change a model. A new bare round( fails here and forces a decision.
# ---------------------------------------------------------------------------

# Display-only sites: number -> text for labels and report tables. They follow the
# user's rounding mode on purpose. file -> expected number of bare round( calls.
ROUND_DISPLAY_SITES <- c(
  "build_lm.R" = 1L,            # "Test (N%)" validation label
  "build_polr.R" = 1L,          # "Test (N%)" validation label
  "chaid.R" = 1L,               # format_chaid_distribution label
  "corresp_report.R" = 4L,      # report matrix coordinate / contribution / cos2 columns
  "factanal.R" = 2L,            # KMO / explained % display strings
  "google_cloud_storage.R" = 1L,# human readable file size
  "prcomp.R" = 3L,              # scale ratio / excluded % / variance % display strings
  "reliability.R" = 6L,         # alpha / CI / missing % display strings
  "stats_wrapper.R" = 1L,       # rows removed % display string
  "util.R" = 1L                 # format_cut_output range labels
)

find_bare_round_calls <- function(r_dir) {
  files <- list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE)
  hits <- list()
  for (f in files) {
    exprs <- tryCatch(parse(f, keep.source = TRUE), error = function(e) NULL)
    if (is.null(exprs)) next
    pd <- utils::getParseData(exprs)
    if (is.null(pd)) next
    terminals <- pd[pd$terminal, ]
    terminals <- terminals[order(terminals$line1, terminals$col1), ]
    idx <- which(terminals$text == "round" & terminals$token %in% c("SYMBOL_FUNCTION_CALL", "SYMBOL"))
    for (i in idx) {
      prev_token <- if (i > 1) terminals$token[i - 1] else ""
      if (prev_token %in% c("NS_GET", "NS_GET_INT", "'$'", "'@'")) next
      # `round <- function(...)` is the definition of the mask itself, not a call.
      next_token <- if (i < nrow(terminals)) terminals$token[i + 1] else ""
      if (next_token %in% c("LEFT_ASSIGN", "EQ_ASSIGN")) next
      hits[[length(hits) + 1]] <- data.frame(file = basename(f), line = terminals$line1[i], stringsAsFactors = FALSE)
    }
  }
  if (length(hits) == 0) return(data.frame(file = character(0), line = integer(0), stringsAsFactors = FALSE))
  do.call(rbind, hits)
}

test_that("guard: every bare round( in R/ is an allow-listed display site", {
  r_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir), "R/ sources are not available (installed-package test run)")
  hits <- find_bare_round_calls(r_dir)
  counts <- table(hits$file)
  found <- stats::setNames(as.integer(counts), names(counts))

  unexpected_files <- setdiff(names(found), names(ROUND_DISPLAY_SITES))
  if (length(unexpected_files) > 0) {
    detail <- hits[hits$file %in% unexpected_files, ]
    fail(paste0(
      "Bare round( in a file that is not an allow-listed display site. ",
      "If it affects computation use base::round(; if it only formats a number for display, add the file to ROUND_DISPLAY_SITES: ",
      paste0(detail$file, ":", detail$line, collapse = ", ")
    ))
  }
  for (f in names(ROUND_DISPLAY_SITES)) {
    actual <- if (f %in% names(found)) found[[f]] else 0L
    expect_equal(actual, ROUND_DISPLAY_SITES[[f]], info = paste("bare round( count in", f))
  }
})
