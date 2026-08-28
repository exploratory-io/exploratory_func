# Stage-by-stage profile of the two paths that allocate n x n objects.
#
# Path 1: do_dist.kv_ equivalent  (subject x key -> dist -> long)
# Path 2: do_cmdscale_ equivalent (long -> n x n matrix -> as.dist -> cmdscale)
#
# Purpose: after upper_gather() stopped allocating dense n x n objects, find out
# which stage is now dominant, BEFORE changing simple_cast().
#
# Usage: Rscript bench/micro/simple_cast_profile.R [n1 n2 ...]
#
# Memory note: gc() inserts a "limit (Mb)" column when a memory limit is set,
# which shifts the column positions. "max used (Mb)" is always the LAST column,
# so this script uses g[, ncol(g)] and never a hard-coded column index.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(rlang)
})

# Run from the repository root, or set EXPLORATORY_REPO_ROOT.
repo_root <- Sys.getenv("EXPLORATORY_REPO_ROOT", unset = ".")
util_path <- file.path(repo_root, "R", "util.R")
stopifnot(file.exists(util_path))

# Source only the functions we need, verbatim, out of R/util.R.
# devtools::load_all() cannot be used here (35 Imports are not installable).
extract_fn <- function(src_lines, fname) {
  start <- grep(paste0("^", fname, " <- function"), src_lines)
  stopifnot(length(start) == 1)
  end <- start - 1 + which(src_lines[start:length(src_lines)] == "}")[1]
  src_lines[start:end]
}
util_lines <- readLines(util_path)
eval(parse(text = c(
  extract_fn(util_lines, "simple_cast"),
  extract_fn(util_lines, "mat_to_df"),
  extract_fn(util_lines, "upper_gather")
)), envir = globalenv())

peak_mb <- function() {
  g <- gc(reset = FALSE)
  sum(g[, ncol(g)])
}

timeit <- function(label, expr) {
  gc(reset = TRUE)
  t <- system.time(value <- force(expr))
  p <- peak_mb()
  cat(sprintf("    %-28s %8.3f s   peak %9.1f MB\n", label, t[["elapsed"]], p))
  list(label = label, sec = as.numeric(t[["elapsed"]]), peak = p, value = value)
}

make_kv_data <- function(n_subject, n_key, seed = 1) {
  set.seed(seed)
  data.frame(
    subject = rep(paste0("s", seq_len(n_subject)), each = n_key),
    key = rep(paste0("k", seq_len(n_key)), times = n_subject),
    value = runif(n_subject * n_key),
    stringsAsFactors = FALSE
  )
}

run_for_n <- function(n, n_key = 20) {
  cat(sprintf("\n=== N = %d (keys = %d) ===\n", n, n_key))
  df <- make_kv_data(n, n_key)
  res <- list(n = n)

  cat("  path 1 (do_dist.kv_ equivalent)\n")
  s1 <- timeit("1 simple_cast (key x subject)",
               simple_cast(df, "key", "subject", "value", fill = 0,
                           fun.aggregate = mean, na.rm = TRUE))
  mat <- t(s1$value)
  s2 <- timeit("2 stats::dist(t(mat))", stats::dist(mat, method = "euclidean"))
  d <- s2$value
  cnames <- c("subject.x", "subject.y", "value")
  s3 <- timeit("3 upper_gather",
               upper_gather(as.vector(d), rownames(mat), diag = NULL,
                            cnames = cnames, na.rm = FALSE, zero.rm = FALSE))
  long <- s3$value
  cat(sprintf("    -> long rows = %d\n", nrow(long)))
  res$p1 <- list(s1 = s1[c("sec", "peak")], s2 = s2[c("sec", "peak")],
                 s3 = s3[c("sec", "peak")], rows = nrow(long))
  rm(s1, s2, s3, d)

  cat("  path 2 (do_cmdscale_ equivalent)\n")
  t1 <- timeit("1 filter(!is.na(value))",
               dplyr::filter(long, !is.na(!!as.symbol(cnames[[3]]))))
  fdf <- t1$value
  cat(sprintf("    -> rows after NA filter = %d\n", nrow(fdf)))
  t2 <- timeit("2 simple_cast (N x N)",
               simple_cast(fdf, cnames[[1]], cnames[[2]], cnames[[3]],
                           fun.aggregate = mean, fill = 0, na.rm = TRUE))
  m2 <- t2$value
  cat(sprintf("    -> matrix dim = %d x %d\n", nrow(m2), ncol(m2)))
  t3 <- timeit("3 t(mat)", t(m2))
  tm <- t3$value
  t4 <- timeit("4 as.dist(t(mat))", stats::as.dist(tm))
  dd <- t4$value
  t5 <- timeit("5 cmdscale(k=2)", stats::cmdscale(dd, eig = FALSE, k = 2))
  res$p2 <- list(t1 = t1[c("sec", "peak")], t2 = t2[c("sec", "peak")],
                 t3 = t3[c("sec", "peak")], t4 = t4[c("sec", "peak")],
                 t5 = t5[c("sec", "peak")],
                 rows_filtered = nrow(fdf), dim = nrow(m2))

  total2 <- t1$sec + t2$sec + t3$sec + t4$sec + t5$sec
  res$p2$total <- total2
  res$p2$cast_share <- if (total2 > 0) t2$sec / total2 else NA_real_
  res$p2$cmdscale_share <- if (total2 > 0) t5$sec / total2 else NA_real_
  cat(sprintf("  path 2 total %.3f s | simple_cast share %.1f%% | cmdscale share %.1f%%\n",
              total2, 100 * res$p2$cast_share, 100 * res$p2$cmdscale_share))
  res
}

args <- commandArgs(trailingOnly = TRUE)
ns <- if (length(args) > 0) as.integer(args) else c(200L, 400L, 800L, 1600L)

all <- list()
for (n in ns) {
  r <- tryCatch(run_for_n(n),
                error = function(e) {
                  cat(sprintf("  *** FAILED at N=%d: %s\n", n, conditionMessage(e)))
                  list(n = n, failed = conditionMessage(e))
                })
  all[[as.character(n)]] <- r
  gc()
}

cat("\n==== summary (path 2 share) ====\n")
cat(sprintf("%6s %10s %10s %10s %10s %10s %10s\n",
            "N", "filter", "cast", "t()", "as.dist", "cmdscale", "cast%"))
for (r in all) {
  if (!is.null(r$failed)) {
    cat(sprintf("%6d  FAILED: %s\n", r$n, r$failed)); next
  }
  p <- r$p2
  cat(sprintf("%6d %10.3f %10.3f %10.3f %10.3f %10.3f %9.1f%%\n",
              r$n, p$t1$sec, p$t2$sec, p$t3$sec, p$t4$sec, p$t5$sec,
              100 * p$cast_share))
}
