# Micro benchmark for the upper_gather() dist vector path.
#
# Compares the implementation on this branch against the verbatim previous one
# on the argument combination the pairwise distance functions actually use
# (distinct = TRUE, so na.rm = FALSE and zero.rm = FALSE), for a range of n.
#
# Usage:
#   Rscript bench/micro/upper_gather_bench.R [path/to/old_impl.R] [n1,n2,...]
#
# See upper_gather_equivalence.R for how to produce the old implementation file.
# Peak memory is gc()'s "max used" across the call, in MB, which counts R heap
# only. Timings are elapsed seconds from system.time().

args <- commandArgs(trailingOnly = TRUE)
OLD_IMPL <- if (length(args) >= 1) args[1] else "/tmp/upper_gather_old_impl.R"
NS <- if (length(args) >= 2) as.integer(strsplit(args[2], ",")[[1]]) else c(500L, 1000L, 2000L, 4000L)

suppressMessages(library(Matrix))
suppressMessages(library(magrittr))
source(OLD_IMPL)
source("R/util.R", local = FALSE)

measure <- function(fn, ...) {
  invisible(gc(reset = TRUE, full = TRUE))
  t <- system.time(res <- fn(...))[["elapsed"]]
  g <- gc(full = TRUE)
  # "max used (Mb)" is always the LAST column of gc(), but its INDEX is not
  # fixed: gc() adds a "limit (Mb)" column when a memory limit is set (see
  # mem.maxVSize / R_MAX_VSIZE), which shifts column 6 from "max used (Mb)" to
  # "max used" in cells. Index from the end so both shapes report Mb.
  peak <- sum(g[, ncol(g)])
  list(sec = t, peak_mb = peak, rows = nrow(res))
}

cat(sprintf("%6s %6s %10s %10s %10s %10s %12s %12s %7s\n",
            "n", "dist_s", "old_s", "new_s", "old_peakMB", "new_peakMB",
            "old_rows", "new_rows", "speedup"))

for (n in NS) {
  set.seed(1)
  m <- matrix(stats::runif(n * 5), nrow = n)
  rownames(m) <- paste0("r", seq_len(n))

  invisible(gc(reset = TRUE, full = TRUE))
  t_dist <- system.time(d <- as.vector(stats::dist(m)))[["elapsed"]]

  old <- try(measure(upper_gather_old, d, rownames(m), diag = NULL,
                     cnames = c("Var1", "Var2", "value"), na.rm = FALSE, zero.rm = FALSE),
             silent = TRUE)
  if (inherits(old, "try-error")) {
    cat(sprintf("%6d %6.3f  OLD FAILED: %s\n", n, t_dist,
                conditionMessage(attr(old, "condition"))))
    old <- list(sec = NA_real_, peak_mb = NA_real_, rows = NA_integer_)
  }

  new <- try(measure(upper_gather, d, rownames(m), diag = NULL,
                     cnames = c("Var1", "Var2", "value"), na.rm = FALSE, zero.rm = FALSE),
             silent = TRUE)
  if (inherits(new, "try-error")) {
    cat(sprintf("%6d %6.3f  NEW FAILED: %s\n", n, t_dist,
                conditionMessage(attr(new, "condition"))))
    next
  }

  cat(sprintf("%6d %6.3f %10.3f %10.3f %10.1f %10.1f %12s %12s %6.1fx\n",
              n, t_dist, old$sec, new$sec, old$peak_mb, new$peak_mb,
              format(old$rows, big.mark = ","), format(new$rows, big.mark = ","),
              old$sec / new$sec))
}

# The matrix path with zero.rm = FALSE, which is what do_cosine_sim.kv uses.
# A sparse input is the interesting case: the previous code forced the sparse
# class to store all n^2 cells before calling which() on them.
cat("\nmatrix path, sparse input, na.rm = FALSE / zero.rm = FALSE (do_cosine_sim.kv)\n")
cat(sprintf("%6s %10s %10s %10s %10s %12s %12s %7s\n",
            "n", "old_s", "new_s", "old_peakMB", "new_peakMB", "old_rows", "new_rows", "speedup"))
for (n in NS) {
  set.seed(2)
  nnz <- max(1L, as.integer(n * n * 0.01))
  sm <- Matrix::sparseMatrix(
    i = sample.int(n, nnz, replace = TRUE),
    j = sample.int(n, nnz, replace = TRUE),
    x = stats::runif(nnz), dims = c(n, n),
    dimnames = list(paste0("r", seq_len(n)), paste0("r", seq_len(n))))

  old <- try(measure(upper_gather_old, sm, diag = FALSE,
                     cnames = c("Var1", "Var2", "value"), na.rm = FALSE, zero.rm = FALSE),
             silent = TRUE)
  if (inherits(old, "try-error")) {
    cat(sprintf("%6d  OLD FAILED: %s\n", n, conditionMessage(attr(old, "condition"))))
    old <- list(sec = NA_real_, peak_mb = NA_real_, rows = NA_integer_)
  }
  new <- try(measure(upper_gather, sm, diag = FALSE,
                     cnames = c("Var1", "Var2", "value"), na.rm = FALSE, zero.rm = FALSE),
             silent = TRUE)
  if (inherits(new, "try-error")) {
    cat(sprintf("%6d  NEW FAILED: %s\n", n, conditionMessage(attr(new, "condition"))))
    next
  }
  cat(sprintf("%6d %10.3f %10.3f %10.1f %10.1f %12s %12s %6.1fx\n",
              n, old$sec, new$sec, old$peak_mb, new$peak_mb,
              format(old$rows, big.mark = ","), format(new$rows, big.mark = ","),
              old$sec / new$sec))
}
