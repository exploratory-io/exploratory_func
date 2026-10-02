# Equivalence harness for upper_gather().
#
# Compares the implementation on this branch against the verbatim previous
# implementation (extracted from the base revision, see OLD_IMPL below) on the
# six argument combinations used by the callers, plus degenerate and
# multibyte-name cases.
#
# Usage:
#   Rscript bench/micro/upper_gather_equivalence.R [path/to/old_impl.R]
#
# The old implementation file must define upper_gather_old() and mat_to_df().
# Produce it with:
#   git show <base-rev>:R/util.R > /tmp/util_old.R
# and copy mat_to_df() and upper_gather() (renamed to upper_gather_old) out of
# it verbatim. Do not retype either function from memory.

args <- commandArgs(trailingOnly = TRUE)
OLD_IMPL <- if (length(args) >= 1) args[1] else "/tmp/upper_gather_old_impl.R"

suppressMessages(library(Matrix))
suppressMessages(library(magrittr))

source(OLD_IMPL)                 # upper_gather_old(), mat_to_df()
source("R/util.R", local = FALSE) # upper_gather() under test (and mat_to_df, unchanged)

results <- list()

# Cases where the two implementations are known and intended to differ.
# The previous vector path routed through mat_to_df(), whose
# tibble::rownames_to_column() step rejects duplicated dimension names with
# "The `.data` argument of `add_column()` must have unique names". The new
# vector path never builds that intermediate frame, so duplicated names now
# produce the natural result instead of an error. Not returning an error is the
# point of the change, so these two are listed here rather than counted as
# regressions. Every other case must be identical().
EXPECTED_DIVERGENCE <- c("D22 vec duplicated names na=F zero=F",
                         "D23 vec duplicated names defaults")

check <- function(label, ...) {
  old <- try(upper_gather_old(...), silent = TRUE)
  new <- try(upper_gather(...), silent = TRUE)
  old_err <- inherits(old, "try-error")
  new_err <- inherits(new, "try-error")
  if (old_err || new_err) {
    same <- old_err && new_err &&
      identical(conditionMessage(attr(old, "condition")),
                conditionMessage(attr(new, "condition")))
    verdict <- if (same) "SAME-ERROR" else "ERROR-MISMATCH"
    cat(sprintf("%-14s %-58s old_err=%s new_err=%s\n", verdict, label, old_err, new_err))
    if (!same) {
      if (old_err) cat("   old: ", conditionMessage(attr(old, "condition")), "\n")
      if (new_err) cat("   new: ", conditionMessage(attr(new, "condition")), "\n")
    }
    results[[length(results) + 1L]] <<- list(label = label, ok = same)
    return(invisible(NULL))
  }
  ok <- identical(old, new)
  detail <- ""
  if (!ok) {
    ae <- all.equal(old, new)
    detail <- paste(utils::head(as.character(ae), 5), collapse = " ; ")
  }
  cat(sprintf("%-14s %-58s rows=%s\n", if (ok) "identical" else "DIFFERENT",
              label, nrow(new)))
  if (!ok) cat("   all.equal: ", detail, "\n")
  results[[length(results) + 1L]] <<- list(label = label, ok = ok)
  invisible(NULL)
}

# ---------------------------------------------------------------- vector path
set.seed(1)
m4 <- matrix(c(1, 2, 3, 4, 2, 1, 5, 1, 9, 3, 2, 4, 0, 1, 1, 1), nrow = 4, byrow = TRUE)
rownames(m4) <- c("a", "b", "c", "d")
d4 <- as.vector(stats::dist(m4))

m5 <- matrix(stats::runif(25), nrow = 5)
rownames(m5) <- c("n1", "n2", "n3", "n4", "n5")
d5 <- as.vector(stats::dist(m5))

# case 1: do_dist.kv_ / do_kl_dist.kv_ / do_dist.cols, distinct=TRUE diag=FALSE
check("C1 vec names diag=NULL na=F zero=F", d4, rownames(m4), diag = NULL,
      cnames = c("Var1", "Var2", "value"), na.rm = FALSE, zero.rm = FALSE)
# case 2: same callers, diag=TRUE -> diag=0
check("C2 vec names diag=0 na=F zero=F", d4, rownames(m4), diag = 0,
      cnames = c("Var1", "Var2", "value"), na.rm = FALSE, zero.rm = FALSE)
# case 3: the same shape with the caller specific cnames and names=NULL
check("C3 vec names=NULL diag=NULL na=F zero=F", d4, NULL, diag = NULL,
      cnames = c("p.x", "p.y", "value"), na.rm = FALSE, zero.rm = FALSE)
check("C3b vec names=NULL diag=0 na=F zero=F", d4, NULL, diag = 0,
      cnames = c("p.x", "p.y", "value"), na.rm = FALSE, zero.rm = FALSE)
check("C3c vec defaults na=T zero=T", d4, rownames(m4), diag = 0)
check("C3d vec defaults diag=NULL", d4, rownames(m4))
check("C3e vec n=5 na=F zero=F", d5, rownames(m5), diag = NULL,
      cnames = c("Var1", "Var2", "value"), na.rm = FALSE, zero.rm = FALSE)
check("C3f vec n=5 na=T zero=F", d5, rownames(m5), diag = 0,
      cnames = c("Var1", "Var2", "value"), na.rm = TRUE, zero.rm = FALSE)
check("C3g vec n=5 na=F zero=T", d5, rownames(m5), diag = 0,
      cnames = c("Var1", "Var2", "value"), na.rm = FALSE, zero.rm = TRUE)

# ---------------------------------------------------------------- matrix path
sim <- matrix(c(1, 0.5, 0, 0.2, 0.5, 1, 0.3, 0, 0, 0.3, 1, 0.9, 0.2, 0, 0.9, 1), nrow = 4)
dimnames(sim) <- list(c("a", "b", "c", "d"), c("a", "b", "c", "d"))

# case 4: do_cosine_sim.kv (the zero.rm=FALSE branch)
check("C4 mat diag=FALSE na=F zero=F", sim, rownames(sim), diag = FALSE,
      cnames = c("Var1", "Var2", "value"), na.rm = FALSE, zero.rm = FALSE)
check("C4b mat diag=NULL na=F zero=F", sim, rownames(sim), diag = NULL,
      cnames = c("Var1", "Var2", "value"), na.rm = FALSE, zero.rm = FALSE)
check("C4c mat diag=TRUE na=F zero=F", sim, rownames(sim), diag = TRUE,
      cnames = c("Var1", "Var2", "value"), na.rm = FALSE, zero.rm = FALSE)

sim_na <- sim
sim_na[3, 1] <- NA; sim_na[1, 3] <- NA; sim_na[2, 4] <- 0; sim_na[4, 2] <- 0
check("C4d mat with NA and 0, na=F zero=F", sim_na, rownames(sim_na), diag = FALSE,
      cnames = c("Var1", "Var2", "value"), na.rm = FALSE, zero.rm = FALSE)
check("C4e mat with NA and 0, na=T zero=F", sim_na, rownames(sim_na), diag = FALSE,
      cnames = c("Var1", "Var2", "value"), na.rm = TRUE, zero.rm = FALSE)

# case 5 / 6: pair_count_ (sparse co-occurrence, defaults)
cm <- Matrix::Matrix(c(3, 1, 0, 2, 1, 4, 0, 0, 0, 0, 2, 1, 2, 0, 1, 5), nrow = 4, sparse = TRUE)
dimnames(cm) <- list(c("a", "b", "c", "d"), c("a", "b", "c", "d"))
check("C5 dgCMatrix diag=FALSE defaults", cm, diag = FALSE, cnames = c("v.x", "v.y", "value"))
check("C6 dgCMatrix diag=TRUE defaults", cm, diag = TRUE, cnames = c("v.x", "v.y", "value"))
check("C6b dgCMatrix diag=FALSE na=F zero=F", cm, diag = FALSE,
      cnames = c("v.x", "v.y", "value"), na.rm = FALSE, zero.rm = FALSE)
check("C6c dgCMatrix diag=TRUE na=F zero=F", cm, diag = TRUE,
      cnames = c("v.x", "v.y", "value"), na.rm = FALSE, zero.rm = FALSE)
check("C6d dgCMatrix no dimnames na=F zero=F", Matrix::Matrix(matrix(1:16, 4), sparse = TRUE),
      diag = FALSE, na.rm = FALSE, zero.rm = FALSE)

# ------------------------------------------------------- degenerate and names
# n < 2
check("D1 vec length 0 (n=1) na=F zero=F", numeric(0), NULL, diag = NULL,
      cnames = c("Var1", "Var2", "value"), na.rm = FALSE, zero.rm = FALSE)
check("D2 vec length 0 (n=1) diag=0", numeric(0), NULL, diag = 0,
      cnames = c("Var1", "Var2", "value"), na.rm = FALSE, zero.rm = FALSE)
check("D3 vec length 0 defaults", numeric(0), NULL)
check("D4 vec length 1 (n=2) na=F zero=F", 2.5, c("x", "y"), diag = NULL,
      cnames = c("Var1", "Var2", "value"), na.rm = FALSE, zero.rm = FALSE)
check("D5 vec length 1 (n=2) defaults", 2.5, c("x", "y"))
# all values NA
check("D6 vec all NA na=F zero=F", rep(NA_real_, 6), letters[1:4], diag = NULL,
      cnames = c("Var1", "Var2", "value"), na.rm = FALSE, zero.rm = FALSE)
check("D7 vec all NA defaults", rep(NA_real_, 6), letters[1:4])
# all values 0
check("D8 vec all zero na=F zero=T", rep(0, 6), letters[1:4], diag = 0,
      cnames = c("Var1", "Var2", "value"), na.rm = FALSE, zero.rm = TRUE)
# name mismatch must still stop() with the same message
check("D9 vec wrong name count", d4, c("a", "b"), diag = NULL)
# 1x1 matrix path
one <- matrix(1, 1, 1, dimnames = list("a", "a"))
check("D10 mat 1x1 diag=FALSE defaults", one, diag = FALSE)
check("D11 mat 1x1 diag=TRUE defaults", one, diag = TRUE)
check("D12 mat 1x1 diag=FALSE na=F zero=F", one, diag = FALSE, na.rm = FALSE, zero.rm = FALSE)
check("D13 mat 1x1 diag=TRUE na=F zero=F", one, diag = TRUE, na.rm = FALSE, zero.rm = FALSE)
# single surviving pair (the is.vector(filtered) path)
two <- matrix(c(0, 7, 7, 0), 2, dimnames = list(c("a", "b"), c("a", "b")))
check("D14 mat 2x2 single pair defaults", two, diag = FALSE)
check("D15 mat 2x2 single pair na=F zero=F", two, diag = FALSE, na.rm = FALSE, zero.rm = FALSE)
# all-zero matrix, zero.rm=TRUE -> empty result
zmat <- matrix(0, 3, 3, dimnames = list(letters[1:3], letters[1:3]))
check("D16 mat all zero defaults (empty)", zmat, diag = FALSE)

# multibyte and near-colliding names (project convention stress name).
# Non ASCII here is test data only; the package sources stay ASCII.
stress <- c("航空 会社 !\"#$%&'()*+, -./:;<=>?@[]^_`{|}~ 表",
            "col", "col ", "col\n")
check("D17 vec multibyte names na=F zero=F", d4, stress, diag = NULL,
      cnames = c("Var1", "Var2", "value"), na.rm = FALSE, zero.rm = FALSE)
check("D18 vec multibyte names diag=0", d4, stress, diag = 0,
      cnames = c("Var1", "Var2", "value"), na.rm = FALSE, zero.rm = FALSE)
check("D19 vec multibyte names defaults", d4, stress, diag = 0)
sim_stress <- sim
dimnames(sim_stress) <- list(stress, stress)
check("D20 mat multibyte names na=F zero=F", sim_stress, stress, diag = FALSE,
      cnames = c("Var1", "Var2", "value"), na.rm = FALSE, zero.rm = FALSE)
check("D21 mat multibyte names defaults", sim_stress, stress, diag = FALSE)
# duplicated names
dup <- c("a", "a", "b", "c")
check("D22 vec duplicated names na=F zero=F", d4, dup, diag = NULL,
      cnames = c("Var1", "Var2", "value"), na.rm = FALSE, zero.rm = FALSE)
check("D23 vec duplicated names defaults", d4, dup, diag = 0)
sim_dup <- sim
dimnames(sim_dup) <- list(dup, dup)
check("D24 mat duplicated names na=F zero=F", sim_dup, dup, diag = FALSE,
      cnames = c("Var1", "Var2", "value"), na.rm = FALSE, zero.rm = FALSE)
# numeric-looking names
check("D25 vec numeric names na=F zero=F", d4, c(10, 20, 30, 40), diag = NULL,
      cnames = c("Var1", "Var2", "value"), na.rm = FALSE, zero.rm = FALSE)
# non square matrix on the matrix path
rect <- matrix(1:12, nrow = 4, dimnames = list(letters[1:4], c("p", "q", "r")))
check("D26 mat non square defaults", rect, diag = FALSE)
check("D27 mat non square na=F zero=F", rect, diag = FALSE, na.rm = FALSE, zero.rm = FALSE)
check("D28 mat non square diag=TRUE na=F zero=F", rect, diag = TRUE, na.rm = FALSE, zero.rm = FALSE)
rect2 <- matrix(1:12, nrow = 3, dimnames = list(letters[1:3], c("p", "q", "r", "s")))
check("D29 mat wide na=F zero=F", rect2, diag = FALSE, na.rm = FALSE, zero.rm = FALSE)
check("D30 mat wide diag=TRUE na=F zero=F", rect2, diag = TRUE, na.rm = FALSE, zero.rm = FALSE)

ok_n <- sum(vapply(results, function(r) isTRUE(r$ok), logical(1)))
cat(sprintf("\n%d / %d cases match\n", ok_n, length(results)))
diverged <- Filter(function(r) !isTRUE(r$ok), results)
expected <- Filter(function(r) r$label %in% EXPECTED_DIVERGENCE, diverged)
unexpected <- Filter(function(r) !(r$label %in% EXPECTED_DIVERGENCE), diverged)
if (length(expected)) {
  cat("expected divergence (old implementation errored, new one does not): ",
      paste(vapply(expected, function(r) r$label, character(1)), collapse = ", "), "\n")
}
if (length(unexpected)) {
  cat("FAILED: ", paste(vapply(unexpected, function(r) r$label, character(1)), collapse = ", "), "\n")
  quit(status = 1)
}
cat("no unexpected divergence\n")
