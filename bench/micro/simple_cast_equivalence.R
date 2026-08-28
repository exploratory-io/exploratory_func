# Equivalence harness for simple_cast().
#
# Compares the current simple_cast() in R/util.R against the previous
# implementation, which is extracted verbatim from a git revision (never
# retyped by hand), using identical() rather than all.equal().
#
# Usage (from the repository root):
#   git show <base-rev>:R/util.R > /tmp/util_old.R
#   Rscript bench/micro/simple_cast_equivalence.R /tmp/util_old.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(rlang)
})

args <- commandArgs(trailingOnly = TRUE)
old_util <- if (length(args) > 0) args[[1]] else "/tmp/util_old.R"
stopifnot(file.exists(old_util), file.exists("R/util.R"))

extract_fn <- function(path, fname) {
  src <- readLines(path)
  start <- grep(paste0("^", fname, " <- function"), src)
  stopifnot(length(start) == 1)
  end <- start - 1 + which(src[start:length(src)] == "}")[1]
  src[start:end]
}

eval(parse(text = extract_fn("R/util.R", "simple_cast")), envir = globalenv())
old_lines <- extract_fn(old_util, "simple_cast")
old_lines[1] <- sub("^simple_cast <- function", "simple_cast_old <- function", old_lines[1])
eval(parse(text = old_lines), envir = globalenv())

MULTIBYTE <- "航空 会社 !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表"

cases <- list()
add_case <- function(name, data, ...) {
  cases[[length(cases) + 1L]] <<- list(name = name, data = data, args = list(...))
}

base_df <- data.frame(
  r = c("a", "a", "b", "b", "c"),
  c = c("x", "y", "x", "z", "y"),
  v = c(1.5, 2.5, 3.5, 4.5, 5.5),
  stringsAsFactors = FALSE
)

add_case("basic", base_df, row = "r", col = "c", val = "v")
add_case("fill non-zero", base_df, row = "r", col = "c", val = "v", fill = -1)
add_case("fill NA", base_df, row = "r", col = "c", val = "v", fill = NA)
add_case("fun sum", base_df, row = "r", col = "c", val = "v", fun.aggregate = sum)
add_case("fun length", base_df, row = "r", col = "c", val = "v", fun.aggregate = length)
add_case("fun max", base_df, row = "r", col = "c", val = "v", fun.aggregate = max)

dup_df <- data.frame(
  r = c("a", "a", "a", "b"),
  c = c("x", "x", "y", "x"),
  v = c(1, 3, 5, 7),
  stringsAsFactors = FALSE
)
add_case("duplicates aggregated", dup_df, row = "r", col = "c", val = "v")
add_case("duplicates sum", dup_df, row = "r", col = "c", val = "v", fun.aggregate = sum)
add_case("duplicates length", dup_df, row = "r", col = "c", val = "v", fun.aggregate = length)

add_case("single row",
         data.frame(r = "a", c = c("x", "y"), v = c(1, 2), stringsAsFactors = FALSE),
         row = "r", col = "c", val = "v")
add_case("single col",
         data.frame(r = c("a", "b"), c = "x", v = c(1, 2), stringsAsFactors = FALSE),
         row = "r", col = "c", val = "v")
add_case("1x1",
         data.frame(r = "a", c = "x", v = 1, stringsAsFactors = FALSE),
         row = "r", col = "c", val = "v")

add_case("some NA values",
         data.frame(r = c("a", "a", "b"), c = c("x", "y", "x"),
                    v = c(1, NA, 3), stringsAsFactors = FALSE),
         row = "r", col = "c", val = "v")
add_case("some NA values na.rm",
         data.frame(r = c("a", "a", "b"), c = c("x", "y", "x"),
                    v = c(1, NA, 3), stringsAsFactors = FALSE),
         row = "r", col = "c", val = "v", na.rm = TRUE)
add_case("all NA values na.rm",
         data.frame(r = c("a", "b"), c = c("x", "y"),
                    v = c(NA_real_, NA_real_), stringsAsFactors = FALSE),
         row = "r", col = "c", val = "v", na.rm = TRUE)
add_case("NA in row key",
         data.frame(r = c("a", NA, "b"), c = c("x", "y", "x"),
                    v = c(1, 2, 3), stringsAsFactors = FALSE),
         row = "r", col = "c", val = "v")

add_case("shared row/col values",
         data.frame(r = c("a", "b", "b"), c = c("b", "a", "b"),
                    v = c(1, 2, 3), stringsAsFactors = FALSE),
         row = "r", col = "c", val = "v")

add_case("unsorted appearance order",
         data.frame(r = c("z", "a", "m"), c = c("q", "b", "d"),
                    v = c(1, 2, 3), stringsAsFactors = FALSE),
         row = "r", col = "c", val = "v")
add_case("sparse rectangular",
         data.frame(r = c("r1", "r2", "r3"), c = c("cB", "cD", "cA"),
                    v = c(1, 2, 3), stringsAsFactors = FALSE),
         row = "r", col = "c", val = "v")

add_case("factor row",
         data.frame(r = factor(c("b", "a", "a"), levels = c("b", "a", "unused")),
                    c = c("x", "y", "x"), v = c(1, 2, 3), stringsAsFactors = FALSE),
         row = "r", col = "c", val = "v")
add_case("factor col",
         data.frame(r = c("a", "b", "a"),
                    c = factor(c("y", "x", "x"), levels = c("y", "x", "unused")),
                    v = c(1, 2, 3), stringsAsFactors = FALSE),
         row = "r", col = "c", val = "v")
add_case("factor both",
         data.frame(r = factor(c("b", "a")), c = factor(c("y", "x")),
                    v = c(1, 2), stringsAsFactors = FALSE),
         row = "r", col = "c", val = "v")

add_case("integer values",
         data.frame(r = c("a", "b"), c = c("x", "y"),
                    v = c(1L, 2L), stringsAsFactors = FALSE),
         row = "r", col = "c", val = "v")
add_case("integer values integer fill",
         data.frame(r = c("a", "b"), c = c("x", "y"),
                    v = c(1L, 2L), stringsAsFactors = FALSE),
         row = "r", col = "c", val = "v", fill = 0L)
add_case("logical values",
         data.frame(r = c("a", "b"), c = c("x", "y"),
                    v = c(TRUE, FALSE), stringsAsFactors = FALSE),
         row = "r", col = "c", val = "v")

add_case("Date rows",
         data.frame(r = as.Date(c("2020-01-02", "2020-01-01")),
                    c = c("x", "y"), v = c(1, 2), stringsAsFactors = FALSE),
         row = "r", col = "c", val = "v")
add_case("POSIXct rows",
         data.frame(r = as.POSIXct(c("2020-01-02 03:00:00", "2020-01-01 04:00:00"), tz = "UTC"),
                    c = c("x", "y"), v = c(1, 2), stringsAsFactors = FALSE),
         row = "r", col = "c", val = "v")
add_case("numeric keys",
         data.frame(r = c(10, 2, 2), c = c(3, 1, 3),
                    v = c(1, 2, 3), stringsAsFactors = FALSE),
         row = "r", col = "c", val = "v")

mb_df <- data.frame(a = c("p", "q", "p"), b = c("u", "v", "v"),
                    v = c(1, 2, 3), stringsAsFactors = FALSE)
names(mb_df)[1] <- MULTIBYTE
add_case("multibyte row column name", mb_df, row = MULTIBYTE, col = "b", val = "v")
mb_df2 <- mb_df
names(mb_df2) <- c("a", MULTIBYTE, "v")
add_case("multibyte col column name", mb_df2, row = "a", col = MULTIBYTE, val = "v")
mb_vals <- data.frame(r = c(MULTIBYTE, "plain"), c = c("x", MULTIBYTE),
                      v = c(1, 2), stringsAsFactors = FALSE)
add_case("multibyte key values", mb_vals, row = "r", col = "c", val = "v")

set.seed(42)
big <- data.frame(
  r = sample(paste0("R", 1:30), 400, replace = TRUE),
  c = sample(paste0("C", 1:20), 400, replace = TRUE),
  v = runif(400), stringsAsFactors = FALSE
)
add_case("random 400 rows with duplicates", big, row = "r", col = "c", val = "v")
add_case("random 400 rows sum", big, row = "r", col = "c", val = "v", fun.aggregate = sum)

# val = NULL branch (untouched by this change, included so a regression there
# would still be caught).
add_case("val NULL", base_df, row = "r", col = "c", val = NULL)
add_case("val NULL fill 1", base_df, row = "r", col = "c", val = NULL, fill = 1)

run_one <- function(fn, case) {
  tryCatch(
    list(ok = TRUE, value = do.call(fn, c(list(data = case$data), case$args))),
    error = function(e) list(ok = FALSE, value = paste0("ERROR: ", conditionMessage(e)))
  )
}

n_same <- 0L
n_diff <- 0L
diffs <- character(0)
for (case in cases) {
  old <- suppressWarnings(suppressMessages(run_one(simple_cast_old, case)))
  new <- suppressWarnings(suppressMessages(run_one(simple_cast, case)))
  same <- identical(old$ok, new$ok) && identical(old$value, new$value)
  if (same) {
    n_same <- n_same + 1L
    cat(sprintf("  OK    %s\n", case$name))
  } else {
    n_diff <- n_diff + 1L
    diffs <- c(diffs, case$name)
    cat(sprintf("  DIFF  %s\n", case$name))
    cat("        old: "); str(old$value, max.level = 1)
    cat("        old dimnames: "); print(dimnames(old$value))
    cat("        new: "); str(new$value, max.level = 1)
    cat("        new dimnames: "); print(dimnames(new$value))
  }
}

cat(sprintf("\n%d / %d identical(), %d different\n", n_same, n_same + n_diff, n_diff))
if (n_diff > 0) cat("different cases: ", paste(diffs, collapse = ", "), "\n")
