# Conditional Aggregation Support in pivot() — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task. Also consult the `r-coding-best-practices` and `r-code-execution` skills before running any R code.

**Goal:** Let `pivot()` call the `_if` conditional-aggregation family
(`sum_if`, `count_if`, `mean_if`, …) as its `fun.aggregate`, by threading an
optional `value_condition` argument through to the per-cell aggregate call.

**Architecture:** `pivot()`'s per-cell aggregation happens in `pivot_each()`
(`R/util.R:1039-1063`), which calls
`fun.aggregate(!!rlang::sym(value_col))` inside a `summarize_group()`-built
`dplyr::summarize()`. The `_if` family (`aggregate_if` dispatcher,
`R/util.R:2816`) needs a second `cond` argument passed as an *unevaluated
expression* in that same NSE frame — not a pre-computed logical vector — to
preserve its ratio/pct-vs-unfiltered-group-total semantics. Add `value_condition`
as a quoted-expression argument and splice it into the aggregate call only when
present.

**Tech Stack:** R (dplyr NSE / rlang tidyeval, testthat).

**Depends on / feeds into:** The `tam` repo's Pivot UI + codegen changes (see
that repo's `docs/plans/design/31867_plan.md`) will emit
`exp_pivot(..., value_condition = "<r-expr-string>")`. Confirm the exact
argument name/shape (string to be parsed, vs. already-quoted) matches what tam
emits before both sides are considered done — coordinate if either side needs
to change.

---

### Task 1: `value_condition` for the value_col-present + `_if` case

**Files:**
- Modify: `R/util.R` — `pivot()` (`:950`), `pivot_each()` (`:1039-1063`)
- Test: `tests/testthat/test_util.R` (append near the existing pivot tests at `:859-935`)

**Context — current relevant code (`R/util.R:1039-1057`):**
```r
pivot_each <- function(df) {
  res <- if(is.null(value_col)) {
    df %>% summarize_group(group_cols = group_cols_arg, group_funs = all_funs, "{value_col_name}" := dplyr::n())
  } else {
    if(na.rm &&
       !identical(na_ratio, fun.aggregate) &&
       !identical(non_na_ratio, fun.aggregate) &&
       !identical(na_pct, fun.aggregate) &&
       !identical(non_na_pct, fun.aggregate) &&
       !identical(na_count, fun.aggregate) &&
       !identical(non_na_count, fun.aggregate)){
      df <- df %>% dplyr::filter(!is.na(!!rlang::sym(value_col)))
    }
    df %>% summarize_group(group_cols = group_cols_arg, group_funs = all_funs, "{value_col_name}" := fun.aggregate(!!rlang::sym(value_col)))
  }
  ...
}
```

**Step 1: Write the failing test**

```r
test_that("pivot with sum_if applies condition per cell", {
  df <- tibble::tibble(
    grp = c("A", "A", "A", "B", "B", "B"),
    col = c("x", "x", "y", "x", "y", "y"),
    val = c(10, 20, 5, 7, 8, 9),
    flag = c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE)
  )
  res <- df %>% exploratory::pivot(
    row_cols = "grp", col_cols = "col", value = "val",
    fun.aggregate = exploratory::sum_if,
    value_condition = quote(flag),
    na.rm = TRUE
  )
  # Group A/col x: rows val=10 (flag=T), val=20 (flag=F) -> sum_if(flag) = 10
  expect_equal(res$x[res$grp == "A"], 10)
  # Group A/col y: val=5, flag=T -> 5
  expect_equal(res$y[res$grp == "A"], 5)
  # Group B/col x: val=7, flag=T -> 7
  expect_equal(res$x[res$grp == "B"], 7)
  # Group B/col y: val=8 (flag=T), val=9 (flag=F) -> 8
  expect_equal(res$y[res$grp == "B"], 8)
})
```

Adjust the exact `value_condition` argument shape (a `quote()`d expression here)
once Step 3's signature is settled — if the implementation instead accepts a
string parsed via `rlang::parse_expr()`, change the test to pass
`value_condition = "flag"` accordingly. Pick whichever shape is simpler for the
tam side to produce (a string is likely simpler for JS to emit than a quoted
call) — **decide this in Step 3 and update this test to match**, don't leave
the two mismatched.

**Step 2: Run test, confirm it fails**

Use the `r-code-execution` skill's guidance for running Rscript/testthat in
this repo. Typical invocation:
```r
devtools::load_all(".")
testthat::test_file("tests/testthat/test_util.R")
```
Confirm failure is "unused argument (value_condition = ...)" or similar —
i.e. failing because the feature doesn't exist yet, not because of a typo in
the test.

**Step 3: Implement**

Add `value_condition = NULL` to `pivot()`'s formals (`R/util.R:950`) and to
`pivot_()`'s pass-through (`R/util.R:934-936`). Inside `pivot_each()`, when
`value_condition` is supplied, parse it once (outside the per-group closure,
since it's the same for every cell) and splice it into the aggregate call:

```r
value_condition_expr <- if (!is.null(value_condition)) {
  if (is.character(value_condition)) rlang::parse_expr(value_condition) else rlang::enquo(value_condition)
}
...
pivot_each <- function(df) {
  res <- if(is.null(value_col)) {
    df %>% summarize_group(group_cols = group_cols_arg, group_funs = all_funs, "{value_col_name}" := dplyr::n())
  } else {
    if(na.rm && ...) {
      df <- df %>% dplyr::filter(!is.na(!!rlang::sym(value_col)))
    }
    if (!is.null(value_condition_expr)) {
      df %>% summarize_group(group_cols = group_cols_arg, group_funs = all_funs, "{value_col_name}" := fun.aggregate(!!rlang::sym(value_col), !!value_condition_expr))
    } else {
      df %>% summarize_group(group_cols = group_cols_arg, group_funs = all_funs, "{value_col_name}" := fun.aggregate(!!rlang::sym(value_col)))
    }
  }
  ...
}
```

Add a roxygen `@param value_condition` line near the existing `@param` block
(`R/util.R:938-949`).

**Step 4: Run test, confirm pass. Run the full pivot test block to confirm no regression:**
```r
testthat::test_file("tests/testthat/test_util.R")
```

**Step 5: Commit**
```bash
git add R/util.R tests/testthat/test_util.R
git commit -m "feat: add value_condition support to pivot() for _if aggregate functions

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>"
```

---

### Task 2: `value_condition` for the `count_if` + no-value-column case

**Files:** Same as Task 1.

**Context:** When `value_col` is `NULL` (user picked "count rows" instead of a
value column), `pivot_each()` currently hardcodes `dplyr::n()`
(`R/util.R:1040-1043`) and never calls `fun.aggregate` at all. `count_if`
(`R/util.R:2886`) takes the condition as its *first* positional argument (no
separate value column) — this is the "Number of Rows + count_if" case that
already exists in Summarize.

**Step 1: Write the failing test**
```r
test_that("pivot with count_if and no value column counts matching rows per cell", {
  df <- tibble::tibble(
    grp = c("A", "A", "A", "B", "B"),
    col = c("x", "x", "y", "x", "y"),
    flag = c(TRUE, FALSE, TRUE, TRUE, TRUE)
  )
  res <- df %>% exploratory::pivot(
    row_cols = "grp", col_cols = "col",
    fun.aggregate = exploratory::count_if,
    value_condition = quote(flag)
  )
  expect_equal(res$x[res$grp == "A"], 1) # 1 of 2 rows in A/x has flag=TRUE
  expect_equal(res$y[res$grp == "A"], 1)
  expect_equal(res$x[res$grp == "B"], 1)
})
```

**Step 2: Run, confirm it fails** (falls into the `dplyr::n()` branch, ignoring
`value_condition` entirely — assert the test fails with a wrong count, not an
error, so you can see the current buggy behavior before fixing it).

**Step 3: Implement**

In `pivot_each()`'s `is.null(value_col)` branch, check whether `fun.aggregate`
is a `count_if`-family function (compare via `identical()` against
`exploratory::count_if`, `count_if_ratio`, `count_if_pct`, matching the pattern
already used elsewhere in this file for `identical(fun.aggregate, na_ratio)`
etc. at `R/util.R:1046-1051`) and `value_condition_expr` is supplied; if so,
call `fun.aggregate(!!value_condition_expr)` instead of `dplyr::n()`.

**Step 4: Run tests, confirm pass.**

**Step 5: Commit**
```bash
git add R/util.R tests/testthat/test_util.R
git commit -m "feat: support count_if in pivot() when no value column is selected

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>"
```

---

### Task 3: Ratio/pct denominator correctness

**Files:** `tests/testthat/test_util.R`

**Step 1: Write failing (or, if Tasks 1-2 already make it pass, write and
confirm-passing — either is fine, this task is primarily test coverage) test:**
```r
test_that("pivot with sum_if_ratio computes ratio against the unfiltered group total", {
  df <- tibble::tibble(
    grp = c("A", "A", "A"),
    col = c("x", "x", "x"),
    val = c(10, 20, 30),
    flag = c(TRUE, FALSE, TRUE)
  )
  res <- df %>% exploratory::pivot(
    row_cols = "grp", col_cols = "col", value = "val",
    fun.aggregate = exploratory::sum_if_ratio,
    value_condition = quote(flag)
  )
  # sum_if_ratio = sum(val[flag]) / sum(val) = (10+30) / (10+20+30) = 40/60
  expect_equal(res$x[res$grp == "A"], 40/60)
})
```

**Step 2-4: Run, implement if needed (should already pass from Task 1's
generic wiring since `aggregate_if` already handles ratio/pct denominators
internally), confirm pass.**

**Step 5: Commit** (test-only, if no implementation change was needed):
```bash
git add tests/testthat/test_util.R
git commit -m "test: verify pivot sum_if_ratio uses unfiltered group total as denominator

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>"
```

---

### Task 4: Empty-post-filter group edge cases

**Files:** `tests/testthat/test_util.R`

**Step 1: Write tests** for a pivot cell where the condition matches zero rows,
one per representative function:
```r
test_that("pivot with sum_if and no matching rows in a cell returns 0", {
  df <- tibble::tibble(grp = c("A","A"), col=c("x","x"), val=c(10,20), flag=c(FALSE,FALSE))
  res <- df %>% exploratory::pivot(row_cols="grp", col_cols="col", value="val",
                                    fun.aggregate=exploratory::sum_if, value_condition=quote(flag))
  expect_equal(res$x[res$grp=="A"], 0)
})

test_that("pivot with mean_if and no matching rows in a cell returns NA with a warning", {
  df <- tibble::tibble(grp = c("A","A"), col=c("x","x"), val=c(10,20), flag=c(FALSE,FALSE))
  expect_warning(
    res <- df %>% exploratory::pivot(row_cols="grp", col_cols="col", value="val",
                                      fun.aggregate=exploratory::mean_if, value_condition=quote(flag))
  )
  expect_true(is.na(res$x[res$grp=="A"]))
})
```

**Step 2-4: Run, fix any implementation gap (e.g. NaN vs NA normalization —
check how `pivot()`'s existing `fill`/NA-type inference (`R/util.R:989-1028`)
treats a `mean_if` result of `NaN`; it may need `mean_if`/`median_if` mapped
into the same NA-type branch as `mean`/`median` at `R/util.R:1010-1017` if not
already covered generically), confirm pass.**

**Step 5: Commit**
```bash
git add R/util.R tests/testthat/test_util.R  # R/util.R only if a fix was needed
git commit -m "test: cover pivot _if functions with empty-post-filter groups

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>"
```

---

### Task 5: Complex column name stress test

**Files:** `tests/testthat/test_util.R`

**Step 1: Write a test** using a value column and/or a condition-referenced
column with a name containing spaces, multibyte characters, and symbols (repo
convention — see `r-coding-best-practices` skill and the equivalent existing
stress tests for `sum_if` under `summarize_group`, e.g.
`grep -n '航空' tests/testthat/test_util.R`):
```r
test_that("pivot with sum_if handles complex column names", {
  df <- tibble::tibble(
    grp = c("A", "A"),
    `航空 会社 !"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表` = c(10, 20),
    flag = c(TRUE, FALSE)
  )
  res <- df %>% exploratory::pivot(
    row_cols = "grp", value = "航空 会社 !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表",
    fun.aggregate = exploratory::sum_if, value_condition = quote(flag)
  )
  expect_equal(res$value, 10)
})
```
(Adjust to whatever `pivot()`'s actual single-value-no-col_cols output shape
is — check an existing `test_util.R` pivot test without `col_cols` for the
exact expected column name.)

**Step 2-4: Run, fix any escaping gap, confirm pass.**

**Step 5: Commit**
```bash
git add tests/testthat/test_util.R
git commit -m "test: pivot sum_if with complex/multibyte column names

Co-Authored-By: Claude Sonnet 5 <noreply@anthropic.com>"
```

---

### Sign-off

1. `git diff origin/master...HEAD` to review the full committed diff.
2. Update this plan's design doc (`2026-09-14-pivot-conditional-aggregation-design.md`)
   status to "Implemented", add an "Edge Cases Covered" list summarizing Tasks 3-5.
3. Confirm with the user the final `value_condition` argument shape (string vs.
   quoted expression) matches what the `tam` side's codegen emits — this is the
   cross-repo contract point most likely to drift if done independently.
4. Push:
```bash
git push -u origin feature/pivot-conditional-aggregation
```
