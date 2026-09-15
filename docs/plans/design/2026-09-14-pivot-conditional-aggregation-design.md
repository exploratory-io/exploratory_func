# Design: Conditional aggregation support in pivot()

## Status
In progress. Task 1 implemented (`value_condition`, string-only — see note below).

## Overview

`pivot()` (`R/util.R:950`, NSE wrapper `pivot_()` at `R/util.R:934`) currently
calls its aggregate function with exactly one positional argument:

```r
fun.aggregate(!!rlang::sym(value_col))
```

This means the `_if` conditional-aggregation family (`sum_if`, `count_if`,
`mean_if`, `median_if`, `min_if`, `max_if`, `count_unique_if`, and their
`_ratio`/`_pct` variants — `aggregate_if` dispatcher at `R/util.R:2816`) cannot
be used as `fun.aggregate` today, since they require a second `cond` argument
captured via `...` and resolved as a quosure inside the same
`dplyr::summarize()` NSE frame (`R/util.R:2817-2822`).

Per-caller use case: a per-value-column filter condition applied before
aggregating within each pivot cell (row × column group), while preserving
`_if`'s existing ratio/pct-vs-*unfiltered*-group-total semantics.

## Design

Add an optional `value_condition` argument to `pivot()`/`pivot_()`: a **string**
containing an R expression, parsed via `rlang::parse_expr()`. (An earlier
revision of this design considered also accepting an already-quoted
expression/quosure for direct R callers; implementation showed that path is
unreliable — a quosure's captured environment doesn't survive being
re-forwarded through `summarize_group()` → `dplyr::summarize()` →
`sum_if`/`aggregate_if`'s own internal `dplyr_quosures(...)` capture, so it was
dropped. `value_condition` is string-only.) Thread it through
`pivot_each()`'s `summarize_group()` call site (`R/util.R:1056`) so that when
`value_condition` is supplied, the aggregate call becomes:

```r
fun.aggregate(!!rlang::sym(value_col), !!condition_expr)
```

instead of:

```r
fun.aggregate(!!rlang::sym(value_col))
```

`count_if`'s single-argument form (`count_if(condition)`, no value column)
needs its own branch, mirroring the existing `value_col == NULL` handling
around `R/util.R:1043`.

### Why not filter-then-aggregate?

`dplyr::filter()`-ing rows before calling a plain aggregate function (e.g.
`sum`) would lose `_if`'s ratio/pct-vs-*unfiltered*-group-total semantics
(`sum_if_ratio`/`sum_if_pct`/etc. need the full group visible to compute the
denominator — see `aggregate_if`, `R/util.R:2838-2866`). Passing the condition
straight into the existing `aggregate_if` dispatcher reuses that logic
directly instead of reimplementing it.

## Files to Modify

- `R/util.R` — `pivot()`, `pivot_()`, `pivot_each()`: add `value_condition`
  formal, thread to the `summarize_group()` call site, branch for `count_if`'s
  1-arg form.
- `NAMESPACE` — no changes expected (no new exported function, only a new
  formal on an already-exported function).

## Testing Strategy (`tests/testthat/test_util.R`)

- `pivot()` combined with each `_if`/`_ratio`/`_pct` variant.
- NA values inside the condition expression's referenced column(s).
- Empty-post-filter pivot cell per function: `sum_if`→0, `mean_if`/
  `median_if`/`min_if`/`max_if`→`NA`/`Inf`/`-Inf` with the same warning
  behavior as calling the function on an empty vector today.
- `_ratio`/`_pct` variants: denominator is the unfiltered group count.
- Complex column name (spaces, multibyte characters, symbols) referenced by
  the condition expression.

## Out of Scope

- Changes to `row_funs`/`col_funs` bucket-function handling.
- Multi-value-column pivot support (`value` stays single-column).
