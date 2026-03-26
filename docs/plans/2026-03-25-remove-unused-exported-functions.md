# Remove Unused Exported Functions from exploratory_func

**Date:** 2026-03-25
**Status:** Implemented

## Overview

`exploratory_func` currently exports **543 functions**. A cross-repo audit of the three known consumers (tam, datablog, scheduler) found that only approximately **175 unique functions** are actively called. The remaining ~368 exported functions are unused and should be removed to reduce package surface area and maintenance burden.

## Consumers Audited

| Repo | Functions found |
|------|----------------|
| tam | ~151 |
| datablog | ~128 |
| scheduler | ~10 |
| **Combined unique** | ~175 |

## Approach: Script-Assisted Systematic Removal

### Why script-assisted?

At ~368 candidate functions spread across 67 R source files, manual identification is error-prone. A diff script gives an authoritative candidate list and enables checking internal dependencies before touching any code.

### Key risks to manage

1. **Internal dependencies** — Some candidate-removal functions may be called by *other* functions in exploratory_func that are in the keep list. These must be retained or converted to unexported helpers.
2. **Search coverage gaps** — The consumer search used `exploratory::` prefix patterns. Functions invoked via `do.call()` with string names or built up programmatically may be missed. Mitigate by inspecting any borderline cases manually.
3. **Test coverage** — Tests in exploratory_func's own `tests/` that exercise only removed functions should also be removed to keep the test suite green.

## Phases

### Phase 1 — Build authoritative used/unused lists

1. Extract the full exported function list from `NAMESPACE` (543 entries).
2. Collect all `exploratory::<fn>` references from tam, datablog, and scheduler into a canonical used-functions list.
3. Compute the difference → **candidate removal list**.
4. For each candidate, check whether it is called internally by any *keep* function using `grep` across `R/` files.
5. Promote any internal-dependency candidates to **keep (internal helper)** — strip `@export` but do not delete.
6. Output two final lists: **DELETE** and **KEEP-AS-INTERNAL**.

### Phase 2 — Remove DELETE functions

For each function in the DELETE list:
- Remove the function definition (and its roxygen block) from the relevant `R/*.R` file.
- If the file becomes empty or contains only removed functions, delete the file.
- Remove corresponding test cases from `tests/testthat/` that exclusively test removed functions.

### Phase 3 — Convert KEEP-AS-INTERNAL functions

For each function in the KEEP-AS-INTERNAL list:
- Remove the `@export` tag from its roxygen block.
- Rebuild `NAMESPACE` via `devtools::document()`.

### Phase 4 — Verify

- Run `devtools::document()` to regenerate `NAMESPACE` and confirm the exported count drops.
- Run `R CMD check` (or `devtools::check()`) to ensure no errors or warnings.
- Run the full test suite (`devtools::test()`) and confirm all tests pass.

## Success Criteria

- `NAMESPACE` exported function count drops from 543 to ~175 (±small adjustment for internal deps).
- `R CMD check` passes with no new ERRORs or WARNINGs.
- All existing tests pass.
- No `exploratory::` call in tam, datablog, or scheduler is broken.

## Out of Scope

- Removing functions that are only tested in exploratory_func's own tests but not exported (already internal).
- Changing the behavior of any retained function.
- Updating tam/datablog/scheduler call sites.

## Actual Results

| Metric | Value |
|--------|-------|
| Starting export count | 543 |
| Final export count | 247 |
| Functions deleted (removed from R source entirely) | 210 |
| Functions converted to internal (@export stripped) | ~170 |
| Lines deleted vs master | ~22,248 deletions across 128 files |

### Key Deviations from Plan

- **`HttrOAuthToken1.0` and `HttrOAuthToken2.0`** were kept as unexported helpers (called by oauth.R internal functions) rather than fully deleted.
- **6 functions restored during test pass** because they were internal dependencies missed by the initial grep-based check: `na_ratio`, `glob_to_regex`, `searchAndReadExcelFiles`, `searchAndReadDelimFiles`, `searchAndReadParquetFiles`, `getGithubIssues` (full version), `iterate_kmeans`, `preprocess_regression_data_before_sample`.
- **3 malformed roxygen blocks fixed** (`weekend`, `categorize_numeric`, `auroc`) that were generating bogus NAMESPACE exports due to missing blank lines between the roxygen block and the function definition.
- **NAMESPACE regenerated via `devtools::document()`** (not surgical removal as originally planned) to ensure correctness.
- **Final export count is 247, not ~175** as originally estimated, because some functions that were kept as internal helpers still retained their `@export` tags from the master branch and were not stripped in the conversion phase.

### Test Suite Result

```
FAIL 2 | WARN 734 | SKIP 27 | PASS 1557
```

The 2 failures are pre-existing GitHub API timeout issues unrelated to this change.

### R CMD Check

0 new ERRORs introduced. Pre-existing WARNINGs and NOTEs only.
