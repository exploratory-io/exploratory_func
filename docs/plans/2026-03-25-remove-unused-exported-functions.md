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
| Final export count | 538 |
| Functions deleted (removed from R source entirely) | 5 |

### Key Deviation from Plan

The initial consumer-repo audit only searched `*.js` and `*.ts` files for `exploratory::funcname` patterns. Consumer repos reference functions in additional ways that were initially missed:
- **Bare strings** in JS/TS code (e.g., `case "build_lm":` in `CommandGeneratorBase.js`)
- **Plugin R scripts** (`public/lib/plugins/*/scripts/plugin.r`)
- **`extension.json` configs** referencing functions as `"function": "exploratory::name"`
- **JSON analysis templates** with inline R code snippets
- **R test scripts** in `tools/`

A full re-audit across all file types (`*.js`, `*.ts`, `*.R`, `*.r`, `*.json`) in tam, datablog, scheduler, and plugin directories, combined with a review of the package's own test suite, confirmed only **5 functions** are safe to remove.

### Functions Removed (5)

**Obsolete data source plugins:** `getTwitter`, `refreshTwitterToken`, `queryNeo4j`, `get_mailchimp_data`, `getGoogleTrends`

These five functions correspond to discontinued third-party integrations (Twitter, Neo4j, Mailchimp, Google Trends) with no active callers in any consumer repo or test file.

### Functions Retained

An additional 20 functions were initially identified as candidates for removal but were restored after further review:

- **Auth/token helpers** (`refreshGoogleTokenForAnalytics`, `refreshSalesforceToken`) — referenced in plugin configs.
- **Broom tidier aliases** (`augment_glm`, `augment_lm`, `glance_glm`, `glance_kmeans`, `glance_lm`, `tidy_glm`, `tidy_kmeans`, `tidy_lm`) — backward-compatibility wrappers with corresponding tests in the package test suite.
- **Statistical functions** (`calc_beta_prior`, `calculate_cohens_f_squared`, `do_kl_dist.kv_`, `do_princomp`, `model_confint`) — exercised by the package's own tests.
- **Misc helpers** (`any_error`, `get_num_errors`, `getConnectionPoolMode`, `getListOfColumns`, `pivot_`) — referenced in the package's own tests (e.g., `test_system.R` calls `getConnectionPoolMode()`).

### Test Suite Result

```
FAIL 0 | WARN 734 | SKIP 27 | PASS 1562
```

### R CMD Check

0 new ERRORs introduced. Pre-existing WARNINGs and NOTEs only.

### Test File Correction

An early failed attempt to remove 210 functions (later reverted) had incorrectly deleted 36 test files and modified 32 others. These were discovered during PR review and restored to master state. No test files are changed by this PR.

### Analytics Obsolescence Check

A follow-up comparison of exploratory_func analytics functions against all active analytics in Exploratory Desktop (tam) confirmed **no analytics functions are obsolete**. All underscore-variant functions (e.g. `do_roc_`, `do_survival_roc_`, `evaluate_binary_`, `evaluate_multi_`) are either directly referenced in tam analysis templates/JS or serve as internal implementation helpers for their exported NSE counterparts.
