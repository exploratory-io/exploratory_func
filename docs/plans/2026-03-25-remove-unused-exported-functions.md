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
| Final export count | 504 |
| Functions deleted (removed from R source entirely) | 39 |
| Functions kept (used in consumer repos) | 504 |

### Key Deviation from Plan

The initial consumer-repo audit only searched for `exploratory::funcname` call patterns. However, consumer repos (particularly tam) reference many functions as **bare strings** in generated R code (e.g., `case "build_lm":` in `CommandGeneratorBase.js`). This caused the initial analysis to misclassify 171 actively-used functions as unused.

A corrected audit searched for bare function name references and found that 171 of the 210 originally identified candidates are still actively called. Only **39 functions** were confirmed unused across all consumer repos and safely removed.

### Functions Removed (39)

`any_error`, `augment_glm`, `augment_lm`, `calc_beta_prior`, `calculate_cohens_f_squared`, `city_code_japan`, `do_kl_dist.kv_`, `do_princomp`, `evaluate_glm_training_and_test`, `exp_add_cluster_column`, `geocode_japan_city`, `get_intercom_data`, `get_mailchimp_data`, `get_num_errors`, `get_stripe_data`, `getConnectionPoolMode`, `getGoogleTrends`, `getListOfColumns`, `getTwitter`, `glance_glm`, `glance_kmeans`, `glance_lm`, `HttrOAuthToken1.0`, `HttrOAuthToken2.0`, `load_fred`, `model_confint`, `pivot_`, `queryMongoDB`, `queryNeo4j`, `querySalesforceDataFromTable`, `refreshGoogleTokenForAnalysis`, `refreshGoogleTokenForAnalytics`, `refreshGoogleTokenForBigQuery`, `refreshSalesforceToken`, `refreshTwitterToken`, `riem_stations_exp`, `tidy_glm`, `tidy_kmeans`, `tidy_lm`

### Test Suite Result

```
FAIL 0 | WARN 734 | SKIP 27 | PASS 1562
```

### R CMD Check

0 new ERRORs introduced. Pre-existing WARNINGs and NOTEs only.
