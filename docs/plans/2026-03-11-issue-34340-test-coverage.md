# Issue #34340: Add Missing Test Cases for Exploratory Package

## Context

GitHub issue [#34340](https://github.com/exploratory-io/tam/issues/34340) identifies that the exploratory package has 70% overall coverage with 224 uncovered functions (0%) and 285 partially covered functions. This plan covers ALL 15 highest-risk files listed in the issue, focusing on functions that can be tested without external services/credentials.

**Current state:** 1,019 test_that blocks across 91 test files.
**Test conventions:** testthat, no mocking framework, temp files for I/O, `expect_equal`/`expect_true`/`expect_error`, `skip_if_not_installed()` for optional deps.

---

## Scope: Testable vs Excluded

### Testable (~50 functions across 8 files)
Pure logic, data transformation, string manipulation, statistical calculations — no external APIs needed.

### Excluded (external service dependent, ~170 functions)
These files are almost entirely external-service-dependent and excluded from this plan:
- **R/oauth.R** (16 functions) — All require OAuth infrastructure
- **R/azure.R** (15 functions) — All require Azure credentials
- **R/aws_s3.R** (14 functions) — All require AWS credentials
- **R/google_cloud_storage.R** (15 functions) — All require Google auth
- **R/google_drive.R** (12 functions) — All require Google auth
- **R/google_analytics.R** (6 functions) — All require Google auth
- **R/salesforce.R** (5 functions) — All require Salesforce auth
- **R/get_mailchimp_data.R** (4 functions) — All require Mailchimp API
- **R/google_sheets.R** (6 of 7 uncovered) — Require Google auth (1 testable, included below)

---

## Task Breakdown by File

### File 1: R/system.R → tests/testthat/test_system.R (append)
**24 new test blocks for untested functions**

| # | Function | Type | Notes |
|---|----------|------|-------|
| 1 | `setTokenInfo` / `getTokenInfo` | env mgmt | get/set/overwrite/NULL |
| 2 | `geocode_us_state` | join | joins us_state_coordinates |
| 3 | `geocode_us_county` | join | joins us_county_coordinates by FIPS |
| 4 | `toDataFrame` (df input) | conversion | data.frame passthrough + type convert |
| 5 | `toDataFrame` (matrix input) | conversion | matrix → data.frame |
| 6 | `toDataFrame` (guessDataType=FALSE) | conversion | skip type conversion |
| 7 | `createTempEnvironment` | env mgmt | new env with globalenv parent |
| 8 | `glob_to_regex` | string | `*.csv` → regex, multi-pattern `*.csv\|*.tsv` |
| 9 | `exp_cut` (basic) | binning | numeric vector → factor |
| 10 | `exp_cut` (custom breaks) | binning | vector of break points |
| 11 | `exp_cut` (edge cases) | binning | empty, all-same, NAs |
| 12 | `exp_cut` (range options) | binning | lower/upper range, include.outside.range |
| 13 | `exp_cut_by_step` (basic) | binning | step size, auto-calc |
| 14 | `exp_cut_by_step` (range) | binning | custom range, outside range |
| 15 | `get_refs_in_script` | parsing | simple refs, pipe, assignment, invalid script |
| 16 | `toJSON` | JSON | roundtrip df → JSON → df |
| 17 | `convertFromJSON` (JSON file) | JSON | temp JSON file → df |
| 18 | `convertFromJSON` (NDJSON file) | JSON | temp NDJSON file → df |
| 19 | `isNDJSON` | JSON | NDJSON vs non-JSON detection |
| 20 | `getObjectListFromRdata` / `getObjectFromRdata` | file I/O | temp .Rdata with df + vec |
| 21 | `read_raw_lines` | file I/O | temp text file, skip/n_max |
| 22 | `read_log_file` | file I/O | temp log file, error on missing |
| 23 | `guess_csv_file_encoding` | file I/O | UTF-8 temp CSV |
| 24 | `searchAndReadParquetFiles` | file I/O | temp dir with parquet files, glob pattern, error cases |

### File 2: R/util.R → tests/testthat/test_util.R (append)
**13 new test blocks**

| # | Function | Type | Notes |
|---|----------|------|-------|
| 1 | `to_same_type` | type cast | factor, Date, POSIXct, numeric, character, logical |
| 2 | `excel_numeric_to_date` | date conversion | Known Excel serial dates (e.g., 44927 = 2022-12-31) |
| 3 | `excel_numeric_to_datetime` | datetime conversion | Excel serial datetime with timezone |
| 4 | `confint_radius` | stats | confidence interval radius at 0.95/0.99 |
| 5 | `prop_confint_radius` | stats | proportion CI radius |
| 6 | `get_score` | ML eval | accuracy/precision/recall from actual vs predicted |
| 7 | `get_optimized_score` | ML eval | threshold-based scoring |
| 8 | `extract_numeric` | string | extract numbers from mixed strings |
| 9 | `separate_japanese_address` | string | split Japanese addresses into prefecture/city/street |
| 10 | `likert_sigma` | stats | sigma for Likert scale |
| 11 | `unixtime_to_datetime` | date | Unix timestamp → datetime |
| 12 | `create_model_meta` | model | metadata from formula + df |
| 13 | `add_response` | model | add predictions column (simple lm) |

### File 3: R/test_wrapper.R → tests/testthat/test_test_wrapper_1.R (append)
**5 new test blocks**

| # | Function | Type | Notes |
|---|----------|------|-------|
| 1 | `generate_ttest_density_data` | stats viz | t-distribution density with critical regions |
| 2 | `generate_chisq_density_data` | stats viz | chi-square density |
| 3 | `generate_ftest_density_data` | stats viz | F-distribution density |
| 4 | `generate_norm_density_data` | stats viz | normal distribution density |
| 5 | `wilcox_norm_dist_sd` | stats | SD for Wilcoxon normal approx |

### File 4: R/build_lightgbm.R → tests/testthat/test_build_lightgbm.R (append)
**3 new test blocks**

| # | Function | Type | Notes |
|---|----------|------|-------|
| 1 | `has_special_json_chars` | string | detect JSON-breaking chars in names |
| 2 | `expand_lightgbm_metric_aliases` | mapping | metric alias expansion |
| 3 | `sanitize_lightgbm_colnames` | string | remove special chars from colnames |

### File 5: R/MarketMatching.R → tests/testthat/test_market_matching.R (new file)
**6 new test blocks**

| # | Function | Type | Notes |
|---|----------|------|-------|
| 1 | `lagp` | vector op | lag with padding |
| 2 | `CMean` | stats | mean filtering zeros |
| 3 | `stopif` | validation | conditional stop |
| 4 | `check_inputs` | validation | null checks, column existence |
| 5 | `mape_no_zeros` | stats | MAPE excluding zeros |
| 6 | `dw` | stats | Durbin-Watson statistic |

### File 6: R/google_sheets.R → tests/testthat/test_google_sheets.R (append or new)
**1 new test block**

| # | Function | Type | Notes |
|---|----------|------|-------|
| 1 | `normalizeDataForGoogleSheetsExport` | data transform | Inf → NA, difftime/period → numeric, integer64 → numeric |

### File 7: R/causal_impact.R → tests/testthat/test_causal_impact.R (new file)
**2 new test blocks**

| # | Function | Type | Notes |
|---|----------|------|-------|
| 1 | `glance.bsts` | model tidy | extract summary stats from mock bsts object |
| 2 | `tidy.bsts` | model tidy | convert bsts summary to tidy df |

---

## Execution Strategy

Since this involves 7 independent test files with no cross-dependencies, use **parallel subagents** — one per file group:

- **Agent A:** system.R tests (Tasks 1-24) — largest batch
- **Agent B:** util.R tests (Tasks 1-13)
- **Agent C:** test_wrapper.R + build_lightgbm.R + MarketMatching.R + google_sheets.R + causal_impact.R tests

Each agent appends tests to the appropriate file, runs `Rscript -e "devtools::test(filter='<test_file>')"` to verify, and commits.

---

## Verification

After all agents complete:

```bash
# Run all modified test files
Rscript -e "devtools::test(filter='test_system')"
Rscript -e "devtools::test(filter='test_util')"
Rscript -e "devtools::test(filter='test_test_wrapper')"
Rscript -e "devtools::test(filter='test_build_lightgbm')"
Rscript -e "devtools::test(filter='test_market_matching')"
Rscript -e "devtools::test(filter='test_google_sheets')"
Rscript -e "devtools::test(filter='test_causal_impact')"
```

Expected: All new tests pass, no regressions in existing tests.

**Total new test_that blocks:** ~50
**Functions newly covered:** ~50 (from 0% → >0%)
