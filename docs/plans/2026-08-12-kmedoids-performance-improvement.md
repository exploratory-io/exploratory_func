# Design: K-Medoids Performance Improvement

**Date:** 2026-08-12
**Status:** In progress
**Scope:** `exploratory_func` and the paired `tam` K-Medoids Analytics template

## Implemented in this increment

- Added the R-side `algorithm = "auto" | "pam" | "clara"` dispatcher.
- Added FastPAM/CLARA fitting with compact model retention and a 5,000-row
  PAM safety threshold.
- Applied diagnostic and map row caps, vectorized distance-to-medoid
  calculation, and persisted algorithm metadata in `glance()` / `counts()`.
- Added the paired `tam` controls and command generation for algorithm and
  CLARA settings; removed the non-functional `iterMax` control while keeping
  the R argument for saved-command compatibility.

## Decision summary

Add an `algorithm` parameter, passed from the Analytics UI to
`exp_kmedoids()`, with these values:

| UI value | R value | Behaviour |
| --- | --- | --- |
| Auto (recommended) | `"auto"` | Use exact PAM for a small analysis data set and CLARA for a large one. |
| PAM (exact) | `"pam"` | Use PAM only. Reject an unsafe input size with an actionable error; never silently fall back. |
| CLARA (approximate) | `"clara"` | Use CLARA's sampled medoid search and assign all analysis rows. |

`"auto"` will be the default so existing saved Analytics commands that do
not contain the new argument keep working. The selected and effective
algorithm, sample sizes, and whether results are approximate will be kept in
the model and shown in the report metadata.

## Current state and problem

There is no algorithm-selection parameter today:

- `exp_kmedoids()` accepts no `algorithm` argument and
  `.kmedoids_fit()` always calls `cluster::pam()`.
- The paired `tam` template exposes sampling, distance, iteration, seed, and
  diagnostic settings, but no algorithm setting.

The current default is not viable for the UI's default 50,000-row sample:

1. `.kmedoids_fit()` passes `keep.diss = TRUE`. A packed double-precision
   distance vector for 50,000 rows has 1,249,975,000 elements and alone
   requires about **9.31 GiB**. PAM also needs additional quadratic working
   memory and computation.
2. The default `elbow_method_mode = "silhouette"` refits the model for every
   `k` from 2 through `max_centers` (default 10): nine additional PAM fits.
3. `silhouette_sample_size = 5000` is shown in the UI but is unused in R.
4. The Cluster Map runs `dist()` and `cmdscale()` over every analysis row. It
   has the same quadratic-memory problem and full PCoA does not scale to
   50,000 rows.
5. `iterMax` is validated but not passed to either `cluster::pam()` or
   `cluster::clara()`; it currently has no effect.

## Goals

- Make the default K-Medoids experience complete without memory exhaustion at
  the current 50,000-row UI sample cap.
- Preserve exact PAM for users and small data sets that need it.
- Make an approximate result explicit, deterministic for a fixed seed, and
  sufficiently documented to interpret correctly.
- Keep the existing report types and output columns stable wherever their
  semantics remain valid.
- Bound the diagnostic and map work independently of the model-fit row count.

## Non-goals

- Changing supported distance metrics (Euclidean and Manhattan remain the
  initial scope).
- Claiming that CLARA yields the identical medoids as exact PAM.
- Rendering all 50,000 observations in a PCoA map.
- Using `iterMax` as a fictitious PAM setting. Its UI contract must be
  corrected rather than silently retained.

## Proposed interface

### R API

Extend `exp_kmedoids()` with the following arguments. New arguments are
appended after the existing public arguments to avoid positional-command
compatibility issues.

```r
exp_kmedoids(
  ...,
  algorithm = c("auto", "pam", "clara"),
  clara_samples = 20,
  clara_sample_size = NULL,
  map_sample_size = 2000
)
```

- `algorithm`: selection requested by the user.
- `clara_samples`: number of CLARA candidate samples. The `cluster` package
  recommends substantially more than its legacy default of five; use 20 as a
  starting default and tune it in the benchmark.
- `clara_sample_size`: observations in each CLARA candidate sample. `NULL`
  selects a bounded internal default based on `centers`; expose it only as an
  advanced setting if tuning is needed.
- `map_sample_size`: maximum number of rows used for PCoA (including medoids,
  which are always included). It is
  independent of `silhouette_sample_size`, because full PCoA is much more
  expensive than silhouette calculation on the same number of rows.

Define one documented internal guard, initially
`KMEDOIDS_PAM_MAX_N = 5000`, subject to benchmark calibration:

```text
algorithm = auto:
  valid_nrow <= KMEDOIDS_PAM_MAX_N  -> PAM
  valid_nrow >  KMEDOIDS_PAM_MAX_N  -> CLARA

algorithm = pam:
  valid_nrow <= KMEDOIDS_PAM_MAX_N  -> PAM
  valid_nrow >  KMEDOIDS_PAM_MAX_N  -> error with guidance to select CLARA

algorithm = clara:
  -> CLARA
```

The guard is intentionally enforced for explicit PAM. Selecting "exact" must
not unexpectedly consume all available memory.

### Analytics UI and command generation

In `tam`'s `kmedoids.json`, add an **Algorithm** select after Random Seed and
map it to the new R argument:

```json
{
  "name": "algorithm",
  "displayName": "Algorithm",
  "type": "select",
  "defaultValue": "auto",
  "analysisExtraArgumentPosition": 6,
  "options": [
    { "displayName": "Auto (recommended)", "value": "auto" },
    { "displayName": "PAM (exact)", "value": "pam" },
    { "displayName": "CLARA (approximate)", "value": "clara" }
  ]
}
```

Add advanced CLARA controls, visible only when `algorithm = "clara"`, and a
**Map Sample Size** control under Cluster Map. The generated command must
include the selected values. Existing saved commands, which omit `algorithm`,
use `"auto"` in R.

The property-dialog help text must state:

- PAM is exact but limited to small data due to its quadratic distance work.
- CLARA is an approximation intended for larger data; its results depend on
  the seed and the sampling settings.
- Auto chooses safely based on the number of valid analysis rows.

## Implementation plan

### 1. Establish a canonical fit adapter

Refactor `R/kmedoids.R` so report code no longer assumes a PAM-specific
object (`id.med`, full-length `silinfo$widths`, and `pam` class).

- Create `.kmedoids_fit()` as a dispatcher and return a canonical list with
  `clustering`, `medoid_indices`, `medoids`, `objective`, optional
  `silhouette_widths`, `silhouette_row_indices`, `algorithm`, and
  `is_approximate`.
- PAM adapter: call `cluster::pam(..., keep.diss = FALSE, keep.data = FALSE,
  pamonce = 5)`. FastPAM improves swap performance while retaining PAM's
  exact-method branch.
- CLARA adapter: call `cluster::clara(..., samples = clara_samples,
  sampsize = clara_sample_size, keep.data = FALSE, rngR = TRUE)`. Normalize
  CLARA's `i.med` to `medoid_indices`.
- Record requested/effective algorithm, analysis-row count, and sampling
  metadata on the model.
- Update summary, profile, representative values, cohesion, data, and medoid
  detail tidiers to use this canonical result.

`keep.diss = FALSE` is safe for the existing basic outputs: PAM still returns
cluster assignments, medoids, objective values, and silhouette information,
but it no longer retains the large distance object after fitting.

### 2. Bound diagnostics and honour existing settings

Use `silhouette_sample_size` for the k=2..`max_centers` comparison:

- Draw one deterministic, representative diagnostic sample (including any
  required medoids where applicable), capped by `silhouette_sample_size`.
- Run FastPAM with `keep.diss = FALSE` for every candidate `k` on that sample
  and retain only the aggregate diagnostic metrics.
- Store `diagnostic_nrow` and show it in the Silhouette/Elbow report text.
- Do not run candidate fits when the user selects `elbow_method_mode = "none"`.

For a CLARA main fit, CLARA's own silhouette widths describe its sampled
observations, not every assigned row. Keep those row indices; do not align a
short width vector to all rows. Non-sampled rows receive `NA` for per-row
silhouette, and aggregate silhouette labels identify their sample size.

### 3. Make the Cluster Map scalable

For `tidy(type = "map")`:

- When the analysis data fit within `map_sample_size`, retain the present PCoA
  behavior.
- Otherwise select a deterministic row sample, always including medoids, and
  run `dist()` / `cmdscale()` only for those rows.
- Return `row_type = "observation"` only for displayed rows and attach map
  sample metadata. The UI text must label the map as a representative sample.
- Apply `map_variable_n`: rank variable vectors by their two-dimensional
  loading magnitude and return only the requested number.

This preserves a faithful PCoA visualization for the displayed rows without
pretending that a 50,000-row eigendecomposition is practical.

### 4. Remove secondary avoidable work

- Vectorize `.kmedoids_distance_to_medoids()` instead of iterating once per
  row in R.
- Compute valid-row indices and the unstandardized fit matrix once during
  fitting; reuse them in `tidy()` methods.
- Make `profile_top_n`, `profile_show_all`, and
  `profile_variable_order` effective before generating profile/distribution
  output, preventing unnecessary row-times-variable payloads.
- Resolve `iterMax` before release: remove it from the UI/API, or replace it
  with a real, documented parameter such as the number of randomized starts.
  Do not retain the present non-functional control.

## Compatibility and reporting rules

| Concern | Rule |
| --- | --- |
| Existing commands | Omitted `algorithm` means `"auto"`. |
| Existing small data | Auto selects PAM; retain current output schemas. |
| Explicit exact PAM on large data | Fail before allocating quadratic memory and explain how to use CLARA or reduce the analysis sample. |
| CLARA medoids | Return actual row IDs from the original sampled analysis data, just as PAM does. |
| Per-row silhouette in CLARA | Populate only sampled rows; do not report fabricated values for all assignments. |
| Summary/diagnostic values | Include method and row/sample count so approximate measures are not mistaken for full-data exact measures. |
| Saved report reproducibility | Persist algorithm and all sample/seed settings in the generated command. |

## Test plan

### `exploratory_func`

- Add PAM, CLARA, and Auto branch tests to `tests/testthat/test_kmedoids.R`.
- Verify `algorithm = "auto"` resolves to PAM below and CLARA above the
  threshold; test the explicit-PAM safety error.
- Verify both distance metrics, standardization, excluded rows, medoid row
  IDs, deterministic seed behavior, and all existing `tidy()` schemas.
- Verify CLARA silhouette indices are respected and non-sampled output rows
  have `NA` rather than misaligned silhouette values.
- Verify diagnostic and map sample caps, medoid inclusion in a map sample,
  and `map_variable_n` enforcement.
- Add a lightweight benchmark script outside ordinary unit tests. Measure
  fit, diagnostics, map, peak memory, and output size separately for 1k, 5k,
  10k, and 50k rows.

### `tam`

- Extend `KMedoidsAnalyticsTemplate.test.js` to assert the `algorithm` select,
  default `auto`, conditional CLARA controls, and generated R command.
- Add command-generation coverage for PAM and CLARA selections and for old
  commands that omit the parameter.
- Add localized UI strings and verify English/Japanese help text.
- Manually verify that the report identifies exact versus approximate fits and
  map/diagnostic samples.

## Acceptance criteria

1. A default 50,000-row K-Medoids run completes without retaining a
   9.31-GiB distance object or attempting a 50,000-row PCoA.
2. Users can choose Auto, exact PAM, or approximate CLARA in the Analytics
   dialog, and the selected value appears in the generated R command.
3. Auto is deterministic for a fixed seed and safely selects CLARA above the
   calibrated PAM threshold.
4. Silhouette and Elbow calculations respect `silhouette_sample_size`; the
   report discloses the diagnostic sample size.
5. Exact small-data outputs preserve the current report contract, and large
   approximate outputs accurately label their scope and limitations.
6. Benchmarks demonstrate the target improvement and tests cover both
   algorithm branches before release.

## Rollout order

1. Add the fit adapter and remove retained PAM distances.
2. Add the R `algorithm` API, Auto guard, and PAM/CLARA unit tests.
3. Add the `tam` controls, command-generation tests, and report wording.
4. Implement bounded diagnostics and scalable map sampling.
5. Apply secondary vectorization/output reductions, then run the benchmark
   matrix to calibrate thresholds and CLARA defaults.
