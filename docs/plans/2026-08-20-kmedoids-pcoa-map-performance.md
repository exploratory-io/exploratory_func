# Design: K-Medoids Performance — the PCoA Cluster Map (tam#38004)

**Date:** 2026-08-20
**Status:** Proposed
**Scope:** `exploratory_func` `R/kmedoids.R` (`.kmedoids_map()`); no `tam` code change required
**Follows:** `docs/plans/2026-08-12-kmedoids-performance-improvement.md` (PR #1604)

## Summary

PR #1604 bounded the fit, the diagnostics and the map row count, and cached the map at
fit time. It never touched the cost of the PCoA itself. That is now essentially the whole
runtime: at the reported 3,600 x 24 scale, **`stats::cmdscale(..., add = TRUE)` is ~90% of
`exp_kmedoids()`**.

Measured end-to-end with `exploratory` 16.0.59, 3,600 rows x 10 selected Likert columns,
the exact argument set from the issue (`centers = 3, distance = "manhattan",
elbow_method_mode = "silhouette", max_centers = 10, silhouette_sample_size = 5000,
map_sample_size = 2000`):

| Stage | Elapsed |
| --- | --- |
| `cluster::pam()` main fit (k = 3, n = 3,600) | 0.23 s |
| Silhouette diagnostics (9 PAM fits, k = 2..10, n = 3,600) | 2.9 s |
| `stats::dist()` for the map (n = 2,000) | 0.01 s |
| **`stats::cmdscale(eig = TRUE, add = TRUE)` (n = 2,000)** | **~55 s** |
| `tidy(type = "distribution")` (36,000 rows), per call | 0.95 s |
| all other `tidy()` types | < 0.02 s each |
| **`exp_kmedoids()` total** | **60.8 s** |

## Root cause

`.kmedoids_map()` (R/kmedoids.R:474) calls:

```r
coordinates <- stats::cmdscale(distance_matrix, k = n_dimension, eig = TRUE, add = TRUE)
```

`add = TRUE` requests the Cailliez additive constant, which makes a non-Euclidean metric
(here Manhattan) embeddable with non-negative eigenvalues. To find it, `cmdscale()` builds
a **2n x 2n non-symmetric** block matrix and calls `eigen(Z, symmetric = FALSE)` on it.
For the map sample that is a 4,000 x 4,000 general eigenproblem — an order of magnitude
more expensive than the n x n symmetric decomposition classical MDS actually needs.

Cost is cubic in the map sample size, which is exactly why 600 rows feels fine and 3,600
does not — the map sample is `min(map_sample_size, valid_nrow)`:

| map sample n | `add = TRUE` | `add = FALSE` |
| --- | --- | --- |
| 600 | 2.0 s | 0.15 s |
| 1,000 | 7.2 s | 0.66 s |
| 1,500 | 23.3 s | 2.37 s |
| 2,000 | 64.3 s | 6.03 s |

So the user's 600-row case caps the map at 600 rows (2 s); the 3,600-row case saturates
`map_sample_size = 2000` (55–64 s). The row count of the *fit* is almost irrelevant.

This also explains the residual ~8 s per map call recorded in the #1604 follow-up note at
1,000 rows — that was this same `add = TRUE` eigenproblem, not `dist()`.

## Proposed change

Replace the `cmdscale(add = TRUE)` call in `.kmedoids_map()` with an in-house classical
PCoA on the doubly-centred matrix `B = -0.5 * J D^2 J`:

1. **Drop the Cailliez correction (`add = FALSE` semantics).** Classical PCoA on a
   non-Euclidean metric yields some negative eigenvalues; they are simply not projected
   onto. This is what `ape::pcoa()` and `vegan` report by default, and `.kmedoids_map()`
   already only ever consumes `coordinates$eig[coordinates$eig > 0]`. On clustered Likert
   data (n = 2,000, 10 vars) the coordinates are visually the same map:
   `cor(Dim1) = 0.998`, `cor(Dim2) = 0.994` against today's output.
2. **Take the top 2 eigenpairs with a randomized subspace iteration** instead of a full
   `eigen()`. `B` is n x n symmetric; a `(k + 12)`-column Gaussian sketch with 3 power
   iterations plus a QR and a small dense `eigen()` on the projected matrix reproduces the
   exact top-2 eigenvalues and eigenvectors to machine precision (`cor = 1.0000`,
   eigenvalues identical to 7+ significant digits in every trial run).
3. **Keep `representation_rate` exact** by taking the full spectrum via
   `eigen(B, symmetric = TRUE, only.values = TRUE)` (values only, no eigenvectors) for the
   `sum(eig[eig > 0])` denominator.

Prototype, n = 2,000, same data:

| Path | Elapsed | Top-2 eigenvalues | Dim1/Dim2 correlation vs `cmdscale(add=FALSE)` |
| --- | --- | --- | --- |
| `cmdscale(add = TRUE)` (today) | 55.4 s | — | 0.998 / 0.994 |
| `cmdscale(add = FALSE)` | 5.19 s | reference | 1 / 1 |
| **Proposed in-house PCoA** | **1.57 s** | identical to reference | **1 / 1** |

`exp_kmedoids()` at 3,600 x 10 goes from **60.8 s to roughly 7 s**, and the map stops
being the dominant term.

### Deliberate behaviour change: `representation_rate`

Dropping the additive constant changes the reported representation rate, because the
Cailliez constant inflates the total inertia. On the same fixture:

| Convention | Dim1 | Dim1+Dim2 |
| --- | --- | --- |
| today (`add = TRUE`, positive-eigenvalue denominator) | 5.1% | 7.8% |
| proposed (`add = FALSE`, positive-eigenvalue denominator) | 26.0% | 37.8% |

Today's numbers are not a better answer that we are trading away for speed — they are the
share of an inertia total that the correction term dominates, which is why a well-separated
3-cluster fixture reports a 5% first axis. The proposed value is the standard PCoA relative
eigenvalue. This is user-visible in the Cluster Map / Fitted Vectors axis titles that
`tam`'s `set_kmedoids_analytics_params()` reads from the `representation_rate` attribute;
no `tam` code change is needed, but the displayed percentage will rise. Call it out in the
release note.

## Implementation plan

### Phase 1 — replace the PCoA (the whole win)

In `R/kmedoids.R`, add an internal helper and call it from `.kmedoids_map()`:

```r
# Classical PCoA (Torgerson) of a distance object, returning the top `k` principal
# coordinates plus the full eigenvalue spectrum.
#
# stats::cmdscale(add = TRUE) is NOT used: the Cailliez constant it computes requires an
# eigendecomposition of a 2n x 2n NON-symmetric matrix, which is ~10x the cost of the n x n
# symmetric problem classical MDS needs and made the Cluster Map ~90% of exp_kmedoids()
# runtime at map_sample_size = 2000 (tam#38004: 55s of a 61s run). Manhattan distances are
# not Euclidean, so B has some negative eigenvalues; they are dropped, exactly as every
# consumer here already does (`eig[eig > 0]`).
.kmedoids_pcoa <- function(distance_matrix, k = 2L, seed = NULL) {
  d <- as.matrix(distance_matrix)
  n <- nrow(d)
  d2 <- d * d
  row_mean <- rowMeans(d2)
  b <- -0.5 * (d2 - row_mean[row(d2)] - row_mean[col(d2)] + mean(d2))
  # Randomized subspace iteration for the top k eigenpairs of the symmetric b.
  ...
  list(points = points, eig = eigenvalues)
}
```

- Oversampling `q = min(n - 1L, k + 12L)`, 3 power iterations. Seed it from `x$seed`
  (offset, like `.kmedoids_sample_indices()` does) so a fixed seed stays reproducible.
- Fall back to `stats::cmdscale(distance_matrix, k = k, eig = TRUE, add = FALSE)` when
  `n` is small (say `n < 50`, where the sketch has no headroom) or when the sketch produces
  a non-finite result. The fallback is cheap at those sizes.
- Preserve every existing downstream contract in `.kmedoids_map()`: the `n_dimension == 1`
  padding, `points` column names `Dim1`/`Dim2`, the `medoid_positions <- match(...)` lookup,
  the loadings/vector rows, and the `representation_rate` / `map_sample_size` /
  `map_sampled` attributes.

### Phase 2 — secondary, only if still warranted after Phase 1

- **`.kmedoids_distribution()` (R/kmedoids.R:445)**: `purrr::map_dfr()` over one tibble per
  row — 3,600 allocations for 36,000 output rows, 0.95 s per `tidy()` call. The whole thing
  is a `rep()`/`t()` reshape; a vectorized version measures 0.00 s and is a strict rewrite,
  no semantics change.
- **Silhouette diagnostics (2.9 s)**: nine PAM fits over the full 3,600 rows, because
  `silhouette_sample_size = 5000` never binds below the 5,000-row PAM cap. Passing a
  precomputed `stats::dist()` does **not** help (measured 3.4 s vs 2.9 s — the swap phase
  dominates, not the distance build). If this needs to come down, the lever is capping the
  diagnostic sample (e.g. 2,000 rows -> ~1 s), which changes diagnostic values and so needs
  a product decision. Recommend leaving it alone for now.
- **`.kmedoids_map_max_n`** is 5,000 while `tam` passes `map_sample_size = 2000`. After
  Phase 1 the exact-denominator `eigen(only.values = TRUE)` is ~1.4 s at n = 2,000 but
  ~28 s at n = 5,000, so the cap is still meaningful. Either lower `.kmedoids_map_max_n` to
  3,000, or switch the denominator to `sum(diag(B))` (O(n), a further convention change) if
  we ever want 5,000-row maps. Not needed for this issue.

## Test plan

Extend `tests/testthat/test_kmedoids.R` (existing map coverage is at lines 232–282):

- `.kmedoids_pcoa()` reproduces `stats::cmdscale(..., add = FALSE)` on a fixture: top-2
  eigenvalues within tolerance and `abs(cor())` of each axis ~ 1 (sign is free in PCoA —
  assert on `abs(cor())`, never on raw coordinates).
- Same seed -> byte-identical map; different seed for the row sample still yields a stable
  spectrum.
- Degenerate inputs keep returning the empty-map sentinel rather than erroring: all-tied
  rows, single valid row, one variable, `n_dimension == 1`.
- `representation_rate` remains a length-2 finite numeric, non-decreasing, `<= 1`, and is
  still attached to both `model$map_result` and `broom::tidy(model, type = 'map')`.
- Medoid rows are still present in the map (`row_type == 'medoid'`) and `map_variable_n`
  still bounds the `row_type == 'vector'` rows.
- A timing guard is deliberately not added to the unit tests; record before/after in the PR
  description instead.

## Acceptance criteria

1. `exp_kmedoids()` on 3,600 rows x 10 numeric columns with the issue's arguments completes
   in under ~10 s (from 60.8 s), on the same machine.
2. Cluster Map coordinates correlate > 0.99 with the current output on a clustered fixture.
3. `representation_rate` is exact for the new convention and the change is documented in the
   release note.
4. `testthat::test_file("tests/testthat/test_kmedoids.R")` passes with the new assertions.

## Risks

- **Visible percentage shift** in the map axis titles (see above). Product-visible; needs a
  release note, not a code change in `tam`.
- **Randomized eigensolver** is approximate in principle. Mitigated by the size of the
  spectral gap at k = 2 in this use, the oversampling, the power iterations, and the
  small-n / non-finite fallback to `cmdscale()`. Every trial so far matched the exact
  solution to machine precision.
- **Map coordinates change slightly** (cor 0.998 rather than 1.0) for saved analytics that
  are re-run. The map is a visualization, and axis signs already flip freely between runs.
