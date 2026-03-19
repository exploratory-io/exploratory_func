# Fix Paired Wilcoxon Test Producing Incorrect Results

**Issue:** [tam#34484](https://github.com/exploratory-io/tam/issues/34484)
**Date:** 2026-03-19

## Problem

When running a paired Wilcoxon signed-rank test through the Analytics View, the results differ significantly from running the equivalent test directly in R.

- **Analytics View:** W = 1830, p = 1.67e-11
- **Direct R:** V(W) = 328, p = 0.0507

## Root Cause

In `R/test_wrapper.R`, the `exp_wilcox()` function uses `Pair(value_col, category_col) ~ 1` for paired tests (line 1402). This was added as an R 4.4 compatibility fix, since R 4.4 removed support for `wilcox.test(value ~ category, paired = TRUE)`.

However, `Pair()` expects **two numeric columns in wide format**, not a numeric value column paired with a factor category column. When a factor is passed to `Pair()`, R coerces it to numeric codes (1, 2), resulting in a meaningless test of `value - as.numeric(factor)` across all rows.

W = 1830 = 60 × 61 / 2 confirms R is treating all 60 rows as independent observations rather than 30 paired differences.

## Relationship to #30927 (Paired t-test Fix)

Issue #30927 fixed the identical problem in `exp_ttest()`. The fix (lines 1086-1106) splits the data into two vectors by category and calls `t.test(vec1, vec2, paired = TRUE, ...)` directly, bypassing the formula interface entirely.

The `Pair()` formula at line 1003 in `exp_ttest` is kept only as a reference comment and is not used.

**`exp_wilcox()` was not updated with the same fix.**

## Fix

Apply the same 2-vector pattern from `exp_ttest()` to `exp_wilcox()`:

1. In `each_func`, before calling `wilcox.test`, check if `paired` is TRUE.
2. If paired, split `var1_col` into two vectors based on the two levels of `var2_col`.
3. Validate equal group sizes.
4. Call `wilcox.test(vec1, vec2, paired = TRUE, ...)` instead of `wilcox.test(formula, data = ...)`.

### Files Changed

- `R/test_wrapper.R` — Modify `exp_wilcox()` function

### Test Plan

- Add test using sample paired data (n=30 subjects, before/after measurements)
- Verify `exp_wilcox(..., paired = TRUE)` produces the same W and p-value as `wilcox.test(x, y, paired = TRUE)`

## Other Analytics Investigation

All test functions in `test_wrapper.R` with `paired` support were audited:

| Function | Status |
|----------|--------|
| `exp_ttest` | Fixed (#30927) |
| `exp_wilcox` | **Bug — this fix** |
| `exp_chisq` | No paired support — N/A |
| `exp_anova` | No paired support — N/A |
| `exp_kruskal` | No paired support — N/A |
| `exp_normality` | No paired support — N/A |
| `exp_ttest_power` | Power analysis only — N/A |
