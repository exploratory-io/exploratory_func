context("tam#39839: explanatory variable 2-unique-values error names the actual count")

# exp_ttest, exp_wilcox and exp_two_sample_prop_test share the same
# "The explanatory variable needs to have 2 unique values." guard (hand-copied
# across the three, see tam#38168 comments in R/test_wrapper.R). Before this
# fix the raw error never said whether the explanatory variable had too FEW
# (1, needs less filtering) or too MANY (3+, needs more filtering or a
# multi-group test) unique values, nor what the actual count was -- customers
# saw the identical generic sentence in both situations (tam#39839). These
# tests pin the new, count-bearing wording for both situations, for each of
# the three wrappers.
#
# stop_explanatory_variable_needs_2_unique_values() is the shared (unexported)
# helper all three call sites now go through.

test_that("exp_ttest: 1 unique value in the explanatory variable reports the actual count", {
  df <- data.frame(val = seq(10), group = rep("A", 10))
  expect_error(
    exp_ttest(df, val, group),
    "The explanatory variable needs to have 2 unique values, but it has 1\\."
  )
})

test_that("exp_ttest: 3 unique values (no NA) in the explanatory variable reports the actual count", {
  df <- data.frame(val = seq(12), group = rep(c("A", "B", "C"), each = 4))
  expect_error(
    exp_ttest(df, val, group),
    "The explanatory variable needs to have 2 unique values, but it has 3\\."
  )
})

test_that("exp_wilcox: 1 unique value in the explanatory variable reports the actual count", {
  df <- data.frame(val = seq(10), group = rep("A", 10))
  expect_error(
    exp_wilcox(df, val, group),
    "The explanatory variable needs to have 2 unique values, but it has 1\\."
  )
})

test_that("exp_wilcox: 3 unique values (no NA) in the explanatory variable reports the actual count", {
  df <- data.frame(val = seq(12), group = rep(c("A", "B", "C"), each = 4))
  expect_error(
    exp_wilcox(df, val, group),
    "The explanatory variable needs to have 2 unique values, but it has 3\\."
  )
})

test_that("exp_two_sample_prop_test: 1 unique value in the explanatory variable reports the actual count", {
  df <- data.frame(outcome = c(TRUE, FALSE, TRUE, FALSE), group = rep("A", 4))
  expect_error(
    exp_two_sample_prop_test(df, outcome, group),
    "The explanatory variable needs to have 2 unique values, but it has 1\\."
  )
})

test_that("exp_two_sample_prop_test: 3 unique values (no NA) in the explanatory variable reports the actual count", {
  df <- data.frame(
    outcome = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    group = c("A", "B", "C", "A", "B")
  )
  expect_error(
    exp_two_sample_prop_test(df, outcome, group),
    "The explanatory variable needs to have 2 unique values, but it has 3\\."
  )
})

test_that("exp_two_sample_prop_test: 3 unique values WITH NA still auto-filters NA instead of erroring", {
  # Regression guard: the "3 distinct values, one of them NA" branch must
  # keep auto-filtering NA (unchanged behavior) rather than falling into the
  # new count-bearing stop(), which would wrongly tell the user they have 3
  # categories when they really have 2 plus missing data.
  df <- data.frame(
    outcome = c(rep(TRUE, 5), rep(FALSE, 5), rep(TRUE, 3), rep(FALSE, 7), TRUE, FALSE),
    group = c(rep("A", 10), rep("B", 10), NA, NA)
  )
  result <- exp_two_sample_prop_test(df, outcome, group)
  expect_true(!is.null(result$model[[1]]))
})

# tam#40039: a NA-masking gap in the guard above. R's dplyr::n_distinct()
# counts NA as a distinct value by default (na.rm = FALSE), so "1 real
# category + some NA rows" counted as 2 "distinct" values and slipped past
# the old `n_distinct(df[[col]]) != 2` guard completely undetected -- the NA
# rows were then silently dropped by the downstream as.factor()/fct_drop()
# conversion, leaving a true 1-level factor that crashed inside base R's own
# check with the raw, untranslated "grouping factor must have exactly 2
# levels" (from t.test.formula / wilcox.test.formula) instead of this
# file's friendly, count-bearing message. These tests pin that the real
# (non-NA) category count is reported and the friendly stop() still fires.

test_that("exp_ttest: 1 real category plus NA rows in the explanatory variable is still caught as 1, not masked as 2 (tam#40039)", {
  df <- data.frame(val = seq(10), group = c(rep("A", 7), NA, NA, NA))
  expect_error(
    exp_ttest(df, val, group),
    "The explanatory variable needs to have 2 unique values, but it has 1\\."
  )
})

test_that("exp_ttest: 3 real categories plus NA rows reports the real count of 3, not the NA-inflated count of 4 (tam#40039)", {
  df <- data.frame(val = seq(13), group = c(rep(c("A", "B", "C"), each = 4), NA))
  expect_error(
    exp_ttest(df, val, group),
    "The explanatory variable needs to have 2 unique values, but it has 3\\."
  )
})

test_that("exp_wilcox: 1 real category plus NA rows in the explanatory variable is still caught as 1, not masked as 2 (tam#40039)", {
  df <- data.frame(val = seq(10), group = c(rep("A", 7), NA, NA, NA))
  expect_error(
    exp_wilcox(df, val, group),
    "The explanatory variable needs to have 2 unique values, but it has 1\\."
  )
})

test_that("exp_two_sample_prop_test: 1 real category plus NA rows in the explanatory variable is still caught as 1, not masked as 2 (tam#40039)", {
  df <- data.frame(
    outcome = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE),
    group = c(rep("A", 4), NA, NA, NA)
  )
  expect_error(
    exp_two_sample_prop_test(df, outcome, group),
    "The explanatory variable needs to have 2 unique values, but it has 1\\."
  )
})

test_that("exp_ttest: 2 real categories plus NA rows still auto-filters NA instead of erroring (tam#40039 regression guard)", {
  # Same shape as the exp_two_sample_prop_test "3 unique values WITH NA"
  # guard above, mirrored for exp_ttest: 2 real groups + NA must keep
  # succeeding (NA dropped), not regress into a false "needs 2, has N" stop.
  df <- data.frame(val = seq(23), group = c(rep("A", 10), rep("B", 10), NA, NA, NA))
  result <- exp_ttest(df, val, group)
  expect_true(!is.null(result$model[[1]]))
})
