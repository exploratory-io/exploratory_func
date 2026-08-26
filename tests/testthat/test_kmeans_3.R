# how to run this test:
# devtools::test(filter="kmeans_3")
#
# Self-contained (no network) regression tests for two K-Means defects found by
# tam#38107's analytics harness. Both were verified to FAIL against the shipped
# 16.0.62 before the fix.

context("test kmeans settings that must reach the fit (tam#38107)")

test_that("build_kmeans.cols honours iter.max instead of a hardcoded 10", {
  # Lloyd needs three passes to converge on this data, so a one-iteration cap
  # must produce a DIFFERENT partition from an uncapped run. Hartigan-Wong is
  # deliberately not used: its inner optimization often reaches the same
  # partition in one pass, which cannot distinguish an honored cap from an
  # ignored one.
  set.seed(38107)
  n <- 300
  segment <- rep(1:3, length.out = n)
  df <- data.frame(
    a = rnorm(n, c(0, 6, 12)[segment], 1.2),
    b = rnorm(n, c(0, 5, 10)[segment], 1.2),
    c = rnorm(n, c(0, 4, 8)[segment], 1.2)
  )

  fit <- function(iter_max) {
    df %>%
      build_kmeans.cols(dplyr::everything(), centers = 3, iter.max = iter_max,
                        nstart = 1, algorithm = "Lloyd", normalize_data = TRUE,
                        keep.source = FALSE, augment = FALSE, seed = 1,
                        na.rm = FALSE) %>%
      dplyr::pull(model) %>% purrr::pluck(1)
  }

  capped <- suppressWarnings(fit(1))
  uncapped <- fit(50)

  # Before the fix these were byte-identical, because kmeans() was always
  # called with the literal iter.max = 10.
  expect_false(identical(capped$cluster, uncapped$cluster))
  expect_equal(capped$iter, 2)

  # The default is 10 either way, so default-settings behaviour must not move.
  expect_identical(fit(10)$cluster, uncapped$cluster)
})

test_that("iterate_kmeans caps k by distinct rows, so repeated rows do not abort the analytics", {
  # 60 rows, 4 distinct. stats::kmeans() refuses more centers than there are
  # distinct data points; capping on nrow() instead reached k = 5 and aborted
  # the whole exp_kmeans() call, losing every chart rather than just the elbow.
  pts <- data.frame(x = c(1, 1, 9, 9), y = c(1, 9, 1, 9))
  idx <- rep(seq_len(nrow(pts)), length.out = 60)
  df <- data.frame(x = pts$x[idx], y = pts$y[idx])

  res <- iterate_kmeans(df, max_centers = 10, normalize_data = TRUE, seed = NULL)
  expect_equal(res$center, 1:3)

  # A single distinct row must still ask for at least one centre: seq(0) is
  # c(1, 0), not an empty sequence.
  flat <- data.frame(x = rep(1, 5), y = rep(2, 5))
  expect_equal(iterate_kmeans(flat, max_centers = 10, normalize_data = TRUE, seed = NULL)$center, 1)

  # Data with no repeats is unaffected.
  set.seed(38107)
  wide <- data.frame(x = rnorm(200), y = rnorm(200))
  expect_equal(iterate_kmeans(wide, max_centers = 6, normalize_data = TRUE, seed = NULL)$center, 1:6)
})

test_that("elbow mode completes end to end on data with repeated rows", {
  pts <- data.frame(x = c(1, 1, 9, 9), y = c(1, 9, 1, 9))
  idx <- rep(seq_len(nrow(pts)), length.out = 60)
  df <- data.frame(x = pts$x[idx], y = pts$y[idx])

  model_df <- df %>% exp_kmeans(x, y, centers = 3, seed = 1,
                                elbow_method_mode = "elbow", max_centers = 10)
  elbow <- model_df %>% dplyr::pull(model) %>% purrr::pluck(1, "elbow_result")
  expect_equal(elbow$center, 1:3)
})
