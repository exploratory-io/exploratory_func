# how to run this test:
# devtools::test(filter="reliability")
context("test Cronbach's Alpha function, exp_cronbach_alpha")

# Build a synthetic scale: 4 items on a 1-5 Likert scale correlated through a latent factor,
# with multibyte column names to exercise column escaping / base-R subsetting.
make_reliability_df <- function(n = 300, seed = 42) {
  set.seed(seed)
  latent <- rnorm(n)
  mk <- function(load, noise) {
    v <- load * latent + noise * rnorm(n)
    as.integer(cut(v, breaks = quantile(v, probs = seq(0, 1, length.out = 6)),
                   include.lowest = TRUE))
  }
  tibble::tibble(
    `現在の仕事はやりがいがある` = mk(1.0, 0.5),
    `仕事に自分の考えが反映されている` = mk(0.9, 0.6),
    `給与は納得できる水準だ` = mk(0.6, 1.0),
    `上司のフィードバックは的確だ` = mk(0.4, 1.2)
  )
}

test_that("exp_cronbach_alpha Pearson report sections", {
  df <- make_reliability_df()

  model_df <- exp_cronbach_alpha(df, dplyr::everything(), correlation_method = "pearson")

  res <- model_df %>% glance_rowwise(model, pretty.name = TRUE)
  expect_equal(colnames(res),
               c("Coefficient", "Alpha", "Standardized Alpha", "CI Lower", "CI Upper",
                 "G6", "Number of Variables", "Complete Responses", "Responses with Missing",
                 "Average Inter-item Correlation", "Correlation Type", "Interpretation"))
  expect_equal(res$Coefficient, "Cronbach's Alpha")
  expect_equal(res$`Correlation Type`, "pearson")
  # Alpha must match psych::alpha's raw_alpha on the same data.
  ref_alpha <- suppressWarnings(psych::alpha(as.data.frame(df)))$total$raw_alpha
  expect_equal(res$Alpha, ref_alpha, tolerance = 1e-6)
  expect_false(is.na(res$`CI Lower`))

  res <- model_df %>% tidy_rowwise(model, type = "summary")
  expect_equal(colnames(res), c("Metric", "Value", "Interpretation"))

  res <- model_df %>% tidy_rowwise(model, type = "alpha_if_dropped")
  expect_equal(colnames(res),
               c("Dropped Item", "Alpha if Dropped", "Difference from Current", "Interpretation"))
  # Sorted ascending by alpha-if-dropped.
  expect_equal(res$`Alpha if Dropped`, sort(res$`Alpha if Dropped`))
  expect_equal(nrow(res), 4)

  res <- model_df %>% tidy_rowwise(model, type = "item_stats")
  expect_equal(colnames(res),
               c("Variable", "Item-Rest Correlation", "Item-Total Correlation",
                 "Standardized Item-Total", "Corrected Correlation", "Rows", "Missing",
                 "Mean", "Standard Deviation", "Interpretation"))
  # Observed item-total correlation is populated for Pearson.
  expect_true(all(!is.na(res$`Item-Total Correlation`)))

  res <- model_df %>% tidy_rowwise(model, type = "correlation")
  expect_equal(colnames(res), c("variable_1", "variable_2", "correlation"))
  expect_equal(nrow(res), 16)

  res <- model_df %>% tidy_rowwise(model, type = "response_distribution")
  expect_equal(colnames(res), c("Variable", "Response", "Count", "Proportion"))

  res <- model_df %>% tidy_rowwise(model, type = "scale_candidates")
  expect_equal(colnames(res), c("Candidate", "Number of Items", "Alpha", "Recommendation"))

  res <- model_df %>% tidy_rowwise(model, type = "data")
  expect_true(all(colnames(df) %in% colnames(res)))
})

test_that("alpha-if-dropped and scale candidates use the reported alpha's estimator (#37175)", {
  df <- make_reliability_df()

  model_df <- exp_cronbach_alpha(df, dplyr::everything(), correlation_method = "pearson")
  reported_alpha <- (model_df %>% glance_rowwise(model, pretty.name = TRUE))$Alpha

  candidates <- model_df %>% tidy_rowwise(model, type = "scale_candidates")
  # "Use all items" always leads the table, and its alpha is the alpha we report.
  expect_equal(candidates$Candidate[[1]], "Use all items")
  expect_equal(candidates$Alpha[[1]], reported_alpha, tolerance = 1e-8)
  # Drop candidates stay sorted best-alpha-first behind the base row.
  if (nrow(candidates) > 2) {
    expect_equal(candidates$Alpha[-1], sort(candidates$Alpha[-1], decreasing = TRUE))
  }

  dropped <- model_df %>% tidy_rowwise(model, type = "alpha_if_dropped")
  # Each if-dropped alpha is the RAW alpha of the remaining items, not the
  # correlation-matrix (standardized) alpha, and the difference is measured
  # against the same reported alpha.
  for (i in seq_len(nrow(dropped))) {
    item <- dropped$`Dropped Item`[[i]]
    remaining <- as.data.frame(df[, setdiff(colnames(df), item), drop = FALSE])
    ref <- suppressWarnings(psych::alpha(remaining))$total$raw_alpha
    expect_equal(dropped$`Alpha if Dropped`[[i]], ref, tolerance = 1e-6)
    expect_equal(dropped$`Difference from Current`[[i]], ref - reported_alpha,
                 tolerance = 1e-6)
  }

  # Polychoric/mixed keep the correlation-matrix estimator, and stay consistent
  # with the ordinal alpha reported for them.
  ordinal_df <- df %>% dplyr::mutate(dplyr::across(dplyr::everything(),
                                                   ~ factor(.x, ordered = TRUE)))
  ordinal_model <- exp_cronbach_alpha(ordinal_df, dplyr::everything(),
                                      correlation_method = "polychoric")
  ordinal_reported <- (ordinal_model %>% glance_rowwise(model, pretty.name = TRUE))$Alpha
  ordinal_candidates <- ordinal_model %>% tidy_rowwise(model, type = "scale_candidates")
  expect_equal(ordinal_candidates$Candidate[[1]], "Use all items")
  expect_equal(ordinal_candidates$Alpha[[1]], ordinal_reported, tolerance = 1e-8)
})

test_that("correlation_method reports the method and why it was chosen (#37175)", {
  df <- make_reliability_df()

  auto_numeric <- exp_cronbach_alpha(df, dplyr::everything()) %>%
    tidy_rowwise(model, type = "correlation_method")
  expect_equal(colnames(auto_numeric), c("Item", "Value"))
  expect_equal(auto_numeric$Item, c("Correlation Type", "Primary Metric", "Selection Reason"))
  expect_equal(auto_numeric$Value,
               c("Pearson Correlation", "Cronbach's Alpha", "All variables are Numeric."))

  ordinal_df <- df %>% dplyr::mutate(dplyr::across(dplyr::everything(),
                                                   ~ factor(.x, ordered = TRUE)))
  auto_ordinal <- exp_cronbach_alpha(ordinal_df, dplyr::everything()) %>%
    tidy_rowwise(model, type = "correlation_method")
  expect_equal(auto_ordinal$Value,
               c("Polychoric Correlation", "Ordinal Alpha",
                 "All variables are Factor or Logical."))

  # An explicitly requested method reports the request as the reason.
  explicit <- exp_cronbach_alpha(df, dplyr::everything(), correlation_method = "mixed") %>%
    tidy_rowwise(model, type = "correlation_method")
  expect_equal(explicit$Value,
               c("Mixed Correlation", "Mixed-Correlation Alpha",
                 "Mixed Correlation was specified in the settings."))
})

test_that("response distribution uses Summary View numeric binning", {
  df <- list(
    low_cardinality = c(1L, 1L, 2L, 4L, 4L),
    continuous = seq(0, 100, length.out = 101),
    missing = rep(NA_real_, 5)
  )

  res <- reliability_response_distribution(df)
  low_cardinality <- res %>% dplyr::filter(variable == "low_cardinality")
  continuous <- res %>% dplyr::filter(variable == "continuous")
  missing <- res %>% dplyr::filter(variable == "missing")

  # Summary View keeps integer values as separate bars when the range is < 13,
  # including empty values inside the observed range.
  expect_equal(low_cardinality$response, c("1", "2", "3", "4"))
  expect_equal(low_cardinality$count, c(2L, 1L, 0L, 2L))

  # Summary View uses ten equal-width bins for continuous numeric data.
  expect_equal(nrow(continuous), 10)
  expect_equal(sum(continuous$count), 101L)
  expect_equal(sum(continuous$proportion), 1)
  expect_equal(nrow(missing), 0)
})

test_that("exp_cronbach_alpha auto-selects polychoric for ordered factors (Ordinal Alpha)", {
  df <- make_reliability_df() %>% dplyr::mutate(dplyr::across(dplyr::everything(), ~ ordered(.x)))

  model_df <- exp_cronbach_alpha(df, dplyr::everything(), correlation_method = "auto")
  expect_equal(model_df$model[[1]]$selected_method, "polychoric")

  res <- model_df %>% glance_rowwise(model, pretty.name = TRUE)
  expect_equal(res$Coefficient, "Ordinal Alpha")

  # Observed item-total correlation is available alongside the matrix-based
  # ordinal statistics.
  res <- model_df %>% tidy_rowwise(model, type = "item_stats")
  expect_true("Item-Total Correlation" %in% colnames(res))
  expect_true(all(!is.na(res$`Item-Total Correlation`)))
  # Standardized item-total is still computed from the correlation matrix.
  expect_true(all(!is.na(res$`Standardized Item-Total`)))
})

test_that("check_keys reverses reverse-worded items across report statistics", {
  df <- tibble::tibble(
    q1 = 1:10,
    q2 = 10:1,
    q3 = c(2, 3, 2, 4, 5, 6, 7, 8, 9, 10)
  )

  without_keys <- exp_cronbach_alpha(df, dplyr::everything(),
                                     correlation_method = "pearson",
                                     check_keys = FALSE)
  with_keys <- exp_cronbach_alpha(df, dplyr::everything(),
                                  correlation_method = "pearson",
                                  check_keys = TRUE)

  expect_gt(with_keys$model[[1]]$alpha, without_keys$model[[1]]$alpha)
  stats <- with_keys %>% tidy_rowwise(model, type = "item_stats")
  expect_true(all(!is.na(stats$`Item-Total Correlation`)))
  expect_true(all(!is.na(stats$`Item-Rest Correlation`)))
  expect_true(all(!is.na(with_keys$model[[1]]$alpha_if_deleted$alpha_if_dropped)))
})

test_that("exp_cronbach_alpha handles mixed correlation", {
  df <- make_reliability_df() %>%
    dplyr::mutate(
      `現在の仕事はやりがいがある` = ordered(`現在の仕事はやりがいがある`),
      `仕事に自分の考えが反映されている` = ordered(`仕事に自分の考えが反映されている`),
      `管理職である` = as.integer(dplyr::row_number() %% 2))

  model_df <- exp_cronbach_alpha(df, dplyr::everything(), correlation_method = "mixed")
  expect_equal(model_df$model[[1]]$selected_method, "mixed")
  res <- model_df %>% glance_rowwise(model, pretty.name = TRUE)
  expect_equal(res$Coefficient, "Mixed-Correlation Alpha")
  expect_true(is.na(res$`CI Lower`))
  expect_true(is.na(res$`CI Upper`))
  res <- model_df %>% tidy_rowwise(model, type = "item_stats")
  expect_true("Item-Total Correlation" %in% colnames(res))
  expect_true(all(!is.na(res$`Item-Total Correlation`)))

  summary <- model_df %>% tidy_rowwise(model, type = "summary")
  ci_row <- summary %>% dplyr::filter(Metric == "95% CI")
  expect_equal(nrow(ci_row), 0)
})

test_that("exp_cronbach_alpha errors with fewer than 2 variables", {
  df <- make_reliability_df() %>% dplyr::select(1)
  expect_error(exp_cronbach_alpha(df, dplyr::everything()),
               "at least 2 variables")
})

test_that("exp_cronbach_alpha rejects nominal (unordered, 3+ level) factor columns", {
  df <- make_reliability_df() %>%
    dplyr::mutate(`部署` = factor(rep(c("A", "B", "C"), length.out = dplyr::n())))
  expect_error(exp_cronbach_alpha(df, dplyr::everything(), correlation_method = "auto"),
               "nominal")
})

test_that("exp_cronbach_alpha treats all-factor Likert columns as ordinal under auto/polychoric", {
  df <- make_reliability_df() %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ factor(.x)))

  model_df <- exp_cronbach_alpha(df, dplyr::everything(), correlation_method = "auto")
  expect_equal(model_df$model[[1]]$selected_method, "polychoric")
  res <- model_df %>% glance_rowwise(model, pretty.name = TRUE)
  expect_equal(res$Coefficient, "Ordinal Alpha")
  expect_false(is.na(res$Alpha))

  model_df <- exp_cronbach_alpha(df, dplyr::everything(), correlation_method = "polychoric")
  expect_equal(model_df$model[[1]]$selected_method, "polychoric")
})

test_that("exp_cronbach_alpha retries polychoric with correct=0 on sparse tables", {
  set.seed(1)
  df <- tibble::tibble(
    a = ordered(sample(1:5, 12, replace = TRUE)),
    b = ordered(sample(1:5, 12, replace = TRUE)),
    c = ordered(sample(1:5, 12, replace = TRUE)),
    d = ordered(sample(1:5, 12, replace = TRUE)),
    e = ordered(sample(1:5, 12, replace = TRUE)),
    f = ordered(sample(1:5, 12, replace = TRUE)),
    g = ordered(sample(1:5, 12, replace = TRUE)),
    h = ordered(sample(1:5, 12, replace = TRUE))
  )

  # Default psych::polychoric(correct=0.5) fails on this sparse case with
  # "attempt to set 'rownames' on an object with no dimensions".
  model_df <- exp_cronbach_alpha(df, dplyr::everything(), correlation_method = "polychoric")
  expect_equal(model_df$model[[1]]$selected_method, "polychoric")
  expect_false(is.na(model_df$model[[1]]$alpha))
  expect_true(any(grepl("continuity correction of 0", model_df$model[[1]]$warnings)))
})

test_that("exp_cronbach_alpha drops constant columns and errors when fewer than 2 remain", {
  # One constant column is dropped; 3 valid items remain -> report still builds.
  df <- make_reliability_df() %>% dplyr::mutate(`定数` = 3L)
  model_df <- exp_cronbach_alpha(df, dplyr::everything(), correlation_method = "pearson")
  res <- model_df %>% tidy_rowwise(model, type = "item_stats")
  expect_false("定数" %in% res$Variable)
  expect_equal(nrow(res), 4)

  # Only one non-constant column left -> not enough columns.
  df2 <- tibble::tibble(a = 1:20, b = rep(1L, 20))
  expect_error(exp_cronbach_alpha(df2, dplyr::everything(), correlation_method = "pearson"),
               "not enough columns")
})

test_that("exp_cronbach_alpha works with group_by (per group model)", {
  df <- make_reliability_df() %>%
    dplyr::mutate(grp = rep(c("A", "B"), length.out = dplyr::n())) %>%
    dplyr::group_by(grp)

  model_df <- exp_cronbach_alpha(df, -grp, correlation_method = "pearson")
  res <- model_df %>% glance_rowwise(model, pretty.name = TRUE)
  expect_equal(nrow(res), 2)
})
