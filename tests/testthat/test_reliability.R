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

test_that("exp_cronbach_alpha auto-selects polychoric for ordered factors (Ordinal Alpha)", {
  df <- make_reliability_df() %>% dplyr::mutate(dplyr::across(dplyr::everything(), ~ ordered(.x)))

  model_df <- exp_cronbach_alpha(df, dplyr::everything(), correlation_method = "auto")
  expect_equal(model_df$model[[1]]$selected_method, "polychoric")

  res <- model_df %>% glance_rowwise(model, pretty.name = TRUE)
  expect_equal(res$Coefficient, "Ordinal Alpha")

  # Observed item-total correlation is available for every correlation method.
  res <- model_df %>% tidy_rowwise(model, type = "item_stats")
  expect_true(all(!is.na(res$`Item-Total Correlation`)))
  # Standardized item-total is still computed from the correlation matrix.
  expect_true(all(!is.na(res$`Standardized Item-Total`)))
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
  expect_false(is.na(res$`CI Lower`))
  expect_false(is.na(res$`CI Upper`))
  res <- model_df %>% tidy_rowwise(model, type = "item_stats")
  expect_true(all(!is.na(res$`Item-Total Correlation`)))

  summary <- model_df %>% tidy_rowwise(model, type = "summary")
  ci_row <- summary %>% dplyr::filter(Metric == "95% CI")
  expect_equal(nrow(ci_row), 1)
  expect_true(nzchar(ci_row$Value[[1]]))
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
