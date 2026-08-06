context("test build_polr")

make_ordinal_test_df <- function(n = 60, seed = 1) {
  set.seed(seed)
  # `年齢` (age) and `満足度` (an ordered 3-level satisfaction outcome) are named with
  # multibyte characters on purpose, per this repo's column-escaping stress-test convention
  # (workflow.md rule 7). A space + symbol column is included too.
  age <- round(stats::runif(n, 20, 60))
  score <- 0.08 * age + stats::rnorm(n)
  satisfaction <- cut(
    score,
    breaks = stats::quantile(score, probs = c(0, 1 / 3, 2 / 3, 1)),
    labels = c("Low", "Medium", "High"),
    include.lowest = TRUE
  )
  data.frame(
    `満足度` = factor(as.character(satisfaction), levels = c("Low", "Medium", "High"), ordered = TRUE),
    `年齢` = age,
    `部署 名!#` = rep(c("Sales", "Support", "Engineering"), length.out = n),
    weight = round(stats::runif(n, 1, 5), 2),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

test_that("build_polr fits and returns a model column", {
  df <- make_ordinal_test_df()
  trial <- df %>% build_polr(`満足度`, `年齢`, `部署 名!#`)

  expect_true("model" %in% colnames(trial))
  expect_true(inherits(trial$model[[1]], "polr_exploratory_0"))
  expect_true(inherits(trial$model[[1]], "polr"))
  expect_equal(length(trial$model[[1]]$lev), 3)
})

test_that("build_ordinal_regression is an alias for build_polr", {
  df <- make_ordinal_test_df()
  trial <- df %>% build_ordinal_regression(`満足度`, `年齢`, `部署 名!#`)
  expect_true(inherits(trial$model[[1]], "polr_exploratory_0"))
})

test_that("build_polr requires 3 or more target categories", {
  df <- make_ordinal_test_df()
  df$`満足度` <- factor(ifelse(df$`満足度` == "Low", "Low", "High"), levels = c("Low", "High"), ordered = TRUE)
  expect_error(
    df %>% build_polr(`満足度`, `年齢`),
    "3 or more categories"
  )
})

test_that("build_polr requires at least 1 predictor", {
  df <- make_ordinal_test_df()
  expect_error(df %>% build_polr(`満足度`), "At least 1 Predictor Variable is required")
})

test_that("build_polr coerces a character target into an ordered factor", {
  df <- make_ordinal_test_df()
  df$`満足度` <- as.character(df$`満足度`)
  trial <- df %>% build_polr(`満足度`, `年齢`)
  expect_true(is.ordered(trial$.train_data[[1]][["満足度"]]))
})

test_that("build_polr rejects a grouping column reused as a variable", {
  df <- make_ordinal_test_df()
  df$group1 <- rep(c("a", "b"), length.out = nrow(df))
  expect_error(
    df %>% build_polr(`満足度`, `年齢`, group1, group_cols = "group1"),
    "grouping column"
  )
})

test_that("build_polr supports group_cols (Repeat By)", {
  df <- make_ordinal_test_df(n = 90)
  df$region <- rep(c("East", "West", "North"), length.out = nrow(df))
  trial <- df %>% build_polr(`満足度`, `年齢`, group_cols = "region")
  expect_equal(sort(trial$region), sort(c("East", "North", "West")))
  expect_equal(nrow(trial), 3)
})

test_that("build_polr supports a weight column", {
  df <- make_ordinal_test_df()
  trial <- df %>% build_polr(`満足度`, `年齢`, weight = weight)
  expect_true(inherits(trial$model[[1]], "polr_exploratory_0"))
})

test_that("build_polr rejects a non-positive weight column", {
  df <- make_ordinal_test_df()
  df$weight[1] <- 0
  expect_error(df %>% build_polr(`満足度`, `年齢`, weight = weight), "Weight column must be positive")
})

test_that("build_polr splits training/test data when test_rate > 0", {
  df <- make_ordinal_test_df(n = 100)
  trial <- df %>% build_polr(`満足度`, `年齢`, `部署 名!#`, test_rate = 0.3, seed = 42)
  expect_true(!is.null(trial$.test_data[[1]]))
  expect_true(nrow(trial$.test_data[[1]]) > 0)
  expect_true(nrow(trial$.train_data[[1]]) + nrow(trial$.test_data[[1]]) <= nrow(df))
})

test_that("tidy.polr_exploratory_0 returns coefficient and intercept rows", {
  df <- make_ordinal_test_df()
  trial <- df %>% build_polr(`満足度`, `年齢`, `部署 名!#`)
  tidied <- tidy_rowwise(trial, model)

  expect_true(all(c("term", "estimate", "std.error", "statistic", "p.value", "coefficient_type") %in% colnames(tidied)))
  expect_true("odds.ratio" %in% colnames(tidied))
  expect_true(any(tidied$coefficient_type == "coefficient"))
  expect_true(any(tidied$coefficient_type == "intercept"))
  # Odds ratio is only meaningful for slope coefficients, not for the polr "intercepts" (thresholds).
  expect_true(all(is.na(tidied$odds.ratio[tidied$coefficient_type == "intercept"])))
  expect_true(all(!is.na(tidied$odds.ratio[tidied$coefficient_type == "coefficient"])))
  # Two ordered thresholds are expected for a 3-level target (Low|Medium and Medium|High).
  expect_equal(sum(tidied$coefficient_type == "intercept"), 2)
})

test_that("tidy.polr_exploratory_0 pretty.name renames columns", {
  df <- make_ordinal_test_df()
  trial <- df %>% build_polr(`満足度`, `年齢`, `部署 名!#`)
  tidied <- tidy_rowwise(trial, model, pretty.name = TRUE)
  expect_true(all(c("Term", "Coefficient", "Std. Error", "z value", "P Value", "Odds Ratio", "Type") %in% colnames(tidied)))
})

test_that("glance.polr_exploratory_0 returns fit statistics", {
  df <- make_ordinal_test_df()
  trial <- df %>% build_polr(`満足度`, `年齢`, `部署 名!#`)
  glanced <- glance_rowwise(trial, model)

  expect_true(all(c("n_classes", "nobs", "edf", "logLik", "AIC", "BIC", "deviance",
                     "df.residual", "null.deviance", "df.null", "mcfadden.r.squared") %in% colnames(glanced)))
  expect_equal(glanced$n_classes, 3)
  expect_true(is.finite(glanced$logLik))
  expect_true(is.finite(glanced$AIC))
  expect_true(is.finite(glanced$mcfadden.r.squared))
  # McFadden's Pseudo R-Squared is conventionally in [0, 1) for a converged model with signal.
  expect_true(glanced$mcfadden.r.squared >= 0 && glanced$mcfadden.r.squared < 1)
})

test_that("glance.polr_exploratory_0 pretty.name renames columns", {
  df <- make_ordinal_test_df()
  trial <- df %>% build_polr(`満足度`, `年齢`, `部署 名!#`)
  glanced <- glance_rowwise(trial, model, pretty.name = TRUE)
  expect_true(all(c("Number of Categories", "Rows", "Log Likelihood", "McFadden R-Squared") %in% colnames(glanced)))
})

test_that("augment.polr_exploratory_0 returns predicted class and per-class probabilities", {
  df <- make_ordinal_test_df()
  trial <- df %>% build_polr(`満足度`, `年齢`, `部署 名!#`)
  augmented <- augment_rowwise(trial, model)

  expect_true(".fitted" %in% colnames(augmented))
  expect_true(all(paste0("predicted_probability_", c("Low", "Medium", "High")) %in% colnames(augmented)))
  # Predicted probabilities across the 3 classes should sum to ~1 for every row.
  prob_cols <- paste0("predicted_probability_", c("Low", "Medium", "High"))
  row_sums <- rowSums(augmented[, prob_cols])
  expect_true(all(abs(row_sums - 1) < 1e-6))
  expect_true(all(as.character(augmented$.fitted) %in% c("Low", "Medium", "High")))
})

test_that("evaluate_polr reports training accuracy", {
  df <- make_ordinal_test_df(n = 100)
  trial <- df %>% build_polr(`満足度`, `年齢`, `部署 名!#`)
  ev <- evaluate_polr(trial, data = "training")

  expect_equal(colnames(ev), c("Data Type", "Rows", "Accuracy Rate", "Misclass. Rate"))
  expect_equal(ev$`Data Type`, "Training")
  expect_true(ev$Rows > 0)
  expect_true(ev$`Accuracy Rate` >= 0 && ev$`Accuracy Rate` <= 1)
  expect_equal(ev$`Accuracy Rate` + ev$`Misclass. Rate`, 1)
})

test_that("evaluate_polr reports training and test accuracy when test_rate > 0", {
  df <- make_ordinal_test_df(n = 150)
  trial <- df %>% build_polr(`満足度`, `年齢`, `部署 名!#`, test_rate = 0.3, seed = 7)
  ev <- evaluate_polr(trial, data = "training_and_test")

  expect_equal(sort(ev$`Data Type`), c("Test", "Training"))
  expect_true(all(ev$Rows > 0))
})

test_that("evaluate_polr_one_model is a pure, directly-callable helper", {
  df <- make_ordinal_test_df(n = 60)
  trial <- df %>% build_polr(`満足度`, `年齢`, `部署 名!#`, test_rate = 0.25, seed = 3)
  model <- trial$model[[1]]
  train_data <- trial$.train_data[[1]]
  test_data <- trial$.test_data[[1]]

  ev <- evaluate_polr_one_model(model, train_data, test_data, "満足度")
  expect_equal(sort(ev$`Data Type`), c("Test", "Training"))

  ev_train_only <- evaluate_polr_one_model(model, train_data, NULL, "満足度", data_types = "Training")
  expect_equal(nrow(ev_train_only), 1)
  expect_equal(ev_train_only$`Data Type`, "Training")
})

test_that("build_polr works with Repeat By (group_cols) end to end through tidy/glance", {
  df <- make_ordinal_test_df(n = 90)
  df$region <- rep(c("East", "West"), length.out = nrow(df))
  trial <- df %>% build_polr(`満足度`, `年齢`, group_cols = "region")

  glanced <- glance_rowwise(trial, model)
  expect_equal(nrow(glanced), 2)
  expect_true(all(glanced$n_classes == 3))

  tidied <- tidy_rowwise(trial, model)
  expect_true(all(c("region", "term", "estimate") %in% colnames(tidied)))
})
