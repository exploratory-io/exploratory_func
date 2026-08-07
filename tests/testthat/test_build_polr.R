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

# --- Report diagnostics: VIF / permutation importance / partial dependence ----
# These feed the 多重共線性 / 説明変数の重要度 / 説明変数の影響度 sections of the
# Analytics report, mirroring what Logistic Regression shows (tam#4453).

test_that("tidy(type='vif') returns one row per predictor with the ORIGINAL column names", {
  df <- make_ordinal_test_df(n = 120)
  trial <- df %>% build_polr(`満足度`, `年齢`, `部署 名!#`)

  vif_df <- tidy_rowwise(trial, model, type = "vif")
  # The `term` column comes back only because build_polr sets terms_mapping --
  # vif_to_dataframe()'s trailing `x$terms_mapping[term]` silently DROPS the
  # column when the map is missing. Assert the column AND its values.
  expect_true("term" %in% colnames(vif_df))
  expect_true("VIF" %in% colnames(vif_df))
  expect_setequal(vif_df$term, c("年齢", "部署 名!#"))
  expect_true(all(vif_df$VIF >= 0))
  # Two near-independent predictors: VIF must be close to 1, never NA.
  expect_false(any(is.na(vif_df$VIF)))
})

test_that("tidy(type='vif') reports a high VIF for a deliberately collinear predictor", {
  df <- make_ordinal_test_df(n = 200)
  set.seed(11)
  df$`年齢 コピー` <- df$`年齢` * 0.95 + stats::rnorm(nrow(df), sd = 0.5)
  trial <- df %>% build_polr(`満足度`, `年齢`, `年齢 コピー`, `部署 名!#`)

  vif_df <- tidy_rowwise(trial, model, type = "vif")
  collinear <- vif_df$VIF[vif_df$term %in% c("年齢", "年齢 コピー")]
  expect_true(all(collinear > 10))
  expect_true(vif_df$VIF[vif_df$term == "部署 名!#"] < 10)
})

test_that("tidy(type='vif') returns an empty frame instead of failing with a single predictor", {
  df <- make_ordinal_test_df(n = 80)
  trial <- df %>% build_polr(`満足度`, `年齢`)
  # VIF is undefined for one term; the group is skipped, not errored, so a
  # Repeat By run with one bad group still renders the others.
  expect_equal(nrow(tidy_rowwise(trial, model, type = "vif")), 0)
})

test_that("tidy(type='importance') ranks predictors and carries a P value per variable", {
  df <- make_ordinal_test_df(n = 150)
  trial <- df %>% build_polr(`満足度`, `年齢`, `部署 名!#`)

  imp <- tidy_rowwise(trial, model, type = "importance")
  expect_true(all(c("variable", "importance", "p.value") %in% colnames(imp)))
  expect_setequal(imp$variable, c("年齢", "部署 名!#"))
  # Importance is clamped at 0 and sorted descending.
  expect_true(all(imp$importance >= 0))
  expect_equal(imp$importance, sort(imp$importance, decreasing = TRUE))
  # P values are matched by PREFIX against the model terms; a categorical
  # predictor contributes one term per level and takes the smallest.
  expect_true(all(!is.na(imp$p.value)))
  expect_true(all(imp$p.value >= 0 & imp$p.value <= 1))
})

test_that("tidy(type='importance') returns a structured empty frame for a single predictor", {
  df <- make_ordinal_test_df(n = 80)
  trial <- df %>% build_polr(`満足度`, `年齢`)
  imp <- tidy_rowwise(trial, model, type = "importance")
  expect_equal(nrow(imp), 0)
  # Structured (not bare data.frame()) so a caller can arrange(desc(importance)).
  expect_true(all(c("variable", "importance", "p.value") %in% colnames(imp)))
})

test_that("lm_partial_dependence() produces a faceted table keyed by the ORIGINAL column names", {
  df <- make_ordinal_test_df(n = 150)
  trial <- df %>% build_polr(`満足度`, `年齢`, `部署 名!#`)

  pd <- trial %>% lm_partial_dependence()
  # x_name survives only because terms_mapping exists (handle_partial_dependence
  # ends with `x$terms_mapping[x_name]`), and the chart facets on it.
  expect_true(all(c("x_name", "x_value", "y_name", "y_value", "chart_type", "x_type") %in% colnames(pd)))
  expect_true(nrow(pd) > 0)
  expect_setequal(unique(pd$x_name), c("年齢", "部署 名!#"))
  # Multiclass: one predicted-probability series per ordered target level.
  expect_setequal(unique(as.character(pd$y_name)), c("Low", "Medium", "High"))
  expect_true(all(pd$y_value >= 0 & pd$y_value <= 1))
  # A numeric predictor draws a line, a categorical one draws points.
  expect_equal(unique(pd$chart_type[pd$x_name == "年齢"]), "line")
  expect_equal(unique(pd$chart_type[pd$x_name == "部署 名!#"]), "scatter")
})

test_that("partial dependence survives a column name full of regex/mmpf-hostile symbols", {
  # mmpf::marginalPrediction cannot handle such names directly (a comma alone
  # breaks it), which is why partial_dependence.polr_exploratory renames columns
  # before handing the data over. Without that, this silently returns 0 rows.
  set.seed(5)
  n <- 150
  stress <- "航空 会社 !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表"
  df <- data.frame(
    `満足度` = factor(sample(c("Low", "Medium", "High"), n, TRUE),
                      levels = c("Low", "Medium", "High"), ordered = TRUE),
    v = stats::rnorm(n),
    g = factor(sample(c("X", "Y", "Z"), n, TRUE)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  names(df)[2] <- stress
  trial <- df %>% build_polr(`満足度`, !!rlang::sym(stress), g)

  pd <- trial %>% lm_partial_dependence()
  expect_true(nrow(pd) > 0)
  expect_setequal(unique(pd$x_name), c(stress, "g"))

  vif_df <- tidy_rowwise(trial, model, type = "vif")
  expect_setequal(vif_df$term, c(stress, "g"))

  imp <- tidy_rowwise(trial, model, type = "importance")
  expect_setequal(imp$variable, c(stress, "g"))
})

test_that("default tidy() output is unchanged by the new type= argument", {
  df <- make_ordinal_test_df(n = 100)
  trial <- df %>% build_polr(`満足度`, `年齢`, `部署 名!#`)
  tidied <- tidy_rowwise(trial, model)
  # Same columns the coefficient table has always returned.
  expect_true(all(c("term", "estimate", "std.error", "statistic", "p.value",
                    "coefficient_type", "conf.low", "conf.high", "odds.ratio") %in% colnames(tidied)))
  expect_true(any(tidied$coefficient_type == "intercept"))
  expect_true(any(tidied$coefficient_type == "coefficient"))
})

test_that("tidy() joins the reference (base) level for each categorical predictor's dummy term", {
  df <- make_ordinal_test_df(n = 150)
  trial <- df %>% build_polr(`満足度`, `年齢`, `部署 名!#`)
  tidied <- tidy_rowwise(trial, model, pretty.name = TRUE)

  expect_true("Base Level" %in% colnames(tidied))
  dummy_rows <- tidied[tidied$Term %in% paste0("部署 名!#", c("Support", "Engineering")), ]
  expect_true(nrow(dummy_rows) > 0)
  # "Sales" is first in the factor's level order (make_ordinal_test_df), so it is
  # the reference level dropped by treatment contrasts.
  expect_true(all(dummy_rows$`Base Level` == "Sales"))

  # A numeric predictor and the intercept (threshold) rows have no base level.
  expect_true(is.na(tidied$`Base Level`[tidied$Term == "年齢"]))
  expect_true(all(is.na(tidied$`Base Level`[tidied$Type == "intercept"])))
})
