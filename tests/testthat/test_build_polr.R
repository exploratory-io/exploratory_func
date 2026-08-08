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
  expect_true(inherits(trial$model[[1]], "clm_exploratory_0"))
  expect_true(inherits(trial$model[[1]], "clm"))
  expect_equal(length(trial$model[[1]]$y.levels), 3)
})

test_that("clm and polr agree on the sign convention, so no flipping is needed", {
  # The report spec requires effects to ALWAYS be presented in the "higher category"
  # direction, and warns that ordinal software differs on the sign of the linear
  # predictor. This pins the fact the migration relied on: for the SAME fit, clm's
  # coefficients AND thresholds match MASS::polr's exactly -- so switching engines
  # changed no number the report shows, and no sign flip is required anywhere.
  skip_if_not_installed("MASS")
  df <- make_ordinal_test_df(n = 200)
  fml <- stats::as.formula("`満足度` ~ `年齢` + `部署 名!#`")

  m_clm <- ordinal::clm(fml, data = df, link = "logit")
  m_polr <- MASS::polr(fml, data = df, Hess = TRUE, method = "logistic")

  expect_equal(unname(m_clm$beta[names(stats::coef(m_polr))]),
               unname(stats::coef(m_polr)), tolerance = 1e-4)
  expect_equal(unname(m_clm$alpha), unname(m_polr$zeta), tolerance = 1e-4)
})

test_that("build_ordinal_regression is an alias for build_polr", {
  df <- make_ordinal_test_df()
  trial <- df %>% build_ordinal_regression(`満足度`, `年齢`, `部署 名!#`)
  expect_true(inherits(trial$model[[1]], "clm_exploratory_0"))
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
  expect_true(inherits(trial$model[[1]], "clm_exploratory_0"))
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

test_that("tidy.clm_exploratory_0 returns coefficient and intercept rows", {
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

test_that("tidy.clm_exploratory_0 pretty.name renames columns", {
  df <- make_ordinal_test_df()
  trial <- df %>% build_polr(`満足度`, `年齢`, `部署 名!#`)
  tidied <- tidy_rowwise(trial, model, pretty.name = TRUE)
  expect_true(all(c("Term", "Coefficient", "Std. Error", "z value", "P Value", "Odds Ratio", "Type") %in% colnames(tidied)))
})

test_that("glance.clm_exploratory_0 returns fit statistics", {
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

test_that("glance.clm_exploratory_0 pretty.name renames columns", {
  df <- make_ordinal_test_df()
  trial <- df %>% build_polr(`満足度`, `年齢`, `部署 名!#`)
  glanced <- glance_rowwise(trial, model, pretty.name = TRUE)
  expect_true(all(c("Number of Categories", "Rows", "Log Likelihood", "McFadden R-Squared") %in% colnames(glanced)))
})

test_that("augment.clm_exploratory_0 returns predicted class and per-class probabilities", {
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

  # Column set is pinned so a future metric addition is a deliberate, reviewed change.
  # tam#4453 report structure follow-up (S20) added the macro one-vs-rest classification
  # metrics (ROC AUC/PR AUC/Balanced Accuracy/F1/Precision/Recall/Specificity) alongside
  # the original ordinal-aware ones -- they answer different questions and both are kept.
  expect_equal(colnames(ev), c("Data Type", "Rows", "Accuracy Rate", "Misclass. Rate",
                               "ROC AUC", "PR AUC", "Balanced Accuracy", "F1 Score",
                               "Precision", "Recall", "Specificity",
                               "Mean Category Error", "Ranked Probability Score",
                               "Weighted Kappa", "Log Loss", "Max VIF"))
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

  # The model's term names carry backticks for a column whose name needs quoting,
  # so match on the model's OWN dummy terms rather than a hand-built string.
  model <- trial$model[[1]]
  dept_levels <- model$xlevels[["部署 名!#"]]
  expect_false(is.null(dept_levels))
  dummy_terms <- names(stats::coef(model))
  dummy_terms <- dummy_terms[grepl("部署", dummy_terms, fixed = TRUE)]
  dummy_rows <- tidied[tidied$Term %in% dummy_terms, ]
  expect_true(nrow(dummy_rows) > 0)

  # The reference level is whichever level ends up FIRST after build_polr's
  # forcats::fct_infreq() reordering of a character predictor -- not necessarily
  # the order the values appear in the source data. Assert against the model's
  # own xlevels so the test states the real contract.
  expect_true(all(dummy_rows$`Base Level` == dept_levels[[1]]))
  # Exactly one level is dropped as the reference; the rest get dummy terms.
  expect_equal(nrow(dummy_rows), length(dept_levels) - 1)

  # A numeric predictor and the intercept (threshold) rows have no base level.
  expect_true(is.na(tidied$`Base Level`[tidied$Term == "年齢"]))
  expect_true(all(is.na(tidied$`Base Level`[tidied$Type == "intercept"])))
})

test_that("evaluate_polr() carries a Max VIF column, matching build_lm.R's convention", {
  df <- make_ordinal_test_df(n = 150)
  trial <- df %>% build_polr(`満足度`, `年齢`, `部署 名!#`)
  ev <- evaluate_polr(trial, data = "training", pretty.name = TRUE)
  expect_true("Max VIF" %in% colnames(ev))
  expect_false(is.na(ev$`Max VIF`))
  expect_true(ev$`Max VIF` >= 1)

  # A single predictor: VIF is undefined, Max VIF must be NA (not an error).
  trial_single <- df %>% build_polr(`満足度`, `年齢`)
  ev_single <- evaluate_polr(trial_single, data = "training", pretty.name = TRUE)
  expect_true(is.na(ev_single$`Max VIF`))
})

test_that("tidy(type='vif') survives a term MASS::polr itself drops for rank-deficiency, per Repeat By group", {
  # Distinct from the earlier "deliberately collinear predictor" test (VIF merely
  # very large, ~87): here `年齢コピー` is an EXACT linear multiple of `年齢` for
  # the "West" region, so MASS::polr's own rank-deficiency handling silently
  # DROPS the redundant term from coef() rather than fitting a full design with
  # an aliased column -- unlike lm/glm, whose coef() keeps a slot and fills it
  # with NA (the case calc_vif_polr's original NA-coefficient guard was built
  # for). Earlier this crashed with "subscript out of bounds": n_coef (survivors
  # only, from vcov/coef) no longer matched the assign-vector length (still
  # derived from the full 2-term formula), so a term index selected columns
  # beyond the truncated 1x1 correlation matrix.
  set.seed(11)
  n <- 200
  region <- rep(c("East", "West"), length.out = n)
  age <- round(stats::runif(n, 20, 60))
  age_copy <- ifelse(region == "West", age * 2, age + stats::rnorm(n))
  score <- 0.08 * age + stats::rnorm(n)
  satisfaction <- cut(score, breaks = stats::quantile(score, probs = c(0, 1 / 3, 2 / 3, 1)),
                       labels = c("Low", "Medium", "High"), include.lowest = TRUE)
  df <- data.frame(
    `満足度` = factor(as.character(satisfaction), levels = c("Low", "Medium", "High"), ordered = TRUE),
    `年齢` = age,
    `年齢コピー` = age_copy,
    region = region,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  trial <- suppressWarnings(df %>% build_polr(`満足度`, `年齢`, `年齢コピー`, group_cols = "region"))

  vif_df <- tidy_rowwise(trial, model, type = "vif")
  # East (no aliasing) contributes 2 rows; West (aliased) contributes 0 --
  # the whole group's VIF is skipped, not a partial/crashing result.
  expect_equal(nrow(vif_df), 2)
  expect_true(all(vif_df$region == "East"))

  # The per-group error message names the DROPPED variable, mirroring lm/glm's
  # NA-coefficient message text exactly (same downstream consumer: the report's
  # Collinearity Error Message chart matches on the substring "perfect collinearity").
  west_model <- trial$model[trial$region == "West"][[1]]
  expect_true(inherits(west_model$vif, "error"))
  expect_true(grepl("perfect collinearity", conditionMessage(west_model$vif), fixed = TRUE))
  expect_true(grepl("年齢コピー", conditionMessage(west_model$vif), fixed = TRUE))

  east_model <- trial$model[trial$region == "East"][[1]]
  expect_true(is.numeric(east_model$vif))
})

test_that("evaluate_polr() reports the ordinal-aware metrics the spec requires", {
  df <- make_ordinal_test_df(n = 200)
  trial <- df %>% build_polr(`満足度`, `年齢`, `部署 名!#`)
  ev <- evaluate_polr(trial, data = "training", pretty.name = TRUE)

  expect_true(all(c("Accuracy Rate", "Mean Category Error", "Ranked Probability Score",
                    "Weighted Kappa", "Log Loss") %in% colnames(ev)))
  # Bounds implied by each definition.
  expect_true(ev$`Mean Category Error` >= 0)
  expect_true(ev$`Ranked Probability Score` >= 0 && ev$`Ranked Probability Score` <= 1)
  expect_true(ev$`Weighted Kappa` <= 1)
  expect_true(ev$`Log Loss` >= 0)
  expect_false(any(is.na(ev[, c("Mean Category Error", "Ranked Probability Score",
                                "Weighted Kappa", "Log Loss")])))
})

test_that("the ordinal metrics match independent hand computations", {
  df <- make_ordinal_test_df(n = 200)
  trial <- df %>% build_polr(`満足度`, `年齢`, `部署 名!#`)
  ev <- evaluate_polr(trial, data = "training", pretty.name = TRUE)

  model <- trial$model[[1]]
  train_data <- trial$.train_data[[1]]
  aug <- augment.clm_exploratory_0(model, newdata = train_data)
  lv <- levels(train_data$`満足度`)
  K <- length(lv)
  actual_rank <- match(as.character(train_data$`満足度`), lv)
  pred_rank <- match(as.character(aug$.fitted), lv)
  P <- as.matrix(aug[, paste0("predicted_probability_", lv)])

  # Mean Category Error = mean(|predicted_rank - actual_rank|), per the spec.
  expect_equal(ev$`Mean Category Error`, mean(abs(pred_rank - actual_rank)))
  # Log Loss = -mean(log(probability assigned to the ACTUAL category)).
  expect_equal(ev$`Log Loss`, -mean(log(P[cbind(seq_len(nrow(P)), actual_rank)])), tolerance = 1e-10)
  # RPS = mean over rows of sum((cumulative predicted - cumulative actual)^2)/(K-1).
  cum_p <- t(apply(P, 1, cumsum))
  cum_a <- t(vapply(actual_rank, function(r) as.numeric(seq_len(K) >= r), numeric(K)))
  expect_equal(ev$`Ranked Probability Score`,
               mean(rowSums((cum_p[, seq_len(K - 1), drop = FALSE] -
                               cum_a[, seq_len(K - 1), drop = FALSE])^2) / (K - 1)),
               tolerance = 1e-10)
})

test_that("Ranked Probability Score matches the textbook definition on a known case", {
  # Epstein/Murphy RPS: 3 categories, actual = category 2, forecast (0.2, 0.5, 0.3).
  # cumulative predicted = (0.2, 0.7); cumulative actual = (0, 1)
  # RPS = ((0.2-0)^2 + (0.7-1)^2) / (3-1) = (0.04 + 0.09) / 2 = 0.065
  P <- matrix(c(0.2, 0.5, 0.3), nrow = 1)
  K <- 3
  actual_rank <- 2
  cum_p <- t(apply(P, 1, cumsum))
  cum_a <- t(vapply(actual_rank, function(r) as.numeric(seq_len(K) >= r), numeric(K)))
  rps <- mean(rowSums((cum_p[, seq_len(K - 1), drop = FALSE] -
                         cum_a[, seq_len(K - 1), drop = FALSE])^2) / (K - 1))
  expect_equal(rps, 0.065)

  # Bounds: a perfect forecast scores 0, a maximally wrong one scores 1.
  perfect <- { p <- c(0, 1, 0); cp <- cumsum(p); ca <- as.numeric(1:3 >= 2); sum((cp[1:2] - ca[1:2])^2) / 2 }
  worst <- { p <- c(0, 0, 1); cp <- cumsum(p); ca <- as.numeric(1:3 >= 1); sum((cp[1:2] - ca[1:2])^2) / 2 }
  expect_equal(perfect, 0)
  expect_equal(worst, 1)
})

test_that("Weighted Kappa uses quadratic weights and agrees with psych::cohen.kappa", {
  skip_if_not_installed("psych")
  df <- make_ordinal_test_df(n = 200)
  trial <- df %>% build_polr(`満足度`, `年齢`, `部署 名!#`)
  ev <- evaluate_polr(trial, data = "training", pretty.name = TRUE)

  model <- trial$model[[1]]
  train_data <- trial$.train_data[[1]]
  aug <- augment.clm_exploratory_0(model, newdata = train_data)
  lv <- levels(train_data$`満足度`)
  actual_rank <- match(as.character(train_data$`満足度`), lv)
  pred_rank <- match(as.character(aug$.fitted), lv)

  expect_equal(ev$`Weighted Kappa`,
               psych::cohen.kappa(cbind(actual_rank, pred_rank))$weighted.kappa,
               tolerance = 1e-8)
})

# --- Proportional-odds assumption test (比例オッズ仮定) --------------------------
# The report spec's most important ordinal-specific section (tam#4453).

test_that("tidy(type='nominal_test') matches ordinal::nominal_test on a simple fit", {
  # Reference values produced by ordinal::nominal_test() on this exact fixture.
  #
  # They are hardcoded rather than computed here ON PURPOSE: nominal_test() refits via
  # update(), which re-evaluates `data =` BY NAME, and that lookup only succeeds when the
  # data lives in the GLOBAL environment. Inside a testthat block (or any function) it
  # silently returns all-NA -- verified for the plain call, for a formula carrying the
  # local environment, and for eval() in a purpose-built env. A computed reference would
  # therefore be NA and the comparison would pass vacuously.
  #
  # Regenerate with (at top level, not inside a function):
  #   set.seed(3); n <- 250
  #   d <- data.frame(a = rnorm(n) * 2, g = factor(sample(c("X","Y"), n, TRUE)))
  #   d$y <- factor(cut(.9*d$a + rnorm(n), breaks = 3, labels = c("lo","mid","hi")),
  #                 levels = c("lo","mid","hi"), ordered = TRUE)
  #   ordinal::nominal_test(ordinal::clm(y ~ a + g, data = d, link = "logit"))
  reference <- data.frame(
    term = c("a", "g"),
    statistic = c(1.7253364503, 0.1835107250),
    df = c(1, 1),
    p.value = c(0.1890075734, 0.6683733783),
    stringsAsFactors = FALSE
  )

  set.seed(3)
  n <- 250
  d <- data.frame(a = stats::rnorm(n) * 2, g = factor(sample(c("X", "Y"), n, TRUE)))
  d$y <- factor(cut(0.9 * d$a + stats::rnorm(n), breaks = 3, labels = c("lo", "mid", "hi")),
                levels = c("lo", "mid", "hi"), ordered = TRUE)

  trial <- d %>% build_polr(y, a, g)
  ours <- tidy_rowwise(trial, model, type = "nominal_test")

  expect_setequal(ours$term, reference$term)
  ours <- ours[match(reference$term, ours$term), ]
  expect_equal(ours$statistic, reference$statistic, tolerance = 1e-6)
  expect_equal(ours$df, reference$df, tolerance = 1e-8)
  expect_equal(ours$p.value, reference$p.value, tolerance = 1e-6)
})

test_that("tidy(type='nominal_test') works for column names ordinal::nominal_test cannot handle", {
  # ordinal::nominal_test() returns all-NA here: its update()-based refit cannot see our
  # local training data, and clm() itself pastes the nominal formula together without
  # re-quoting, so a name with a space/symbol is unparseable. Our implementation refits
  # from the stored model frame under sanitized names, so it produces real statistics.
  df <- make_ordinal_test_df(n = 300)
  trial <- df %>% build_polr(`満足度`, `年齢`, `部署 名!#`)

  nt <- tidy_rowwise(trial, model, type = "nominal_test")
  expect_setequal(nt$term, c("年齢", "部署 名!#"))
  expect_true(all(is.finite(nt$statistic)))
  expect_true(all(nt$df > 0))
  expect_true(all(nt$p.value >= 0 & nt$p.value <= 1))
  # A 3-level categorical predictor gets one extra threshold-specific effect per
  # non-reference level per extra boundary.
  expect_true(nt$df[nt$term == "部署 名!#"] > nt$df[nt$term == "年齢"])
})

test_that("tidy(type='nominal_test') renames for display with pretty.name", {
  df <- make_ordinal_test_df(n = 200)
  trial <- df %>% build_polr(`満足度`, `年齢`, `部署 名!#`)
  nt <- tidy_rowwise(trial, model, type = "nominal_test", pretty.name = TRUE)
  expect_true(all(c("Variable", "Likelihood Ratio Statistic", "Degree of Freedom", "P Value")
                  %in% colnames(nt)))
})

test_that("tidy(type='nominal_test') degrades to an empty frame, not an error", {
  df <- make_ordinal_test_df(n = 80)
  trial <- df %>% build_polr(`満足度`, `年齢`)
  nt <- tidy_rowwise(trial, model, type = "nominal_test")
  # One predictor still has a testable nominal effect; the contract is only that it
  # never errors and always returns the documented columns.
  expect_true(all(c("term", "statistic", "df", "p.value") %in% colnames(nt)))
})

# --- tam#4453 report structure follow-up (S14/S17/S20/S22) -------------------

test_that("evaluate_polr() reports macro one-vs-rest classification metrics with a multibyte-column model", {
  df <- make_ordinal_test_df(n = 200)
  trial <- df %>% build_polr(`満足度`, `年齢`, `部署 名!#`, test_rate = 0.25, seed = 1)
  ev <- evaluate_polr(trial, data = "training_and_test")

  expect_true(all(c("ROC AUC", "PR AUC", "Balanced Accuracy", "Precision", "Recall", "Specificity",
                     "F1 Score") %in% colnames(ev)))
  # Macro recall and Balanced Accuracy are the SAME statistic by definition (unweighted mean
  # of per-category recall) -- pin the identity so a future refactor of either helper can't
  # silently diverge them.
  expect_equal(ev$Recall, ev$`Balanced Accuracy`)
  numeric_cols <- c("ROC AUC", "PR AUC", "Balanced Accuracy", "Precision", "Recall", "Specificity", "F1 Score")
  for (col in numeric_cols) {
    expect_true(all(ev[[col]] >= 0 & ev[[col]] <= 1), info = col)
  }
})

test_that("polr_macro_precision_recall_specificity() matches a hand-computed 3-class confusion matrix", {
  # actual: A A A B B C C C C ; predicted: A B A B B C A C C
  actual <- c("A", "A", "A", "B", "B", "C", "C", "C", "C")
  predicted <- c("A", "B", "A", "B", "B", "C", "A", "C", "C")
  # Per class: A: TP=2,FN=1,FP=1,TN=5 -> precision 2/3, recall 2/3, specificity 5/6
  #            B: TP=2,FN=0,FP=1,TN=6 -> precision 2/3, recall 1,   specificity 6/7
  #            C: TP=3,FN=1,FP=0,TN=5 -> precision 1,   recall 3/4, specificity 1
  r <- polr_macro_precision_recall_specificity(actual, predicted)
  expect_equal(r$precision, mean(c(2 / 3, 2 / 3, 1)), tolerance = 1e-9)
  expect_equal(r$recall, mean(c(2 / 3, 1, 3 / 4)), tolerance = 1e-9)
  expect_equal(r$specificity, mean(c(5 / 6, 6 / 7, 1)), tolerance = 1e-9)
})

test_that("polr_report_multiclass_probabilities() returns one One-vs-Rest row per (observation, category)", {
  df <- make_ordinal_test_df(n = 150)
  trial <- df %>% build_polr(`満足度`, `年齢`, `部署 名!#`, test_rate = 0.2, seed = 1)
  rows <- polr_report_multiclass_probabilities(trial)

  n_train <- nrow(trial$.train_data[[1]])
  n_test <- nrow(trial$.test_data[[1]])
  expect_equal(nrow(rows), (n_train + n_test) * 3) # 3 satisfaction categories
  expect_true(all(rows$`Predicted Probability` >= 0 & rows$`Predicted Probability` <= 1))
  expect_setequal(unique(rows$Category), c("Low", "Medium", "High"))
  expect_setequal(levels(rows$`Actual Group`), c("This Category", "Other Categories"))
  # baseline_precision for a (Category, is_test_data) group must equal that group's own
  # actual positive share -- catches an accidental global (not per-group) average.
  for (dt in c(FALSE, TRUE)) {
    for (cat in c("Low", "Medium", "High")) {
      sub <- rows[rows$Category == cat & rows$is_test_data == dt, ]
      if (nrow(sub) > 0) {
        expect_equal(unique(sub$baseline_precision), mean(sub$`Actual Positive`), tolerance = 1e-9)
      }
    }
  }
})

test_that("polr_report_multiclass_probabilities() returns an empty frame for a non-clm model or missing columns", {
  expect_equal(nrow(polr_report_multiclass_probabilities(data.frame())), 0)
  expect_equal(nrow(polr_report_multiclass_probabilities(data.frame(model = I(list(1L))))), 0)
})

test_that("polr_report_basic_info() reports Target/Categories/Category Order/Predictors/Rows/Model", {
  df <- make_ordinal_test_df(n = 90)
  trial <- df %>% build_polr(`満足度`, `年齢`, `部署 名!#`, seed = 1)
  info <- polr_report_basic_info(trial, test_mode = FALSE)

  expect_equal(info$Target, "満足度")
  expect_equal(info$Categories, 3L)
  expect_equal(info$`Category Order`, "Low < Medium < High")
  expect_equal(info$Predictors, 2L)
  expect_equal(info$Rows, 90L)
  expect_equal(info$Model, "Ordered Logistic Regression")
  expect_equal(info$Evaluation, "Training")
  expect_equal(polr_report_basic_info(trial, test_mode = TRUE)$Evaluation, "Test Data")
})

test_that("partial dependence for a build_polr() model flags numeric predictors as 'line' and categorical as 'scatter'", {
  # tam#4453 S17: the report renders one line per category for a numeric predictor, and a
  # different layout for a categorical one -- handle_partial_dependence()'s generic
  # chart_type/x_type columns are what the tam preprocessor branches on.
  df <- make_ordinal_test_df(n = 120)
  trial <- df %>% build_polr(`満足度`, `年齢`, `部署 名!#`, seed = 1)
  pd <- tidy_rowwise(trial, model, type = "partial_dependence")

  expect_true(all(pd$chart_type[pd$x_name == "年齢"] == "line"))
  expect_true(all(pd$x_type[pd$x_name == "年齢"] == "numeric"))
  expect_true(all(pd$chart_type[pd$x_name == "部署 名!#"] == "scatter"))
  expect_true(all(pd$x_type[pd$x_name == "部署 名!#"] == "character"))
  expect_setequal(unique(pd$y_name), c("Low", "Medium", "High"))
})
