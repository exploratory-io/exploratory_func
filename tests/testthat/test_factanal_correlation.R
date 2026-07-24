# Correlation-type selection and polychoric support for Factor Analysis (issue #26623).
# how to run this test:
# devtools::test(filter="factanal_correlation")
context("test factor analysis correlation type, exp_factanal cor_type")

# =============================================================================
# Polychoric correlation support (issue #26623)
# =============================================================================

# Builds ordinal survey-like data: q1-q3 load on one latent factor, q4-q6 on another.
# `skew=TRUE` concentrates the responses so the 5-category rule picks Polychoric.
factanal_ordinal_fixture <- function(n = 300, skew = TRUE, seed = 42) {
  set.seed(seed)
  lat1 <- stats::rnorm(n)
  lat2 <- stats::rnorm(n)
  make_col <- function(latent) {
    z <- 0.75 * latent + sqrt(1 - 0.75^2) * stats::rnorm(n)
    if (skew) {
      as.integer(cut(z, breaks = c(-Inf, stats::quantile(z, c(.55, .75, .88, .96)), Inf), labels = FALSE))
    } else {
      as.integer(cut(z, breaks = stats::quantile(z, probs = seq(0, 1, length.out = 6)),
                     include.lowest = TRUE, labels = FALSE))
    }
  }
  data.frame(q1 = make_col(lat1), q2 = make_col(lat1), q3 = make_col(lat1),
             q4 = make_col(lat2), q5 = make_col(lat2), q6 = make_col(lat2))
}

# Swaps build_factor_correlation for a stub and returns a restore function.
#
# The package namespace is LOCKED when the tests run against an installed build (Jenkins /
# R CMD check), so a bare assign() fails there with "cannot change value of locked binding".
# Mirrors the unlockBinding pattern already used in test_pdf.R. The global-environment copy is
# patched too, so the suite also works when the R files are source()'d for local iteration --
# in that case exp_factanal()/compute_parallel_analysis() resolve the helper from globalenv,
# not from the namespace.
stub_build_factor_correlation <- function(stub) {
  restorers <- list()

  ns <- tryCatch(asNamespace("exploratory"), error = function(e) NULL)
  if (!is.null(ns) && exists("build_factor_correlation", envir = ns, inherits = FALSE)) {
    original_ns <- get("build_factor_correlation", envir = ns)
    was_locked <- bindingIsLocked("build_factor_correlation", ns)
    if (was_locked) unlockBinding("build_factor_correlation", ns)
    assign("build_factor_correlation", stub, envir = ns)
    restorers <- c(restorers, function() {
      assign("build_factor_correlation", original_ns, envir = ns)
      if (was_locked) lockBinding("build_factor_correlation", ns)
    })
  }

  if (exists("build_factor_correlation", envir = globalenv(), inherits = FALSE)) {
    original_global <- get("build_factor_correlation", envir = globalenv())
    assign("build_factor_correlation", stub, envir = globalenv())
    restorers <- c(restorers, function() {
      assign("build_factor_correlation", original_global, envir = globalenv())
    })
  }

  function() {
    for (restore in restorers) restore()
  }
}

test_that("correlation type auto-selection rules (issue #26623)", {
  n <- 300
  set.seed(11)
  lat <- stats::rnorm(n)
  ordinal_col <- function(k) {
    z <- 0.7 * lat + sqrt(1 - 0.7^2) * stats::rnorm(n)
    as.integer(cut(z, breaks = stats::quantile(z, probs = seq(0, 1, length.out = k + 1)),
                   include.lowest = TRUE, labels = FALSE))
  }
  method_of <- function(df) select_factor_correlation_type(df)$selected_method

  # All numeric -> Pearson.
  expect_equal(method_of(data.frame(a = stats::rnorm(n), b = stats::rnorm(n), c = stats::rnorm(n))), "pearson")
  # All binary -> Tetrachoric (numeric 0/1 and logical alike).
  expect_equal(method_of(data.frame(a = stats::rbinom(n, 1, .4), b = stats::rbinom(n, 1, .5), c = stats::rbinom(n, 1, .6))), "tetrachoric")
  expect_equal(method_of(data.frame(a = stats::rbinom(n, 1, .4) == 1, b = stats::rbinom(n, 1, .5) == 1, c = stats::rbinom(n, 1, .6) == 1)), "tetrachoric")
  # All ordinal with at most 4 categories -> Polychoric.
  expect_equal(method_of(data.frame(a = ordinal_col(4), b = ordinal_col(4), c = ordinal_col(4))), "polychoric")
  # Ordered factors are ordinal too.
  expect_equal(method_of(data.frame(a = factor(ordinal_col(4), ordered = TRUE),
                                    b = factor(ordinal_col(4), ordered = TRUE),
                                    c = factor(ordinal_col(4), ordered = TRUE))), "polychoric")
  # 5 categories: skewed/concentrated -> Polychoric, balanced -> Pearson.
  expect_equal(method_of(factanal_ordinal_fixture(skew = TRUE)), "polychoric")
  expect_equal(method_of(factanal_ordinal_fixture(skew = FALSE)), "pearson")
  # 6 or more ordered categories -> Pearson by default.
  expect_equal(method_of(data.frame(a = ordinal_col(7), b = ordinal_col(7), c = ordinal_col(7))), "pearson")
  # Numeric + ordinal mix -> Mixed correlation.
  expect_equal(method_of(data.frame(a = stats::rnorm(n), b = ordinal_col(5), c = ordinal_col(5))), "mixed")
  # A nominal category makes the combination unsupported.
  nominal <- select_factor_correlation_type(data.frame(a = factor(sample(letters[1:4], n, TRUE)),
                                                       b = ordinal_col(4), c = ordinal_col(4)))
  expect_equal(nominal$selected_method, "unsupported")
  expect_true(grepl("Nominal categorical variables", nominal$reason))
  # A constant column is invalid, not silently analyzed.
  invalid <- select_factor_correlation_type(data.frame(a = rep(1, n), b = ordinal_col(4), c = ordinal_col(4)))
  expect_equal(invalid$selected_method, "unsupported")
  expect_true(grepl("Invalid variables", invalid$reason))
})

test_that("numeric rating-scale detection does not reclassify ordinary measurements (issue #26623)", {
  # A 1-5 (or 0-4) consecutive integer scale is treated as ordinal...
  expect_true(is_rating_scale_numeric(c(1, 2, 3, 4, 5)))
  expect_true(is_rating_scale_numeric(c(0, 1, 2, 3)))
  # ...but a measurement with gaps, non-integers, too many levels, or not starting at 0/1 is not.
  expect_false(is_rating_scale_numeric(c(4, 6, 8)))          # mtcars$cyl
  expect_false(is_rating_scale_numeric(c(1.5, 2.5, 3.5)))
  expect_false(is_rating_scale_numeric(1:8))
  expect_false(is_rating_scale_numeric(c(3, 4, 5)))
  # So an all-numeric data frame like mtcars keeps using Pearson.
  expect_equal(select_factor_correlation_type(mtcars[, c("cyl", "mpg", "hp", "drat")])$selected_method, "pearson")
})

test_that("exp_factanal Pearson results are unchanged by the correlation-type support (issue #26623)", {
  set.seed(3)
  n <- 200
  lat1 <- stats::rnorm(n); lat2 <- stats::rnorm(n)
  df <- data.frame(a = lat1 + stats::rnorm(n), b = lat1 + stats::rnorm(n),
                   c = lat2 + stats::rnorm(n), e = lat2 + stats::rnorm(n))
  fit <- exp_factanal(df, a, b, c, e, nfactors = 2, fm = "minres", rotate = "varimax",
                      cor_type = "pearson", parallel_n_iter = 5)$model[[1]]
  reference <- psych::fa(df, nfactors = 2, fm = "minres", scores = "regression", rotate = "varimax")
  expect_equal(unclass(fit$loadings), unclass(reference$loadings))
  expect_equal(fit$scores, reference$scores)
  # Auto on all-numeric data resolves to Pearson, so existing analyses keep their results.
  auto_fit <- exp_factanal(df, a, b, c, e, nfactors = 2, fm = "minres", rotate = "varimax",
                           parallel_n_iter = 5)$model[[1]]
  expect_equal(auto_fit$correlation_type, "pearson")
  expect_equal(unclass(auto_fit$loadings), unclass(reference$loadings))
})

test_that("every downstream computation uses the polychoric matrix, not Pearson (issue #26623)", {
  df <- factanal_ordinal_fixture()
  poly <- exp_factanal(df, q1, q2, q3, q4, q5, q6, nfactors = 2, rotate = "varimax",
                       parallel_n_iter = 5)$model[[1]]
  pearson <- exp_factanal(df, q1, q2, q3, q4, q5, q6, nfactors = 2, rotate = "varimax",
                          cor_type = "pearson", parallel_n_iter = 5)$model[[1]]
  expect_equal(poly$correlation_type, "polychoric")
  expect_true(poly$correlation_is_auto)
  expect_false(pearson$correlation_is_auto)

  # The correlation matrix itself, and therefore KMO / Bartlett / eigenvalues, differ.
  expect_false(isTRUE(all.equal(poly$correlation, pearson$correlation)))
  expect_false(isTRUE(all.equal(poly$kmo, pearson$kmo)))
  expect_false(isTRUE(all.equal(poly$bartlett$chisq, pearson$bartlett$chisq)))

  # The parallel analysis compares against the SAME matrix fa() was fitted on.
  expect_equal(poly$parallel$table$actual_eigenvalue,
               eigen(poly$correlation, symmetric = TRUE, only.values = TRUE)$values)
  # The scree plot / Kaiser criterion read the same matrix as well.
  expect_equal(tidy(poly, type = "screeplot")$eigenvalue,
               eigen(poly$correlation, only.values = TRUE)$values)

  # fa() on a correlation matrix returns no scores, so they are computed from that matrix.
  expect_false(is.null(poly$scores))
  expect_equal(nrow(poly$scores), nrow(df))
  expect_equal(ncol(poly$scores), 2)
  expect_equal(nrow(tidy(poly, type = "data")), nrow(df))
})

test_that("analysis_method and cor_diagnostics tidy types (issue #26623)", {
  df <- factanal_ordinal_fixture()
  poly <- exp_factanal(df, q1, q2, q3, q4, q5, q6, nfactors = 2, fm = "minres", rotate = "varimax",
                       parallel_n_iter = 5)$model[[1]]

  method_tbl <- tidy(poly, type = "analysis_method")
  expect_equal(method_tbl$Item, c("Correlation", "Factor Extraction Method", "Rotation", "Target Variables", "Data Rows"))
  expect_equal(method_tbl$Value[[1]], "Polychoric Correlation")
  expect_equal(method_tbl$Value[[2]], "Minimum Residual")
  expect_equal(method_tbl$Value[[3]], "Varimax (Orthogonal)")
  expect_equal(method_tbl$Value[[4]], "6")
  expect_equal(method_tbl$Value[[5]], as.character(nrow(df)))
  # Hidden columns the client binds the report explanation from.
  expect_equal(unique(method_tbl$correlation_type), "polychoric")
  expect_true(all(method_tbl$correlation_is_auto == "TRUE"))
  expect_true(nchar(method_tbl$reason[[1]]) > 0)

  diagnostics <- tidy(poly, type = "cor_diagnostics")
  expect_equal(diagnostics$Diagnostic,
               c("Number of Categories", "Sparse Categories", "Empty Category Combinations",
                 "Correlation Estimation Failures", "Positive Definiteness of the Correlation Matrix",
                 "Smoothing Applied"))
  expect_equal(diagnostics$Judgement[[1]], "All categorical variables have 5 categories")
  expect_equal(diagnostics$Judgement[[4]], "None")   # no estimation failure on this fixture
  expect_equal(diagnostics$Judgement[[5]], "No problem")
  expect_equal(diagnostics$Judgement[[6]], "None")
  expect_equal(diagnostics$status[[5]], "ok")

  # On a Pearson analysis the diagnostics table is empty (the report hides the section) but keeps
  # its column shape.
  pearson <- exp_factanal(df, q1, q2, q3, q4, q5, q6, nfactors = 2, rotate = "varimax",
                          cor_type = "pearson", parallel_n_iter = 5)$model[[1]]
  empty <- tidy(pearson, type = "cor_diagnostics")
  expect_equal(nrow(empty), 0)
  expect_equal(colnames(empty), c("Diagnostic", "Judgement", "Description", "status"))
  expect_equal(tidy(pearson, type = "analysis_method")$Value[[1]], "Pearson Correlation")
})

test_that("tetrachoric, mixed and unsupported paths (issue #26623)", {
  set.seed(5)
  n <- 250
  lat1 <- stats::rnorm(n); lat2 <- stats::rnorm(n)
  binary_col <- function(latent) as.integer(0.75 * latent + sqrt(1 - 0.75^2) * stats::rnorm(n) > 0.3)
  ordinal_col <- function(latent) {
    z <- 0.75 * latent + sqrt(1 - 0.75^2) * stats::rnorm(n)
    as.integer(cut(z, breaks = c(-Inf, stats::quantile(z, c(.55, .75, .88, .96)), Inf), labels = FALSE))
  }
  binary_df <- data.frame(b1 = binary_col(lat1), b2 = binary_col(lat1), b3 = binary_col(lat1),
                          b4 = binary_col(lat2), b5 = binary_col(lat2), b6 = binary_col(lat2))
  tet <- exp_factanal(binary_df, b1, b2, b3, b4, b5, b6, nfactors = 2, rotate = "varimax",
                      parallel_n_iter = 5)$model[[1]]
  expect_equal(tet$correlation_type, "tetrachoric")
  expect_equal(tidy(tet, type = "analysis_method")$Value[[1]], "Tetrachoric Correlation")
  expect_false(is.null(tet$scores))

  mixed_df <- data.frame(x1 = lat1 + stats::rnorm(n, 0, .5), x2 = lat1 + stats::rnorm(n, 0, .5),
                         q1 = ordinal_col(lat2), q2 = ordinal_col(lat2), q3 = ordinal_col(lat2))
  mixed <- exp_factanal(mixed_df, x1, x2, q1, q2, q3, nfactors = 2, rotate = "varimax",
                        parallel_n_iter = 5)$model[[1]]
  expect_equal(mixed$correlation_type, "mixed")
  expect_equal(tidy(mixed, type = "analysis_method")$Value[[1]], "Mixed Correlation")

  # A nominal column aborts with the analytics error code, not a raw psych error.
  nominal_df <- binary_df
  nominal_df$cat <- factor(sample(c("a", "b", "c"), n, TRUE))
  expect_error(exp_factanal(nominal_df, b1, b2, cat, nfactors = 1, parallel_n_iter = 5),
               "Nominal categorical variables")
})

test_that("build_factor_correlation returns one matrix for the whole analysis (issue #26623)", {
  df <- factanal_ordinal_fixture(n = 200)
  pearson <- build_factor_correlation(df, "pearson")
  expect_equal(pearson$type, "pearson")
  expect_equal(pearson$correlation, stats::cor(df, use = "pairwise.complete.obs"))
  expect_null(pearson$thresholds)

  poly <- build_factor_correlation(df, "polychoric")
  expect_equal(poly$type, "polychoric")
  expect_equal(dim(poly$correlation), c(ncol(df), ncol(df)))
  expect_false(is.null(poly$thresholds))
  expect_false(poly$failed)
  # Thresholds are what makes it polychoric: the matrix must not equal the Pearson one.
  expect_false(isTRUE(all.equal(poly$correlation, pearson$correlation)))

  expect_error(build_factor_correlation(df, "nonsense"))
  expect_error(resolve_factanal_correlation_type("nonsense", select_factor_correlation_type(df)),
               "Unknown correlation type")
  # A manual choice wins over the automatic selection.
  manual <- resolve_factanal_correlation_type("polychoric", select_factor_correlation_type(mtcars[, 1:4]))
  expect_equal(manual$type, "polychoric")
  expect_false(manual$auto)
})

test_that("verification pass 1 findings (issue #26623)", {
  df <- factanal_ordinal_fixture()

  # The "Type of Scores" property must not become a no-op on the correlation-matrix path.
  expect_equal(factanal_score_method("regression"), "Thurstone")   # fa()'s name for the same method
  expect_equal(factanal_score_method("Bartlett"), "Bartlett")
  expect_equal(factanal_score_method("tenBerge"), "tenBerge")
  expect_equal(factanal_score_method(NULL), "Thurstone")
  regression_fit <- exp_factanal(df, q1, q2, q3, q4, q5, q6, nfactors = 2, rotate = "varimax",
                                 scores = "regression", parallel_n_iter = 3)$model[[1]]
  bartlett_fit <- exp_factanal(df, q1, q2, q3, q4, q5, q6, nfactors = 2, rotate = "varimax",
                               scores = "Bartlett", parallel_n_iter = 3)$model[[1]]
  expect_false(isTRUE(all.equal(regression_fit$scores, bartlett_fit$scores)))

  # Rotation names are mapped, not title-cased: "bentlerT" must not render as "Bentlert".
  expect_equal(factanal_rotation_label("bentlerT"), "Bentler (Orthogonal)")
  expect_equal(factanal_rotation_label("geominT"), "Geomin (Orthogonal)")
  expect_equal(factanal_rotation_label("promax"), "Promax with Kaiser Normalization")
  expect_equal(factanal_rotation_label("none"), "None")
  rotated <- exp_factanal(df, q1, q2, q3, q4, q5, q6, nfactors = 2, rotate = "bentlerT",
                          cor_type = "polychoric", parallel_n_iter = 3)$model[[1]]
  expect_equal(tidy(rotated, type = "analysis_method")$Value[[3]], "Bentler (Orthogonal)")

  # A manual choice must not be reported as an automatic one, and must not read "Correlation
  # correlation".
  expect_equal(rotated$correlation_reason, "Polychoric Correlation was selected manually.")
  expect_false(rotated$correlation_is_auto)
  method_tbl <- tidy(rotated, type = "analysis_method")
  expect_equal(method_tbl$correlation_is_auto[[1]], "FALSE")
  expect_equal(method_tbl$has_diagnostics[[1]], "TRUE")

  # An unsupported variable combination stays unsupported when the correlation is chosen manually:
  # picking Pearson must not smuggle a nominal column in as arbitrary integer codes.
  nominal_df <- df
  nominal_df$cat <- factor(sample(c("a", "b", "c"), nrow(df), TRUE))
  expect_error(exp_factanal(nominal_df, q1, q2, cat, nfactors = 1, cor_type = "pearson", parallel_n_iter = 3),
               "Nominal categorical variables")
  expect_error(exp_factanal(nominal_df, q1, q2, cat, nfactors = 1, cor_type = "polychoric", parallel_n_iter = 3),
               "Nominal categorical variables")
})

test_that("mixed-correlation diagnostics only describe the categorical variables (issue #26623)", {
  set.seed(21)
  n <- 300
  lat1 <- stats::rnorm(n); lat2 <- stats::rnorm(n)
  ordinal_col <- function(latent) {
    z <- 0.75 * latent + sqrt(1 - 0.75^2) * stats::rnorm(n)
    as.integer(cut(z, breaks = c(-Inf, stats::quantile(z, c(.55, .75, .88, .96)), Inf), labels = FALSE))
  }
  mixed_df <- data.frame(x1 = lat1 + stats::rnorm(n, 0, .5), x2 = lat1 + stats::rnorm(n, 0, .5),
                         q1 = ordinal_col(lat2), q2 = ordinal_col(lat2), q3 = ordinal_col(lat2))
  fit <- exp_factanal(mixed_df, x1, x2, q1, q2, q3, nfactors = 2, rotate = "varimax",
                      parallel_n_iter = 3)$model[[1]]
  expect_equal(fit$correlation_type, "mixed")
  diagnostics <- tidy(fit, type = "cor_diagnostics")
  # A continuous column has one "category" per distinct value; counting it would report hundreds of
  # categories and flag every variable as sparse and every pair as having empty combinations.
  expect_equal(diagnostics$Judgement[[1]], "All categorical variables have 5 categories")
  expect_equal(diagnostics$Judgement[[2]], "Detected in 3 variables")   # the 3 ordinal ones at most
  expect_false(grepl("Detected in 10 variable pairs", diagnostics$Judgement[[3]]))
})

test_that("verification pass 2 findings (issue #26623)", {
  df <- factanal_ordinal_fixture()
  fit <- exp_factanal(df, q1, q2, q3, q4, q5, q6, nfactors = 2, rotate = "varimax",
                      parallel_n_iter = 3)$model[[1]]
  method_tbl <- tidy(fit, type = "analysis_method")

  # Rotation / extraction labels echo the property dropdown's wording, so the report and the
  # settings panel cannot disagree about what was run.
  expect_equal(factanal_rotation_label("varimax"), "Varimax (Orthogonal)")
  expect_equal(factanal_rotation_label("oblimin"), "Oblimin (Oblique)")
  expect_equal(factanal_extraction_method_label("gls"), "Generalized Weighted Least Squares")

  # Whether Polychoric is worth suggesting is data-dependent.
  expect_true(factanal_polychoric_available(select_factor_correlation_type(df)))
  expect_false(factanal_polychoric_available(select_factor_correlation_type(mtcars[, c("mpg", "hp", "drat", "wt")])))
  expect_equal(method_tbl$polychoric_available[[1]], "TRUE")
  expect_equal(method_tbl$degraded_from[[1]], "")

  # The category-count row says "categorical variables": in a mixed analysis the continuous
  # columns are not counted, so "all variables" would contradict the Target Variables row.
  diagnostics <- tidy(fit, type = "cor_diagnostics")
  expect_true(grepl("^All categorical variables have", diagnostics$Judgement[[1]]))

  # Selector warnings reach the client as language-neutral tokens (never English prose).
  sparse_df <- df
  sparse_df$q1[sparse_df$q1 == 5] <- 4       # make the top category nearly empty
  sparse_df$q1[seq_len(2)] <- 5              # 2 of 300 responses = below the 5% cutoff
  tokens <- factanal_selection_warning_tokens(select_factor_correlation_type(sparse_df))
  expect_true("sparse_categories" %in% tokens)
  expect_equal(factanal_selection_warning_tokens(select_factor_correlation_type(mtcars[, 1:4])), character())

  # The parallel analysis reuses the analysis's own matrix rather than recomputing a second one.
  parallel_result <- compute_parallel_analysis(mtcars[, c("mpg", "hp", "drat", "wt")], n_iter = 3,
                                               cor_type = "pearson",
                                               cor_matrix = cor(mtcars[, c("mpg", "hp", "drat", "wt")]))
  expect_equal(parallel_result$table$actual_eigenvalue,
               eigen(cor(mtcars[, c("mpg", "hp", "drat", "wt")]), only.values = TRUE)$values)
})

test_that("positive definiteness reports the matrix BEFORE psych's smoothing (issue #26623)", {
  df <- factanal_ordinal_fixture(n = 200)
  # psych smooths a non-positive-definite matrix automatically, so testing the returned matrix
  # would always pass and contradict the "Smoothing Applied" row.
  smoothed_result <- list(correlation = diag(3), thresholds = NULL, type = "polychoric",
                          smoothed = TRUE, warnings = character(), failed = FALSE)
  selection <- select_factor_correlation_type(df[, 1:3])
  diagnostics <- compute_polychoric_diagnostics(df[, 1:3], smoothed_result, selection)
  expect_equal(diagnostics$Judgement[[5]], "Not positive definite")
  expect_equal(diagnostics$Judgement[[6]], "Applied")
  expect_equal(diagnostics$status[[5]], "caution")

  clean_result <- list(correlation = diag(3), thresholds = NULL, type = "polychoric",
                       smoothed = FALSE, warnings = character(), failed = FALSE)
  clean_diagnostics <- compute_polychoric_diagnostics(df[, 1:3], clean_result, selection)
  expect_equal(clean_diagnostics$Judgement[[5]], "No problem")
  expect_equal(clean_diagnostics$Judgement[[6]], "None")
})

test_that("an unsupported variable combination uses its own error id with the column names (issue #26623)", {
  df <- factanal_ordinal_fixture(n = 150)
  df$cat <- factor(sample(c("a", "b", "c"), nrow(df), TRUE))
  # EXP-ANA-6 belongs to PCA's reserved-column-name error; reusing it would show the user a
  # message about renaming PC1/PC2. The params carry the offending column names.
  err <- tryCatch({
    exp_factanal(df, q1, q2, cat, nfactors = 1, parallel_n_iter = 3)
    NULL
  }, error = function(e) conditionMessage(e))
  expect_false(is.null(err))
  expect_true(grepl("^EXP-ANA-35 :: ", err))
  expect_false(grepl("EXP-ANA-6", err))
  expect_true(grepl('\\["cat"\\]', err))
})

test_that("verification pass 3 findings (issue #26623)", {
  df <- factanal_ordinal_fixture(n = 200)
  selection <- select_factor_correlation_type(df[, 1:3])

  # A failed estimation leaves the PEARSON fallback matrix in hand, so its positive definiteness
  # and smoothing say nothing about the correlation that failed.
  failed_result <- list(correlation = diag(3), thresholds = NULL, type = "pearson",
                        smoothed = FALSE, warnings = "boom", failed = TRUE)
  failed_diagnostics <- compute_polychoric_diagnostics(df[, 1:3], failed_result, selection)
  expect_equal(failed_diagnostics$Judgement[[4]], "Estimation failed")
  expect_equal(failed_diagnostics$Judgement[[5]], "Not Available")
  expect_equal(failed_diagnostics$Judgement[[6]], "Not Available")
  expect_equal(failed_diagnostics$status[[5]], "na")

  # The sparse-category description must not name Polychoric: the same row appears in tetrachoric
  # and mixed analyses.
  expect_false(any(grepl("Polychoric correlations may become unstable", failed_diagnostics$Description)))
  expect_true(any(grepl("Correlations estimated from categories", failed_diagnostics$Description)))

  # Counts are singular when there is exactly one hit. Balanced columns, then one rare category
  # injected into a single variable.
  balanced <- data.frame(q1 = rep(1:4, 50), q2 = rep(1:4, 50), q3 = rep(1:4, 50))
  balanced$q1[balanced$q1 == 4] <- 3
  balanced$q1[1] <- 4   # a single response in the top category (1 of 200 = below the 5% cutoff)
  balanced_selection <- select_factor_correlation_type(balanced)
  single <- compute_polychoric_diagnostics(balanced,
                                           list(correlation = diag(3), thresholds = NULL,
                                                type = "polychoric", smoothed = FALSE,
                                                warnings = character(), failed = FALSE),
                                           balanced_selection)
  expect_equal(single$Judgement[[2]], "Detected in 1 variable")

  # Category counts come from the DEFINED levels the selection branched on, not observed codes.
  ordered_df <- data.frame(
    a = factor(c("low", "mid", "high")[c(1, 2, 1, 2, 1, 2, 1, 2)], levels = c("low", "mid", "high"), ordered = TRUE),
    b = factor(c("low", "mid", "high")[c(1, 2, 3, 2, 1, 2, 3, 2)], levels = c("low", "mid", "high"), ordered = TRUE),
    c = factor(c("low", "mid", "high")[c(3, 2, 1, 2, 3, 2, 1, 2)], levels = c("low", "mid", "high"), ordered = TRUE)
  )
  ordered_selection <- select_factor_correlation_type(ordered_df)
  ordered_diagnostics <- compute_polychoric_diagnostics(encode_factanal_data(ordered_df, ordered_selection),
                                                        list(correlation = diag(3), thresholds = NULL,
                                                             type = "polychoric", smoothed = FALSE,
                                                             warnings = character(), failed = FALSE),
                                                        ordered_selection)
  # Column `a` never takes its third level, but it still HAS three defined categories.
  expect_equal(ordered_diagnostics$Judgement[[1]], "All categorical variables have 3 categories")

  # Polychoric stays offered for scale-like data the rating-scale heuristic classifies numeric
  # (a 1-10 scale, or a 1-5 scale with a gap), and is dropped only for genuine measurements.
  wide_scale <- data.frame(a = rep(1:10, 20), b = rep(1:10, 20), c = rep(1:10, 20))
  expect_true(factanal_polychoric_available(select_factor_correlation_type(wide_scale), wide_scale))
  continuous <- mtcars[, c("mpg", "disp", "drat", "wt")]
  expect_false(factanal_polychoric_available(select_factor_correlation_type(continuous), continuous))

  # The unknown-correlation-type error lists what the guard actually accepts.
  expect_error(resolve_factanal_correlation_type("nonsense", selection),
               "auto, pearson, polychoric, tetrachoric, and mixed")
})

test_that("the parallel analysis null distribution never mixes in Pearson eigenvalues (issue #26623)", {
  # A permutation whose polychoric estimation fails returns the Pearson matrix with failed = TRUE.
  # Those iterations must be dropped, not compared against polychoric eigenvalues.
  df <- factanal_ordinal_fixture(n = 150)[, 1:3]
  calls <- 0
  fake_build <- function(data, correlation_type, ...) {
    calls <<- calls + 1
    if (calls %% 2 == 0) {
      list(correlation = stats::cor(data), thresholds = NULL, type = "pearson",
           smoothed = FALSE, warnings = "failed", failed = TRUE)
    } else {
      list(correlation = diag(ncol(data)), thresholds = NULL, type = correlation_type,
           smoothed = FALSE, warnings = character(), failed = FALSE)
    }
  }
  restore <- stub_build_factor_correlation(fake_build)
  on.exit(restore(), add = TRUE)
  result <- compute_parallel_analysis(df, n_iter = 4, cor_type = "polychoric",
                                      cor_matrix = diag(ncol(df)))
  # Only the successful iterations (identity matrices) contribute, so every threshold is 1.
  expect_true(all(abs(result$table$random_eigenvalue_threshold - 1) < 1e-8))
})

test_that("verification pass 4 findings (issue #26623)", {
  df <- factanal_ordinal_fixture(n = 200)

  # Manual Polychoric on continuous columns: there is no categorical variable to describe, so the
  # category rows must report Not Available rather than inventing "400 categories" and flagging
  # every variable and pair.
  continuous <- data.frame(a = stats::rnorm(200), b = stats::rnorm(200), c = stats::rnorm(200))
  continuous_selection <- select_factor_correlation_type(continuous)
  continuous_diagnostics <- compute_polychoric_diagnostics(
    continuous, list(correlation = diag(3), thresholds = NULL, type = "polychoric",
                     smoothed = FALSE, warnings = character(), failed = FALSE),
    continuous_selection)
  expect_equal(continuous_diagnostics$Judgement[[1]], "Not Available")
  expect_equal(continuous_diagnostics$Judgement[[2]], "Not Available")
  expect_equal(continuous_diagnostics$Judgement[[3]], "Not Available")
  expect_equal(continuous_diagnostics$status[[1]], "na")

  # A DECLARED category with zero responses is the sparsest case there is; table() never shows it,
  # so it has to be padded in from the defined levels.
  zero_level_df <- data.frame(
    a = factor(c("low", "mid", "high")[c(1, 2, 1, 2, 1, 2, 1, 2)], levels = c("low", "mid", "high"), ordered = TRUE),
    b = factor(c("low", "mid", "high")[c(1, 2, 3, 2, 1, 2, 3, 2)], levels = c("low", "mid", "high"), ordered = TRUE),
    c = factor(c("low", "mid", "high")[c(3, 2, 1, 2, 3, 2, 1, 2)], levels = c("low", "mid", "high"), ordered = TRUE)
  )
  zero_selection <- select_factor_correlation_type(zero_level_df)
  zero_diagnostics <- compute_polychoric_diagnostics(
    encode_factanal_data(zero_level_df, zero_selection),
    list(correlation = diag(3), thresholds = NULL, type = "polychoric",
         smoothed = FALSE, warnings = character(), failed = FALSE),
    zero_selection)
  expect_equal(zero_diagnostics$Judgement[[1]], "All categorical variables have 3 categories")
  # Column `a` never takes its third level -> 0% of the responses -> sparse.
  expect_equal(zero_diagnostics$Judgement[[2]], "Detected in 1 variable")
})

test_that("build_factor_correlation retries with correct=0 before degrading", {
  # Same sparse contingency pattern that makes psych::polychoric(correct=0.5) throw
  # "attempt to set 'rownames' on an object with no dimensions", while correct=0 succeeds.
  set.seed(1)
  df <- data.frame(lapply(1:8, function(i) as.integer(sample(1:5, 12, replace = TRUE))))
  names(df) <- paste0("q", 1:8)

  expect_true(inherits(
    suppressWarnings(tryCatch(psych::polychoric(df, correct = 0.5), error = function(e) e)),
    "error"))
  expect_false(inherits(
    suppressWarnings(tryCatch(psych::polychoric(df, correct = 0), error = function(e) e)),
    "error"))

  result <- build_factor_correlation(df, "polychoric")
  expect_equal(result$type, "polychoric")
  expect_false(result$failed)
  expect_equal(dim(result$correlation), c(8L, 8L))
  expect_true(any(grepl("continuity correction of 0", result$warnings)))

  # End-to-end: exp_factanal must keep polychoric rather than degrade to Pearson.
  fit <- exp_factanal(df, q1, q2, q3, q4, q5, q6, q7, q8, nfactors = 2, rotate = "varimax",
                      cor_type = "polychoric", parallel_n_iter = 3)$model[[1]]
  expect_equal(fit$correlation_type, "polychoric")
  expect_equal(fit$correlation_degraded_from, "")
})

test_that("a failed estimation degrades to Pearson end to end (issue #26623)", {
  # The degrade path mutates `resolved` after construction and re-points the diagnostics and the
  # parallel analysis; exercise it through exp_factanal, not just the helper.
  df <- factanal_ordinal_fixture(n = 150)
  original_build <- build_factor_correlation
  failing_build <- function(data, correlation_type = c("pearson", "polychoric", "tetrachoric", "mixed"),
                            use = "pairwise.complete.obs", correct = 0.5) {
    correlation_type <- match.arg(correlation_type)
    if (identical(correlation_type, "pearson")) {
      return(original_build(data, correlation_type, use = use, correct = correct))
    }
    list(correlation = stats::cor(data, use = use), thresholds = NULL, type = "pearson",
         smoothed = FALSE, warnings = "simulated polychoric failure", failed = TRUE)
  }
  restore <- stub_build_factor_correlation(failing_build)
  on.exit(restore(), add = TRUE)

  fit <- exp_factanal(df, q1, q2, q3, q4, q5, q6, nfactors = 2, rotate = "varimax",
                      cor_type = "polychoric", parallel_n_iter = 3)$model[[1]]
  # The fit itself falls back to Pearson...
  expect_equal(fit$correlation_type, "pearson")
  # ...but the report can still say WHICH correlation failed, and still shows the diagnostics.
  expect_equal(fit$correlation_degraded_from, "polychoric")
  expect_true(grepl("could not be estimated", fit$correlation_reason))
  expect_false(is.null(fit$cor_diagnostics))
  method_tbl <- tidy(fit, type = "analysis_method")
  expect_equal(method_tbl$Value[[1]], "Pearson Correlation")
  expect_equal(method_tbl$degraded_from[[1]], "polychoric")
  expect_equal(method_tbl$has_diagnostics[[1]], "TRUE")
  diagnostics <- tidy(fit, type = "cor_diagnostics")
  expect_equal(diagnostics$Judgement[[4]], "Estimation failed")
  # The Pearson fallback matrix says nothing about the correlation that failed.
  expect_equal(diagnostics$Judgement[[5]], "Not Available")
  expect_equal(diagnostics$Judgement[[6]], "Not Available")
  # Scores and the parallel analysis still come out of the fallback matrix, not a half-built one.
  expect_false(is.null(fit$scores))
  expect_false(is.null(fit$parallel))
})

test_that("Repeat By groups all use the same correlation (issue #26623)", {
  # Group A is ordinal (would pick Polychoric on its own), group B is continuous (Pearson). One
  # report describes one correlation, and loadings must be comparable across facets, so the choice
  # is made once from the whole data.
  set.seed(31)
  n <- 200
  latent_a <- stats::rnorm(n)
  ordinal_col <- function(latent) {
    z <- 0.75 * latent + sqrt(1 - 0.75^2) * stats::rnorm(n)
    as.integer(cut(z, breaks = c(-Inf, stats::quantile(z, c(.55, .75, .88, .96)), Inf), labels = FALSE))
  }
  group_a <- data.frame(g = "A", q1 = ordinal_col(latent_a), q2 = ordinal_col(latent_a),
                        q3 = ordinal_col(latent_a), stringsAsFactors = FALSE)
  latent_b <- stats::rnorm(n)
  group_b <- data.frame(g = "B",
                        q1 = latent_b + stats::rnorm(n), q2 = latent_b + stats::rnorm(n),
                        q3 = latent_b + stats::rnorm(n), stringsAsFactors = FALSE)
  grouped <- dplyr::group_by(rbind(group_a, group_b), g)

  model_df <- exp_factanal(grouped, q1, q2, q3, nfactors = 1, rotate = "none", parallel_n_iter = 3)
  types <- vapply(model_df$model, function(m) m$correlation_type, character(1))
  expect_equal(length(unique(types)), 1)
  # And the per-group diagnostics still reflect that one correlation.
  has_diagnostics <- vapply(model_df$model, function(m) !is.null(m$cor_diagnostics), logical(1))
  expect_equal(length(unique(has_diagnostics)), 1)
})

test_that("an unsupported facet is skipped, not fatal, under Repeat By (issue #26623)", {
  # Mirrors the not-enough-columns guard: one degenerate facet must not abort the whole run.
  # Facet "good" sees only two distinct answers for q1 (binary -> supported); facet "bad" sees
  # three (nominal -> unsupported), so the unsupported branch is reached per group.
  set.seed(77)
  n <- 120
  latent <- stats::rnorm(n)
  ordinal_col <- function() {
    z <- 0.75 * latent + sqrt(1 - 0.75^2) * stats::rnorm(n)
    as.integer(cut(z, breaks = c(-Inf, stats::quantile(z, c(.55, .75, .88, .96)), Inf), labels = FALSE))
  }
  good <- data.frame(g = "good", q1 = sample(c("yes", "no"), n, TRUE),
                     q2 = ordinal_col(), q3 = ordinal_col(), stringsAsFactors = FALSE)
  bad <- data.frame(g = "bad", q1 = sample(c("yes", "no", "maybe"), n, TRUE),
                    q2 = ordinal_col(), q3 = ordinal_col(), stringsAsFactors = FALSE)
  grouped <- dplyr::group_by(rbind(good, bad), g)

  model_df <- exp_factanal(grouped, q1, q2, q3, nfactors = 1, rotate = "none", parallel_n_iter = 3)
  # The good facet still produced a model; the unsupported one was skipped (NULL model, the same
  # shape the not-enough-columns guard produces), not fatal.
  expect_true("good" %in% model_df$g)
  good_model <- model_df$model[[which(model_df$g == "good")]]
  expect_false(is.null(good_model))
  if ("bad" %in% model_df$g) {
    expect_true(is.null(model_df$model[[which(model_df$g == "bad")]]))
  }

  # Without Repeat By the same unsupported data still raises the error, so nothing is swallowed.
  expect_error(exp_factanal(dplyr::select(bad, q1, q2, q3), q1, q2, q3, nfactors = 1, parallel_n_iter = 3),
               "EXP-ANA-35")
})

test_that("a categorical correlation always yields a non-empty diagnostics table (issue #26623)", {
  # The report ALWAYS shows the diagnostics section for a categorical correlation, so cor_diagnostics
  # must never be NULL there -- otherwise the section renders a heading over an empty table, or (with
  # the old has_diagnostics-only gate) vanishes entirely on a real polychoric analysis.
  df <- factanal_ordinal_fixture()
  poly <- exp_factanal(df, q1, q2, q3, q4, q5, q6, nfactors = 2, rotate = "varimax",
                       cor_type = "polychoric", parallel_n_iter = 3)$model[[1]]
  expect_false(is.null(poly$cor_diagnostics))
  expect_equal(nrow(poly$cor_diagnostics), 6)
  # The analysis_method table reports it as available, as an explicit "TRUE" string (not a logical
  # that could serialize as "1"/"0" and read falsy on the client).
  method_tbl <- tidy(poly, type = "analysis_method")
  expect_equal(method_tbl$has_diagnostics[[1]], "TRUE")
  expect_true(method_tbl$has_diagnostics[[1]] %in% c("TRUE", "FALSE"))

  # If the diagnostics computation itself fails, the fallback keeps the section non-empty rather
  # than returning NULL.
  fallback <- unavailable_polychoric_diagnostics()
  expect_equal(nrow(fallback), 6)
  expect_true(all(fallback$Judgement == "Not Available"))
  expect_equal(fallback$Diagnostic, poly$cor_diagnostics$Diagnostic)
})
