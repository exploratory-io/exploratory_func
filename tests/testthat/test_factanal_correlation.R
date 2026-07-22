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
  expect_equal(method_tbl$Value[[3]], "Varimax")
  expect_equal(method_tbl$Value[[4]], "6")
  expect_equal(method_tbl$Value[[5]], as.character(nrow(df)))
  # Hidden columns the client binds the report explanation from.
  expect_equal(unique(method_tbl$correlation_type), "polychoric")
  expect_true(all(method_tbl$correlation_is_auto))
  expect_true(nchar(method_tbl$reason[[1]]) > 0)

  diagnostics <- tidy(poly, type = "cor_diagnostics")
  expect_equal(diagnostics$Diagnostic,
               c("Number of Categories", "Sparse Categories", "Empty Category Combinations",
                 "Correlation Estimation Failures", "Positive Definiteness of the Correlation Matrix",
                 "Smoothing Applied"))
  expect_equal(diagnostics$Judgement[[1]], "All variables have 5 categories")
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
  expect_false(method_tbl$correlation_is_auto[[1]])
  expect_true(method_tbl$has_diagnostics[[1]])

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
  expect_equal(diagnostics$Judgement[[1]], "All variables have 5 categories")
  expect_equal(diagnostics$Judgement[[2]], "Detected in 3 variables")   # the 3 ordinal ones at most
  expect_false(grepl("Detected in 10 variable pairs", diagnostics$Judgement[[3]]))
})
