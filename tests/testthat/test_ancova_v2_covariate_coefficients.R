# tidy(type = "covariate_coefficients") for ANCOVA V2 (tam#39478).
# how to run this test: devtools::test(filter="ancova_v2_covariate_coefficients")
#
# One row per covariate from the ADDITIVE (common-slope) model: the change in
# the target per 1-unit increase of the covariate, holding the group and the
# other covariates fixed. Every number is compared with an INDEPENDENT
# lm(target ~ group + covariates) fit computed in the test on the raw scale.
context("ANCOVA V2 covariate coefficients, tam#39478")

COV_COEF_COLS <- c("Covariate", "Coefficient", "Standard Error", "Conf Low",
                   "Conf High", "P Value")

make_cc_data <- function(n_per_group = 50, seed = 11) {
  set.seed(seed)
  group <- factor(rep(c("A", "B", "C"), each = n_per_group))
  n <- length(group)
  x1 <- runif(n, 0, 10)
  x2 <- rnorm(n, 50, 8)
  x3 <- rnorm(n, 5, 2)
  y <- c(A = 0, B = 4, C = 9)[as.character(group)] + 2 * x1 - 0.3 * x2 + 1.2 * x3 +
    rnorm(n, 0, 3)
  data.frame(y = as.numeric(y), group = group, X1 = x1, X2 = x2, X3 = x3,
             stringsAsFactors = FALSE)
}

# Independent reference: plain lm on the raw covariates. Columns are renamed to
# syntactic names first so lm's coefficient names are predictable.
reference_coefs <- function(df, covariates, level = 0.95) {
  safe <- paste0("v", seq_along(covariates))
  ref_df <- data.frame(y = df$y, group = df$group, stringsAsFactors = FALSE)
  for (j in seq_along(covariates)) ref_df[[safe[j]]] <- df[[covariates[j]]]
  m <- stats::lm(stats::reformulate(c("group", safe), response = "y"), data = ref_df)
  cf <- summary(m)$coefficients
  ci <- stats::confint(m, level = level)
  list(est = unname(cf[safe, "Estimate"]), se = unname(cf[safe, "Std. Error"]),
       lo = unname(ci[safe, 1]), hi = unname(ci[safe, 2]),
       p = unname(cf[safe, "Pr(>|t|)"]))
}

cc_tidy <- function(df, covariates, ...) {
  tidy_rowwise(exp_ancova(df, "y", "group", covariates = covariates, ...), model,
               type = "covariate_coefficients")
}

expect_matches_reference <- function(tbl, df, covariates, level = 0.95) {
  ref <- reference_coefs(df, covariates, level)
  expect_equal(colnames(tbl), COV_COEF_COLS)
  expect_equal(nrow(tbl), length(covariates))
  expect_equal(as.character(tbl$Covariate), covariates)
  expect_equal(tbl$Coefficient, ref$est, tolerance = 1e-8)
  expect_equal(tbl$`Standard Error`, ref$se, tolerance = 1e-8)
  expect_equal(tbl$`Conf Low`, ref$lo, tolerance = 1e-8)
  expect_equal(tbl$`Conf High`, ref$hi, tolerance = 1e-8)
  expect_equal(tbl$`P Value`, ref$p, tolerance = 1e-6)
}

test_that("one covariate matches an independent lm + confint + summary", {
  df <- make_cc_data()
  expect_matches_reference(cc_tidy(df, "X1"), df, "X1")
})

test_that("two covariates match an independent lm, in the order the user gave them", {
  df <- make_cc_data()
  expect_matches_reference(cc_tidy(df, c("X1", "X2")), df, c("X1", "X2"))
  expect_matches_reference(cc_tidy(df, c("X2", "X1")), df, c("X2", "X1"))
})

test_that("three covariates match an independent lm", {
  df <- make_cc_data()
  expect_matches_reference(cc_tidy(df, c("X1", "X2", "X3")), df, c("X1", "X2", "X3"))
})

test_that("the confidence interval uses the analysis's configured level (1 - alpha)", {
  df <- make_cc_data()
  tbl <- cc_tidy(df, c("X1", "X2"), test_sig_level = 0.10)
  ref <- reference_coefs(df, c("X1", "X2"), level = 0.90)
  expect_equal(tbl$`Conf Low`, ref$lo, tolerance = 1e-8)
  expect_equal(tbl$`Conf High`, ref$hi, tolerance = 1e-8)
  # The p-value does not depend on the interval level.
  expect_equal(tbl$`P Value`, reference_coefs(df, c("X1", "X2"))$p, tolerance = 1e-6)
})

test_that("the coefficient is on the ORIGINAL covariate scale, not shifted by centering", {
  df <- make_cc_data()
  shifted <- df
  shifted$X1 <- shifted$X1 - mean(shifted$X1)       # already mean-centered by the user
  shifted$X2 <- shifted$X2 + 1000                   # arbitrary big offset
  a <- cc_tidy(df, c("X1", "X2"))
  b <- cc_tidy(shifted, c("X1", "X2"))
  # A slope is invariant to shifting the covariate; only the (unreported)
  # intercept moves.
  expect_equal(a$Coefficient, b$Coefficient, tolerance = 1e-8)
  expect_equal(a$`Standard Error`, b$`Standard Error`, tolerance = 1e-8)
  expect_equal(a$`P Value`, b$`P Value`, tolerance = 1e-6)
  # And a rescaled covariate changes the slope by exactly the inverse factor.
  scaled <- df
  scaled$X1 <- scaled$X1 * 10
  c10 <- cc_tidy(scaled, c("X1", "X2"))
  expect_equal(c10$Coefficient[1], a$Coefficient[1] / 10, tolerance = 1e-8)
})

test_that("zero covariates (one-way ANOVA) returns a zero-row tibble with the same columns", {
  df <- make_cc_data()
  tbl <- tidy_rowwise(exp_ancova(df, "y", "group", covariates = NULL), model,
                      type = "covariate_coefficients")
  expect_equal(nrow(tbl), 0)
  expect_equal(colnames(tbl), COV_COEF_COLS)
})

test_that("a zero-covariate V2 model object also returns the zero-row shape", {
  df <- make_cc_data()
  fit <- exp_ancova(df, "y", "group", covariates = "X1")$model[[1]]
  fit$covariates <- character(0)
  tbl <- tidy(fit, type = "covariate_coefficients")
  expect_equal(nrow(tbl), 0)
  expect_equal(colnames(tbl), COV_COEF_COLS)
})

test_that("covariate names with spaces / multibyte / symbols come back verbatim", {
  stress <- "航空 会社 !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表"
  df <- make_cc_data()
  names(df)[names(df) == "X1"] <- stress
  names(df)[names(df) == "X2"] <- "売上 高"
  tbl <- cc_tidy(df, c(stress, "売上 高"))
  expect_equal(colnames(tbl), COV_COEF_COLS)
  expect_equal(as.character(tbl$Covariate), c(stress, "売上 高"))
  ref <- reference_coefs(df, c(stress, "売上 高"))
  expect_equal(tbl$Coefficient, ref$est, tolerance = 1e-8)
  expect_equal(tbl$`Conf Low`, ref$lo, tolerance = 1e-8)
  expect_equal(tbl$`P Value`, ref$p, tolerance = 1e-6)
  # No internal name may leak.
  expect_false(any(grepl("ancova_x", tbl$Covariate, fixed = TRUE)))
})

test_that("Repeat By gives one block per group, each equal to a stand-alone fit", {
  df <- make_cc_data()
  df$seg <- rep(c("s1", "s2"), length.out = nrow(df))
  fit <- exp_ancova(df %>% dplyr::group_by(seg), "y", "group", covariates = c("X1", "X2"))
  tbl <- tidy_rowwise(fit, model, type = "covariate_coefficients")
  expect_equal(nrow(tbl), 4)
  expect_true("seg" %in% colnames(tbl))
  for (s in c("s1", "s2")) {
    sub <- tbl %>% dplyr::ungroup() %>% dplyr::filter(seg == s)
    expect_matches_reference(sub %>% dplyr::select(-seg), df %>% dplyr::filter(seg == s),
                             c("X1", "X2"))
  }
})

test_that("an error model returns an empty tibble rather than throwing", {
  fit <- structure(list(message = "boom"),
                   class = c("ancova_v2_exploratory", "error", "condition"))
  expect_equal(nrow(tidy(fit, type = "covariate_coefficients")), 0)
})
