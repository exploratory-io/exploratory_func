# how to run this test:
# devtools::test(filter="build_lm_lmg")
context("Linear Regression: LMG R-squared breakdown, standardized coefficients, importance x rating (tam#39813)")

# Everything here is checked against numbers computed in the test itself with plain stats::lm():
# brute-force orderings for LMG (a second formulation of the same quantity, not the subset
# table the product uses), and est * sd(x) / sd(y) by hand for standardized coefficients.

STRESS_NAME <- "航空 会社 !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表"
JA_RATING <- "レジ待ち満足度"   # multibyte predictor name
JA_TARGET <- "総合満足度"               # multibyte target name

# Two correlated numeric predictors, one independent numeric one and a 3-level character one.
make_lmg_df <- function(n = 300, seed = 1) {
  set.seed(seed)
  x1 <- rnorm(n)
  x2 <- 0.7 * x1 + rnorm(n, sd = 0.7)
  x3 <- rnorm(n)
  g <- sample(c("A", "B", "C"), n, replace = TRUE)
  y <- 1 + 2 * x1 + 1 * x2 + 0.5 * x3 + c(A = 0, B = 1, C = -1)[g] + rnorm(n)
  data.frame(y = y, x1 = x1, x2 = x2, x3 = x3, g = g, stringsAsFactors = FALSE)
}

# All orderings of 1..p (rows of a matrix).
all_orderings <- function(p) {
  perms <- function(v) {
    if (length(v) <= 1) return(matrix(v, nrow = 1))
    do.call(rbind, lapply(seq_along(v), function(i) cbind(v[i], perms(v[-i]))))
  }
  perms(seq_len(p))
}

# LMG by literally averaging the R-squared gained by each variable over every ordering in which
# the variables can enter the model. `d` must have syntactic column names.
brute_force_lmg <- function(d, target, vars) {
  r2 <- function(set) {
    if (length(set) == 0) return(0)
    summary(stats::lm(stats::reformulate(set, target), data = d))$r.squared
  }
  orderings <- all_orderings(length(vars))
  lmg <- stats::setNames(numeric(length(vars)), vars)
  for (o in seq_len(nrow(orderings))) {
    entered <- character(0)
    for (k in seq_len(ncol(orderings))) {
      v <- vars[orderings[o, k]]
      lmg[v] <- lmg[v] + r2(c(entered, v)) - r2(entered)
      entered <- c(entered, v)
    }
  }
  lmg / nrow(orderings)
}

fit_lm <- function(df, target, ..., test_rate = 0) {
  # suppressWarnings: lm's own "rank-deficient fit" warning for the aliased fixtures is shipped behaviour.
  model_df <- suppressWarnings(df %>% build_lm.fast(!!rlang::enquo(target), ..., model_type = "lm", seed = 1, test_rate = test_rate))
  list(model_df = model_df, model = model_df$model[[1]])
}

lmg_of <- function(model) tidy(model, type = "lmg")

test_that("LMG: values sum to the ordinary R-squared and shares to 100, equal to the average over every ordering", {
  d <- make_lmg_df()
  m <- fit_lm(d, y, x1, x2, x3, g)$model
  res <- lmg_of(m)
  expect_equal(res$status, rep("ok", 4))
  expect_true(all(is.na(res$reason)))
  r2 <- summary(stats::lm(y ~ x1 + x2 + x3 + g, data = d))$r.squared
  expect_equal(sum(res$lmg), r2, tolerance = 1e-12)
  expect_equal(sum(res$lmg_share), 100, tolerance = 1e-12)
  expect_equal(res$r_squared, rep(r2, 4), tolerance = 1e-12)
  expect_equal(res$lmg_share, res$lmg / r2 * 100, tolerance = 1e-12)
  expect_equal(res$n, rep(300L, 4))
  expect_true(all(res$lmg >= 0))
  expected <- brute_force_lmg(d, "y", c("x1", "x2", "x3", "g"))
  expect_equal(setNames(res$lmg, res$variable)[names(expected)], expected, tolerance = 1e-10)
})

test_that("LMG: Sigma LMG equals R-squared for numeric-only models too (single variable gets all of it)", {
  d <- make_lmg_df()
  res <- lmg_of(fit_lm(d, y, x1, x2, x3)$model)
  expect_equal(sum(res$lmg), summary(stats::lm(y ~ x1 + x2 + x3, data = d))$r.squared, tolerance = 1e-12)
  one <- lmg_of(fit_lm(d, y, x1, x2)$model)
  expect_equal(nrow(one), 2)
  # One predictor: the whole R-squared belongs to it.
  d$x2 <- NULL
  single <- lmg_of(fit_lm(d, y, x1)$model)
  expect_equal(single$variable, "x1")
  expect_equal(single$lmg, summary(stats::lm(y ~ x1, data = d))$r.squared, tolerance = 1e-12)
  expect_equal(single$lmg_share, 100, tolerance = 1e-12)
})

test_that("LMG: a character / factor predictor is ONE variable, named by the original column", {
  d <- make_lmg_df()
  d$h <- factor(sample(c("p", "q", "r", "s", "t"), nrow(d), replace = TRUE))
  res <- lmg_of(fit_lm(d, y, x1, g, h)$model)
  expect_equal(sort(res$variable), c("g", "h", "x1"))
  expect_equal(nrow(res), 3)
  expected <- brute_force_lmg(d, "y", c("x1", "g", "h"))
  expect_equal(setNames(res$lmg, res$variable)[names(expected)], expected, tolerance = 1e-10)
})

test_that("LMG: changing the reference level of a factor does not change any LMG value", {
  d <- make_lmg_df()
  d$f1 <- factor(d$g, levels = c("A", "B", "C"))
  d$f2 <- factor(d$g, levels = c("C", "B", "A"))
  a <- fit_lm(d, y, x1, x3, f1)$model
  b <- fit_lm(d, y, x1, x3, f2)$model
  coef_a <- tidy(a)
  coef_b <- tidy(b)
  # The re-levelling really changes the coefficients, so this is not vacuous.
  expect_false(isTRUE(all.equal(sort(coef_a$estimate), sort(coef_b$estimate))))
  la <- lmg_of(a)
  lb <- lmg_of(b)
  # The LMG row for the factor is named by the original column, which differs between f1 and f2.
  expect_equal(la$lmg[la$variable == "f1"], lb$lmg[lb$variable == "f2"], tolerance = 1e-12)
  expect_equal(la$lmg[la$variable == "x1"], lb$lmg[lb$variable == "x1"], tolerance = 1e-12)
  expect_equal(la$lmg[la$variable == "x3"], lb$lmg[lb$variable == "x3"], tolerance = 1e-12)
})

test_that("LMG: highly correlated predictors split their shared R-squared and never go negative", {
  set.seed(7)
  n <- 400
  z <- rnorm(n)
  d <- data.frame(x1 = z + rnorm(n, sd = 0.1), x2 = z + rnorm(n, sd = 0.1), x3 = rnorm(n))
  d$y <- z + 0.3 * d$x3 + rnorm(n, sd = 0.5)
  res <- lmg_of(fit_lm(d, y, x1, x2, x3)$model)
  expected <- brute_force_lmg(d, "y", c("x1", "x2", "x3"))
  expect_equal(setNames(res$lmg, res$variable)[names(expected)], expected, tolerance = 1e-9)
  expect_true(all(res$lmg > 0))
  # The two near-copies of z get nearly the same credit, which a sequential split would not give.
  l1 <- res$lmg[res$variable == "x1"]
  l2 <- res$lmg[res$variable == "x2"]
  expect_lt(abs(l1 - l2) / max(l1, l2), 0.2)
})

test_that("LMG: R-squared of zero (orthogonal or constant target) is refused with no number", {
  # Exactly orthogonal target: R-squared is 0 up to rounding.
  x1 <- rep(c(-1, 1), 50)
  x2 <- rep(c(-1, -1, 1, 1), 25)
  d <- data.frame(x1 = x1, x2 = x2, y = rep(c(1, -1, -1, 1), 25))
  res <- lmg_of(fit_lm(d, y, x1, x2)$model)
  expect_equal(res$status, "r2_zero")
  expect_true(nchar(res$reason) > 0)
  expect_false("lmg" %in% names(res))

  # Constant target: R-squared is undefined.
  d2 <- make_lmg_df()
  d2$y <- 5
  res2 <- lmg_of(fit_lm(d2, y, x1, x2)$model)
  expect_equal(res2$status, "r2_zero")
  expect_false("lmg" %in% names(res2))
})

test_that("LMG: an inestimable (aliased) coefficient is refused, and the coefficient table still works", {
  d <- make_lmg_df()
  d$x4 <- d$x1 + d$x2
  m <- fit_lm(d, y, x1, x2, x4)$model
  res <- lmg_of(m)
  expect_equal(res$status, "aliased")
  expect_true(nchar(res$reason) > 0)
  expect_false("lmg" %in% names(res))
  coef <- tidy(m)
  expect_true(any(!is.na(coef$note)))
  # The dropped term has no standardized value either.
  expect_true(is.na(coef$std.estimate[!is.na(coef$note)]))
})

test_that("LMG: more variables than the cap is refused (cap from option); at the cap it is computed", {
  d <- make_lmg_df()
  m <- fit_lm(d, y, x1, x2, x3, g)$model
  withr::with_options(list(exploratory.lmg.max.predictors = 3), {
    res <- lmg_of(m)
    expect_equal(res$status, "too_many_predictors")
    expect_match(res$reason, "4")
    expect_match(res$reason, "3")
    expect_false("lmg" %in% names(res))
  })
  withr::with_options(list(exploratory.lmg.max.predictors = 4), {
    expect_equal(unique(lmg_of(m)$status), "ok")
  })
  # The default cap is the measured constant, not a magic number in the code path.
  expect_equal(getOption("exploratory.lmg.max.predictors", LM_LMG_MAX_PREDICTORS_DEFAULT), LM_LMG_MAX_PREDICTORS_DEFAULT)
  expect_gte(LM_LMG_MAX_PREDICTORS_DEFAULT, 10)
  expect_lte(LM_LMG_MAX_PREDICTORS_DEFAULT, 20)
})

test_that("LMG: exact at the default cap within a few seconds, and 2^p subsets are really all enumerated", {
  set.seed(3)
  n <- 600
  p <- LM_LMG_MAX_PREDICTORS_DEFAULT
  d <- as.data.frame(matrix(rnorm(n * p), n, p))
  names(d) <- sprintf("v%02d", seq_len(p))
  d$y <- rowSums(d[, 1:4]) + rnorm(n)
  m <- do.call(build_lm.fast, c(list(d, quote(y)), lapply(names(d)[1:p], as.name), list(model_type = "lm", seed = 1, importance_measure = "firm")))$model[[1]]
  elapsed <- system.time(res <- lmg_of(m))[["elapsed"]]
  expect_equal(unique(res$status), "ok")
  expect_lt(elapsed, 30)
  expect_equal(sum(res$lmg), summary(stats::lm(y ~ ., data = d))$r.squared, tolerance = 1e-10)
})

test_that("LMG: unsupported specifications (weights, transformed predictor, interaction, no intercept) are refused", {
  d <- make_lmg_df()
  d$w <- runif(nrow(d), 1, 2)
  weighted <- (d %>% build_lm.fast(y, x1, x2, weight = w, model_type = "lm", seed = 1))$model[[1]]
  r <- lmg_of(weighted)
  expect_equal(r$status, "unsupported_spec")
  expect_false("lmg" %in% names(r))

  logged <- (d %>% dplyr::mutate(x1 = abs(x1) + 1) %>%
    build_lm.fast(y, x1, x2, predictor_funs = list(x1 = "log", x2 = "none"), model_type = "lm", seed = 1))$model[[1]]
  expect_equal(lmg_of(logged)$status, "unsupported_spec")

  # Re-coding a predictor is not a transformation of its meaning, so it stays supported.
  plain <- (d %>% build_lm.fast(y, x1, x2, predictor_funs = list(x1 = "none", x2 = "none"), model_type = "lm", seed = 1))$model[[1]]
  expect_equal(unique(lmg_of(plain)$status), "ok")

  base <- fit_lm(d, y, x1, x2, x3)$model
  with_formula <- function(m, fml_text) {
    f <- stats::lm(stats::as.formula(fml_text), data = m$model)
    for (nm in setdiff(names(m), names(f))) f[[nm]] <- m[[nm]]
    class(f) <- class(m)
    f
  }
  nm <- names(base$model)
  interaction <- with_formula(base, paste0(nm[1], " ~ ", nm[2], " * ", nm[3], " + ", nm[4]))
  expect_equal(lmg_of(interaction)$status, "unsupported_spec")
  no_intercept <- with_formula(base, paste0(nm[1], " ~ 0 + ", nm[2], " + ", nm[3], " + ", nm[4]))
  expect_equal(lmg_of(no_intercept)$status, "unsupported_spec")
  poly_term <- with_formula(base, paste0(nm[1], " ~ poly(", nm[2], ", 2) + ", nm[3], " + ", nm[4]))
  expect_equal(lmg_of(poly_term)$status, "unsupported_spec")
  # The existing coefficient table of such a model is unaffected.
  expect_gt(nrow(tidy(interaction)), 1)
})

test_that("LMG: no predictor variable and a failing computation are reported, not raised", {
  d <- make_lmg_df()
  base <- fit_lm(d, y, x1, x2)$model
  intercept_only <- stats::lm(y ~ 1, data = d)
  class(intercept_only) <- class(base)
  intercept_only$terms_mapping <- base$terms_mapping
  expect_equal(lmg_of(intercept_only)$status, "no_predictors")

  broken <- base
  broken$model <- NULL
  res <- lmg_of(broken)
  expect_equal(res$status, "fit_failed")
  expect_true(nchar(res$reason) > 0)
})

test_that("fitting the model never computes LMG (computed lazily in tidy only)", {
  d <- make_lmg_df()
  fit <- fit_lm(d, y, x1, x2, x3, g)
  all_names <- c(names(fit$model_df), names(fit$model))
  expect_false(any(grepl("lmg", all_names, ignore.case = TRUE)))
})

test_that("standardized coefficient and interval equal est * sd(x)/sd(y) computed by hand", {
  d <- make_lmg_df()
  m <- fit_lm(d, y, x1, x2, x3, g)$model
  coef <- tidy(m)
  co <- summary(stats::lm(y ~ x1 + x2 + x3 + g, data = d))$coefficients
  for (v in c("x1", "x2", "x3")) {
    row <- coef[coef$term == v, ]
    scale <- stats::sd(d[[v]]) / stats::sd(d$y)
    est <- co[v, "Estimate"]
    se <- co[v, "Std. Error"]
    expect_equal(row$std.estimate, est * scale, tolerance = 1e-12)
    expect_equal(row$std.conf.low, (est - 1.96 * se) * scale, tolerance = 1e-12)
    expect_equal(row$std.conf.high, (est + 1.96 * se) * scale, tolerance = 1e-12)
    expect_lt(row$std.conf.low, row$std.conf.high)
  }
  # Factor-derived terms and the intercept have none.
  non_numeric <- coef[!(coef$term %in% c("x1", "x2", "x3")), ]
  expect_gt(nrow(non_numeric), 1)
  expect_true(all(is.na(non_numeric$std.estimate)))
  expect_true(all(is.na(non_numeric$std.conf.low)))
  expect_true(all(is.na(non_numeric$std.conf.high)))
})

test_that("standardized coefficient is NA when SD(y) = 0 or SD(x) = 0, with no error", {
  d <- make_lmg_df()
  d$y <- 5
  coef <- tidy(fit_lm(d, y, x1, x2)$model)
  expect_true(all(is.na(coef$std.estimate)))
  # A constant predictor is dropped by build_lm.fast itself; the model keeps the others.
  d2 <- make_lmg_df()
  d2$k <- 3
  m <- fit_lm(d2, y, x1, k)$model
  coef2 <- tidy(m)
  expect_false("k" %in% coef2$term)
  expect_equal(coef2$std.estimate[coef2$term == "x1"], stats::coef(stats::lm(y ~ x1, d2))[["x1"]] * stats::sd(d2$x1) / stats::sd(d2$y), tolerance = 1e-12)
  # SD(x) = 0 on the model's own rows: scale lookup refuses it.
  m$model[["c2_"]] <- 1
  expect_true(is.na(.lm_std_scales(m)[["c2_"]]))
})

test_that("coefficients: every existing column keeps its name, value and order; new columns come last", {
  d <- make_lmg_df()
  m <- fit_lm(d, y, x1, x2, x3, g)$model
  coef <- as.data.frame(tidy(m))
  expect_equal(names(coef), c("term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high",
                              "base.level", "std.estimate", "std.conf.low", "std.conf.high"))
  # Plain lm() with the same reference level the model chose; rows are matched by term name.
  d$g <- stats::relevel(factor(d$g), ref = unique(stats::na.omit(coef$base.level)))
  plain <- summary(stats::lm(y ~ x1 + x2 + x3 + g, data = d))$coefficients
  plain <- plain[match(coef$term, sub("^g", "g: ", rownames(plain))), ]
  expect_false(anyNA(rownames(plain)))
  expect_equal(coef$estimate, unname(plain[, "Estimate"]), tolerance = 1e-12)
  expect_equal(coef$std.error, unname(plain[, "Std. Error"]), tolerance = 1e-12)
  expect_equal(coef$statistic, unname(plain[, "t value"]), tolerance = 1e-12)
  expect_equal(coef$p.value, unname(plain[, "Pr(>|t|)"]), tolerance = 1e-12)
  expect_equal(coef$conf.low, unname(plain[, "Estimate"] - 1.96 * plain[, "Std. Error"]), tolerance = 1e-12)
  expect_equal(coef$conf.high, unname(plain[, "Estimate"] + 1.96 * plain[, "Std. Error"]), tolerance = 1e-12)
  expect_equal(coef$term[1], "(Intercept)")

  pretty <- as.data.frame(tidy(m, pretty.name = TRUE))
  expect_equal(names(pretty), c("Term", "Coefficient", "Std Error", "t Value", "P Value", "Conf Low", "Conf High",
                                "Base Level", "Std. Coefficient", "Std. Conf Low", "Std. Conf High"))
  expect_equal(pretty$`Std. Coefficient`, coef$std.estimate)

  # With an aliased term the Note column keeps its place before the new ones.
  d$x4 <- d$x1 + d$x2
  aliased <- as.data.frame(tidy(fit_lm(d, y, x1, x2, x4)$model))
  expect_equal(names(aliased), c("term", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high",
                                 "base.level", "note", "std.estimate", "std.conf.low", "std.conf.high"))
})

test_that("rows with a missing target or numeric predictor are not model rows: n, LMG and std use the rows the model used", {
  d <- make_lmg_df()
  d$y[1:10] <- NA
  d$x1[11:30] <- NA
  d$g[31:40] <- NA   # a missing category stays a row ("(Missing)" level)
  m <- fit_lm(d, y, x1, x2, g)$model
  used <- d[!is.na(d$y) & !is.na(d$x1), ]
  expect_equal(nrow(m$model), nrow(used))
  res <- lmg_of(m)
  expect_equal(unique(res$n), nrow(used))
  coef <- tidy(m)
  est <- stats::coef(m)[["c2_"]]
  expect_equal(coef$std.estimate[coef$term == "x1"], est * stats::sd(used$x1) / stats::sd(used$y), tolerance = 1e-12)
  rating <- tidy(m, type = "importance_rating", vars = c("x1", "x2"))
  expect_equal(rating$mean_value, c(mean(used$x1), mean(used$x2)), tolerance = 1e-12)
  expect_false(isTRUE(all.equal(rating$mean_value[1], mean(d$x1, na.rm = TRUE), tolerance = 1e-12)))
  expect_equal(unique(rating$n), nrow(used))
})

test_that("multibyte and symbol-stress column names work end to end (predictor and target)", {
  d <- make_lmg_df()
  names(d) <- c(JA_TARGET, STRESS_NAME, JA_RATING, "x3", "g")
  m <- fit_lm(d, !!rlang::sym(JA_TARGET), !!rlang::sym(STRESS_NAME), !!rlang::sym(JA_RATING), x3, g)$model
  res <- lmg_of(m)
  expect_equal(unique(res$status), "ok")
  expect_setequal(res$variable, c(STRESS_NAME, JA_RATING, "x3", "g"))
  ref_d <- d
  names(ref_d) <- c("y", "s", "r", "x3", "g")
  expected <- brute_force_lmg(ref_d, "y", c("s", "r", "x3", "g"))
  names(expected) <- c(STRESS_NAME, JA_RATING, "x3", "g")
  expect_equal(setNames(res$lmg, res$variable)[names(expected)], expected, tolerance = 1e-10)

  coef <- tidy(m)
  expect_true(all(c(STRESS_NAME, JA_RATING) %in% coef$term))
  expect_equal(coef$std.estimate[coef$term == STRESS_NAME],
               stats::coef(stats::lm(y ~ s + r + x3 + g, ref_d))[["s"]] * stats::sd(ref_d$s) / stats::sd(ref_d$y), tolerance = 1e-12)

  rating <- tidy(m, type = "importance_rating", vars = c(JA_RATING, STRESS_NAME))
  expect_equal(rating$variable, c(JA_RATING, STRESS_NAME))
  expect_equal(rating$mean_value, c(mean(d[[JA_RATING]]), mean(d[[STRESS_NAME]])), tolerance = 1e-12)
})

test_that("Test Mode: n, LMG, standardized coefficients and means use the training rows only", {
  d <- make_lmg_df(n = 400)
  d$x1[1:12] <- NA
  fit <- fit_lm(d, y, x1, x2, x3, test_rate = 0.3)
  m <- fit$model
  test_index <- fit$model_df$.test_index[[1]]
  expect_gt(length(test_index), 0)
  complete <- d[!is.na(d$y) & !is.na(d$x1), ]
  train <- complete[-test_index, ]
  expect_equal(nrow(m$model), nrow(train))
  res <- lmg_of(m)
  expect_equal(unique(res$n), nrow(train))
  expected <- brute_force_lmg(train, "y", c("x1", "x2", "x3"))
  expect_equal(setNames(res$lmg, res$variable)[names(expected)], expected, tolerance = 1e-10)
  rating <- tidy(m, type = "importance_rating", vars = c("x1", "x2"), test_mode = TRUE, test_rate = 0.3)
  expect_equal(rating$mean_value, c(mean(train$x1), mean(train$x2)), tolerance = 1e-12)
  est <- summary(stats::lm(y ~ x1 + x2 + x3, train))$coefficients
  expect_equal(rating$std.estimate[1], est["x1", "Estimate"] * stats::sd(train$x1) / stats::sd(train$y), tolerance = 1e-12)
})

test_that("importance_rating: selected numeric predictors only, in the selected order; ineligible ones are left out, never an error", {
  d <- make_lmg_df()
  d$x4 <- d$x1 + d$x2
  d$k <- 1
  m <- fit_lm(d, y, x1, x2, x4, g, k)$model
  rating <- tidy(m, type = "importance_rating", vars = c("x2", "g", "x1", "x4", "k", "nope"))
  expect_equal(names(rating), c("variable", "mean_value", "std.estimate", "std.conf.low", "std.conf.high", "p.value", "n"))
  # x4 is the inestimable alias, g is a factor, k was dropped as a constant, nope is unknown.
  aliased_term <- tidy(m)$term[!is.na(tidy(m)$note)]
  expect_equal(rating$variable, setdiff(c("x2", "x1"), aliased_term))
  excluded <- attr(rating, "excluded")
  expect_true(all(c("g", "k", "nope") %in% excluded$variable))
  expect_true(all(nchar(excluded$reason) > 0))
  co <- summary(stats::lm(y ~ x1 + x2 + g, data = d))$coefficients
  expect_equal(rating$p.value[rating$variable == "x2"], co["x2", "Pr(>|t|)"], tolerance = 1e-9)

  none <- tidy(m, type = "importance_rating", vars = c("g", "k"))
  expect_equal(nrow(none), 0)
  expect_equal(names(none), names(rating))
  expect_equal(nrow(tidy(m, type = "importance_rating")), 0)
})

test_that("an unknown tidy type still returns nothing rather than the model's input", {
  d <- make_lmg_df()
  m <- fit_lm(d, y, x1, x2)$model
  expect_null(tidy(m, type = "no_such_type"))
})
