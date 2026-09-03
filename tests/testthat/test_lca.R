testthat::skip_if_not_installed("poLCA")

test_that("exp_lca selects a BIC-minimum categorical model and exposes its report outputs", {
  set.seed(24817)
  n <- 180
  segment <- rep(c("A", "B", "C"), each = n / 3)
  df <- data.frame(
    `利用目的` = ifelse(segment == "A", "仕事", ifelse(segment == "B", "買い物", "趣味")),
    `利用時間帯` = ifelse(segment == "A", "朝", ifelse(segment == "B", "夜", "昼")),
    `満足度` = ifelse(segment == "A", "高", ifelse(segment == "B", "中", "低")),
    `居住地域` = ifelse(segment == "A", "東", ifelse(segment == "B", "西", "南")),
    check.names = FALSE
  )
  # Keep three planted segments while making four-class comparison feasible.
  # A perfectly deterministic three-pattern data set cannot identify a fourth
  # model, so it would (correctly) be excluded before poLCA is called.
  df$`利用時間帯`[sample(seq_len(n), 24)] <- sample(c("朝", "昼", "夜"), 24, replace = TRUE)
  df$`満足度`[sample(seq_len(n), 24)] <- sample(c("高", "中", "低"), 24, replace = TRUE)
  df$`利用目的`[seq(5, n, by = 19)] <- NA

  model_df <- exp_lca(
    df, `利用目的`, `利用時間帯`, `満足度`,
    min_nclass = 2, max_nclass = 4, nrep = 3, maxiter = 200, seed = 1,
    relationship_column = `居住地域`, feature_top_n = 2
  )
  model <- model_df$model[[1]]

  expect_s3_class(model, "lca_exploratory")
  selection <- tidy(model, type = "class_selection")
  # tam#38352: a 1-class baseline is always fit in addition to the requested
  # 2:4 explore range.
  expect_equal(selection$number_of_classes, 1:4)
  expect_equal(nrow(selection), 4)
  converged_rows <- selection[selection$converged, , drop = FALSE]
  expect_equal(glance(model)$selected_classes,
               converged_rows$number_of_classes[which.min(converged_rows$bic)])

  profiles <- tidy(model, type = "profiles")
  expect_true(all(c("variable", "category", "class", "probability", "overall_probability", "difference") %in% names(profiles)))
  expect_true(all(profiles$probability >= 0 & profiles$probability <= 1))
  # Class-conditional response probabilities are P(category | class, variable),
  # so within one variable and one class they must sum to 1 across categories.
  # A variable/category/class melt bug (tam#38349) can leave these off 100%.
  profile_sums <- as.numeric(tapply(profiles$probability, interaction(profiles$variable, profiles$class, drop = TRUE), sum))
  expect_equal(profile_sums, rep(1, length(profile_sums)), tolerance = 1e-8)
  expect_equal(nrow(tidy(model, type = "characteristics")), glance(model)$selected_classes * 2)
  expect_true(all(tidy(model, type = "discrimination")$max_minus_min_probability >= 0))

  assignments <- tidy(model, type = "data")
  expect_equal(nrow(assignments), nrow(df))
  expect_true(any(assignments$`Is Excluded`))
  expect_true(all(is.na(assignments$`Latent Class`[assignments$`Is Excluded`])))
  expect_true(all(assignments$`Assignment Confidence`[!assignments$`Is Excluded`] >= 0))
  probability_columns <- paste("Class", seq_len(glance(model)$selected_classes), "Probability")
  expect_true(all(probability_columns %in% names(assignments)))
  included <- !assignments$`Is Excluded`
  posterior <- as.matrix(assignments[included, probability_columns, drop = FALSE])
  expect_equal(unname(rowSums(posterior)), rep(1, sum(included)), tolerance = 1e-8)
  expect_equal(as.character(assignments$`Latent Class`[included]),
               paste("Class", max.col(posterior, ties.method = "first")))
  expect_equal(unname(assignments$`Assignment Confidence`[included]), unname(apply(posterior, 1, max)), tolerance = 1e-8)
  expect_true(all(is.na(as.matrix(assignments[!included, probability_columns, drop = FALSE]))))
  relationship <- tidy(model, type = "relationship")
  expect_true(nrow(relationship) > 0)
  expect_equal(as.integer(tapply(relationship$rows, relationship$class, sum)),
               tabulate(model$selected_fit$predclass, nbins = glance(model)$selected_classes))
})

test_that("exp_lca respects the requested lower class bound for tiny data", {
  df <- data.frame(a = c("x", "y"), b = c("m", "n"))
  model <- exp_lca(df, a, b, min_nclass = 2, max_nclass = 6,
                   nrep = 1, maxiter = 20)$model[[1]]

  # tam#38352: 1-class baseline is always added, so a requested range that
  # can only fit nclass=2 still reports 2 rows (1 and 2), not 1.
  expect_equal(tidy(model, type = "class_selection")$number_of_classes, c(1L, 2L))
  conditions <- tidy(model, type = "analysis_conditions")
  expect_equal(conditions$Value[conditions$Metric == "Class Counts Compared"],
               "1 (baseline), 2 to 2")
  assignments <- tidy(model, type = "data")
  probability_columns <- c("Class 1 Probability", "Class 2 Probability")
  expect_true(all(probability_columns %in% names(assignments)))
  expect_equal(unname(rowSums(as.matrix(assignments[, probability_columns]))),
               rep(1, nrow(assignments)), tolerance = 1e-8)
})

test_that("exp_lca does not report convergence when maxiter is reached", {
  set.seed(24818)
  df <- data.frame(
    a = sample(c("x", "y", "z"), 80, replace = TRUE),
    b = sample(c("m", "n", "o"), 80, replace = TRUE),
    c = sample(c("low", "mid", "high"), 80, replace = TRUE)
  )
  model <- exp_lca(df, a, b, c, min_nclass = 2, max_nclass = 2,
                   nrep = 1, maxiter = 1)$model[[1]]

  selection <- tidy(model, type = "class_selection")
  nclass2 <- selection[selection$number_of_classes == 2, , drop = FALSE]
  expect_false(nclass2$converged)
  # tam#38352: a successful-but-unconverged fit's Error column now names why,
  # instead of staying blank as if nothing were wrong.
  expect_equal(nclass2$error, "Reached maximum iterations")
})

test_that("exp_lca treats the exact 1-class baseline as converged at maxiter=1", {
  n <- 180
  segment <- rep(c("A", "B", "C"), each = n / 3)
  df <- data.frame(
    a = ifelse(segment == "A", "p", ifelse(segment == "B", "q", "r")),
    b = ifelse(segment == "A", "u", ifelse(segment == "B", "v", "w")),
    c = ifelse(segment == "A", "x", ifelse(segment == "B", "y", "z"))
  )
  model <- exp_lca(df, a, b, c, min_nclass = 2, max_nclass = 3,
                   nrep = 1, maxiter = 1, seed = 1)$model[[1]]
  selection <- tidy(model, type = "class_selection")
  baseline <- selection[selection$number_of_classes == 1L, , drop = FALSE]

  expect_true(baseline$converged)
  expect_true(is.na(baseline$error))
  expect_false(model$used_unconverged_fallback)
  expect_equal(glance(model)$selected_classes, 1L)
})

test_that("exp_lca excludes non-converged fits from the recommended model and always fits a 1-class baseline (tam#38352)", {
  set.seed(24819)
  df <- data.frame(
    a = sample(c("x", "y", "z"), 90, replace = TRUE),
    b = sample(c("m", "n", "o"), 90, replace = TRUE),
    c = sample(c("low", "mid", "high"), 90, replace = TRUE)
  )
  # A generous iteration/restart budget so every candidate (including the
  # always-added 1-class baseline) converges normally on this run -- the
  # invariant under test is that the recommended model is always drawn from
  # the CONVERGED candidates, which class_selection's own converged column
  # lets this test check without needing to engineer a non-convergent fit.
  model <- exp_lca(df, a, b, c, min_nclass = 2, max_nclass = 3,
                   nrep = 3, maxiter = 1000, seed = 3)$model[[1]]
  selection <- tidy(model, type = "class_selection")

  expect_true(1L %in% selection$number_of_classes)
  expect_true(all(1:3 %in% selection$number_of_classes))

  recommended <- glance(model)$selected_classes
  recommended_row <- selection[selection$number_of_classes == recommended, , drop = FALSE]
  expect_true(nrow(recommended_row) == 1)
  expect_true(recommended_row$converged)
})

test_that("exp_lca rejects non-categorical, insufficient, and invalid class selections", {
  df <- data.frame(a = c("x", "y", "x", "y"), b = c("m", "m", "n", "n"), number = 1:4)
  expect_error(exp_lca(df, a), "at least 2 categorical variables")
  expect_error(exp_lca(df, a, number), "supports character, factor, ordered, and logical")
  expect_error(exp_lca(df, a, b, min_nclass = 4, max_nclass = 2), "Class counts")
  expect_error(exp_lca(df, a, b, relationship_column = a), "must be different")
})

test_that("exp_lca reports normalized entropy per candidate class count", {
  set.seed(11)
  n <- 400
  cls <- sample(1:2, n, replace = TRUE)
  df <- data.frame(
    a = ifelse(cls == 1, sample(c("x", "y"), n, TRUE, c(.9, .1)), sample(c("x", "y"), n, TRUE, c(.15, .85))),
    b = ifelse(cls == 1, sample(c("m", "n"), n, TRUE, c(.85, .15)), sample(c("m", "n"), n, TRUE, c(.2, .8))),
    c = ifelse(cls == 1, sample(c("lo", "hi"), n, TRUE, c(.8, .2)), sample(c("lo", "hi"), n, TRUE, c(.25, .75))),
    stringsAsFactors = FALSE
  )
  model <- exp_lca(df, a, b, c, min_nclass = 2, max_nclass = 3,
                   nrep = 3, maxiter = 500, seed = 1)$model[[1]]
  selection <- tidy(model, type = "class_selection")

  expect_true("entropy" %in% names(selection))
  # tam#38383: entropy is undefined for the always-added 1-class baseline --
  # log(K) is 0 there, and there is nothing to separate. It must be NA rather
  # than a divide-by-zero artifact or a spurious 1.
  expect_true(is.na(selection$entropy[selection$number_of_classes == 1]))

  multi <- selection[selection$number_of_classes >= 2, , drop = FALSE]
  expect_false(any(is.na(multi$entropy)))
  expect_true(all(multi$entropy >= 0 & multi$entropy <= 1))

  # Independent recomputation straight from the candidate posteriors, so this
  # pins the FORMULA, not just the column's presence.
  for (candidate in model$candidates) {
    if (is.null(candidate$fit) || candidate$nclass < 2) next
    p <- as.matrix(candidate$fit$posterior)
    nz <- p[p > 0]
    expected <- 1 - (-sum(nz * log(nz))) / (nrow(p) * log(ncol(p)))
    expect_equal(selection$entropy[selection$number_of_classes == candidate$nclass],
                 expected, tolerance = 1e-10)
  }

  # A well-separated 2-class fixture must not look like noise.
  expect_gt(selection$entropy[selection$number_of_classes == 2], 0.5)
})

test_that("lca_entropy returns 1 for a perfectly separating posterior", {
  # Hard 0/1 assignments carry no uncertainty, so the entropy term is 0 and the
  # normalized value is exactly 1 -- the definitional upper bound. This also
  # exercises the 0*log(0) guard: half the matrix is zeros, which is NaN if the
  # zeros are multiplied instead of dropped.
  perfect <- matrix(c(1, 0, 0, 1, 1, 0, 0, 1), ncol = 2, byrow = TRUE)
  expect_equal(lca_entropy(list(posterior = perfect)), 1)

  # Maximum ambiguity: every row equally likely in either class -> entropy 0.
  ambiguous <- matrix(0.5, nrow = 8, ncol = 2)
  expect_equal(lca_entropy(list(posterior = ambiguous)), 0)

  # Degenerate shapes report NA rather than erroring or dividing by zero.
  expect_true(is.na(lca_entropy(list(posterior = matrix(1, nrow = 4, ncol = 1)))))
  expect_true(is.na(lca_entropy(list(posterior = NULL))))
})
