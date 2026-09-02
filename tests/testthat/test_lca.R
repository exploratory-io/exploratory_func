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
  expect_equal(selection$number_of_classes, 2:4)
  expect_equal(nrow(selection), 3)
  expect_equal(glance(model)$selected_classes, selection$number_of_classes[which.min(selection$bic)])

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

  expect_equal(tidy(model, type = "class_selection")$number_of_classes, 2L)
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

  expect_false(tidy(model, type = "class_selection")$converged)
})

test_that("exp_lca rejects non-categorical, insufficient, and invalid class selections", {
  df <- data.frame(a = c("x", "y", "x", "y"), b = c("m", "m", "n", "n"), number = 1:4)
  expect_error(exp_lca(df, a), "at least 2 categorical variables")
  expect_error(exp_lca(df, a, number), "supports character, factor, ordered, and logical")
  expect_error(exp_lca(df, a, b, min_nclass = 4, max_nclass = 2), "Class counts")
  expect_error(exp_lca(df, a, b, relationship_column = a), "must be different")
})
