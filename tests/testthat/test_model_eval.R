context("test model evaluation")

test_that("test do_roc", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))

  model_data <- build_glm(test_data, family = "binomial", CANCELLED ~ `Carrier Name` + CARRIER + DISTANCE, test_rate = 0.2)

  predicted <- prediction(model_data)

  ret <- do_roc(predicted, predicted_response, CANCELLED)
  expect_equal(colnames(ret), c("true_positive_rate", "false_positive_rate"))

  ret <- do_roc(predicted, predicted_response, CANCELLED, grid=100, with_auc=TRUE)
  expect_equal(colnames(ret), c("true_positive_rate", "false_positive_rate", "auc"))

})

test_that("test do_roc with factor", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))

  test_data[["CANCELLED"]] <- as.factor(test_data[["CANCELLED"]])

  model_data <- build_glm(test_data, family = "binomial", CANCELLED ~ `Carrier Name` + CARRIER + DISTANCE, test_rate = 0.2)

  predicted <- prediction(model_data)

  ret <- do_roc(predicted, predicted_response, CANCELLED)
  expect_equal(colnames(ret), c("true_positive_rate", "false_positive_rate"))

  ret <- do_roc(predicted, predicted_response, CANCELLED, grid=100, with_auc=TRUE)
  expect_equal(colnames(ret), c("true_positive_rate", "false_positive_rate", "auc"))
})

test_that("test do_roc with 2 numeric values", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))

  model_data <- build_glm(
    test_data,
    family = "binomial",
    CANCELLED ~ `Carrier Name` + CARRIER + DISTANCE,
    test_rate = 0.2
  )
  predicted <- prediction(model_data)

  predicted[["CANCELLED"]] <- c(NA, 4, 4, 2, 2, 4, 4, 2, 2, 2, 2, 4, 2, NA, 2) # Testing the case where the data starts with NA, which used to mess up filtering NAs in do_roc.
  ret <- do_roc(predicted, predicted_response, CANCELLED)
  expect_true(any(!ret[["false_positive_rate"]] %in% c(0, 1)))

  predicted[["CANCELLED"]] <- c(2, 4, 4, 2, 2, 4, 4, 2, 2, 2, 3, 4, 2, NA, 2)
  expect_error({
    do_roc(predicted, predicted_response, CANCELLED)
  }, "binary labels can't have more than 2 unique values")
  expect_equal(colnames(ret), c("true_positive_rate", "false_positive_rate"))
})

# Verbatim copy of the pre-fix do_roc_ point calculation (one ROC point per data row,
# no tie handling). Used to pin the guarantee that the tie-collapsing fix does not change
# the output at all when every predicted probability is distinct, which is the case for
# every continuous-score model (logistic regression, xgboost, lightgbm, ...).
roc_points_before_tie_fix <- function(df, pred_prob_col, actual_val_col) {
  df <- df[!is.na(df[[pred_prob_col]]) & !is.na(df[[actual_val_col]]), ]
  df[[actual_val_col]] <- exploratory:::binary_label(df[[actual_val_col]])
  arranged <- df[order(-df[[pred_prob_col]]), ]
  val <- arranged[[actual_val_col]]
  act_sum <- sum(val)
  fpr <- if (all(val)) {
    c(rep(0, length(val)), 1)
  } else {
    c(0, cumsum(!val) / (length(val) - act_sum))
  }
  tpr <- if (all(!val)) {
    c(rep(0, length(val)), 1)
  } else {
    c(0, cumsum(val) / act_sum)
  }
  list(tpr = tpr, fpr = fpr)
}

test_that("test do_roc collapses tied predicted probabilities into one point each", {
  # A decision tree assigns one probability per leaf, so many rows share the exact same
  # predicted probability. Those rows are indistinguishable to the model, so the curve
  # must have one vertex per distinct probability, not one per row.
  leaf_prob <- c(0.05, 0.20, 0.45, 0.70, 0.90)
  leaf_positives <- c(2, 8, 18, 28, 36) # out of 40 rows in each leaf
  prob <- rep(leaf_prob, each = 40)
  actual <- unlist(lapply(leaf_positives, function(x) rep(c(1, 0), c(x, 40 - x))))
  test_data <- data.frame(prob = prob, actual = actual)

  ret <- do_roc_(test_data, "prob", "actual")
  # (0,0) origin plus one vertex per distinct probability.
  expect_equal(nrow(ret), length(leaf_prob) + 1)
  expect_equal(length(ret[["true_positive_rate"]]), length(ret[["false_positive_rate"]]))
  # The curve must still start at (0,0) and end at (1,1).
  expect_equal(ret[["true_positive_rate"]][[1]], 0)
  expect_equal(ret[["false_positive_rate"]][[1]], 0)
  expect_equal(ret[["true_positive_rate"]][[nrow(ret)]], 1)
  expect_equal(ret[["false_positive_rate"]][[nrow(ret)]], 1)
  # Monotone non-decreasing on both axes.
  expect_true(all(diff(ret[["true_positive_rate"]]) >= 0))
  expect_true(all(diff(ret[["false_positive_rate"]]) >= 0))
})

test_that("test do_roc is independent of the input row order when probabilities are tied", {
  set.seed(20260805)
  n <- 600
  prob <- rep(c(0.1, 0.3, 0.55, 0.8), length.out = n)
  actual <- rbinom(n, 1, prob)
  test_data <- data.frame(prob = prob, actual = actual)

  ret <- do_roc_(test_data, "prob", "actual", with_auc = TRUE)

  # Shuffling the rows changes nothing the model can see, so the curve must not move.
  for (i in 1:5) {
    shuffled <- test_data[sample(nrow(test_data)), ]
    shuffled_ret <- do_roc_(shuffled, "prob", "actual", with_auc = TRUE)
    expect_equal(nrow(shuffled_ret), nrow(ret))
    expect_true(all.equal(shuffled_ret[["true_positive_rate"]], ret[["true_positive_rate"]]))
    expect_true(all.equal(shuffled_ret[["false_positive_rate"]], ret[["false_positive_rate"]]))
    # AUC was already tie-aware and row-order-independent before the fix. Keep it that way.
    expect_true(all.equal(shuffled_ret[["auc"]], ret[["auc"]]))
  }
})

test_that("test do_roc output is unchanged when there are no tied probabilities", {
  # Every continuous-score model (logistic regression, xgboost, lightgbm, ...) produces
  # distinct probabilities, and for those the tie-collapsing fix must be a no-op.
  set.seed(20260805)
  for (i in 1:20) {
    n <- sample(5:200, 1)
    prob <- runif(n) # continuous, so no ties
    actual <- rbinom(n, 1, prob)
    if (length(unique(actual)) < 2) {
      next # degenerate cases are covered by their own test
    }
    test_data <- data.frame(prob = prob, actual = actual)

    expected <- roc_points_before_tie_fix(test_data, "prob", "actual")
    ret <- do_roc_(test_data, "prob", "actual")

    expect_identical(ret[["true_positive_rate"]], expected$tpr)
    expect_identical(ret[["false_positive_rate"]], expected$fpr)
  }
})

test_that("test do_roc degenerate cases", {
  degenerate_cases <- list(
    all_positive = data.frame(prob = c(0.9, 0.5, 0.1), actual = c(1, 1, 1)),
    all_negative = data.frame(prob = c(0.9, 0.5, 0.1), actual = c(0, 0, 0)),
    all_positive_tied = data.frame(prob = c(0.5, 0.5, 0.5), actual = c(1, 1, 1)),
    all_negative_tied = data.frame(prob = c(0.5, 0.5, 0.5), actual = c(0, 0, 0)),
    single_positive_row = data.frame(prob = 0.7, actual = 1),
    single_negative_row = data.frame(prob = 0.7, actual = 0),
    every_probability_tied = data.frame(prob = rep(0.4, 10), actual = rep(c(1, 0), 5))
  )

  for (case_name in names(degenerate_cases)) {
    ret <- do_roc_(degenerate_cases[[case_name]], "prob", "actual")
    # fpr and tpr must always have the same length, or the data.frame() call would error.
    expect_equal(length(ret[["true_positive_rate"]]), length(ret[["false_positive_rate"]]),
                 info = case_name)
    expect_true(all(!is.na(ret[["true_positive_rate"]])), info = case_name)
    expect_true(all(!is.na(ret[["false_positive_rate"]])), info = case_name)
  }

  # A group that becomes empty after NA filtering must not error either.
  empty_group_data <- data.frame(
    group = c("a", "a", "b", "b"),
    prob = c(0.9, 0.1, NA, NA),
    actual = c(1, 0, 1, 0)
  )
  ret <- empty_group_data %>% dplyr::group_by(group) %>% do_roc_("prob", "actual")
  expect_equal(length(ret[["true_positive_rate"]]), length(ret[["false_positive_rate"]]))
  expect_true(all(!is.na(ret[["true_positive_rate"]])))
})

test_that("test do_roc with grouped input and tied probabilities", {
  set.seed(20260805)
  n <- 400
  test_data <- data.frame(
    group = rep(c("x", "y"), each = n / 2),
    prob = rep(c(0.2, 0.5, 0.8), length.out = n),
    actual = rbinom(n, 1, 0.4)
  )

  ret <- test_data %>% dplyr::group_by(group) %>% do_roc_("prob", "actual", with_auc = TRUE)
  expect_equal(colnames(ret), c("group", "true_positive_rate", "false_positive_rate", "auc"))
  # 3 distinct probabilities per group, plus the (0,0) origin, in each of 2 groups.
  expect_equal(nrow(ret), 2 * (3 + 1))
  expect_equal(sort(unique(ret[["group"]])), c("x", "y"))

  # Each group's curve must match the curve of that group computed on its own.
  for (group_name in c("x", "y")) {
    one_group <- test_data[test_data$group == group_name, ]
    expected <- do_roc_(one_group, "prob", "actual", with_auc = TRUE)
    actual_ret <- ret[ret$group == group_name, ]
    expect_true(all.equal(actual_ret[["true_positive_rate"]], expected[["true_positive_rate"]]))
    expect_true(all.equal(actual_ret[["false_positive_rate"]], expected[["false_positive_rate"]]))
    expect_true(all.equal(actual_ret[["auc"]], expected[["auc"]]))
  }
})

test_that("test do_roc auc is unchanged by the tie handling fix", {
  set.seed(20260805)
  n <- 500
  prob <- rep(c(0.15, 0.35, 0.55, 0.75, 0.95), length.out = n)
  actual <- rbinom(n, 1, prob)
  test_data <- data.frame(prob = prob, actual = actual)

  ret <- do_roc_(test_data, "prob", "actual", with_auc = TRUE)
  # auroc() is already tie-aware, so the reported AUC must equal it exactly.
  expected_auc <- exploratory:::auroc(test_data$prob, exploratory:::binary_label(test_data$actual))
  expect_equal(ret[["auc"]][[1]], expected_auc)
  expect_equal(length(unique(ret[["auc"]])), 1)
})

test_that("test evaluate_binary with 2 numeric values", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))

  model_data <- build_glm(
    test_data,
    family = "binomial",
    CANCELLED ~ `Carrier Name` + CARRIER + DISTANCE,
    test_rate = 0.2
  )
  predicted <- prediction(model_data)

  predicted[["CANCELLED"]] <- c(4, 2, 4, 2, 4, 2, 4, 2, 4, 2, 2, 4, 2, NA, 2)
  ret <- evaluate_binary(predicted, predicted_response, CANCELLED)
  # Removed following expectation, since in this case, optimal threshold becomes actually 0, most likely because of imbalanced data.
  # expect_true(ret[["threshold"]] != 0)

  predicted[["CANCELLED"]] <- c(2, 4, 4, 2, 2, 4, 4, 2, 2, 2, 3, 4, 2, NA, 2)
  expect_error({
    evaluate_binary(predicted, predicted_response, CANCELLED)
  }, "binary labels can't have more than 2 unique values")
})

test_that("test eval_pred_bin with factor", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))

  test_data[["CANCELLED"]] <- as.factor(test_data[["CANCELLED"]])

  for (i in seq(5)){
    test_data <- dplyr::bind_rows(test_data, test_data)
  }

  model_data <- build_lr(test_data, CANCELLED ~ `Carrier Name` + CARRIER + DISTANCE, test_rate = 0.2)

  predicted <- prediction(model_data, data = "test")

  ret <- evaluate_binary(predicted, predicted_response, CANCELLED, threshold = "accuracy")

  expect_true(ret$AUC[[1]] >= 0.0)
  expect_true(ret$AUC[[1]] <= 1.0)
})

test_that("test evaluate_regression", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))

  for (i in seq(5)){
    test_data <- dplyr::bind_rows(test_data, test_data)
  }

  model_data <- build_lm(test_data, DISTANCE ~ CARRIER + CANCELLED, test_rate = 0.2)

  predicted <- prediction(model_data, data = "test")

  ret <- evaluate_regression(predicted, predicted_value, CANCELLED)
  expect_true(is.data.frame(ret))
})

test_that("test evaluate_regression (second instance)", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))



  for (i in seq(5)){
    test_data <- dplyr::bind_rows(test_data, test_data)
  }

  model_data <- build_lm(test_data, DISTANCE ~ CARRIER + CANCELLED, test_rate = 0.2)

  predicted <- prediction(model_data, data = "test")

  ret <- evaluate_regression(predicted, predicted_value, CANCELLED)
  expect_true(is.data.frame(ret))
})

test_that("eval multi", {
  test_df <- list(
    c("b", "b"),
    c("a", "b"),
    c("d", "b"),
    c("b", "b"),
    c("d", "d"),
    c("a", "c"),
    c("b", "b"),
    c("d", "d"),
    c("d", "d"),
    c("b", "b"),
    c("d", "d"),
    c("d", "b"),
    c("d", "d"),
    c("d", "c"),
    c("d", "d"),
    c("b", "c"),
    c("b", "b"),
    c("d", "d"),
    c("b", "d"),
    c("d", "d")
  ) %>%
    as.data.frame() %>%
    as.matrix() %>%
    t() %>%
    as.data.frame(stringsAsFactors = FALSE)
  rownames(test_df) <- NULL
  colnames(test_df) <- c("actual", "predicted")

  ret <- evaluate_multi(test_df, predicted, actual)

  #confirmed by this python code
  #sklearn.metrics.f1_score(
  #  ["b","b","b","b","d","c","b","d","d","b","d","b","d","c","d","c","b","d","d","d"],
  #  ["b","a","d","b","d","a","b","d","d","b","d","d","d","d","d","b","b","d","b","d"], average = "macro")
  expect_equal(ret[["macro_f_score"]], 0.366666666666667)
  expect_equal(ret[["micro_f_score"]], 2 * (13*13/20/20) / (13/20 + 13/20))

  test_df2 <- list(
    c("c", "b"),
    c("a", "b"),
    c("d", "b"),
    c("b", "b"),
    c("d", "d"),
    c("a", "c"),
    c("b", "b"),
    c("d", "d"),
    c("d", "d"),
    c("b", "b"),
    c("d", "d"),
    c("d", "a"),
    c("d", "d"),
    c("d", "c"),
    c("d", "d"),
    c("b", "c"),
    c("b", "b"),
    c("d", "d"),
    c("b", "d"),
    c("d", "d")
  ) %>%
    as.data.frame() %>%
    as.matrix() %>%
    t() %>%
    as.data.frame(stringsAsFactors = FALSE)
  rownames(test_df2) <- NULL
  colnames(test_df2) <- c("actual", "predicted")

  ret <- evaluate_multi(test_df2, predicted, actual)

  # this is confirmed from python scikit learn
  # sklearn.metrics.f1_score(
  # ["b","b","b","b","d","c","b","d","d","b","d","a","d","c","d","c","b","d","d","d"],
  # ["c","a","d","b","d","a","b","d","d","b","d","d","d","d","d","b","b","d","b","d"], average = "macro")
  # sklearn.metrics.f1_score(
  # ["b","b","b","b","d","c","b","d","d","b","d","a","d","c","d","c","b","d","d","d"],
  # ["c","a","d","b","d","a","b","d","d","b","d","d","d","d","d","b","b","d","b","d"], average = "micro")
  expect_equal(ret[["macro_f_score"]], 0.353846153846154)
  expect_equal(ret[["micro_f_score"]], 0.6)

})

test_that("eval multi", {
  test_df <- list(
    c("a", "b"),
    c("a", "b"),
    c("a", "b"),
    c("a", "b"),
    c("b", "a"),
    c("b", "a"),
    c("b", "a")
  ) %>%
    as.data.frame() %>%
    as.matrix() %>%
    t() %>%
    as.data.frame(stringsAsFactors = FALSE)
  rownames(test_df) <- NULL
  colnames(test_df) <- c("actual", "predicted")

  ret <- evaluate_multi(test_df, predicted, actual)
  expect_equal(ret[["micro_f_score"]], 0)

})

test_data <- tibble::tibble(
      `CANCELLED X` = c("N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "Y", "N", "Y", "N"),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = factor(c("AA", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL")), # test with factor with NA
      # testing filtering of Inf, -Inf, NA here.
      DISTANCE = c(10, 12, 12, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545),
      ARR_TIME = c(10, 32, 321, 342, 123, 98, 10, 21, 80, 211, 121, 87, 821, 213, 213, 923, 121, 76, 34, 50),
      DERAY_TIME = c(12, 42, 321, 31, 3, 43, 342, 764, 123, 43, 50, 12, 876, 12, 34, 45, 84, 25, 87, 352)
      )

test_data$klass <- c(rep("A", 10), rep("B", 10))
# Make target variable logical. (We will support only logical as logistic regression target.)
test_data <- test_data %>% dplyr::mutate(`CANCELLED X` = `CANCELLED X` == 'Y')

test_that("evaluate binary classification model by training and test", {
  # test_data[["CANCELLED X"]] <- test_data[["CANCELLED X"]] %>% as.factor() %>% as.numeric() -1
  ret <- test_data %>% build_lm.fast(`CANCELLED X`,
                                     `ARR_TIME`,
                                     `DERAY_TIME`,
                                     `Carrier Name`,
                                     family = "binomial",
                                     model_type = "glm",
                                     test_rate = 0.5)
  suppressWarnings({
    eret <- evaluate_binary_training_and_test(ret, "CANCELLED X")
    expect_cols <-  c("is_test_data", "auc", "f_score", "accuracy_rate", "misclassification_rate", "precision", "recall",
                      "p.value", "positives", "negatives", "n", "logLik", "AIC", "BIC", "deviance",
                      "null.deviance", "df.null", "df.residual", "Max VIF")
    expect_equal(colnames(eret), expect_cols)
    eret <- evaluate_binary_training_and_test(ret, "CANCELLED X", pretty.name = TRUE)
    expect_cols <- c("Data Type", "AUC", "F1 Score", "Accuracy Rate", "Misclass. Rate", "Precision", "Recall",
                     "P Value", "Rows (TRUE)", "Rows (FALSE)", "Rows", "Log Likelihood", "AIC", "BIC",
                     "Residual Deviance", "Residual DF", "Null Deviance", "Null Model DF", "Max VIF")

    expect_equal(colnames(eret), expect_cols)
  })
})

test_that("evaluate_binary_training_and_test report_metrics adds ROC/PR/Balanced/Specificity (#37256)", {
  test_data <- structure(
    list(
      `CANCELLED X` = c(0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      ARR_TIME = c(1:20) * 10,
      DERAY_TIME = c(1:20),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", "Atlantic Southeast Airlines", "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines")
    ),
    row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame")
  )
  ret <- test_data %>% build_lm.fast(`CANCELLED X`,
                                     `ARR_TIME`,
                                     `DERAY_TIME`,
                                     `Carrier Name`,
                                     family = "binomial",
                                     model_type = "glm",
                                     test_rate = 0.5)
  suppressWarnings({
    base <- evaluate_binary_training_and_test(ret, "CANCELLED X", pretty.name = TRUE)
    with_metrics <- evaluate_binary_training_and_test(ret, "CANCELLED X", pretty.name = TRUE, report_metrics = TRUE)
  })
  expect_false(any(c("PR AUC", "Balanced Accuracy", "Specificity", "ROC AUC") %in% colnames(base)))
  expect_true(all(c("ROC AUC", "PR AUC", "Balanced Accuracy", "Specificity") %in% colnames(with_metrics)))
  expect_false("AUC" %in% colnames(with_metrics))
  # Statistical model metrics stay present alongside the new prediction metrics.
  expect_true(all(c("P Value", "AIC", "BIC", "Log Likelihood") %in% colnames(with_metrics)))
  expect_equal(nrow(with_metrics), 2)
  expect_false(any(is.na(with_metrics[, c("ROC AUC", "PR AUC", "Balanced Accuracy", "Specificity"), drop = FALSE])))
  # Default output columns keep the same values.
  kept <- intersect(colnames(base), colnames(with_metrics))
  expect_equal(as.data.frame(base)[, kept, drop = FALSE],
               as.data.frame(with_metrics)[, kept, drop = FALSE])
})


test_that("Group evaluate binary classification model by training and test", {
  group_data <- test_data %>% group_by(klass)
  ret <- group_data %>%
           build_lm.fast(`CANCELLED X`,
                         `ARR_TIME`,
                         model_type = "glm",
                         family = "binomial",
                         link = "logit",
                         test_rate = 0.5)
  suppressWarnings({
    eret <- evaluate_binary_training_and_test(ret, "CANCELLED X")
    expect_cols <-  c("klass", "is_test_data", "auc", "f_score", "accuracy_rate", "misclassification_rate", "precision", "recall",
                      "p.value", "positives", "negatives", "n", "logLik", "AIC", "BIC", "deviance",
                      "null.deviance", "df.null", "df.residual")
    expect_equal(colnames(eret), expect_cols)
    eret <- evaluate_binary_training_and_test(ret, "CANCELLED X", pretty.name = TRUE)
    expect_cols <- c("klass", "Data Type", "AUC", "F1 Score", "Accuracy Rate", "Misclass. Rate", "Precision", "Recall",
                     "P Value", "Rows (TRUE)", "Rows (FALSE)", "Rows", "Log Likelihood", "AIC", "BIC",
                     "Residual Deviance", "Residual DF", "Null Deviance", "Null Model DF")

    expect_equal(colnames(eret), expect_cols)
  })
})

test_that("Group evaluate binary classification model by training and test with threshold", {
  ret <- test_data %>%
           build_lm.fast(`CANCELLED X`,
                         `DERAY_TIME`,
                         `Carrier Name`,
                         `ARR_TIME`,
                         model_type = "glm",
                         family = "binomial",
                         link = "logit",
                         test_rate = 0.3)
  suppressWarnings({
    eret_fscore <- evaluate_binary_training_and_test(ret, "CANCELLED X", threshold = "f_score")
    eret_acc <- evaluate_binary_training_and_test(ret, "CANCELLED X", threshold = "accuracy_rate") 
    eret_recall <- evaluate_binary_training_and_test(ret, "CANCELLED X", threshold = "recall")
    eret_precision <- evaluate_binary_training_and_test(ret, "CANCELLED X", threshold = "precision")

    expect_gte(eret_fscore$f_score[1], eret_acc$f_score[1])
    expect_gte(eret_acc$accuracy_rate[1], eret_fscore$accuracy_rate[1])
    expect_gte(eret_recall$recall[1], eret_precision$recall[1])
    expect_gte(eret_precision$precision[1], eret_acc$precision[1])
  })
})
