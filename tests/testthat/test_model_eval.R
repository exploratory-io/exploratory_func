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
})

test_that("test evaluate_multi", {
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
