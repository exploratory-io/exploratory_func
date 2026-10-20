# tam#39338: predicted labels of multiclass Random Forest must be mapped by the
# probability matrix's column names, not by position in forest$levels.
context("Test Random Forest multiclass label mapping when a class is missing from training")

test_that("calc_feature_imp multiclass labels are correct when a non-last class exists only in test data", {
  set.seed(1)
  n <- 300
  x <- runif(n)
  y <- ifelse(x < 0.5, "b", "c")
  y[271:300] <- "a" # Class "a" appears only in the last 10%, which goes to the test data with ordered split.
  df <- data.frame(y = y, x = x, z = rnorm(n))
  model_df <- suppressWarnings(calc_feature_imp(df, y, x, z, test_rate = 0.1, test_split_type = "ordered"))
  fit <- model_df$model[[1]]

  # Premise: forest$levels has the unseen class, but the probability matrix does not.
  expect_equal(fit$forest$levels, c("a", "b", "c"))
  expect_equal(colnames(fit$prediction_training$predictions), c("b", "c"))

  aug <- augment(fit, data = model_df$source.data[[1]][-model_df$.test_index[[1]], ], data_type = "training")
  n_train <- nrow(aug)

  # All training rows must be on the diagonal of the confusion matrix.
  conf_mat <- tidy(fit, type = "conf_mat")
  expect_equal(sum(conf_mat$count), n_train)
  expect_equal(sum(conf_mat$count[conf_mat$actual_value == conf_mat$predicted_value]), n_train)

  # predicted_label must be the argmax of predicted_probability_* columns.
  prob_cols <- c("predicted_probability_b", "predicted_probability_c")
  expect_true(all(prob_cols %in% colnames(aug)))
  expected_label <- c("b", "c")[max.col(as.matrix(aug[, prob_cols]), ties.method = "first")]
  expect_equal(as.character(aug$predicted_label), expected_label)
  expect_equal(as.character(aug$predicted_label), as.character(aug$y))
})

test_that("calc_feature_imp multiclass labels are unchanged when all classes are in training data", {
  set.seed(1)
  n <- 300
  x <- runif(n)
  y <- ifelse(x < 0.33, "a", ifelse(x < 0.66, "b", "c"))
  df <- data.frame(y = y, x = x, z = rnorm(n))
  model_df <- suppressWarnings(calc_feature_imp(df, y, x, z, test_rate = 0.1, seed = 1))
  fit <- model_df$model[[1]]
  expect_equal(colnames(fit$prediction_training$predictions), fit$forest$levels)

  pred <- fit$prediction_training$predictions
  # Same result as the previous positional mapping: levels[which.max()].
  old_labels <- fit$forest$levels[apply(pred, 1, which.max)]
  new_labels <- predict_value_from_prob(fit$forest$levels, pred, fit$y)
  expect_equal(as.character(new_labels), old_labels)

  aug <- augment(fit, data = model_df$source.data[[1]][-model_df$.test_index[[1]], ], data_type = "training")
  expect_equal(as.character(aug$predicted_label), old_labels)
})

test_that("predict_value_from_prob falls back to levels_var when the matrix has no column names", {
  pred <- matrix(c(0.1, 0.7, 0.2,
                   0.6, 0.3, 0.1), nrow = 2, byrow = TRUE)
  expect_equal(predict_value_from_prob(c("x", "y", "z"), pred, c("x", "y")), c("y", "x"))
})

test_that("predict_value_from_prob maps by column names, keeping the type of y_value", {
  pred <- matrix(c(0.1, 0.9,
                   0.8, 0.2), nrow = 2, byrow = TRUE, dimnames = list(NULL, c("b", "c")))
  # levels_var has an extra leading class, which must not shift the labels.
  expect_equal(predict_value_from_prob(c("a", "b", "c"), pred, c("b", "c")), c("c", "b"))
  expect_equal(predict_value_from_prob(c("a", "b", "c"), pred, factor(c("b", "c"))),
               factor(c("c", "b"), levels = c("b", "c")))
})
