context("test decision tree multiclass probability report")

test_that("dtree_report_multiclass_probabilities returns train and test one-vs-rest rows", {
  set.seed(1)
  n <- 90
  source_data <- data.frame(
    target = factor(rep(c("A", "B", "C"), each = n / 3), levels = c("A", "B", "C")),
    predictor = c(rnorm(n / 3, -2), rnorm(n / 3, 0), rnorm(n / 3, 2))
  )
  model <- rpart::rpart(target ~ predictor, data = source_data, minsplit = 2, cp = 0)
  model$classification_type <- "multi"
  model$orig_target_col <- "target"

  test_index <- seq(3, n, by = 4)
  model_df <- data.frame(
    model = I(list(model)),
    .test_index = I(list(test_index)),
    source.data = I(list(source_data))
  )

  ret <- exploratory:::dtree_report_multiclass_probabilities(model_df)

  expect_equal(nrow(ret), n * 3)
  expect_equal(sort(unique(ret$Category)), c("A", "B", "C"))
  expect_true(all(ret$`Predicted Probability` >= 0 & ret$`Predicted Probability` <= 1))
  expect_true(all(c("Actual Positive", "Actual Group", "Actual Category", "is_test_data", "baseline_precision") %in% colnames(ret)))
  expect_equal(sum(ret$is_test_data), length(test_index) * 3)
  expect_equal(sum(!ret$is_test_data), (n - length(test_index)) * 3)
  expect_equal(levels(ret$`Actual Group`), c("This Category", "Other Categories"))

  category_a <- ret[ret$Category == "A" & !ret$is_test_data, , drop = FALSE]
  expect_equal(category_a$baseline_precision, rep(mean(category_a$`Actual Positive`), nrow(category_a)))
})
