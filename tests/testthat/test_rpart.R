# how to run this test:
# devtools::test(filter="rpart")
context("test rpart functions")

if (!exists("flight")) {
  # To skip repeated data loading, run the following outside of the context of the test,
  # so that it stays even after the test.
  flight <- exploratory::read_delim_file("https://exploratory-download.s3.us-west-2.amazonaws.com/test/airline_2013_10_tricky_v3.csv", ",", quote = "\"", skip = 0 , col_names = TRUE , na = c("","NA") , locale=readr::locale(encoding = "UTF-8", decimal_mark = "."), trim_ws = FALSE , progress = FALSE) %>% exploratory::clean_data_frame()
  set.seed(1) # Stable fixture across CI machines and test order (slice_sample is RNG-dependent).
  flight <- flight %>% slice_sample(n=5000)
}


test_that("exp_rpart regression", {
  model_df <- flight %>% exp_rpart(`ARR DELAY`,`DEP DELAY`)
  res <- model_df %>% tidy_rowwise(model, type="importance")
  res <- model_df %>% tidy_rowwise(model, type="evaluation", pretty.name=TRUE)
  res <- model_df %>% tidy_rowwise(model, type="scatter")
  expect_true(is.data.frame(res))
})

test_that("exp_rpart binary classification", {
  flight2 <- flight %>% filter(`ORIGIN STATE ABR` %in% c("CA","NY"))
  model_df <- flight2 %>% exp_rpart(`ORIGIN STATE ABR`,`DEP DELAY`, smote=T)
  res <- model_df %>% tidy_rowwise(model, type="importance")
  res <- model_df %>% tidy_rowwise(model, type="evaluation", pretty.name=TRUE)
  res <- model_df %>% tidy_rowwise(model, type="evaluation_by_class", pretty.name=TRUE)
  res <- model_df %>% tidy_rowwise(model, type="conf_mat")
  expect_true(is.data.frame(res))
})

test_that("exp_rpart binary classification with logical", {
  model_df <- flight %>% exp_rpart(`delay ed`,`DEP DELAY`, smote=T)
  res <- model_df %>% tidy_rowwise(model, type="importance")
  res <- model_df %>% tidy_rowwise(model, type="evaluation", pretty.name=TRUE)
  res <- model_df %>% tidy_rowwise(model, type="evaluation_by_class", pretty.name=TRUE)
  res <- model_df %>% tidy_rowwise(model, type="conf_mat")
  expect_true(is.data.frame(res))
})

test_that("exp_rpart multiclass classification", {
  flight2 <- flight %>% filter(`ORIGIN STATE ABR` %in% c("CA","NY","TX"))
  model_df <- flight2 %>% exp_rpart(`ORIGIN STATE ABR`,`DEP DELAY`)
  res <- model_df %>% tidy_rowwise(model, type="importance")
  res <- model_df %>% tidy_rowwise(model, type="evaluation", pretty.name=TRUE)
  res <- model_df %>% tidy_rowwise(model, type="evaluation_by_class", pretty.name=TRUE)
  res <- model_df %>% tidy_rowwise(model, type="conf_mat")
  expect_true(is.data.frame(res))
})

test_that("exp_rpart multiclass prediction includes per-class probabilities", {
  set.seed(1)
  model_df <- iris %>%
    exp_rpart(Species, Sepal.Length, Sepal.Width, test_rate = 0.2)

  probability_columns <- paste0("predicted_probability_", levels(iris$Species))
  training <- prediction(model_df, data = "training")
  test <- prediction(model_df, data = "test")
  combined <- prediction(model_df, data = "training_and_test")

  for (result in list(training, test, combined)) {
    expect_true(all(probability_columns %in% colnames(result)))
    probabilities <- as.matrix(result[, probability_columns, drop = FALSE])
    expect_equal(result$predicted_probability, apply(probabilities, 1, max), tolerance = 1e-12)
    predicted_class <- as.character(result$predicted_label)
    max_probability <- apply(probabilities, 1, max)
    expect_true(all(mapply(function(label, row, maximum) {
      label %in% levels(iris$Species)[which(row == maximum)]
    }, predicted_class, split(probabilities, row(probabilities)), max_probability)))
  }

  expect_equal(nrow(combined), nrow(training) + nrow(test))

  pretty <- prediction(model_df, data = "training_and_test", pretty.name = TRUE)
  expect_true(all(paste0("Predicted Probability for ", levels(iris$Species)) %in% colnames(pretty)))

  unknown_category_data <- tibble::tibble(
    target = factor(c("A", "B", "C", "A", "B", "C", "A", "B", "C", "A"),
                    levels = c("A", "B", "C")),
    category = factor(c(rep("known", 8), rep("unseen", 2)),
                      levels = c("known", "unseen")),
    value = seq_len(10)
  )
  unknown_category_model <- unknown_category_data %>%
    exp_rpart(target, category, value, test_rate = 0.2, test_split_type = "ordered")
  unknown_category_test <- prediction(unknown_category_model, data = "test")
  expect_equal(nrow(unknown_category_test), 3)
  expect_equal(sum(is.na(unknown_category_test$predicted_probability)), 2)
  unknown_probability_columns <- paste0("predicted_probability_", levels(unknown_category_data$target))
  expect_true(all(unknown_probability_columns %in% colnames(unknown_category_test)))
})

test_that("exp_rpart regression", {
  model_df <- flight %>% exp_rpart(`DEP DELAY`, `delay ed`, `ARR DELAY`, test_rate = 0.3)
  train_ret <- prediction(model_df)
  expect_equal(colnames(train_ret), c("DEP DELAY", "ARR DELAY", "delay ed", "predicted_value"))
  test_ret <- prediction(model_df, data = "test")
  expect_equal(colnames(train_ret), c("DEP DELAY", "ARR DELAY", "delay ed", "predicted_value"))
})

test_that("exp_rpart throws error with classification with only one unique value", {
  expect_error({
    flight2 <- flight %>% filter(`ORIGIN STATE ABR` %in% c("CA"))
    model_df <- flight2 %>% exp_rpart(`ORIGIN STATE ABR`,`DEP DELAY`)
  }, "Categorical Target Variable must have 2 or more unique values.")
})

test_that("exp_rpart prediction", {
  model_df <- flight %>% exp_rpart(`ORIGIN STATE ABR`,`DEP DELAY`, test_rate = 0.3)
  ret <- model_df %>% prediction(.)
  test_ret <- model_df %>% prediction(., data = "test")
  ret_all <- prediction_training_and_test(model_df)
  expect_true(is.data.frame(ret_all))
})

test_that("exp_rpart() error handling for predictor with single unique value", {
  expect_error({
    model_df <- flight %>% mutate(Const=1) %>%
      exp_rpart(`ORIGIN STATE ABR`,Const, test_rate = 0.3)
  }, "Invalid Predictors: Only one unique value.")
})
