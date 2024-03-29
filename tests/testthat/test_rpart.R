# how to run this test:
# devtools::test(filter="rpart")
context("test rpart functions")

if (!exists("flight")) {
  # To skip repeated data loading, run the following outside of the context of the test,
  # so that it stays even after the test.
  flight <- exploratory::read_delim_file("https://exploratory-download.s3.us-west-2.amazonaws.com/test/airline_2013_10_tricky_v3.csv", ",", quote = "\"", skip = 0 , col_names = TRUE , na = c("","NA") , locale=readr::locale(encoding = "UTF-8", decimal_mark = "."), trim_ws = FALSE , progress = FALSE) %>% exploratory::clean_data_frame()
  flight <- flight %>% slice_sample(n=5000)
}


test_that("exp_rpart regression", {
  model_df <- flight %>% exp_rpart(`ARR DELAY`,`DEP DELAY`)
  res <- model_df %>% tidy_rowwise(model, type="importance")
  res <- model_df %>% tidy_rowwise(model, type="evaluation", pretty.name=TRUE)
  res <- model_df %>% tidy_rowwise(model, type="scatter")
})

test_that("exp_rpart binary classification", {
  flight2 <- flight %>% filter(`ORIGIN STATE ABR` %in% c("CA","NY"))
  model_df <- flight2 %>% exp_rpart(`ORIGIN STATE ABR`,`DEP DELAY`, smote=T)
  res <- model_df %>% tidy_rowwise(model, type="importance")
  res <- model_df %>% tidy_rowwise(model, type="evaluation", pretty.name=TRUE)
  res <- model_df %>% tidy_rowwise(model, type="evaluation_by_class", pretty.name=TRUE)
  res <- model_df %>% tidy_rowwise(model, type="conf_mat")
})

test_that("exp_rpart binary classification with logical", {
  model_df <- flight %>% exp_rpart(`delay ed`,`DEP DELAY`, smote=T)
  res <- model_df %>% tidy_rowwise(model, type="importance")
  res <- model_df %>% tidy_rowwise(model, type="evaluation", pretty.name=TRUE)
  res <- model_df %>% tidy_rowwise(model, type="evaluation_by_class", pretty.name=TRUE)
  res <- model_df %>% tidy_rowwise(model, type="conf_mat")
})

test_that("exp_rpart multiclass classification", {
  flight2 <- flight %>% filter(`ORIGIN STATE ABR` %in% c("CA","NY","TX"))
  model_df <- flight2 %>% exp_rpart(`ORIGIN STATE ABR`,`DEP DELAY`)
  res <- model_df %>% tidy_rowwise(model, type="importance")
  res <- model_df %>% tidy_rowwise(model, type="evaluation", pretty.name=TRUE)
  res <- model_df %>% tidy_rowwise(model, type="evaluation_by_class", pretty.name=TRUE)
  res <- model_df %>% tidy_rowwise(model, type="conf_mat")
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
})

test_that("exp_rpart() error handling for predictor with single unique value", {
  expect_error({
    model_df <- flight %>% mutate(Const=1) %>%
      exp_rpart(`ORIGIN STATE ABR`,Const, test_rate = 0.3)
  }, "Invalid Predictors: Only one unique value.")
})
