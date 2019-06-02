# how to run this test:
# devtools::test(filter="rpart")
context("test rpart functions")

if (!exists("flight")) {
  # To skip repeated data loading, run the following outside of the context of the test,
  # so that it stays even after the test.
  flight <- exploratory::read_delim_file("https://www.dropbox.com/s/f47baw5f3v0xoll/airline_2013_10_tricky_v3.csv?dl=1", ",", quote = "\"", skip = 0 , col_names = TRUE , na = c("","NA") , locale=readr::locale(encoding = "UTF-8", decimal_mark = "."), trim_ws = FALSE , progress = FALSE) %>% exploratory::clean_data_frame()
  flight <- flight %>% sample_n(5000)
}


test_that("exp_rpart regression", {
  model_df <- flight %>% exp_rpart(`ARR DELAY`,`DEP DELAY`)
  res <- model_df %>% tidy(model, type="importance")
  res <- model_df %>% tidy(model, type="evaluation", pretty.name=TRUE)
  res <- model_df %>% tidy(model, type="scatter")
})

test_that("exp_rpart binary classification", {
  flight2 <- flight %>% filter(`ORIGIN STATE ABR` %in% c("CA","NY"))
  model_df <- flight2 %>% exp_rpart(`ORIGIN STATE ABR`,`DEP DELAY`, smote=T)
  res <- model_df %>% tidy(model, type="importance")
  res <- model_df %>% tidy(model, type="evaluation", pretty.name=TRUE)
  res <- model_df %>% tidy(model, type="evaluation_by_class", pretty.name=TRUE)
  res <- model_df %>% tidy(model, type="conf_mat")
})

test_that("exp_rpart binary classification with logical", {
  model_df <- flight %>% exp_rpart(`delay ed`,`DEP DELAY`, smote=T)
  res <- model_df %>% tidy(model, type="importance")
  res <- model_df %>% tidy(model, type="evaluation", pretty.name=TRUE)
  res <- model_df %>% tidy(model, type="evaluation_by_class", pretty.name=TRUE)
  res <- model_df %>% tidy(model, type="conf_mat")
})

test_that("exp_rpart multiclass classification", {
  flight2 <- flight %>% filter(`ORIGIN STATE ABR` %in% c("CA","NY","TX"))
  model_df <- flight2 %>% exp_rpart(`ORIGIN STATE ABR`,`DEP DELAY`)
  res <- model_df %>% tidy(model, type="importance")
  res <- model_df %>% tidy(model, type="evaluation", pretty.name=TRUE)
  res <- model_df %>% tidy(model, type="evaluation_by_class", pretty.name=TRUE)
  res <- model_df %>% tidy(model, type="conf_mat")
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

