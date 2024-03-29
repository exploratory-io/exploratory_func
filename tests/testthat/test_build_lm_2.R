# how to run this test:
# devtools::test(filter="lm_1")

context("test lm/glm prediction with training/test data")

testdata_dir <- tempdir()
testdata_filename <- "airline_2013_10_tricky_v3_5k.csv" 
testdata_file_path <- paste0(testdata_dir, '/', testdata_filename)

filepath <- if (!testdata_filename %in% list.files(testdata_dir)) {
  "https://exploratory-download.s3.us-west-2.amazonaws.com/test/airline_2013_10_tricky_v3.csv"
} else {
  testdata_file_path
}

flight <- exploratory::read_delim_file(filepath, ",", quote = "\"", skip = 0 , col_names = TRUE , na = c("","NA") , locale=readr::locale(encoding = "UTF-8", decimal_mark = "."), trim_ws = FALSE , progress = FALSE) %>% exploratory::clean_data_frame()

if (!testdata_filename %in% list.files(testdata_dir)) {
  set.seed(1)
  flight <- flight %>% slice_sample(n=5000)
  write.csv(flight, testdata_file_path) # save sampled-down data for performance.
}

test_that("build_lm.fast (linear regression) evaluate training and test with FIRM importance", {
  model_df <- flight %>%
                build_lm.fast(`ARR DELAY`, `DIS TANCE`, `DEP DELAY`, `CAR RIER`, test_rate = 0.3, seed=1, importance_measure="firm")

  ret <- model_df %>% prediction(data="training_and_test", pretty.name=TRUE)
  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1475) # Not very stable for some reason. Will revisit.
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3444) # Not very stable for some reason. Will revisit.
  ret <- model_df %>% evaluate_lm_training_and_test(pretty.name=TRUE)
  expect_equal(nrow(ret), 2) # 2 for train and test

  # Check order of variable importance result.
  ret <- model_df %>% tidy_rowwise(model, type="importance")
  # unname() is necessary for the result to be equal to the expectation.
  expect_equal(unname((ret %>% arrange(-importance))$variable), c("DEP DELAY", "CAR RIER", "DIS TANCE"))

  # Test univariate case handling
  model_df <- flight %>%
                build_lm.fast(`ARR DELAY`, `DIS TANCE`, test_rate = 0.3, seed=1)
  ret <- model_df %>% tidy_rowwise(model, type="importance")
  expect_equal(nrow(ret), 0)
})

test_that("build_lm.fast (linear regression) evaluate training and test with permutation importance", {
  model_df <- flight %>%
                build_lm.fast(`ARR DELAY`, `DIS TANCE`, `DEP DELAY`, `CAR RIER`, test_rate = 0.3, seed=1)

  ret <- model_df %>% prediction(data="training_and_test", pretty.name=TRUE)
  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1475) # Not very stable for some reason. Will revisit.
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3444) # Not very stable for some reason. Will revisit.
  ret <- model_df %>% evaluate_lm_training_and_test(pretty.name=TRUE)
  expect_equal(nrow(ret), 2) # 2 for train and test

  # Check order of variable importance result.
  ret <- model_df %>% tidy_rowwise(model, type="permutation_importance")
  # unname() is necessary for the result to be equal to the expectation.
  expect_equal(unname((ret %>% arrange(-importance))$term), c("DEP DELAY", "CAR RIER", "DIS TANCE"))

  # Test univariate case handling
  model_df <- flight %>%
                build_lm.fast(`ARR DELAY`, `DIS TANCE`, test_rate = 0.3, seed=1)
  ret <- model_df %>% tidy_rowwise(model, type="permutation_importance")
  expect_equal(nrow(ret), 0)
})

test_that("build_lm.fast (linear regression) evaluate training and test with permutation importance with weight", {
  set.seed(0)
  model_df <- flight %>% mutate(Weight=0.3*sin(1:n())+1) %>%
                build_lm.fast(`ARR DELAY`, `DIS TANCE`, `DEP DELAY`, `CAR RIER`, test_rate = 0.3, seed=1, weight=Weight)

  # Check the numbers so that we can detect any change in broom or stats in the future.
  expect_equal((model_df %>% tidy_rowwise(model))$estimate[1:3], c(-4.5642712, 1.8621591, 0.5881287), tolerance=1e-4)
  ret <- model_df %>% prediction(data="training_and_test", pretty.name=TRUE)
  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1475) # Not very stable for some reason. Will revisit.
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3444) # Not very stable for some reason. Will revisit.
  ret <- model_df %>% evaluate_lm_training_and_test(pretty.name=TRUE)
  expect_equal(nrow(ret), 2) # 2 for train and test

  # Check order of variable importance result.
  ret <- model_df %>% tidy_rowwise(model, type="permutation_importance")
  # unname() is necessary for the result to be equal to the expectation.
  expect_equal(unname((ret %>% arrange(-importance))$term), c("DEP DELAY", "CAR RIER", "DIS TANCE"))

  # Test univariate case handling
  model_df <- flight %>%
                build_lm.fast(`ARR DELAY`, `DIS TANCE`, test_rate = 0.3, seed=1)
  ret <- model_df %>% tidy_rowwise(model, type="permutation_importance")
  expect_equal(nrow(ret), 0)
})

test_that("build_lm.fast (logistic regression(numeric)) evaluate training and test", {
  model_df <- flight %>%
                build_lm.fast(`is delayed`, `DIS TANCE`, `DEP TIME`, model_type = "glm", test_rate = 0.3)

  ret <- model_df %>% prediction_binary(data="training_and_test", threshold = 0.5)
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1480) # Not very stable for some reason. Will revisit
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3454) # Not very stable for some reason. Will revisit
  ret <- model_df %>% evaluate_binary_training_and_test("is delayed", pretty.name=TRUE, threshold=0.1)
  expect_gt(ret$Recall[[1]], 0.8) # Expect Recall to be ok since threshold is as low as 0.1.
  expect_equal(nrow(ret), 2) # 2 for train and test
  ret <- model_df %>% prediction_training_and_test(prediction_type = 'conf_mat', threshold = 0.5)
})

test_that("build_lm.fast (logistic regression(logical)) evaluate training and test with FIRM importance", {
  model_df <- flight %>% mutate(`is delayed`=as.logical(`is delayed`)) %>%
                build_lm.fast(`is delayed`, `DIS TANCE`, `DEP TIME`, model_type = "glm", test_rate = 0.3, importance_measure="firm")

  # Check variable importance output.
  ret <- model_df %>% tidy_rowwise(model, type="importance")
  expect_equal(colnames(ret), c("variable", "importance", "p.value"))

  ret <- model_df %>% prediction_binary(data="training_and_test", threshold = 0.5, pretty.name=TRUE)
  ret <- model_df %>% prediction_binary(data="training_and_test", threshold = 0.5)
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1480) # Not very stable for some reason. Will revisit
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3454) # Not very stable for some reason. Will revisit
  ret <- model_df %>% evaluate_binary_training_and_test("is delayed", pretty.name=TRUE, threshold=0.1)
  expect_gt(ret$Recall[[1]], 0.8) # Expect Recall to be ok since threshold is as low as 0.1.
  expect_equal(nrow(ret), 2) # 2 for train and test
  ret <- model_df %>% prediction_training_and_test(prediction_type = 'conf_mat', threshold = 0.5)
})

test_that("build_lm.fast (logistic regression(logical)) evaluate training and test with permutation importance", {
  model_df <- flight %>% mutate(`is delayed`=as.logical(`is delayed`)) %>%
                build_lm.fast(`is delayed`, `DIS TANCE`, `DEP TIME`, model_type = "glm", test_rate = 0.3)

  ret <- model_df %>% prediction_binary(data="training_and_test", threshold = 0.5, pretty.name=TRUE)
  ret <- model_df %>% prediction_binary(data="training_and_test", threshold = 0.5)
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1480) # Not very stable for some reason. Will revisit
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3454) # Not very stable for some reason. Will revisit
  ret <- model_df %>% evaluate_binary_training_and_test("is delayed", pretty.name=TRUE, threshold=0.1)
  expect_gt(ret$Recall[[1]], 0.8) # Expect Recall to be ok since threshold is as low as 0.1.
  expect_equal(nrow(ret), 2) # 2 for train and test
  ret <- model_df %>% prediction_training_and_test(prediction_type = 'conf_mat', threshold = 0.5)
})

test_that("build_lm.fast (logistic regression(character)) evaluate training and test", {
  expect_error({
    model_df <- flight %>% mutate(`is delayed`=if_else(as.logical(`is delayed`), "A", "B")) %>%
      build_lm.fast(`is delayed`, `DIS TANCE`, `DEP TIME`, model_type = "glm", test_rate = 0.3)
  }, "Target variable for logistic regression must be a logical.")
})

test_that("build_lm.fast (logistic regression) evaluate training and test with SMOTE", {
  # test mode case
  model_df <- flight %>%
                build_lm.fast(`is delayed`, `DIS TANCE`, `DEP TIME`, model_type = "glm", test_rate = 0.3, smote=T)

  ret <- model_df %>% prediction_binary(data="training_and_test", threshold = 0.5)
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1480) # Not very stable for some reason. Will revisit
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3454) # Not very stable for some reason. Will revisit
  ret <- model_df %>% evaluate_binary_training_and_test("is delayed", pretty.name=TRUE)
  expect_equal(nrow(ret), 2) # 2 for train and test
  ret <- model_df %>% prediction_training_and_test(prediction_type = 'conf_mat', threshold = 0.5)

  # training only case
  model_df <- flight %>%
                build_lm.fast(`is delayed`, `DIS TANCE`, `DEP TIME`, model_type = "glm", test_rate = 0, smote=T)

  ret <- model_df %>% prediction_binary(data="training_and_test", threshold = 0.5)
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1480) # Not very stable for some reason. Will revisit
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3454) # Not very stable for some reason. Will revisit
  ret <- model_df %>% evaluate_binary_training_and_test("is delayed", pretty.name=TRUE)
  expect_equal(nrow(ret), 1) # 1 for train
  ret <- model_df %>% prediction_training_and_test(prediction_type = 'conf_mat', threshold = 0.5)
})

test_that("build_lm.fast (gaussian regression) evaluate training and test", {
  model_df <- flight %>%
                build_lm.fast(`FL NUM`, `DIS TANCE`, `DEP TIME`, model_type = "glm", family = "gaussian", test_rate = 0.3)

  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1475) # Not very stable for some reason. Will revisit
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3444) # Not very stable for some reason. Will revisit
  ret <- model_df %>% evaluate_lm_training_and_test(pretty.name=TRUE)
  expect_equal(nrow(ret), 2) # 2 for train and test
})

test_that("build_lm.fast (binomial regression) evaluate training and test", {
  model_df <- flight %>%
                build_lm.fast(`is delayed`, `DIS TANCE`, `DEP TIME`, model_type = "glm", family = "binomial", test_rate = 0.3)

  ret <- model_df %>% prediction_binary(data="training_and_test", threshold = 0.5)
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1480) # Not very stable for some reason. Will revisit
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3454) # Not very stable for some reason. Will revisit
  ret <- model_df %>% evaluate_binary_training_and_test("is delayed", pretty.name=TRUE)
  expect_equal(nrow(ret), 2) # 2 for train and test
  ret <- model_df %>% prediction_training_and_test(prediction_type = 'conf_mat', threshold = 0.5)
})
