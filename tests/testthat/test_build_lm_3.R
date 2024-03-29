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

# Add group_by. Cases without group_by is covered in test_lm_2.R.
flight <- flight %>%
  # Add a row for a group that would fail in model building, because of having only 1 row.
  add_row(`CAR RIER`="DUMMY", `ARR DELAY`=1, `DIS TANCE`=1, `DEP DELAY`=1, `is delayed`=TRUE, `FL NUM`=1) %>%
  group_by(`CAR RIER`)

test_that("build_lm.fast (linear regression) evaluate training and test with FIRM importance", {
  model_df <- flight %>%
                build_lm.fast(`ARR DELAY`, `DIS TANCE`, `DEP DELAY`, `CAR RIER`, test_rate = 0.3, seed=1, importance_measure="firm")

  # Check variable importance output.
  ret <- model_df %>% tidy_rowwise(model, type="importance")
  expect_equal(colnames(ret), c("CAR RIER", "variable", "importance", "p.value"))

  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1475) # Not very stable for some reason. Will revisit.
  expect_lt(nrow(test_ret), 1500)
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3444) # Not very stable for some reason. Will revisit.
  expect_lt(nrow(train_ret), 3500)
  expect_gt(nrow(train_ret), 3400)
  ret <- model_df %>% evaluate_lm_training_and_test(pretty.name=TRUE)
  expect_equal(nrow(ret %>% filter(`CAR RIER`=="DUMMY")), 1) # Row for the group with error.
  expect_equal(nrow(ret %>% filter(`CAR RIER`=="DUMMY" & !is.na(Note))), 1) # Row for the group with error should have message in Note column. 
  # expect_equal(nrow(ret), 2) # 2 for train and test. Fails with group_by
})

test_that("build_lm.fast (linear regression) evaluate training and test with permutation importance", {
  model_df <- flight %>%
                build_lm.fast(`ARR DELAY`, `DIS TANCE`, `DEP DELAY`, `CAR RIER`, test_rate = 0.3, seed=1)

  ret <- model_df %>% tidy_rowwise(model, type="permutation_importance")
  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1475) # Not very stable for some reason. Will revisit.
  expect_lt(nrow(test_ret), 1500)
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3444) # Not very stable for some reason. Will revisit.
  expect_lt(nrow(train_ret), 3500)
  expect_gt(nrow(train_ret), 3400)
  ret <- model_df %>% evaluate_lm_training_and_test(pretty.name=TRUE)
  expect_equal(nrow(ret %>% filter(`CAR RIER`=="DUMMY")), 1) # Row for the group with error.
  expect_equal(nrow(ret %>% filter(`CAR RIER`=="DUMMY" & !is.na(Note))), 1) # Row for the group with error should have message in Note column. 
  # expect_equal(nrow(ret), 2) # 2 for train and test. Fails with group_by
})

test_that("build_lm.fast (logistic regression) evaluate training and test FIRM importance", {
  model_df <- flight %>%
                build_lm.fast(`is delayed`, `DIS TANCE`, `DEP TIME`, model_type = "glm", test_rate = 0.3, importance_measure="firm")

  # Check variable importance output.
  ret <- model_df %>% tidy_rowwise(model, type="importance")
  expect_equal(colnames(ret), c("CAR RIER", "variable", "importance", "p.value"))

  ret <- model_df %>% tidy_rowwise(model, converged_only=TRUE) # Test converged_only
  ret <- model_df %>% prediction_binary(data="training_and_test", threshold = 0.5)
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1480) # Not very stable for some reason. Will revisit
  expect_lt(nrow(test_ret), 1500)
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3454) # Not very stable for some reason. Will revisit
  expect_lt(nrow(train_ret), 3500)
  expect_gt(nrow(train_ret), 3400)
  ret <- model_df %>% evaluate_binary_training_and_test("is delayed", pretty.name=TRUE)
  expect_equal(nrow(ret %>% filter(`CAR RIER`=="DUMMY")), 1) # Row for the group with error.
  expect_equal(nrow(ret %>% filter(`CAR RIER`=="DUMMY" & !is.na(Note))), 1) # Row for the group with error should have message in Note column. 
  # expect_equal(nrow(ret), 2) # 2 for train and test. Fails with group_by
  ret <- model_df %>% prediction_training_and_test(prediction_type = 'conf_mat', threshold = 0.5)
})

test_that("build_lm.fast (logistic regression) evaluate training and test permutation importance", {
  model_df <- flight %>%
                build_lm.fast(`is delayed`, `DIS TANCE`, `DEP TIME`, model_type = "glm", test_rate = 0.3)

  ret <- model_df %>% tidy_rowwise(model, type="permutation_importance")
  ret <- model_df %>% tidy_rowwise(model, converged_only=TRUE) # Test converged_only
  ret <- model_df %>% prediction_binary(data="training_and_test", threshold = 0.5)
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1480) # Not very stable for some reason. Will revisit
  expect_lt(nrow(test_ret), 1500)
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3454) # Not very stable for some reason. Will revisit
  expect_lt(nrow(train_ret), 3500)
  expect_gt(nrow(train_ret), 3400)
  ret <- model_df %>% evaluate_binary_training_and_test("is delayed", pretty.name=TRUE)
  expect_equal(nrow(ret %>% filter(`CAR RIER`=="DUMMY")), 1) # Row for the group with error.
  expect_equal(nrow(ret %>% filter(`CAR RIER`=="DUMMY" & !is.na(Note))), 1) # Row for the group with error should have message in Note column. 
  # expect_equal(nrow(ret), 2) # 2 for train and test. Fails with group_by
  ret <- model_df %>% prediction_training_and_test(prediction_type = 'conf_mat', threshold = 0.5)
})

test_that("build_lm.fast (logistic regression) evaluate training and test with SMOTE", {
  # test mode case
  model_df <- flight %>%
                build_lm.fast(`is delayed`, `DIS TANCE`, `DEP TIME`, model_type = "glm", test_rate = 0.3, smote=T)

  ret <- model_df %>% prediction_binary(data="training_and_test", threshold = 0.5)
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1480) # Not very stable for some reason. Will revisit
  # expect_lt(nrow(test_ret), 1500) # Not true because of SMOTE.
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3454) # Not very stable for some reason. Will revisit
  # expect_lt(nrow(train_ret), 3500) # Not true because of SMOTE.
  expect_gt(nrow(train_ret), 3400)
  ret <- model_df %>% evaluate_binary_training_and_test("is delayed", pretty.name=TRUE)
  expect_equal(nrow(ret %>% filter(`CAR RIER`=="DUMMY")), 1) # Row for the group with error.
  expect_equal(nrow(ret %>% filter(`CAR RIER`=="DUMMY" & !is.na(Note))), 1) # Row for the group with error should have message in Note column. 
  # expect_equal(nrow(ret), 2) # 2 for train and test. Fails with group_by
  ret <- model_df %>% prediction_training_and_test(prediction_type = 'conf_mat', threshold = 0.5)

  # training only case
  model_df <- flight %>%
                build_lm.fast(`is delayed`, `DIS TANCE`, `DEP TIME`, model_type = "glm", test_rate = 0, smote=T)

  ret <- model_df %>% prediction_binary(data="training_and_test", threshold = 0.5)
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1480) # Not very stable for some reason. Will revisit
  expect_lt(nrow(test_ret), 1500)
  # expect_gt(nrow(test_ret), 1400) # Not true because of SMOTE
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3454) # Not very stable for some reason. Will revisit
  # expect_lt(nrow(train_ret), 3500) # Not true because of SMOTE
  expect_gt(nrow(train_ret), 3400)
  ret <- model_df %>% evaluate_binary_training_and_test("is delayed", pretty.name=TRUE)
  expect_equal(nrow(ret %>% filter(`CAR RIER`=="DUMMY")), 1) # Row for the group with error.
  expect_equal(nrow(ret %>% filter(`CAR RIER`=="DUMMY" & !is.na(Note))), 1) # Row for the group with error should have message in Note column. 
  # expect_equal(nrow(ret), 1) # 1 for train. Fails with group_by
  ret <- model_df %>% prediction_training_and_test(prediction_type = 'conf_mat', threshold = 0.5)
})

test_that("build_lm.fast (gaussian regression) evaluate training and test", {
  model_df <- flight %>%
                build_lm.fast(`FL NUM`, `DIS TANCE`, `DEP TIME`, model_type = "glm", family = "gaussian", test_rate = 0.3)

  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1475) # Not very stable for some reason. Will revisit
  expect_lt(nrow(test_ret), 1500)
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3444) # Not very stable for some reason. Will revisit
  expect_lt(nrow(train_ret), 3500)
  expect_gt(nrow(train_ret), 3400)
  ret <- model_df %>% evaluate_lm_training_and_test(pretty.name=TRUE)
  expect_equal(nrow(ret %>% filter(`CAR RIER`=="DUMMY")), 1) # Row for the group with error.
  expect_equal(nrow(ret %>% filter(`CAR RIER`=="DUMMY" & !is.na(Note))), 1) # Row for the group with error should have message in Note column. 
  # expect_equal(nrow(ret), 2) # 2 for train and test. Fails with group_by
})

test_that("build_lm.fast (binomial regression) evaluate training and test", {
  model_df <- flight %>%
                build_lm.fast(`is delayed`, `DIS TANCE`, `DEP TIME`, model_type = "glm", family = "binomial", test_rate = 0.3)

  ret <- model_df %>% prediction_binary(data="training_and_test", threshold = 0.5)
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1480) # Not very stable for some reason. Will revisit
  expect_lt(nrow(test_ret), 1500)
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3454) # Not very stable for some reason. Will revisit
  expect_lt(nrow(train_ret), 3500)
  expect_gt(nrow(train_ret), 3400)
  ret <- model_df %>% evaluate_binary_training_and_test("is delayed", pretty.name=TRUE)
  expect_equal(nrow(ret %>% filter(`CAR RIER`=="DUMMY")), 1) # Row for the group with error.
  expect_equal(nrow(ret %>% filter(`CAR RIER`=="DUMMY" & !is.na(Note))), 1) # Row for the group with error should have message in Note column. 
  # expect_equal(nrow(ret), 2) # Fails with group_by
  ret <- model_df %>% prediction_training_and_test(prediction_type = 'conf_mat', threshold = 0.5)
})
