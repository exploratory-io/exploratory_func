context("test tidiers for LightGBM (training and test data)")

testthat::skip_if_not_installed("lightgbm")

testdata_dir <- tempdir()
testdata_filename <- "airline_2013_10_tricky_v3_5k.csv"
testdata_file_path <- paste0(testdata_dir, "/", testdata_filename)

filepath <- if (!testdata_filename %in% list.files(testdata_dir)) {
  "https://exploratory-download.s3.us-west-2.amazonaws.com/test/airline_2013_10_tricky_v3.csv"
} else {
  testdata_file_path
}

flight <- exploratory::read_delim_file(filepath, ",", quote = "\"", skip = 0, col_names = TRUE, na = c("", "NA"), locale = readr::locale(encoding = "UTF-8", decimal_mark = "."), trim_ws = FALSE, progress = FALSE) %>%
  exploratory::clean_data_frame()

if (!testdata_filename %in% list.files(testdata_dir)) {
  set.seed(1)
  flight <- flight %>% slice_sample(n = 5000)
  write.csv(flight, testdata_file_path)
}

test_that("exp_lightgbm(regression) evaluate training and test with FIRM importance", {
  set.seed(1)
  model_df <- flight %>%
    exp_lightgbm(`ARR DELAY`, `CAR RIER`, `ORI GIN`, `DEP DELAY`, `AIR TIME`, `FL DATE`,
      predictor_funs = list(`CAR RIER` = "none", `ORI GIN` = "none", `DEP DELAY` = "none", `AIR TIME` = "none", list(`FL DATE_y` = "year", `FL DATE_m` = "monname", `FL DATE_dom` = "day", `FL DATE_dow` = "wday")),
      test_rate = 0.3,
      test_split_type = "ordered", pd_with_bin_means = TRUE,
      watchlist_rate = 0.1,
      importance_measure = "firm"
    )

  ret <- model_df %>% prediction(data = "training_and_test", pretty.name = TRUE)

  ret <- flight %>% select(-`ARR DELAY`) %>% add_prediction(model_df = model_df)
  ret <- model_df %>% prediction(data = "newdata", data_frame = flight)

  ret <- model_df %>% tidy_rowwise(model, type = "evaluation_log")
  ret <- model_df %>% prediction(data = "training_and_test")
  test_ret <- ret %>% filter(is_test_data == TRUE)
  expect_lt(nrow(test_ret), 1500)
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data == FALSE)
  expect_lt(nrow(train_ret), 3500)
  expect_gt(nrow(train_ret), 3300)

  # Check result of variable importance
  ret <- model_df %>% tidy_rowwise(model, type = "importance")
  expect_equal(as.character(ret$variable[[1]]), "DEP DELAY")

  ret <- rf_evaluation_training_and_test(model_df, pretty.name = TRUE)
  expect_equal(nrow(ret), 2)

  ret <- model_df %>% rf_partial_dependence()
  expect_equal(class(ret$conf_high), "numeric")

  model_df <- flight %>% exp_lightgbm(`FL NUM`, `DIS TANCE`, `DEP TIME`, test_rate = 0, pd_with_bin_means = TRUE)
  ret <- model_df %>% prediction(data = "training_and_test")
  train_ret <- ret %>% filter(is_test_data == FALSE)
  expect_lt(nrow(train_ret), 5000)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1)
})

test_that("exp_lightgbm(regression) evaluate training and test with permutation importance", {
  set.seed(1)
  model_df <- flight %>%
    exp_lightgbm(`ARR DELAY`, `CAR RIER`, `ORI GIN`, `DEP DELAY`, `AIR TIME`, `FL DATE`,
      predictor_funs = list(`CAR RIER` = "none", `ORI GIN` = "none", `DEP DELAY` = "none", `AIR TIME` = "none", list(`FL DATE_y` = "year", `FL DATE_m` = "monname", `FL DATE_dom` = "day", `FL DATE_dow` = "wday")),
      test_rate = 0.3,
      test_split_type = "ordered", pd_with_bin_means = TRUE,
      watchlist_rate = 0.1
    )

  ret <- model_df %>% prediction(data = "training_and_test", pretty.name = TRUE)

  ret <- flight %>% select(-`ARR DELAY`) %>% add_prediction(model_df = model_df)
  ret <- model_df %>% prediction(data = "newdata", data_frame = flight)

  ret <- model_df %>% tidy_rowwise(model, type = "evaluation_log")
  ret <- model_df %>% prediction(data = "training_and_test")
  test_ret <- ret %>% filter(is_test_data == TRUE)
  expect_lt(nrow(test_ret), 1500)
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data == FALSE)
  expect_lt(nrow(train_ret), 3500)
  expect_gt(nrow(train_ret), 3300)

  ret <- model_df %>% tidy_rowwise(model, type = "importance")
  expect_equal(as.character(ret$variable[[1]]), "DEP DELAY")
  ret2 <- model_df %>% rf_partial_dependence()
  variables <- ret$variable
  names(variables) <- NULL
  expect_equal(levels(ret2$x_name), variables)

  ret <- rf_evaluation_training_and_test(model_df, pretty.name = TRUE)
  expect_equal(nrow(ret), 2)

  ret <- model_df %>% rf_partial_dependence()
  expect_equal(class(ret$conf_high), "numeric")

  model_df <- flight %>% exp_lightgbm(`FL NUM`, `DIS TANCE`, `DEP TIME`, test_rate = 0, pd_with_bin_means = TRUE)
  ret <- model_df %>% prediction(data = "training_and_test")
  train_ret <- ret %>% filter(is_test_data == FALSE)
  expect_lt(nrow(train_ret), 5000)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1)
})

test_that("exp_lightgbm - regression - evaluate training and test with locale conversion", {
  set.seed(1)
  orig_locale <- Sys.getlocale("LC_TIME")
  tryCatch({
    Sys.setlocale("LC_TIME", "ja_JP.UTF-8")
    model_df <- flight %>%
      exp_lightgbm(`ARR DELAY`, `CAR RIER`, `ORI GIN`, `DEP DELAY`, `AIR TIME`, `FL DATE`,
        predictor_funs = list(`CAR RIER` = "none", `ORI GIN` = "none", `DEP DELAY` = "none", `AIR TIME` = "none", list(`FL DATE_y` = "year", `FL DATE_m` = "monname", `FL DATE_dom` = "day", `FL DATE_dow` = "wday")),
        test_rate = 0.3,
        test_split_type = "ordered", pd_with_bin_means = TRUE,
        watchlist_rate = 0.1
      )

    if (Sys.info()[["sysname"]] == "Windows") {
      Sys.setlocale("LC_TIME", "English_United States.1252")
    } else {
      Sys.setlocale("LC_TIME", "en_US.UTF-8")
    }

    ret <- flight %>% select(-`ARR DELAY`) %>% add_prediction(model_df = model_df)
    expect_equal(nrow(ret), 5000)
  }, finally = {
    Sys.setlocale("LC_TIME", orig_locale)
  })
})

test_that("exp_lightgbm evaluate training and test with FIRM importance - binary", {
  set.seed(1)
  data <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`))
  model_df <- data %>% exp_lightgbm(is_delayed, `DIS TANCE`, `DEP TIME`,
    predictor_funs = list(`DIS TANCE` = "none", `DEP TIME` = "none"),
    test_rate = 0.3, pd_with_bin_means = TRUE,
    importance_measure = "firm"
  )

  ret <- model_df %>% tidy_rowwise(model, type = "importance")
  expect_equal(colnames(ret), c("variable", "importance"))

  ret1 <- data %>% select(-is_delayed) %>% add_prediction(model_df = model_df, binary_classification_threshold = 0.5)
  expect_equal(class(ret1$predicted_label), "logical")
  ret2 <- data %>% select(-is_delayed) %>% add_prediction(model_df = model_df, binary_classification_threshold = 0.01)
  expect_gt(sum(ret2$predicted_label == TRUE, na.rm = TRUE), sum(ret1$predicted_label == TRUE, na.rm = TRUE))
  ret <- model_df %>% prediction(data = "newdata", data_frame = flight)

  ret <- rf_evaluation_training_and_test(model_df, binary_classification_threshold = 0.5)
  expect_true(all(c("auc", "f_score", "accuracy_rate", "misclassification_rate", "precision", "recall") %in% colnames(ret)))
  expect_equal(nrow(ret), 2)
  expect_gt(ret$auc[[1]], 0.5)

  ret <- model_df %>% tidy_rowwise(model, type = "conf_mat")
  ret <- model_df %>% tidy_rowwise(model, type = "partial_dependence")

  res_partial_dependence <- model_df %>% rf_partial_dependence()
  ret <- model_df %>% prediction(data = "training_and_test")
  test_ret <- ret %>% filter(is_test_data == TRUE)
  expect_lt(nrow(test_ret), 1500)
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data == FALSE)
  expect_lt(nrow(train_ret), 3500)
  expect_gt(nrow(train_ret), 3400)

  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")
  expect_equal(nrow(ret), 8)
  expect_equal(n_distinct(ret$is_test_data), 2)

  model_df <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`)) %>% exp_lightgbm(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0, pd_with_bin_means = TRUE)
  ret <- model_df %>% prediction(data = "training_and_test")
  train_ret <- ret %>% filter(is_test_data == FALSE)
  expect_lt(nrow(train_ret), 5000)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1)

  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")
})

test_that("exp_lightgbm evaluate training and test with permutation importance - binary", {
  set.seed(1)
  data <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`))
  model_df <- data %>% exp_lightgbm(is_delayed, `DIS TANCE`, `DEP TIME`,
    predictor_funs = list(`DIS TANCE` = "none", `DEP TIME` = "none"),
    test_rate = 0.3, pd_with_bin_means = TRUE
  )

  ret1 <- data %>% select(-is_delayed) %>% add_prediction(model_df = model_df, binary_classification_threshold = 0.5)
  expect_equal(class(ret1$predicted_label), "logical")
  ret2 <- data %>% select(-is_delayed) %>% add_prediction(model_df = model_df, binary_classification_threshold = 0.01)
  expect_gt(sum(ret2$predicted_label == TRUE, na.rm = TRUE), sum(ret1$predicted_label == TRUE, na.rm = TRUE))
  ret <- model_df %>% prediction(data = "newdata", data_frame = flight)

  ret <- rf_evaluation_training_and_test(model_df, binary_classification_threshold = 0.5)
  expect_true(all(c("auc", "f_score", "accuracy_rate", "misclassification_rate", "precision", "recall") %in% colnames(ret)))
  expect_equal(nrow(ret), 2)
  expect_gt(ret$auc[[1]], 0.5)

  ret <- model_df %>% tidy_rowwise(model, type = "conf_mat")
  ret <- model_df %>% tidy_rowwise(model, type = "partial_dependence")
  ret <- model_df %>% tidy_rowwise(model, type = "importance")

  res_partial_dependence <- model_df %>% rf_partial_dependence()
  ret <- model_df %>% prediction(data = "training_and_test")
  test_ret <- ret %>% filter(is_test_data == TRUE)
  expect_lt(nrow(test_ret), 1500)
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data == FALSE)
  expect_lt(nrow(train_ret), 3500)
  expect_gt(nrow(train_ret), 3400)

  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")
  expect_equal(nrow(ret), 8)
  expect_equal(n_distinct(ret$is_test_data), 2)

  model_df <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`)) %>% exp_lightgbm(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0, pd_with_bin_means = TRUE)
  ret <- model_df %>% prediction(data = "training_and_test")
  train_ret <- ret %>% filter(is_test_data == FALSE)
  expect_lt(nrow(train_ret), 5000)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1)

  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")
})


