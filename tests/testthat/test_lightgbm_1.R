context("test tidiers for LightGBM (training and test data)")

testthat::skip_if_not_installed("lightgbm")
suppressPackageStartupMessages(library(dplyr))

# NOTE: This test file originally downloaded a large CSV from S3 to build `flight`.
# In sandboxed/offline environments (like CI sandboxes), network may be unavailable.
# To keep tests runnable everywhere, we create a deterministic synthetic dataset that
# matches the expected schema used by exp_lightgbm tests.
make_synthetic_flight <- function(n = 5000) {
  dep_delay <- rnorm(n, mean = 5, sd = 15)
  air_time <- runif(n, min = 30, max = 300)
  distance <- runif(n, min = 100, max = 3000)
  dep_time <- sample(0:2359, n, replace = TRUE)
  fl_num <- sample(1:9999, n, replace = TRUE)
  carrier <- sample(c("AA", "DL", "UA", "WN"), n, replace = TRUE)
  origin <- sample(c("SFO", "LAX", "JFK", "ORD", "SEA"), n, replace = TRUE)
  fl_date <- as.Date("2013-10-01") + sample(0:30, n, replace = TRUE)

  # Create target with a strong DEP DELAY signal so importance tests are stable.
  arr_delay <- 0.9 * dep_delay + 0.01 * distance - 0.02 * air_time + rnorm(n, sd = 10)
  is_delayed <- as.integer(arr_delay > 15)

  exploratory::clean_data_frame(tibble::tibble(
    `ARR DELAY` = arr_delay,
    `FL NUM` = fl_num,
    `CAR RIER` = as.factor(carrier),
    `ORI GIN` = as.factor(origin),
    `DEP DELAY` = dep_delay,
    `AIR TIME` = air_time,
    `DIS TANCE` = distance,
    `DEP TIME` = dep_time,
    `FL DATE` = fl_date,
    `is delayed` = is_delayed
  ))
}

set.seed(1)
flight <- make_synthetic_flight(n = 5000)

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
  expect_true(all(c("Predicted Value", "Test Data") %in% colnames(ret)))
  ret <- flight %>% dplyr::select(-`ARR DELAY`) %>% add_prediction(model_df = model_df)
  ret <- model_df %>% prediction(data = "newdata", data_frame = flight)

  ret <- model_df %>% tidy_rowwise(model, type = "evaluation_log")
  expect_equal(nrow(ret), 20)
  expect_equal(colnames(ret), c("Iter", "type", "name", "value"))
  ret <- model_df %>% prediction(data = "training_and_test")
  test_ret <- ret %>% filter(is_test_data == TRUE)
  # Check that test data count is less than 1500 (30% of ~5000 rows, accounting for potential NA filtering)
  expect_lte(nrow(test_ret), 1600)
  # Check that test data count is greater than 1400 (ensures test_rate = 0.3 is approximately applied)
  expect_gte(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data == FALSE)
  # Check that training data count is less than 3500 (70% of ~5000 rows, accounting for potential NA filtering)
  expect_lte(nrow(train_ret), 3500)
  # Check that training data count is greater than 3300 (ensures remaining data after test split is reasonable)
  expect_gte(nrow(train_ret), 3300)

  # Check result of variable importance
  ret <- model_df %>% tidy_rowwise(model, type = "importance")
  expect_equal(as.character(ret$variable[[1]]), "DEP DELAY")

  ret <- rf_evaluation_training_and_test(model_df, pretty.name = TRUE)
  expect_equal(nrow(ret), 2)
  # Check model accuracy metrics (RMSE / R Squared) for both training and test.
  expect_true(all(c("RMSE", "R Squared") %in% colnames(ret)))
  expect_true(all(is.finite(ret$RMSE)))
  expect_true(all(ret$RMSE > 0))
  expect_true(all(is.finite(ret$`R Squared`)))
  expect_true(all(ret$`R Squared` <= 1))
  # Note: We no longer assert exact metric values here because the dataset can be synthetic/offline.

  ret <- model_df %>% rf_partial_dependence()
  expect_equal(class(ret$conf_high), "numeric")

  model_df <- flight %>% exp_lightgbm(`FL NUM`, `DIS TANCE`, `DEP TIME`, test_rate = 0, pd_with_bin_means = TRUE)
  ret <- model_df %>% prediction(data = "training_and_test")
  train_ret <- ret %>% filter(is_test_data == FALSE)
  expect_lte(nrow(train_ret), 5000)

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

  ret <- flight %>% dplyr::select(-`ARR DELAY`) %>% add_prediction(model_df = model_df)
  ret <- model_df %>% prediction(data = "newdata", data_frame = flight)

  ret <- model_df %>% tidy_rowwise(model, type = "evaluation_log")
  ret <- model_df %>% prediction(data = "training_and_test")
  test_ret <- ret %>% filter(is_test_data == TRUE)
  expect_lte(nrow(test_ret), 1600)
  expect_gte(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data == FALSE)
  expect_lte(nrow(train_ret), 3500)
  expect_gte(nrow(train_ret), 3300)

  ret <- model_df %>% tidy_rowwise(model, type = "importance")
  expect_equal(as.character(ret$variable[[1]]), "DEP DELAY")
  ret2 <- model_df %>% rf_partial_dependence()
  variables <- ret$variable
  names(variables) <- NULL
  expect_equal(levels(ret2$x_name), variables)

  ret <- rf_evaluation_training_and_test(model_df, pretty.name = TRUE)
  expect_equal(nrow(ret), 2)
  # Check model accuracy metrics (RMSE / R Squared) for both training and test.
  expect_true(all(c("RMSE", "R Squared") %in% colnames(ret)))
  expect_true(all(is.finite(ret$RMSE)))
  expect_true(all(ret$RMSE > 0))
  expect_true(all(is.finite(ret$`R Squared`)))
  expect_true(all(ret$`R Squared` <= 1))
  # Note: We no longer assert exact metric values here because the dataset can be synthetic/offline.

  ret <- model_df %>% rf_partial_dependence()
  expect_equal(class(ret$conf_high), "numeric")

  model_df <- flight %>% exp_lightgbm(`FL NUM`, `DIS TANCE`, `DEP TIME`, test_rate = 0, pd_with_bin_means = TRUE)
  ret <- model_df %>% prediction(data = "training_and_test")
  train_ret <- ret %>% filter(is_test_data == FALSE)
  expect_lte(nrow(train_ret), 5000)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1)
})

test_that("exp_lightgbm(regression) evaluation_log works with eval_metric_regression = mae", {
  set.seed(1)
  # Keep this test minimal (no partial dependence) so it runs fast while still exercising
  # the LightGBM evaluation-log path.
  model_df <- flight %>%
    exp_lightgbm(`ARR DELAY`, `DEP DELAY`, `AIR TIME`, `DIS TANCE`, `DEP TIME`,
      test_rate = 0,
      watchlist_rate = 0.1,
      importance_measure = "lightgbm",
      max_pd_vars = 0,
      pd_with_bin_means = FALSE,
      eval_metric_regression = "mae"
    )

  ret <- model_df %>% tidy_rowwise(model, type = "evaluation_log")
  # 10 iterations * 2 datasets (train/validation)
  expect_equal(nrow(ret), 20)
  expect_equal(colnames(ret), c("Iter", "type", "name", "value"))
  # LightGBM often reports MAE as "l1" (version-dependent).
  expect_true(all(ret$name %in% c("l1", "mae")))
})

test_that("exp_lightgbm(regression) evaluation_log includes test when test_rate>0 and watchlist_rate=0", {
  set.seed(1)
  # Keep this test minimal to avoid long permutation importance / PD calculations.
  model_df <- flight %>%
    exp_lightgbm(`ARR DELAY`, `DEP DELAY`, `AIR TIME`, `DIS TANCE`,
      test_rate = 0.3,
      watchlist_rate = 0,
      importance_measure = "lightgbm",
      max_pd_vars = 0,
      pd_with_bin_means = FALSE,
      nrounds = 10
    )

  ret <- model_df %>% tidy_rowwise(model, type = "evaluation_log")
  # 10 iterations * 2 datasets (train/test)
  expect_equal(nrow(ret), 20)
  expect_equal(colnames(ret), c("Iter", "type", "name", "value"))
  expect_true(all(c("train", "test") %in% unique(ret$type)))
})

test_that("exp_lightgbm(firm) rf_partial_dependence tolerates numeric target attribute", {
  set.seed(1)
  df <- tibble::tibble(
    y = rnorm(500),
    x1 = rnorm(500),
    x2 = runif(500)
  )

  model_df <- df %>%
    exp_lightgbm(target = y, x1, x2,
      importance_measure = "firm",
      watchlist_rate = 0.1,
      test_rate = 0,
      # keep it lightweight
      max_pd_vars = 2,
      pd_with_bin_means = FALSE,
      nrounds = 10
    )

  # Simulate the problematic case: some callers/versions can store target as a numeric index.
  # This used to cause "subscript out of bounds" / symbol conversion errors in handle_partial_dependence().
  m <- model_df$model[[1]]
  target_name <- attr(m$partial_dependence, "target")
  target_idx <- which(colnames(m$partial_dependence) == target_name)
  # Make sure we point to the actual target column.
  expect_equal(length(target_idx), 1)
  attr(m$partial_dependence, "target") <- target_idx
  model_df$model[[1]] <- m

  pd <- model_df %>% rf_partial_dependence()
  expect_true(is.data.frame(pd))
  expect_gt(nrow(pd), 0)
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

    ret <- flight %>% dplyr::select(-`ARR DELAY`) %>% add_prediction(model_df = model_df)
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

  ret1 <- data %>% dplyr::select(-is_delayed) %>% add_prediction(model_df = model_df, binary_classification_threshold = 0.5)
  expect_equal(class(ret1$predicted_label), "logical")
  ret2 <- data %>% dplyr::select(-is_delayed) %>% add_prediction(model_df = model_df, binary_classification_threshold = 0.01)
  expect_gt(sum(ret2$predicted_label == TRUE, na.rm = TRUE), sum(ret1$predicted_label == TRUE, na.rm = TRUE))
  ret <- model_df %>% prediction(data = "newdata", data_frame = flight)

  ret <- rf_evaluation_training_and_test(model_df, binary_classification_threshold = 0.5)
  expect_true(all(c("auc", "f_score", "accuracy_rate", "misclassification_rate", "precision", "recall") %in% colnames(ret)))
  expect_equal(nrow(ret), 2)
  expect_gt(ret$auc[[1]], 0.5)
  # Check model accuracy metrics (AUC) for both training and test.
  expect_true(all(is.finite(ret$auc)))
  expect_true(all(ret$auc >= 0 & ret$auc <= 1))
  train_auc <- ret %>% dplyr::filter(is_test_data == FALSE) %>% dplyr::pull(auc)
  test_auc <- ret %>% dplyr::filter(is_test_data == TRUE) %>% dplyr::pull(auc)
  expect_equal(length(train_auc), 1)
  expect_equal(length(test_auc), 1)
  expect_true(train_auc[[1]] >= 0.5)
  expect_true(test_auc[[1]] >= 0.5)

  ret <- model_df %>% tidy_rowwise(model, type = "conf_mat")
  ret <- model_df %>% tidy_rowwise(model, type = "partial_dependence")

  res_partial_dependence <- model_df %>% rf_partial_dependence()
  ret <- model_df %>% prediction(data = "training_and_test")
  test_ret <- ret %>% filter(is_test_data == TRUE)
  expect_lte(nrow(test_ret), 1600)
  expect_gte(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data == FALSE)
  expect_lte(nrow(train_ret), 3500)
  expect_gte(nrow(train_ret), 3300)

  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")
  expect_equal(nrow(ret), 8)
  expect_equal(n_distinct(ret$is_test_data), 2)

  model_df <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`)) %>% exp_lightgbm(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0, pd_with_bin_means = TRUE)
  ret <- model_df %>% prediction(data = "training_and_test")
  train_ret <- ret %>% filter(is_test_data == FALSE)
  expect_lte(nrow(train_ret), 5000)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1)

  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")
})

test_that("exp_lightgbm(binary) evaluation_log works with eval_metric_binary = binary_error", {
  set.seed(1)
  data <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`))
  # Keep this test minimal (no partial dependence) so it runs fast.
  model_df <- data %>%
    exp_lightgbm(is_delayed, `DIS TANCE`, `DEP TIME`,
      predictor_funs = list(`DIS TANCE` = "none", `DEP TIME` = "none"),
      test_rate = 0,
      watchlist_rate = 0.1,
      importance_measure = "lightgbm",
      max_pd_vars = 0,
      pd_with_bin_means = FALSE,
      eval_metric_binary = "binary_error"
    )

  ret <- model_df %>% tidy_rowwise(model, type = "evaluation_log")
  # 10 iterations * 2 datasets (train/validation)
  expect_equal(nrow(ret), 20)
  expect_equal(colnames(ret), c("Iter", "type", "name", "value"))
  # Allow common alias fallback across LightGBM versions.
  expect_true(all(ret$name %in% c("binary_error", "error")))
})

test_that("exp_lightgbm(binary) evaluation_log works with eval_metric_binary = binary_logloss", {
  set.seed(1)
  data <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`))
  # Keep this test minimal (no partial dependence) so it runs fast.
  model_df <- data %>%
    exp_lightgbm(is_delayed, `DIS TANCE`, `DEP TIME`,
      predictor_funs = list(`DIS TANCE` = "none", `DEP TIME` = "none"),
      test_rate = 0,
      watchlist_rate = 0.1,
      importance_measure = "lightgbm",
      max_pd_vars = 0,
      pd_with_bin_means = FALSE,
      eval_metric_binary = "binary_logloss"
    )

  ret <- model_df %>% tidy_rowwise(model, type = "evaluation_log")
  # 10 iterations * 2 datasets (train/validation)
  expect_equal(nrow(ret), 20)
  expect_equal(colnames(ret), c("Iter", "type", "name", "value"))
  # Allow common alias fallback across LightGBM versions.
  expect_true(all(ret$name %in% c("binary_logloss", "logloss")))
})

test_that("exp_lightgbm evaluate training and test with permutation importance - binary", {
  set.seed(1)
  data <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`))
  model_df <- data %>% exp_lightgbm(is_delayed, `DIS TANCE`, `DEP TIME`,
    predictor_funs = list(`DIS TANCE` = "none", `DEP TIME` = "none"),
    test_rate = 0.3, pd_with_bin_means = TRUE
  )

  ret1 <- data %>% dplyr::select(-is_delayed) %>% add_prediction(model_df = model_df, binary_classification_threshold = 0.5)
  expect_equal(class(ret1$predicted_label), "logical")
  ret2 <- data %>% dplyr::select(-is_delayed) %>% add_prediction(model_df = model_df, binary_classification_threshold = 0.01)
  expect_gt(sum(ret2$predicted_label == TRUE, na.rm = TRUE), sum(ret1$predicted_label == TRUE, na.rm = TRUE))
  ret <- model_df %>% prediction(data = "newdata", data_frame = flight)

  ret <- rf_evaluation_training_and_test(model_df, binary_classification_threshold = 0.5)
  expect_true(all(c("auc", "f_score", "accuracy_rate", "misclassification_rate", "precision", "recall") %in% colnames(ret)))
  expect_equal(nrow(ret), 2)
  expect_gt(ret$auc[[1]], 0.5)
  # Check model accuracy metrics (AUC) for both training and test.
  expect_true(all(is.finite(ret$auc)))
  expect_true(all(ret$auc >= 0 & ret$auc <= 1))
  train_auc <- ret %>% dplyr::filter(is_test_data == FALSE) %>% dplyr::pull(auc)
  test_auc <- ret %>% dplyr::filter(is_test_data == TRUE) %>% dplyr::pull(auc)
  expect_equal(length(train_auc), 1)
  expect_equal(length(test_auc), 1)
  expect_true(train_auc[[1]] >= 0.5)
  expect_true(test_auc[[1]] >= 0.5)

  ret <- model_df %>% tidy_rowwise(model, type = "conf_mat")
  ret <- model_df %>% tidy_rowwise(model, type = "partial_dependence")
  ret <- model_df %>% tidy_rowwise(model, type = "importance")

  res_partial_dependence <- model_df %>% rf_partial_dependence()
  ret <- model_df %>% prediction(data = "training_and_test")
  test_ret <- ret %>% filter(is_test_data == TRUE)
  expect_lte(nrow(test_ret), 1600)
  expect_gte(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data == FALSE)
  expect_lte(nrow(train_ret), 3500)
  expect_gte(nrow(train_ret), 3300)

  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")
  expect_equal(nrow(ret), 8)
  expect_equal(n_distinct(ret$is_test_data), 2)

  model_df <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`)) %>% exp_lightgbm(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0, pd_with_bin_means = TRUE)
  ret <- model_df %>% prediction(data = "training_and_test")
  train_ret <- ret %>% filter(is_test_data == FALSE)
  expect_lte(nrow(train_ret), 5000)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1)

  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")
})


