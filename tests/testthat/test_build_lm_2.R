context("test build_lm part 2")

test_that("binary prediction with character target column", {
  test_data <- structure(
    list(
      `CANCELLED X` = c("N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "Y", "N", "Y", "N"),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = factor(c(NA, "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL")), # test with factor with NA
      # testing filtering of Inf, -Inf, NA here.
      DISTANCE = c(Inf, -Inf, NA, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED X", "Carrier Name", "CARRIER", "DISTANCE"))

  # duplicate rows to make some predictable data
  # otherwise, the number of rows of the result of prediction becomes 0
  test_data <- dplyr::bind_rows(test_data, test_data)

  model_data <- build_lm.fast(test_data, `CANCELLED X`, `Carrier Name`, CARRIER, DISTANCE, model_type = "glm", smote=FALSE, with_marginal_effects=TRUE, with_marginal_effects_confint=TRUE)
  ret <- model_data %>% broom::glance(model, pretty.name=TRUE)
  expect_equal(ret$`Data Size for Y`, 4) # This ends up to be 4 after doubling
  expect_equal(ret$`Data Size for N`, 30) # This ends up to be 30 after doubling and removing NA rows.
  ret <- model_data %>% broom::tidy(model)
  ret <- model_data %>% broom::augment(model)

  expect_true(nrow(ret) > 0)
})

test_that("binary prediction with factor target column", {
  test_data <- structure(
    list(
      `CANCELLED X` = factor(c("N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "Y", "N", "Y", "N"), levels=c("A","N","Y","B")),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = factor(c(NA, "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL")), # test with factor with NA
      # testing filtering of Inf, -Inf, NA here.
      DISTANCE = c(Inf, -Inf, NA, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED X", "Carrier Name", "CARRIER", "DISTANCE"))

  # duplicate rows to make some predictable data
  # otherwise, the number of rows of the result of prediction becomes 0
  test_data <- dplyr::bind_rows(test_data, test_data)

  model_data <- build_lm.fast(test_data, `CANCELLED X`, `Carrier Name`, CARRIER, DISTANCE, model_type = "glm", smote=FALSE, with_marginal_effects=TRUE, with_marginal_effects_confint=FALSE)
  ret <- model_data %>% broom::glance(model, pretty.name=TRUE)
  expect_equal(ret$`Data Size for Y`, 4) # This ends up to be 4 after doubling
  expect_equal(ret$`Data Size for N`, 30) # This ends up to be 30 after doubling and removing NA rows.
  ret <- model_data %>% broom::tidy(model)
  ret <- model_data %>% broom::augment(model)

  expect_true(nrow(ret) > 0)
})

test_that("binary prediction with variable_metric argument", {
  test_data <- structure(
    list(
      `CANCELLED X` = factor(c("N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "Y", "N", "Y", "N"), levels=c("A","N","Y","B")),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = factor(c(NA, "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL")), # test with factor with NA
      # testing filtering of Inf, -Inf, NA here.
      DISTANCE = c(Inf, -Inf, NA, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED X", "Carrier Name", "CARRIER", "DISTANCE"))

  # duplicate rows to make some predictable data
  # otherwise, the number of rows of the result of prediction becomes 0
  test_data <- dplyr::bind_rows(test_data, test_data)

  model_data <- build_lm.fast(test_data, `CANCELLED X`, `Carrier Name`, CARRIER, DISTANCE, model_type = "glm", smote=FALSE, variable_metric="odds_ratio")
  ret <- model_data %>% broom::tidy(model, variable_metric="odds_ratio")

  model_data <- build_lm.fast(test_data, `CANCELLED X`, `Carrier Name`, CARRIER, DISTANCE, model_type = "glm", smote=FALSE, variable_metric="coefficient")
  ret <- model_data %>% broom::tidy(model, variable_metric="coefficient")

  model_data <- build_lm.fast(test_data, `CANCELLED X`, `Carrier Name`, CARRIER, DISTANCE, model_type = "glm", smote=FALSE, variable_metric="ame")
  ret <- model_data %>% broom::tidy(model, variable_metric="ame")
  expect_true(c("ame") %in% colnames(ret))

  expect_true(nrow(ret) > 0)
})

test_data <- structure(
    list(
      `CANCELLED X` = c("N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "Y", "N", "Y", "N"),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = factor(c("AA", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL")), # test with factor with NA
      # testing filtering of Inf, -Inf, NA here.
      DISTANCE = c(10, 12, 12, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545),
      ARR_TIME = c(10, 32, 321, 342, 123, 98, 10, 21, 80, 211, 121, 87, 821, 213, 213, 923, 121, 76, 34, 50),
      DERAY_TIME = c(12, 42, 321, 31, 3, 43, 342, 764, 123, 43, 50, 12, 876, 12, 34, 45, 84, 25, 87, 352, 10)
      ), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED X", "Carrier Name", "CARRIER", "DISTANCE", "ARR_TIME", "DERAY_TIME"))

test_data$klass <- c(rep("A", 10), rep("B", 10))

test_that("Linear Regression with test rate", {
  ret <- test_data %>% build_lm.fast(`DISTANCE`,
                                     `ARR_TIME`,
                                     `DERAY_TIME`,
                                     `Carrier Name`,
                                     model_type = "lm",
                                     test_rate = 0.2)
  expect_equal(colnames(ret), c("model", ".test_index", "source.data"))
  test_rownum <- length(ret$.test_index[[1]])
  training_rownum <- nrow(test_data) - test_rownum

  suppressWarnings({
    pred_training <- prediction(ret, data = "training")
    pred_test <- prediction(ret, data = "test")
    expect_equal(training_rownum, nrow(pred_training))
    expect_equal(test_rownum, nrow(pred_test))

    expected_cols <- c("Carrier.Name", "DISTANCE", "ARR_TIME", "DERAY_TIME",
                       "predicted_value", "standard_error", "conf_low", "conf_high", "residuals", "hat",
                       "residual_standard_deviation", "cooks_distance", "standardised_residuals")
    expect_equal(colnames(pred_training), expected_cols)
    expected_cols <- c("Carrier.Name", "DISTANCE", "ARR_TIME", "DERAY_TIME", "predicted_value", "standard_error", "conf_low", "conf_high")
    expect_equal(colnames(pred_test), expected_cols)
   })
})

test_that("Group Linear Regression with test_rate", {
  group_data <- test_data %>% group_by(klass)
  ret <- group_data %>%
           build_lm.fast(`DISTANCE`,
                        `ARR_TIME`,
                        model_type = "lm",
                        test_rate = 0.2)
  expect_equal(colnames(ret), c("klass", "model", ".test_index", "source.data"))
  group_nrows <- group_data %>% summarize(n=n()) %>% `[[`("n")
  test_nrows <- sapply(ret$.test_index, length, simplify=TRUE)
  training_nrows <- group_nrows - test_nrows

  suppressWarnings({
    pred_training <- prediction(ret, data = "training")
    pred_test <- prediction(ret, data = "test")
    expect_equal(pred_training %>% summarize(n=n()) %>% `[[`("n"),
                 training_nrows)
    expect_equal(pred_test %>% summarize(n=n()) %>% `[[`("n"),
                 test_nrows)

    expected_cols <- c("klass", "DISTANCE", "ARR_TIME", "klass1", "predicted_value",
                       "standard_error", "conf_low", "conf_high", "residuals", "hat", "residual_standard_deviation",
                       "cooks_distance", "standardised_residuals")
    expect_equal(colnames(pred_training), expected_cols)

    expected_cols <- c("klass", "DISTANCE", "ARR_TIME", "klass1", "predicted_value",
                       "standard_error", "conf_low", "conf_high")
    expect_equal(colnames(pred_test), expected_cols)
   })
})

test_that("GLM - Normal Destribution with test_rate", {
  ret <- test_data %>% build_lm.fast(`DISTANCE`,
                                     `ARR_TIME`,
                                     `DERAY_TIME`,
                                     `Carrier Name`,
                                     model_type = "glm",
                                     family = "gaussian",
                                     test_rate = 0.2)
  expect_equal(colnames(ret), c("model", ".test_index", "source.data"))
  test_rownum <- length(ret$.test_index[[1]])
  training_rownum <- nrow(test_data) - test_rownum

  suppressWarnings({
    pred_training <- prediction(ret, data = "training")
    pred_test <- prediction(ret, data = "test")
    expect_equal(training_rownum, nrow(pred_training))
    expect_equal(test_rownum, nrow(pred_test))

    expected_cols <- c("Carrier.Name", "DISTANCE", "ARR_TIME", "DERAY_TIME",
                       "predicted_value", "standard_error", "conf_low", "conf_high", "residuals", "hat",
                       "residual_standard_deviation", "cooks_distance", "standardised_residuals", "predicted_response")
    expect_equal(colnames(pred_training), expected_cols)
    expected_cols <- c("Carrier.Name", "DISTANCE", "ARR_TIME", "DERAY_TIME", "predicted_value", "standard_error",
                       "conf_low", "conf_high", "predicted_response")
    expect_equal(colnames(pred_test), expected_cols)
   })
})

test_that("Group GLM - Normal Destribution with test_rate", {
  group_data <- test_data %>% group_by(klass)
  ret <- group_data %>%
           build_lm.fast(`DISTANCE`,
                        `ARR_TIME`,
                        model_type = "glm",
                        family = "gaussian",
                        test_rate = 0.2)
  expect_equal(colnames(ret), c("klass", "model", ".test_index", "source.data"))
  group_nrows <- group_data %>% summarize(n=n()) %>% `[[`("n")
  test_nrows <- sapply(ret$.test_index, length, simplify=TRUE)
  training_nrows <- group_nrows - test_nrows

  suppressWarnings({
    pred_training <- prediction(ret, data = "training")
    pred_test <- prediction(ret, data = "test")
    expect_equal(pred_training %>% summarize(n=n()) %>% `[[`("n"),
                 training_nrows)
    expect_equal(pred_test %>% summarize(n=n()) %>% `[[`("n"),
                 test_nrows)

    expected_cols <- c("klass", "DISTANCE", "ARR_TIME", "klass1", "predicted_value",
                       "standard_error", "conf_low", "conf_high", "residuals", "hat", "residual_standard_deviation",
                       "cooks_distance", "standardised_residuals", "predicted_response")
    expect_equal(colnames(pred_training), expected_cols)

    expected_cols <- c("klass", "DISTANCE", "ARR_TIME", "klass1", "predicted_value",
                       "standard_error", "conf_low", "conf_high", "predicted_response")
    expect_equal(colnames(pred_test), expected_cols)
   })
})

test_that("GLM - Gamma Destribution with test_rate", {
  ret <- test_data %>% build_lm.fast(`DISTANCE`,
                                     `ARR_TIME`,
                                     `DERAY_TIME`,
                                     `Carrier Name`,
                                     model_type = "glm",
                                     family = "Gamma",
                                     test_rate = 0.2)
  expect_equal(colnames(ret), c("model", ".test_index", "source.data"))
  test_rownum <- length(ret$.test_index[[1]])
  training_rownum <- nrow(test_data) - test_rownum

  suppressWarnings({
    pred_training <- prediction(ret, data = "training")
    pred_test <- prediction(ret, data = "test")
    expect_equal(training_rownum, nrow(pred_training))
    expect_equal(test_rownum, nrow(pred_test))

    expected_cols <- c("Carrier.Name", "DISTANCE", "ARR_TIME", "DERAY_TIME",
                       "predicted_value", "standard_error", "conf_low", "conf_high", "residuals", "hat",
                       "residual_standard_deviation", "cooks_distance", "standardised_residuals", "predicted_response")
    expect_equal(colnames(pred_training), expected_cols)
    expected_cols <- c("Carrier.Name", "DISTANCE", "ARR_TIME", "DERAY_TIME", "predicted_value", "standard_error",
                       "conf_low", "conf_high", "predicted_response")
    expect_equal(colnames(pred_test), expected_cols)
   })
})

test_that("Group GLM - Inverse Gaussian Destribution with test_rate", {
  group_data <- test_data %>% group_by(klass)
  ret <- group_data %>%
           build_lm.fast(`DISTANCE`,
                        `ARR_TIME`,
                        model_type = "glm",
                        family = "inverse.gaussian",
                        test_rate = 0.2)
  expect_equal(colnames(ret), c("klass", "model", ".test_index", "source.data"))
  group_nrows <- group_data %>% summarize(n=n()) %>% `[[`("n")
  test_nrows <- sapply(ret$.test_index, length, simplify=TRUE)
  training_nrows <- group_nrows - test_nrows

  suppressWarnings({
    pred_training <- prediction(ret, data = "training")
    pred_test <- prediction(ret, data = "test")
    expect_equal(pred_training %>% summarize(n=n()) %>% `[[`("n"),
                 training_nrows)
    expect_equal(pred_test %>% summarize(n=n()) %>% `[[`("n"),
                 test_nrows)

    expected_cols <- c("klass", "DISTANCE", "ARR_TIME", "klass1", "predicted_value",
                       "standard_error", "conf_low", "conf_high", "residuals", "hat", "residual_standard_deviation",
                       "cooks_distance", "standardised_residuals", "predicted_response")
    expect_equal(colnames(pred_training), expected_cols)

    expected_cols <- c("klass", "DISTANCE", "ARR_TIME", "klass1", "predicted_value",
                       "standard_error", "conf_low", "conf_high", "predicted_response")
    expect_equal(colnames(pred_test), expected_cols)
   })
})

test_that("Group GLM - Inverse Gaussian Destribution with test_rate", {
  group_data <- test_data %>% group_by(klass)
  ret <- group_data %>%
           build_lm.fast(`DISTANCE`,
                        `ARR_TIME`,
                        model_type = "glm",
                        family = "inverse.gaussian",
                        test_rate = 0.2)
  expect_equal(colnames(ret), c("klass", "model", ".test_index", "source.data"))
  group_nrows <- group_data %>% summarize(n=n()) %>% `[[`("n")
  test_nrows <- sapply(ret$.test_index, length, simplify=TRUE)
  training_nrows <- group_nrows - test_nrows

  suppressWarnings({
    pred_training <- prediction(ret, data = "training")
    pred_test <- prediction(ret, data = "test")
    expect_equal(pred_training %>% summarize(n=n()) %>% `[[`("n"),
                 training_nrows)
    expect_equal(pred_test %>% summarize(n=n()) %>% `[[`("n"),
                 test_nrows)

    expected_cols <- c("klass", "DISTANCE", "ARR_TIME", "klass1", "predicted_value",
                       "standard_error", "conf_low", "conf_high", "residuals", "hat", "residual_standard_deviation",
                       "cooks_distance", "standardised_residuals", "predicted_response")
    expect_equal(colnames(pred_training), expected_cols)

    expected_cols <- c("klass", "DISTANCE", "ARR_TIME", "klass1", "predicted_value",
                       "standard_error", "conf_low", "conf_high", "predicted_response")
    expect_equal(colnames(pred_test), expected_cols)
   })
})

test_that("Group GLM - Poisson Destribution with test_rate", {
  group_data <- test_data %>% group_by(klass)
  ret <- group_data %>%
           build_lm.fast(`DISTANCE`,
                        `ARR_TIME`,
                        model_type = "glm",
                        family = "poisson",
                        test_rate = 0.2)
  expect_equal(colnames(ret), c("klass", "model", ".test_index", "source.data"))
  group_nrows <- group_data %>% summarize(n=n()) %>% `[[`("n")
  test_nrows <- sapply(ret$.test_index, length, simplify=TRUE)
  training_nrows <- group_nrows - test_nrows

  suppressWarnings({
    pred_training <- prediction(ret, data = "training")
    pred_test <- prediction(ret, data = "test")
    expect_equal(pred_training %>% summarize(n=n()) %>% `[[`("n"),
                 training_nrows)
    expect_equal(pred_test %>% summarize(n=n()) %>% `[[`("n"),
                 test_nrows)

    expected_cols <- c("klass", "DISTANCE", "ARR_TIME", "klass1", "predicted_value",
                       "standard_error", "conf_low", "conf_high", "residuals", "hat", "residual_standard_deviation",
                       "cooks_distance", "standardised_residuals", "predicted_response")
    expect_equal(colnames(pred_training), expected_cols)

    expected_cols <- c("klass", "DISTANCE", "ARR_TIME", "klass1", "predicted_value",
                       "standard_error", "conf_low", "conf_high", "predicted_response")
    expect_equal(colnames(pred_test), expected_cols)
   })
})

test_that("Group GLM - Poisson Destribution with test_rate", {
  group_data <- test_data %>% group_by(klass)
  ret <- group_data %>%
           build_lm.fast(`DISTANCE`,
                        `ARR_TIME`,
                        model_type = "glm",
                        family = "poisson",
                        test_rate = 0.2)
  expect_equal(colnames(ret), c("klass", "model", ".test_index", "source.data"))
  group_nrows <- group_data %>% summarize(n=n()) %>% `[[`("n")
  test_nrows <- sapply(ret$.test_index, length, simplify=TRUE)
  training_nrows <- group_nrows - test_nrows

  suppressWarnings({
    pred_training <- prediction(ret, data = "training")
    pred_test <- prediction(ret, data = "test")
    expect_equal(pred_training %>% summarize(n=n()) %>% `[[`("n"),
                 training_nrows)
    expect_equal(pred_test %>% summarize(n=n()) %>% `[[`("n"),
                 test_nrows)

    expected_cols <- c("klass", "DISTANCE", "ARR_TIME", "klass1", "predicted_value",
                       "standard_error", "conf_low", "conf_high", "residuals", "hat", "residual_standard_deviation",
                       "cooks_distance", "standardised_residuals", "predicted_response")
    expect_equal(colnames(pred_training), expected_cols)

    expected_cols <- c("klass", "DISTANCE", "ARR_TIME", "klass1", "predicted_value",
                       "standard_error", "conf_low", "conf_high", "predicted_response")
    expect_equal(colnames(pred_test), expected_cols)
   })
})

test_that("Group GLM - Negative Binomial Destribution with test_rate", {
  group_data <- test_data %>% group_by(klass)
  ret <- group_data %>%
           build_lm.fast(`DISTANCE`,
                        `ARR_TIME`,
                        model_type = "glm",
                        family = "negativebinomial",
                        test_rate = 0.2)
  expect_equal(colnames(ret), c("klass", "model", ".test_index", "source.data"))
  group_nrows <- group_data %>% summarize(n=n()) %>% `[[`("n")
  test_nrows <- sapply(ret$.test_index, length, simplify=TRUE)
  training_nrows <- group_nrows - test_nrows

  suppressWarnings({
    pred_training <- prediction(ret, data = "training")
    pred_test <- prediction(ret, data = "test")
    expect_equal(pred_training %>% summarize(n=n()) %>% `[[`("n"),
                 training_nrows)
    expect_equal(pred_test %>% summarize(n=n()) %>% `[[`("n"),
                 test_nrows)

    expected_cols <- c("klass", "DISTANCE", "ARR_TIME", "klass1", "predicted_value",
                       "standard_error", "conf_low", "conf_high", "residuals", "hat", "residual_standard_deviation",
                       "cooks_distance", "standardised_residuals", "predicted_response")
    expect_equal(colnames(pred_training), expected_cols)

    expected_cols <- c("klass", "DISTANCE", "ARR_TIME", "klass1", "predicted_value",
                       "standard_error", "conf_low", "conf_high", "predicted_response")
    expect_equal(colnames(pred_test), expected_cols)
   })
})

test_that("Group GLM - Negative Binomial Destribution with test_rate", {
  group_data <- test_data %>% group_by(klass)
  ret <- group_data %>%
           build_lm.fast(`DISTANCE`,
                        `ARR_TIME`,
                        model_type = "glm",
                        family = "negativebinomial",
                        test_rate = 0.2)
  expect_equal(colnames(ret), c("klass", "model", ".test_index", "source.data"))
  group_nrows <- group_data %>% summarize(n=n()) %>% `[[`("n")
  test_nrows <- sapply(ret$.test_index, length, simplify=TRUE)
  training_nrows <- group_nrows - test_nrows

  suppressWarnings({
    pred_training <- prediction(ret, data = "training")
    pred_test <- prediction(ret, data = "test")
    expect_equal(pred_training %>% summarize(n=n()) %>% `[[`("n"),
                 training_nrows)
    expect_equal(pred_test %>% summarize(n=n()) %>% `[[`("n"),
                 test_nrows)

    expected_cols <- c("klass", "DISTANCE", "ARR_TIME", "klass1", "predicted_value",
                       "standard_error", "conf_low", "conf_high", "residuals", "hat", "residual_standard_deviation",
                       "cooks_distance", "standardised_residuals", "predicted_response")
    expect_equal(colnames(pred_training), expected_cols)

    expected_cols <- c("klass", "DISTANCE", "ARR_TIME", "klass1", "predicted_value",
                       "standard_error", "conf_low", "conf_high", "predicted_response")
    expect_equal(colnames(pred_test), expected_cols)
   })
})

