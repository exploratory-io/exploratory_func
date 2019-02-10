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
