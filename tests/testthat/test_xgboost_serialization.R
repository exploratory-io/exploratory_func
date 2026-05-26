context("test xgboost model serialization round-trip")

# Regression tests: xgboost 3.x stores the model behind a bare external
# pointer ($ptr) that does NOT survive saveRDS/readRDS. Exploratory persists model
# data frames with saveRDS (_tam_saveRObjectOnSession) and restores them with readRDS
# on project re-open, so after a re-open the booster's $ptr is blank and any consume
# point (predict, variable importance) throws:
#   invalid 'xgb.Booster' (blank 'externalptr').
# These tests round-trip a freshly built model through saveRDS/readRDS to simulate the
# project re-open, then assert the model is still usable.

# Round-trip a model object through saveRDS/readRDS, simulating the project re-open path.
round_trip_rds <- function(obj) {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  saveRDS(obj, tmp)
  readRDS(tmp)
}

test_that("xgboost_reg prediction survives saveRDS/readRDS round-trip", {
  set.seed(1)
  train_data <- data.frame(y = rnorm(100), x1 = runif(100), x2 = runif(100))
  model <- xgboost_reg(train_data, formula = y ~ x1 + x2, nrounds = 5)

  restored <- round_trip_rds(model)
  predicted <- predict_xgboost(restored, train_data)

  expect_true(is.numeric(predicted))
  expect_equal(length(predicted), nrow(train_data))
})

test_that("xgboost_binary prediction survives saveRDS/readRDS round-trip", {
  set.seed(1)
  train_data <- data.frame(
    label = rep(c(TRUE, FALSE, FALSE), 100),
    num1 = runif(300),
    num2 = runif(300)
  )
  model <- xgboost_binary(train_data, formula = label ~ num1 + num2, nrounds = 5)

  restored <- round_trip_rds(model)
  predicted <- predict_xgboost(restored, train_data)

  expect_true(is.numeric(predicted))
  expect_equal(length(predicted), nrow(train_data))
})

test_that("xgboost_multi prediction survives saveRDS/readRDS round-trip", {
  set.seed(1)
  train_data <- data.frame(
    label = rep(c(5, 10, 15), 100),
    num1 = rep(seq(3), 100) + runif(300),
    num2 = rep(seq(3), 100) + runif(300)
  )
  model <- xgboost_multi(train_data, formula = label ~ num1 + num2, nrounds = 5)

  restored <- round_trip_rds(model)
  predicted <- predict_xgboost(restored, train_data)

  # multi:softprob returns a probability matrix (one row per observation).
  expect_equal(nrow(predicted), nrow(train_data))
})

test_that("xgboost variable importance survives saveRDS/readRDS round-trip", {
  set.seed(1)
  train_data <- data.frame(y = rnorm(100), x1 = runif(100), x2 = runif(100))
  model <- xgboost_reg(train_data, formula = y ~ x1 + x2, nrounds = 5)

  restored <- round_trip_rds(model)
  imp <- tidy.xgb.Booster(restored, type = "weight")

  # On a blank booster, tidy.xgb.Booster swallows the error into an "Error" column.
  expect_false("Error" %in% colnames(imp))
  expect_true("feature" %in% colnames(imp))
  expect_true(nrow(imp) > 0)
})

test_that("xgboost predictions are numerically identical before and after round-trip", {
  set.seed(1)
  train_data <- data.frame(y = rnorm(100), x1 = runif(100), x2 = runif(100))
  model <- xgboost_reg(train_data, formula = y ~ x1 + x2, nrounds = 5)

  before <- predict_xgboost(model, train_data)
  restored <- round_trip_rds(model)
  after <- predict_xgboost(restored, train_data)

  expect_equal(after, before)
})

if (Sys.info()["sysname"] != "Windows") {
  test_that("xgboost prediction with multibyte column names survives round-trip", {
    set.seed(1)
    train_data <- data.frame(y = rnorm(100), age = runif(100), sex = runif(100))
    colnames(train_data) <- c("結果", "年齢", "性別")
    model <- xgboost_reg(train_data, formula = `結果` ~ `年齢` + `性別`, nrounds = 5)

    restored <- round_trip_rds(model)
    predicted <- predict_xgboost(restored, train_data)

    expect_true(is.numeric(predicted))
    expect_equal(length(predicted), nrow(train_data))
  })
}

test_that("legacy xgboost model without $raw fails gracefully without retry loop", {
  set.seed(1)
  train_data <- data.frame(y = rnorm(100), x1 = runif(100), x2 = runif(100))
  model <- xgboost_reg(train_data, formula = y ~ x1 + x2, nrounds = 5)
  # Simulate a model built before this fix: no serializable raw bytes to restore from.
  model$raw <- NULL

  restored <- round_trip_rds(model)
  # No $raw means nothing to restore; predict should surface the original blank-pointer
  # error directly (no infinite retry, no different error).
  expect_error(predict_xgboost(restored, train_data), "externalptr")
})
