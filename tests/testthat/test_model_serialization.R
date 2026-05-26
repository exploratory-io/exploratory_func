context("test model serialization round-trip")

# Regression tests that a built model survives a saveRDS/readRDS round-trip and is still
# usable (predict / variable importance). This simulates the project re-open path, where
# model data frames are persisted with saveRDS and restored with readRDS in a new session.
#
# xgboost is the model that actually regressed (#35943): xgboost 3.x stores the model
# behind a bare external pointer ($ptr) that does NOT survive saveRDS/readRDS, so after a
# re-open the booster's $ptr is blank and any consume point throws:
#   invalid 'xgb.Booster' (blank 'externalptr').
# The other models here are pure-R or retain their own serializable representation and
# round-trip fine; their tests are regression guards against a future change reintroducing
# the same bare-handle mistake.

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

# ---------------------------------------------------------------------------
# Regression guards for the other model types in the package. These all retain a
# serializable representation today and round-trip fine; the tests lock that in so
# a future change cannot reintroduce the xgboost-style bare-handle bug unnoticed.
# Each builds a model df (the object that is persisted), round-trips it, and runs
# the model's normal consume step (add_prediction / prediction / tidy) on the
# restored object.
# ---------------------------------------------------------------------------

test_that("lm model survives saveRDS/readRDS round-trip", {
  set.seed(1)
  d <- data.frame(y = rnorm(120), x1 = runif(120), x2 = runif(120))
  restored <- round_trip_rds(build_lm(d, formula = y ~ x1 + x2))
  expect_equal(nrow(d %>% add_prediction(model_df = restored)), nrow(d))
})

test_that("glm model survives saveRDS/readRDS round-trip", {
  set.seed(1)
  d <- data.frame(y = rnorm(120), x1 = runif(120), x2 = runif(120))
  restored <- round_trip_rds(build_glm(d, formula = y ~ x1 + x2))
  expect_equal(nrow(d %>% add_prediction(model_df = restored)), nrow(d))
})

test_that("multinom model survives saveRDS/readRDS round-trip", {
  set.seed(1)
  d <- data.frame(cls = factor(rep(c("a", "b", "c"), 40)),
                  x1 = runif(120) + rep(c(0, 1, 2), 40), x2 = runif(120))
  restored <- round_trip_rds(build_multinom(d, formula = cls ~ x1 + x2))
  expect_equal(nrow(d %>% add_prediction(model_df = restored)), nrow(d))
})

test_that("ranger model survives saveRDS/readRDS round-trip", {
  set.seed(1)
  d <- data.frame(y = rnorm(120), x1 = runif(120), x2 = runif(120))
  restored <- round_trip_rds(build_model(d, model_func = rangerReg, formula = y ~ x1 + x2))
  expect_equal(nrow(d %>% add_prediction(model_df = restored)), nrow(d))
})

test_that("randomForest model survives saveRDS/readRDS round-trip", {
  set.seed(1)
  d <- data.frame(y = rnorm(120), x1 = runif(120), x2 = runif(120))
  restored <- round_trip_rds(build_model(d, model_func = randomForestReg, formula = y ~ x1 + x2))
  expect_equal(nrow(d %>% add_prediction(model_df = restored)), nrow(d))
})

test_that("lightgbm model survives saveRDS/readRDS round-trip", {
  set.seed(1)
  d <- data.frame(y = rnorm(120), x1 = runif(120), x2 = runif(120))
  restored <- round_trip_rds(build_model(d, model_func = lightgbm_reg, formula = y ~ x1 + x2))
  expect_equal(nrow(d %>% add_prediction(model_df = restored)), nrow(d))
})

test_that("coxph model survives saveRDS/readRDS round-trip", {
  set.seed(1)
  d <- data.frame(time = rpois(120, 10) + 1, status = rbinom(120, 1, 0.6),
                  age = runif(120) * 50, sex = sample(c("M", "F"), 120, replace = TRUE))
  restored <- round_trip_rds(build_coxph(d, formula = survival::Surv(time, status) ~ age + sex))
  expect_equal(nrow(prediction(restored, data = "training")), nrow(d))
})

test_that("rpart model survives saveRDS/readRDS round-trip", {
  set.seed(1)
  d <- data.frame(y = rnorm(120), x1 = runif(120), x2 = runif(120))
  restored <- round_trip_rds(d %>% exp_rpart(y, x1, x2))
  expect_equal(nrow(prediction(restored)), nrow(d))
})

test_that("arima model survives saveRDS/readRDS round-trip", {
  set.seed(1)
  d <- data.frame(ds = seq(as.Date("2021-01-01"), by = "day", length.out = 120),
                  val = rnorm(120) + seq(120) / 40)
  restored <- round_trip_rds(d %>% exp_arima(ds, val, 10, time_unit = "day"))
  expect_gt(nrow(restored %>% tidy_rowwise(model)), 0)
})

test_that("prophet model survives saveRDS/readRDS round-trip", {
  set.seed(1)
  d <- data.frame(ds = seq(as.Date("2021-01-01"), by = "day", length.out = 120),
                  val = rnorm(120) + seq(120) / 40)
  restored <- round_trip_rds(d %>% do_prophet(ds, val, 10, time_unit = "day", output = "model"))
  expect_gt(nrow(restored %>% tidy_rowwise(model)), 0)
})
