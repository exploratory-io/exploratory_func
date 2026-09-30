# how to run this test:
# devtools::test(filter="build_lm_analysis_conditions")

context("lm/glm analysis_conditions table (tam#38536)")

# 100 rows: 6 have NA target, 4 more have NA in the numeric predictor, 2 more have Inf target.
# The character predictor's NA rows must NOT count, since they become a "(Missing)" level.
make_conditions_df <- function() {
  set.seed(1)
  n <- 100
  df <- tibble::tibble(
    y = rnorm(n, 10),
    x1 = rnorm(n),
    x2 = sample(c("a", "b", "c"), n, replace = TRUE),
    flag = rep(c(TRUE, FALSE), n / 2),
    grp = rep(c("g1", "g2"), each = n / 2)
  )
  df$y[1:6] <- NA          # 6 NA target (4 in g1 + ... see grouped test below)
  df$x1[7:10] <- NA        # 4 NA numeric predictor
  df$y[11:12] <- Inf       # 2 Inf target
  df$x2[13:15] <- NA       # categorical NA -> "(Missing)", kept
  df$flag[1:6] <- NA       # same rows as NA target, keeps logistic's NA target in step with lm's
  df
}

get_value <- function(res, metric) {
  res$Value[res$Metric == metric]
}

test_that("lm: Rows Removed counts NA/Inf rows, sits right after Row Count, and ignores the test split", {
  df <- make_conditions_df()
  model_df <- df %>% build_lm.fast(y, x1, x2, test_rate = 0.3)
  model <- model_df$model[[1]]
  expect_equal(model$excluded_nrow, 12)
  res <- tidy(model, type = "analysis_conditions", test_mode = TRUE, test_rate = 0.3)
  expect_equal(res$Metric, c("Target Variable", "Explanatory Variables", "Row Count", "Rows Removed", "Validation Data"))
  expect_equal(get_value(res, "Rows Removed"), "12")
  expect_equal(get_value(res, "Validation Data"), "Test (30%)")
  # Row Count is the training split; test rows are validation data, not removed rows.
  expect_equal(get_value(res, "Row Count"), as.character(nrow(model$model)))
  expect_true(nrow(model$model) < 100 - 12)
})

test_that("glm binomial: Rows Removed in issue order with Number of Categories and test mode", {
  df <- make_conditions_df()
  model_df <- df %>% build_lm.fast(flag, x1, x2, model_type = "glm", family = "binomial", test_rate = 0.2)
  model <- model_df$model[[1]]
  # Only the NA target (6) and NA numeric predictor (4) rows; flag has no Inf.
  expect_equal(model$excluded_nrow, 10)
  res <- tidy(model, type = "analysis_conditions", test_mode = TRUE, test_rate = 0.2)
  expect_equal(res$Metric, c("Target Variable", "Number of Categories", "Explanatory Variables",
                             "Row Count", "Rows Removed", "Validation Data"))
  expect_equal(get_value(res, "Rows Removed"), "10")
  expect_equal(get_value(res, "Validation Data"), "Test (20%)")
})

test_that("other glm families (poisson, gaussian, negativebinomial) carry Rows Removed too", {
  df <- make_conditions_df() %>% dplyr::mutate(cnt = abs(round(y)))
  for (fam in c("poisson", "gaussian", "negativebinomial")) {
    model_df <- df %>% build_lm.fast(cnt, x1, x2, model_type = "glm", family = fam)
    model <- model_df$model[[1]]
    expect_false("error" %in% class(model), info = fam)
    expect_equal(model$excluded_nrow, 12, info = fam)
    res <- tidy(model, type = "analysis_conditions")
    expect_equal(get_value(res, "Rows Removed"), "12", info = fam)
    expect_equal(get_value(res, "Validation Data"), "None", info = fam)
  }
})

test_that("Rows Removed shows 0 (not hidden) when nothing is dropped", {
  df <- make_conditions_df() %>% dplyr::filter(!is.na(y), is.finite(y), !is.na(x1))
  model_df <- df %>% build_lm.fast(y, x1, x2)
  res <- tidy(model_df$model[[1]], type = "analysis_conditions")
  expect_equal(get_value(res, "Rows Removed"), "0")
})

test_that("down-sampling (max_nrow) is not counted as removed", {
  df <- make_conditions_df()
  model_df <- df %>% build_lm.fast(y, x1, x2, max_nrow = 50)
  model <- model_df$model[[1]]
  expect_equal(model$sampled_nrow, 50)
  expect_equal(model$excluded_nrow, 12)
})

test_that("outlier-filtered rows are counted as removed", {
  df <- make_conditions_df()
  df$y[20] <- 1e6
  model_df <- df %>% build_lm.fast(y, x1, x2, target_outlier_filter_type = "percentile", target_outlier_filter_threshold = 0.99)
  model <- model_df$model[[1]]
  expect_true(model$excluded_nrow > 12)
  expect_equal(model$excluded_nrow, 12 + (100 - 12 - nrow(model$model)))
})

test_that("Repeat By: each group's model gets its own Rows Removed", {
  df <- make_conditions_df()
  # NA/Inf rows 1:12 are all in g1 (rows 1..50).
  df$y[60] <- NA # 1 removed row in g2
  model_df <- df %>% dplyr::group_by(grp) %>% build_lm.fast(y, x1, x2, test_rate = 0.3)
  res <- model_df %>% tidy_rowwise(model, type = "analysis_conditions", test_mode = TRUE, test_rate = 0.3)
  removed <- res %>% dplyr::filter(Metric == "Rows Removed") %>% dplyr::arrange(grp)
  expect_equal(removed$grp, c("g1", "g2"))
  expect_equal(removed$Value, c("12", "1"))
})

test_that("complex column names round-trip and Rows Removed is still counted", {
  df <- make_conditions_df()
  target_name <- "航空 会社 !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表"
  pred_name <- "予測 変数 !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 列"
  df <- df %>% dplyr::rename(!!target_name := y, !!pred_name := x1)
  model_df <- df %>% build_lm.fast(!!rlang::sym(target_name), !!rlang::sym(pred_name), x2, test_rate = 0.3)
  model <- model_df$model[[1]]
  expect_false("error" %in% class(model))
  res <- tidy(model, type = "analysis_conditions", test_mode = TRUE, test_rate = 0.3)
  expect_equal(get_value(res, "Target Variable"), target_name)
  expect_true(grepl(pred_name, get_value(res, "Explanatory Variables"), fixed = TRUE))
  expect_equal(get_value(res, "Rows Removed"), "12")
})

test_that("models saved before excluded_nrow existed show N/A instead of a made-up 0", {
  df <- make_conditions_df()
  model <- (df %>% build_lm.fast(y, x1, x2))$model[[1]]
  model$excluded_nrow <- NULL
  res <- tidy(model, type = "analysis_conditions")
  expect_equal(get_value(res, "Rows Removed"), "N/A")
  expect_equal(res$Metric[4], "Rows Removed")
})
