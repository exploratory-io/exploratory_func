# how to run this test:
# devtools::test(filter="build_polr_analysis_conditions")

context("build_polr analysis_conditions table (tam#38536 rework)")

# Mirrors make_ordinal_test_df() in test_build_polr.R (kept local here since a test file
# cannot rely on another test file's top-level helper being sourced first).
make_conditions_polr_df <- function(n = 90, seed = 1) {
  set.seed(seed)
  age <- round(stats::runif(n, 20, 60))
  score <- 0.08 * age + stats::rnorm(n)
  satisfaction <- cut(
    score,
    breaks = stats::quantile(score, probs = c(0, 1 / 3, 2 / 3, 1)),
    labels = c("Low", "Medium", "High"),
    include.lowest = TRUE
  )
  data.frame(
    `満足度` = factor(as.character(satisfaction), levels = c("Low", "Medium", "High"), ordered = TRUE),
    `年齢` = age,
    `部署 名!#` = rep(c("Sales", "Support", "Engineering"), length.out = n),
    region = rep(c("East", "West"), length.out = n),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

get_value <- function(res, metric) {
  res$Value[res$Metric == metric]
}

test_that("Target Variable/Categories/Category Order/#ExplVars/Explanatory Variables/Number of Rows/Rows Excluded/Evaluation, in issue order", {
  df <- make_conditions_polr_df(n = 90)
  trial <- df %>% build_polr(`満足度`, `年齢`, `部署 名!#`, test_rate = 0.3, seed = 42)
  model <- trial$model[[1]]

  res <- tidy(model, type = "analysis_conditions", test_mode = TRUE, test_rate = 0.3)
  expect_equal(res$Metric, c("Target Variable", "Number of Categories", "Category Order",
                            "# of Explanatory Variables", "Explanatory Variables",
                            "Number of Rows", "Rows Excluded", "Evaluation"))
  expect_equal(get_value(res, "Target Variable"), "満足度")
  expect_equal(get_value(res, "Number of Categories"), "3")
  expect_equal(get_value(res, "Category Order"), "Low < Medium < High")
  expect_equal(get_value(res, "# of Explanatory Variables"), "2")
  expect_equal(get_value(res, "Explanatory Variables"), "年齢, 部署 名!#")
  expect_equal(get_value(res, "Number of Rows"), as.character(nrow(model$model)))
  expect_equal(get_value(res, "Rows Excluded"), "0")
  expect_equal(get_value(res, "Evaluation"), "Test (30%)")
})

test_that("Evaluation is None when test_mode is off, even with a stale test_rate", {
  df <- make_conditions_polr_df(n = 60)
  trial <- df %>% build_polr(`満足度`, `年齢`, `部署 名!#`)
  model <- trial$model[[1]]

  res <- tidy(model, type = "analysis_conditions", test_mode = FALSE, test_rate = 0.3)
  expect_equal(get_value(res, "Evaluation"), "None")
})

test_that("default tidy_rowwise() call (no test_mode/test_rate) still returns Evaluation = None", {
  df <- make_conditions_polr_df(n = 60)
  trial <- df %>% build_polr(`満足度`, `年齢`, `部署 名!#`)

  res <- tidy_rowwise(trial, model, type = "analysis_conditions")
  expect_equal(get_value(res, "Evaluation"), "None")
})

test_that("Repeat By: each group's model gets its own Number of Rows and Category Order", {
  df <- make_conditions_polr_df(n = 120)
  trial <- df %>% build_polr(`満足度`, `年齢`, group_cols = "region")

  res <- trial %>% tidy_rowwise(model, type = "analysis_conditions")
  by_group <- split(res, res$region)
  expect_setequal(names(by_group), c("East", "West"))
  for (grp in names(by_group)) {
    g <- by_group[[grp]]
    expect_equal(get_value(g, "Category Order"), "Low < Medium < High")
    expect_true(as.numeric(get_value(g, "Number of Rows")) > 0)
  }
})

test_that("Rows Excluded counts NA rows independently for each Repeat By group", {
  df <- make_conditions_polr_df(n = 120)
  df$`年齢`[1:2] <- NA
  df$`満足度`[3] <- NA
  trial <- df %>% build_polr(`満足度`, `年齢`, group_cols = "region")

  res <- trial %>% tidy_rowwise(model, type = "analysis_conditions")
  excluded <- res %>%
    dplyr::filter(Metric == "Rows Excluded") %>%
    dplyr::arrange(region)

  expect_equal(excluded$region, c("East", "West"))
  expect_equal(excluded$Value, c("2", "1"))
  expect_false(any(grepl(".excluded_nrow", names(trial$source.data[[1]]), fixed = TRUE)))
})

test_that("complex column names round-trip", {
  df <- make_conditions_polr_df(n = 90)
  target_name <- "航空 会社 !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表"
  pred_name <- "予測 変数 !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 列"
  df <- df %>% dplyr::rename(!!target_name := `満足度`, !!pred_name := `年齢`)
  trial <- df %>% build_polr(!!rlang::sym(target_name), !!rlang::sym(pred_name), `部署 名!#`)
  model <- trial$model[[1]]

  res <- tidy(model, type = "analysis_conditions")
  expect_equal(get_value(res, "Target Variable"), target_name)
  expect_true(grepl(pred_name, get_value(res, "Explanatory Variables"), fixed = TRUE))
  expect_equal(get_value(res, "# of Explanatory Variables"), "2")
  expect_equal(get_value(res, "Category Order"), "Low < Medium < High")
})

test_that("a single predictor still returns # of Explanatory Variables = 1 and a well-formed Explanatory Variables value", {
  df <- make_conditions_polr_df(n = 60)
  trial <- df %>% build_polr(`満足度`, `年齢`)
  model <- trial$model[[1]]

  res <- tidy(model, type = "analysis_conditions")
  expect_equal(get_value(res, "# of Explanatory Variables"), "1")
  expect_equal(get_value(res, "Explanatory Variables"), "年齢")
})
