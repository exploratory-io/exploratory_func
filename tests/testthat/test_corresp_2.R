context("test correspondence analysis report redesign (#37086)")

# Synthetic categorical data with a real association (brand skewed by age).
make_ca_data <- function(n = 500, seed = 42) {
  set.seed(seed)
  df <- data.frame(
    brand  = sample(c("A", "B", "C", "D"), n, replace = TRUE, prob = c(.4, .3, .2, .1)),
    age    = sample(c("10s", "20s", "30s", "40s"), n, replace = TRUE),
    gender = sample(c("M", "F"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  df$brand[df$age == "10s"] <- sample(c("A", "A", "B"), sum(df$age == "10s"), replace = TRUE)
  df
}

test_that("MCA branch: 3 variables produce mca_exploratory with all new tidy types", {
  m <- make_ca_data() %>% exp_mca(brand, age, gender, ncp = 5)
  model <- m$model[[1]]
  expect_equal(model$analysis_type, "MCA")
  expect_true(inherits(model, "mca_exploratory"))
  expect_false(inherits(model, "ca_exploratory"))

  expect_equal(colnames(m %>% tidy_rowwise(model, type = "analysis_summary")),
               c("Item", "Value", "status"))
  expect_equal(colnames(m %>% tidy_rowwise(model, type = "category_map")),
               c("Variable", "Category", "Dimension 1", "Dimension 2", "Count", "Share",
                 "Contribution 1", "Contribution 2", "Cos2 1", "Cos2 2"))
  pa <- m %>% tidy_rowwise(model, type = "pairwise_association")
  expect_equal(nrow(pa), 3)  # C(3,2) pairs
  expect_true(all(c("pair_id", "chi_square", "df", "p_value", "adjusted_p_value",
                    "cramers_v", "association_strength", "judgement") %in% colnames(pa)))
  rc <- m %>% tidy_rowwise(model, type = "residual_cells")
  expect_true(all(c("row_category", "column_category", "adjusted_standardized_residual",
                    "final_judgement", "heatmap_fill_value") %in% colnames(rc)))
  expect_true(nrow(m %>% tidy_rowwise(model, type = "dimension_summary")) >= 1)
  expect_true(all(c("variable", "category", "dimension", "coordinate", "contribution_pct", "cos2")
                  %in% colnames(m %>% tidy_rowwise(model, type = "dimension_matrix"))))
  expect_true(all(c("variable", "category", "side", "coordinate", "cos2_quality", "default_display")
                  %in% colnames(m %>% tidy_rowwise(model, type = "category_details"))))
})

test_that("CA branch: 2 variables produce ca_exploratory (still inherits mca_exploratory)", {
  m <- make_ca_data() %>% exp_mca(brand, age, ncp = 5)
  model <- m$model[[1]]
  expect_equal(model$analysis_type, "CA")
  expect_true(inherits(model, "ca_exploratory"))
  expect_true(inherits(model, "mca_exploratory"))
  pa <- m %>% tidy_rowwise(model, type = "pairwise_association")
  expect_equal(nrow(pa), 1)  # single pair
  # every report type runs without error
  for (ty in c("analysis_summary", "category_map", "residual_cells", "featured_combinations",
               "dimension_summary", "dimension_matrix", "category_details")) {
    expect_error(m %>% tidy_rowwise(model, type = ty), NA)
  }
})

test_that("chi-square statistic matches base chisq.test", {
  df <- make_ca_data()
  m <- df %>% exp_mca(brand, age, gender, ncp = 5)
  pa <- m %>% tidy_rowwise(m$model[[1]], type = "pairwise_association")
  ct <- table(df$brand, df$age)
  direct <- suppressWarnings(chisq.test(ct, correct = FALSE))
  row <- pa[pa$pair_id == "brand × age", ]
  expect_equal(row$chi_square, as.numeric(direct$statistic), tolerance = 1e-6)
  expect_equal(row$df, as.numeric(direct$parameter))
  # adjusted p-values are never below raw p-values (Holm is conservative)
  expect_true(all(pa$adjusted_p_value >= pa$p_value - 1e-9, na.rm = TRUE))
})

test_that("single-dimension CA (2-category variable) does not error", {
  set.seed(7); n <- 300
  df <- data.frame(
    gender = sample(c("M", "F"), n, replace = TRUE),
    age    = sample(c("10s", "20s", "30s"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  m <- df %>% exp_mca(gender, age, ncp = 5)
  model <- m$model[[1]]
  expect_equal(model$analysis_type, "CA")
  cm <- m %>% tidy_rowwise(model, type = "category_map")
  expect_true(all(c("Dimension 1", "Dimension 2") %in% colnames(cm)))
  for (ty in c("categories", "contrib", "data", "variables", "quanti_sup")) {
    expect_error(m %>% tidy_rowwise(model, type = ty), NA)
  }
})

test_that("multibyte and colon-containing category values survive intact", {
  set.seed(3); n <- 300
  stress <- '航空 会社 !"#$%&\'()*+, -./;<=>?@[]^_`{|}~ 表'
  df <- tibble::tibble(
    !!stress := sample(c("時間:午前", "時間:午後", "夜"), n, replace = TRUE),
    gender = sample(c("M", "F"), n, replace = TRUE),
    age = sample(c("10s", "20s", "30s"), n, replace = TRUE)
  )
  m <- df %>% exp_mca(!!rlang::sym(stress), gender, age, ncp = 5)
  cm <- m %>% tidy_rowwise(m$model[[1]], type = "category_map")
  sub <- cm[cm$Variable == stress, ]
  expect_true("時間:午前" %in% sub$Category)  # colon preserved, not split
})

test_that("legacy tidy types remain byte-compatible (MCA)", {
  m <- mtcars %>% exp_mca(vs, am, gear, carb, ncp = 5,
                          quanti_sups = c("mpg", "disp", "hp", "drat", "wt", "qsec"))
  expect_equal(colnames(m %>% tidy_rowwise(model, type = "categories")),
               c("variable", "category", "Dimension 1", "Dimension 2"))
  cat_val <- m %>% tidy_rowwise(model, type = "categories")
  expect_equal(cat_val[[1, 3]], -0.6075913, tolerance = 0.001)
  expect_equal(colnames(m %>% tidy_rowwise(model, type = "contrib")),
               c("Category", "Dimension", "Value"))
  expect_equal(colnames(m %>% tidy_rowwise(model, type = "variables")),
               c("variable", "Dimension 1", "Dimension 2"))
})

test_that("Repeat By (grouped) analysis runs per group", {
  df <- make_ca_data()
  df$segment <- sample(c("X", "Y"), nrow(df), replace = TRUE)
  m <- df %>% dplyr::group_by(segment) %>% exp_mca(brand, age, gender, ncp = 5)
  expect_equal(nrow(m), 2)  # one model per segment
})
