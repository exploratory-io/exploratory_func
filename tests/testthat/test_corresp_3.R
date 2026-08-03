context("test correspondence analysis with aggregated (cross tab) input (#28467)")

# Raw row-level fixture. Deliberately asymmetric so brand and age are associated.
# Both variables are factors so the category order is the declared level order on
# BOTH paths (ca_get_category_levels() falls back to first-appearance order for
# character columns, which the aggregated cross tab cannot reproduce).
make_ca_raw_data <- function(n = 500, seed = 42) {
  set.seed(seed)
  df <- data.frame(
    brand = sample(c("A", "B", "C", "D"), n, replace = TRUE, prob = c(.4, .3, .2, .1)),
    age = sample(c("10s", "20s", "30s", "40s"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  df$brand[df$age == "10s"] <- sample(c("A", "A", "B"), sum(df$age == "10s"), replace = TRUE)
  df$brand <- factor(df$brand, levels = c("A", "B", "C", "D"))
  df$age <- factor(df$age, levels = c("10s", "20s", "30s", "40s"))
  df
}

# The same data as the wide cross tab the customer starts from:
# one row-category column plus one numeric count column per column category.
make_ca_wide_data <- function(raw = make_ca_raw_data()) {
  raw %>%
    dplyr::count(brand, age) %>%
    tidyr::pivot_wider(names_from = age, values_from = n, values_fill = 0) %>%
    dplyr::arrange(brand)
}

test_that("exp_mca_aggregated matches exp_mca on the same underlying data", {
  raw <- make_ca_raw_data()
  wide <- make_ca_wide_data(raw)

  raw_model <- (raw %>% exp_mca(brand, age))$model[[1]]
  agg_model <- (wide %>% exp_mca_aggregated(brand, `10s`, `20s`, `30s`, `40s`,
                                            column_variable_name = "age"))$model[[1]]

  expect_equal(agg_model$analysis_type, "CA")
  expect_true(inherits(agg_model, "ca_exploratory"))
  expect_true(inherits(agg_model, "mca_exploratory"))

  # Same contingency table, therefore the same decomposition.
  expect_equal(unname(as.matrix(agg_model$contingency_table)),
               unname(as.matrix(raw_model$contingency_table)))
  expect_equal(rownames(agg_model$contingency_table), rownames(raw_model$contingency_table))
  expect_equal(colnames(agg_model$contingency_table), colnames(raw_model$contingency_table))

  expect_equal(agg_model$eig, raw_model$eig, tolerance = 1e-6)
  expect_equal(agg_model$row$coord, raw_model$row$coord, tolerance = 1e-6)
  expect_equal(agg_model$col$coord, raw_model$col$coord, tolerance = 1e-6)

  # The analysis N of an aggregated table is the total count, not the row count.
  expect_equal(agg_model$n_used, raw_model$n_used)
  expect_equal(agg_model$association$settings$analysis_n,
               raw_model$association$settings$analysis_n)

  agg_pairs <- agg_model %>% (function(m) m$association$variable_pair_results)
  raw_pairs <- raw_model %>% (function(m) m$association$variable_pair_results)
  expect_equal(nrow(agg_pairs), 1)
  expect_equal(agg_pairs$chi_square, raw_pairs$chi_square, tolerance = 1e-6)
  expect_equal(agg_pairs$df, raw_pairs$df)
  expect_equal(agg_pairs$p_value, raw_pairs$p_value, tolerance = 1e-6)
  expect_equal(agg_pairs$cramers_v, raw_pairs$cramers_v, tolerance = 1e-6)
  expect_equal(agg_pairs$judgement, raw_pairs$judgement)
  expect_equal(agg_pairs$n, raw_pairs$n)

  # The report tables the Analytics View renders must come out identical too.
  raw_df <- raw %>% exp_mca(brand, age)
  agg_df <- wide %>% exp_mca_aggregated(brand, `10s`, `20s`, `30s`, `40s`,
                                        column_variable_name = "age")
  for (ty in c("category_map", "residual_cells", "dimension_summary", "category_details",
               "variance", "dimension_matrix")) {
    expect_equal(
      as.data.frame(agg_df %>% tidy_rowwise(model, type = ty)),
      as.data.frame(raw_df %>% tidy_rowwise(model, type = ty)),
      tolerance = 1e-6,
      info = paste0("tidy output differs for type: ", ty)
    )
  }
})

test_that("exp_mca_aggregated cross-checks against base R chisq.test", {
  wide <- make_ca_wide_data()
  model <- (wide %>% exp_mca_aggregated(brand, `10s`, `20s`, `30s`, `40s`,
                                        column_variable_name = "age"))$model[[1]]
  pairs <- model$association$variable_pair_results

  expected <- chisq.test(as.matrix(wide[, c("10s", "20s", "30s", "40s")]), correct = FALSE)
  expect_equal(pairs$chi_square, unname(expected$statistic), tolerance = 1e-6)
  expect_equal(pairs$df, unname(expected$parameter), tolerance = 1e-6)
})

test_that("exp_mca_aggregated produces every report tidy type", {
  wide <- make_ca_wide_data()
  model_df <- wide %>% exp_mca_aggregated(brand, `10s`, `20s`, `30s`, `40s`,
                                          column_variable_name = "age")

  for (ty in c("analysis_summary", "category_map", "pairwise_association", "residual_cells",
               "featured_combinations", "dimension_summary", "dimension_matrix",
               "category_details", "variance", "data", "categories", "contrib")) {
    expect_error(model_df %>% tidy_rowwise(model, type = ty), NA,
                 info = paste0("tidy type failed: ", ty))
  }

  # MCA-only types stay empty rather than erroring, as on the raw 2-variable path.
  expect_equal(nrow(model_df %>% tidy_rowwise(model, type = "variables")), 0)
  expect_equal(nrow(model_df %>% tidy_rowwise(model, type = "quanti_sup")), 0)

  # The category map must carry both the row and the column variable's categories.
  category_map <- model_df %>% tidy_rowwise(model, type = "category_map")
  expect_true(all(c("brand", "age") %in% unique(category_map$Variable)))

  # tidy(type="data") joins the row-category dimension scores onto the input frame.
  data_out <- model_df %>% tidy_rowwise(model, type = "data")
  expect_equal(nrow(data_out), nrow(wide))
  expect_true("brand" %in% colnames(data_out))
})

test_that("exp_mca_aggregated treats NA cells as zero counts", {
  wide <- make_ca_wide_data()
  with_na <- wide
  with_na$`40s`[[1]] <- NA_integer_

  zeroed <- wide
  zeroed$`40s`[[1]] <- 0L

  na_model <- (with_na %>% exp_mca_aggregated(brand, `10s`, `20s`, `30s`, `40s`))$model[[1]]
  zero_model <- (zeroed %>% exp_mca_aggregated(brand, `10s`, `20s`, `30s`, `40s`))$model[[1]]

  expect_equal(unname(as.matrix(na_model$contingency_table)),
               unname(as.matrix(zero_model$contingency_table)))
  expect_equal(na_model$n_used, zero_model$n_used)
})

test_that("exp_mca_aggregated sums rows that repeat a category label", {
  split_rows <- data.frame(
    brand = c("A", "A", "B", "C"),
    x = c(5, 5, 20, 30),
    y = c(1, 9, 40, 10),
    stringsAsFactors = FALSE
  )
  merged_rows <- data.frame(
    brand = c("A", "B", "C"),
    x = c(10, 20, 30),
    y = c(10, 40, 10),
    stringsAsFactors = FALSE
  )

  split_model <- (split_rows %>% exp_mca_aggregated(brand, x, y))$model[[1]]
  merged_model <- (merged_rows %>% exp_mca_aggregated(brand, x, y))$model[[1]]

  expect_equal(unname(as.matrix(split_model$contingency_table)),
               unname(as.matrix(merged_model$contingency_table)))
  expect_equal(split_model$n_used, 120)
})

test_that("exp_mca_aggregated drops all-zero rows and columns", {
  df <- data.frame(
    brand = c("A", "B", "C", "Empty"),
    x = c(10, 20, 30, 0),
    y = c(5, 25, 15, 0),
    z = c(0, 0, 0, 0),
    stringsAsFactors = FALSE
  )
  model <- (df %>% exp_mca_aggregated(brand, x, y, z))$model[[1]]

  expect_equal(rownames(model$contingency_table), c("A", "B", "C"))
  expect_equal(colnames(model$contingency_table), c("x", "y"))
})

test_that("exp_mca_aggregated clamps the number of dimensions to the table size", {
  df <- data.frame(
    brand = c("A", "B"),
    x = c(10, 20), y = c(5, 25), z = c(7, 3),
    stringsAsFactors = FALSE
  )
  # A 2 x 3 table supports only one dimension even though ncp defaults to 5.
  model <- (df %>% exp_mca_aggregated(brand, x, y, z, ncp = 5))$model[[1]]
  expect_equal(model$n_dims, 1)
  expect_error(model %>% (function(m) m$section5$dimension_summary), NA)
})

test_that("exp_mca_aggregated works with Repeat By", {
  wide <- make_ca_wide_data()
  grouped <- dplyr::bind_rows(
    wide %>% dplyr::mutate(region = "East"),
    wide %>% dplyr::mutate(region = "West")
  ) %>% dplyr::group_by(region)

  model_df <- grouped %>% exp_mca_aggregated(brand, `10s`, `20s`, `30s`, `40s`,
                                             column_variable_name = "age")
  expect_equal(nrow(model_df), 2)
  expect_equal(sort(model_df$region), c("East", "West"))
  expect_error(model_df %>% tidy_rowwise(model, type = "analysis_summary"), NA)
})

test_that("exp_mca_aggregated rejects invalid input with readable messages", {
  wide <- make_ca_wide_data()

  expect_error(wide %>% exp_mca_aggregated(brand, `10s`),
               "Select two or more columns that hold the aggregated counts.")

  expect_error(wide %>% exp_mca_aggregated(brand, `10s`, `20s`,
                                           column_variable_name = "brand"),
               "The column variable name must be different")

  non_numeric <- wide %>% dplyr::mutate(`20s` = as.character(`20s`))
  expect_error(non_numeric %>% exp_mca_aggregated(brand, `10s`, `20s`),
               "The aggregated count columns must be numeric")

  negative <- wide
  negative$`20s`[[1]] <- -1L
  expect_error(negative %>% exp_mca_aggregated(brand, `10s`, `20s`),
               "The aggregated counts must not be negative.")

  fractional <- wide %>% dplyr::mutate(`20s` = `20s` + 0.5)
  expect_error(fractional %>% exp_mca_aggregated(brand, `10s`, `20s`),
               "The aggregated counts must be whole numbers.")

  too_small <- data.frame(brand = c("A", "B"), x = c(1, 0), y = c(0, 0),
                          stringsAsFactors = FALSE)
  expect_error(too_small %>% exp_mca_aggregated(brand, x, y),
               "There are not enough categories")
})

test_that("the counts entry point reproduces the row-level pairwise association", {
  raw <- make_ca_raw_data()

  from_rows <- build_pairwise_association_results(
    data = raw, variables = c("brand", "age"),
    overall_adjust_method = "holm", cell_adjust_method = "holm", alpha = 0.05,
    missing_method = "listwise", simulation_count = 2000, seed = 123
  )
  contingency_table <- table(
    factor(raw$brand, levels = ca_get_category_levels(raw$brand)),
    factor(raw$age, levels = ca_get_category_levels(raw$age))
  )
  from_counts <- build_pairwise_association_results_from_counts(
    contingency_table = contingency_table,
    row_variable_name = "brand", column_variable_name = "age",
    overall_adjust_method = "holm", cell_adjust_method = "holm", alpha = 0.05,
    simulation_count = 2000, seed = 123
  )

  compared_columns <- c("pair_id", "n", "chi_square", "df", "p_value", "cramers_v",
                        "association_strength", "test_method", "judgement")
  expect_equal(
    as.data.frame(from_counts$variable_pair_results[, compared_columns]),
    as.data.frame(from_rows$variable_pair_results[, compared_columns]),
    tolerance = 1e-6
  )
  expect_equal(nrow(from_counts$residual_heatmap_data), nrow(from_rows$residual_heatmap_data))
  expect_equal(from_counts$residual_heatmap_data$adjusted_standardized_residual,
               from_rows$residual_heatmap_data$adjusted_standardized_residual,
               tolerance = 1e-6)
})
