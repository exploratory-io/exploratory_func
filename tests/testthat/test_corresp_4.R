context("test correspondence analysis with aggregated LONG input (#28467)")

make_ca_raw_data_long <- function(n = 500, seed = 42) {
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

test_that("long, wide and raw inputs produce the same model", {
  raw <- make_ca_raw_data_long()
  long <- raw %>% dplyr::count(brand, age)
  wide <- long %>% tidyr::pivot_wider(names_from = age, values_from = n, values_fill = 0) %>%
    dplyr::arrange(brand)

  raw_m <- (raw %>% exp_mca(brand, age))$model[[1]]
  wide_m <- (wide %>% exp_mca_aggregated(brand, `10s`, `20s`, `30s`, `40s`,
                                         column_variable_name = "age"))$model[[1]]
  long_m <- (long %>% exp_mca_aggregated_long(brand, age, n))$model[[1]]

  expect_equal(long_m$analysis_type, "CA")
  expect_true(inherits(long_m, "ca_exploratory"))

  # Same contingency table on all three paths.
  expect_equal(unname(as.matrix(long_m$contingency_table)), unname(as.matrix(raw_m$contingency_table)))
  expect_equal(unname(as.matrix(long_m$contingency_table)), unname(as.matrix(wide_m$contingency_table)))
  expect_equal(rownames(long_m$contingency_table), rownames(raw_m$contingency_table))
  expect_equal(colnames(long_m$contingency_table), colnames(raw_m$contingency_table))

  expect_equal(long_m$eig, raw_m$eig, tolerance = 1e-6)
  expect_equal(long_m$row$coord, raw_m$row$coord, tolerance = 1e-6)
  expect_equal(long_m$col$coord, raw_m$col$coord, tolerance = 1e-6)
  expect_equal(long_m$n_used, raw_m$n_used)

  lp <- long_m$association$variable_pair_results
  rp <- raw_m$association$variable_pair_results
  expect_equal(lp$chi_square, rp$chi_square, tolerance = 1e-6)
  expect_equal(lp$df, rp$df)
  expect_equal(lp$p_value, rp$p_value, tolerance = 1e-6)
  expect_equal(lp$cramers_v, rp$cramers_v, tolerance = 1e-6)

  # The column variable name comes from the column-category COLUMN, not a text arg.
  expect_equal(long_m$column_variable_name, "age")
  expect_equal(long_m$row_variable_name, "brand")
  expect_equal(lp$pair_id, "brand × age")
})

test_that("long input produces every report tidy type identically to wide", {
  raw <- make_ca_raw_data_long()
  long <- raw %>% dplyr::count(brand, age)
  wide <- long %>% tidyr::pivot_wider(names_from = age, values_from = n, values_fill = 0) %>%
    dplyr::arrange(brand)

  long_df <- long %>% exp_mca_aggregated_long(brand, age, n)
  wide_df <- wide %>% exp_mca_aggregated(brand, `10s`, `20s`, `30s`, `40s`,
                                         column_variable_name = "age")

  for (ty in c("analysis_summary", "category_map", "pairwise_association", "residual_cells",
               "featured_combinations", "dimension_summary", "dimension_matrix",
               "category_details", "variance", "categories", "contrib")) {
    expect_error(long_df %>% tidy_rowwise(model, type = ty), NA, info = paste0("tidy failed: ", ty))
    expect_equal(
      as.data.frame(long_df %>% tidy_rowwise(model, type = ty)),
      as.data.frame(wide_df %>% tidy_rowwise(model, type = ty)),
      tolerance = 1e-6, info = paste0("long/wide differ for tidy type: ", ty)
    )
  }
})

test_that("long input sums duplicated row/column combinations", {
  dup <- data.frame(
    r = c("A", "A", "B", "C", "A"),
    c = c("x", "x", "y", "x", "y"),
    n = c(3, 7, 40, 30, 10),
    stringsAsFactors = FALSE
  )
  merged <- data.frame(
    r = c("A", "A", "B", "C"),
    c = c("x", "y", "y", "x"),
    n = c(10, 10, 40, 30),
    stringsAsFactors = FALSE
  )
  dup_m <- (dup %>% exp_mca_aggregated_long(r, c, n))$model[[1]]
  merged_m <- (merged %>% exp_mca_aggregated_long(r, c, n))$model[[1]]
  expect_equal(unname(as.matrix(dup_m$contingency_table)), unname(as.matrix(merged_m$contingency_table)))
  expect_equal(dup_m$n_used, 90)
})

test_that("long input treats NA counts as zero and honors factor level order", {
  df <- data.frame(
    r = factor(c("Z", "Y", "X", "Z", "Y", "X"), levels = c("Z", "Y", "X")),
    c = factor(c("q", "q", "q", "p", "p", "p"), levels = c("q", "p")),
    n = c(10, 20, 30, NA, 15, 25),
    stringsAsFactors = FALSE
  )
  m <- (df %>% exp_mca_aggregated_long(r, c, n))$model[[1]]
  expect_equal(rownames(m$contingency_table), c("Z", "Y", "X"))
  expect_equal(colnames(m$contingency_table), c("q", "p"))
  # The NA cell counted as 0, so Z/p is 0 and the total excludes it.
  expect_equal(m$n_used, 100)
})

test_that("long input works with Repeat By", {
  raw <- make_ca_raw_data_long()
  long <- raw %>% dplyr::count(brand, age)
  grouped <- dplyr::bind_rows(
    long %>% dplyr::mutate(region = "East"),
    long %>% dplyr::mutate(region = "West")
  ) %>% dplyr::group_by(region)

  model_df <- grouped %>% exp_mca_aggregated_long(brand, age, n)
  expect_equal(nrow(model_df), 2)
  expect_error(model_df %>% tidy_rowwise(model, type = "analysis_summary"), NA)
})

test_that("long input rejects invalid input with readable messages", {
  raw <- make_ca_raw_data_long()
  long <- raw %>% dplyr::count(brand, age)

  expect_error(long %>% exp_mca_aggregated_long(brand, brand, n),
               "must be different columns")

  non_numeric <- long %>% dplyr::mutate(n = as.character(n))
  expect_error(non_numeric %>% exp_mca_aggregated_long(brand, age, n),
               "The aggregated count column must be numeric.")

  negative <- long
  negative$n[[1]] <- -1L
  expect_error(negative %>% exp_mca_aggregated_long(brand, age, n),
               "must not be negative")

  fractional <- long %>% dplyr::mutate(n = n + 0.5)
  expect_error(fractional %>% exp_mca_aggregated_long(brand, age, n),
               "must be whole numbers")

  too_small <- data.frame(r = c("A", "B"), c = c("x", "x"), n = c(5, 6),
                          stringsAsFactors = FALSE)
  expect_error(too_small %>% exp_mca_aggregated_long(r, c, n),
               "There are not enough categories")
})
