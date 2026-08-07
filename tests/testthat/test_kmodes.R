context("test K-Modes clustering (exp_kmodes)")

# A categorical fixture that exercises every supported input type: character
# (incl. multibyte), factor, ordered factor, logical, low-cardinality numeric
# (kept as categories) and wide numeric (binned), plus rows with missing values.
kmodes_test_df <- function(n = 240, with_na = TRUE) {
  set.seed(42)
  df <- tibble::tibble(
    `利用目的` = sample(c("業務", "研究", "学習", "その他"), n, TRUE, prob = c(.4, .25, .25, .1)),
    `契約タイプ` = factor(sample(c("Business", "Personal", "Academic"), n, TRUE)),
    `導入経路` = sample(c("紹介", "Web検索", "大学"), n, TRUE),
    flag = sample(c(TRUE, FALSE), n, TRUE),
    small_num = sample(1:5, n, TRUE),
    wide_num = stats::rnorm(n, 100, 30),
    ord = factor(sample(c("low", "mid", "high"), n, TRUE),
                 levels = c("low", "mid", "high"), ordered = TRUE)
  )
  if (with_na) {
    df$`利用目的`[c(3, 7, 11)] <- NA
  }
  df
}

test_that("exp_kmodes clusters mixed categorical input including multibyte names", {
  df <- kmodes_test_df()
  model_df <- exp_kmodes(df, `利用目的`, `契約タイプ`, `導入経路`, flag, small_num, wide_num, ord,
                         centers = 3, seed = 1, elbow_method_mode = "none",
                         silhouette_sample_size = 200, map_sample_size = 200)
  model <- model_df$model[[1]]
  expect_true("kmodes_exploratory" %in% class(model))
  expect_equal(model$centers, 3)
  expect_equal(model$n_variables, 7)
  # The three rows with a missing 利用目的 are excluded from the fit.
  expect_equal(model$excluded_nrow, 3)
  expect_equal(model$n_used, nrow(df) - 3)
  expect_true(all(model$cluster %in% 1:3))
})

test_that("exp_kmodes is deterministic for a given seed", {
  df <- kmodes_test_df()
  first <- exp_kmodes(df, `利用目的`, `契約タイプ`, `導入経路`, centers = 3, seed = 1,
                      elbow_method_mode = "none")
  second <- exp_kmodes(df, `利用目的`, `契約タイプ`, `導入経路`, centers = 3, seed = 1,
                       elbow_method_mode = "none")
  expect_identical(first$model[[1]]$cluster, second$model[[1]]$cluster)
})

test_that("ordered factors are clustered as unordered categories", {
  # An ordered factor must contribute match/mismatch only. If the order leaked
  # into the distance, "low" would be closer to "mid" than to "high"; here every
  # differing pair costs exactly 1.
  df <- tibble::tibble(
    a = factor(c("low", "mid", "high", "low"), levels = c("low", "mid", "high"), ordered = TRUE),
    b = c("x", "x", "y", "x")
  )
  prepared <- kmodes_prepare_data(df, c("a", "b"))
  expect_type(prepared$prepared$a, "character")
  expect_setequal(unique(prepared$prepared$a), c("low", "mid", "high"))
})

test_that("numeric handling keeps low-cardinality values and bins wide ones", {
  df <- tibble::tibble(small = rep(1:3, 20), wide = seq_len(60))
  auto <- kmodes_prepare_data(df, c("small", "wide"), numeric_handling = "auto", numeric_bins = 4)
  expect_setequal(unique(auto$prepared$small), c("1", "2", "3"))
  expect_equal(dplyr::n_distinct(auto$prepared$wide), 4)
  expect_equal(sort(auto$numeric_conversion$variable), c("small", "wide"))
  expect_equal(auto$numeric_conversion$conversion[auto$numeric_conversion$variable == "small"],
               "as_category")
  expect_equal(auto$numeric_conversion$conversion[auto$numeric_conversion$variable == "wide"],
               "equal_width")

  forced <- kmodes_prepare_data(df, c("wide"), numeric_handling = "as_category")
  expect_equal(dplyr::n_distinct(forced$prepared$wide), 60)
})

test_that("kmodes_mode_value breaks ties deterministically", {
  expect_equal(kmodes_mode_value(c("b", "a", "a", "b")), "a")
  expect_equal(kmodes_mode_value(c("c", "c", "a")), "c")
  expect_equal(kmodes_mode_value(c(NA, NA)), NA)
})

test_that("the distance is the simple matching mismatch count, not a numeric distance", {
  # Codes 1 and 3 differ by 2 numerically but by exactly 1 mismatch.
  mat <- matrix(c(1L, 3L, 1L,
                  1L, 1L, 2L), nrow = 3)
  d <- as.matrix(kmodes_matching_distance(mat))
  expect_equal(d[1, 2], 1)
  expect_equal(d[1, 3], 1)
  expect_equal(d[2, 3], 2)
})

test_that("tidy summary carries the size, matching rate, silhouette and one Mode per variable", {
  df <- kmodes_test_df()
  model_df <- exp_kmodes(df, `利用目的`, `契約タイプ`, `導入経路`, centers = 3, seed = 1,
                         elbow_method_mode = "none")
  res <- model_df %>% tidy_rowwise(model, type = "summary")
  expect_equal(nrow(res), 3)
  expect_true(all(c("cluster", "size", "pct_size", "avg_matching_rate_to_mode",
                    "avg_dissimilarity_to_mode", "avg_dissimilarity_rate",
                    "avg_silhouette", "min_silhouette", "pct_negative") %in% colnames(res)))
  # One Mode column per selected variable.
  expect_true(all(c("利用目的", "契約タイプ", "導入経路") %in% colnames(res)))
  expect_true(all(res$avg_matching_rate_to_mode >= 0 & res$avg_matching_rate_to_mode <= 1))
  expect_equal(sum(res$pct_size), 1)

  with_excluded <- model_df %>% tidy_rowwise(model, type = "summary", with_excluded_rows = TRUE)
  expect_equal(nrow(with_excluded), 4)
  expect_true(is.na(with_excluded$cluster[[4]]))
  expect_equal(with_excluded$size[[4]], 3)
})

test_that("the matching rate is the complement of the dissimilarity rate", {
  df <- kmodes_test_df()
  model_df <- exp_kmodes(df, `利用目的`, `契約タイプ`, `導入経路`, centers = 3, seed = 1,
                         elbow_method_mode = "none")
  res <- model_df %>% tidy_rowwise(model, type = "summary")
  expect_equal(res$avg_matching_rate_to_mode, 1 - res$avg_dissimilarity_rate)
})

test_that("silhouette scores stay within [-1, 1] or are NA", {
  df <- kmodes_test_df()
  res <- exp_kmodes(df, `利用目的`, `契約タイプ`, `導入経路`, flag, centers = 3, seed = 1,
                    elbow_method_mode = "none") %>%
    tidy_rowwise(model, type = "data")
  scores <- res$`Silhouette Score`
  expect_true(all(is.na(scores) | (scores >= -1 & scores <= 1)))
})

test_that("silhouette mode returns the cluster-count comparison table", {
  df <- kmodes_test_df()
  res <- exp_kmodes(df, `利用目的`, `契約タイプ`, `導入経路`, centers = 3, seed = 1,
                    elbow_method_mode = "silhouette", max_centers = 5,
                    silhouette_sample_size = 200) %>%
    tidy_rowwise(model, type = "silhouette")
  expect_equal(colnames(res), c("center", "avg_silhouette", "min_silhouette", "pct_negative"))
  expect_equal(min(res$center), 2)
  expect_true(max(res$center) <= 5)
})

test_that("elbow mode reports the total mismatch count, never a sum of squares", {
  df <- kmodes_test_df()
  res <- exp_kmodes(df, `利用目的`, `契約タイプ`, `導入経路`, centers = 3, seed = 1,
                    elbow_method_mode = "elbow", max_centers = 5) %>%
    tidy_rowwise(model, type = "elbow")
  expect_equal(colnames(res), c("center", "total_dissimilarity", "avg_dissimilarity",
                                "decrease_ratio"))
  expect_false(any(grepl("withinss|squares", colnames(res), ignore.case = TRUE)))
  # More clusters can never increase the total mismatch.
  expect_true(all(diff(res$total_dissimilarity) <= 0))
  expect_true(is.na(res$decrease_ratio[[1]]))
})

test_that("the unselected cluster-count method returns an empty typed table", {
  df <- kmodes_test_df()
  model_df <- exp_kmodes(df, `利用目的`, `契約タイプ`, `導入経路`, centers = 3, seed = 1,
                         elbow_method_mode = "none")
  expect_equal(nrow(tidy_rowwise(model_df, model, type = "silhouette")), 0)
  expect_equal(nrow(tidy_rowwise(model_df, model, type = "elbow")), 0)
  expect_equal(ncol(tidy_rowwise(model_df, model, type = "elbow")), 4)
})

test_that("variable importance is bounded, sorted by Cramer's V and keeps the p-value", {
  df <- kmodes_test_df()
  res <- exp_kmodes(df, `利用目的`, `契約タイプ`, `導入経路`, flag, ord, centers = 3, seed = 1,
                    elbow_method_mode = "none") %>%
    tidy_rowwise(model, type = "variable_importance")
  expect_equal(colnames(res), c("variable", "cramers_v", "chi_square", "df", "p_value"))
  expect_true(all(res$cramers_v >= 0 & res$cramers_v <= 1, na.rm = TRUE))
  # Sorted by Cramer's V descending -- the p-value must not drive the order.
  expect_equal(res$cramers_v, sort(res$cramers_v, decreasing = TRUE))
  expect_true(all(!is.na(res$p_value)))
})

test_that("characteristic categories carry the ratio, residual and Mode flag", {
  df <- kmodes_test_df()
  model_df <- exp_kmodes(df, `利用目的`, `契約タイプ`, `導入経路`, centers = 3, seed = 1,
                         elbow_method_mode = "none")
  res <- model_df %>% tidy_rowwise(model, type = "characteristic_categories")
  expect_equal(colnames(res), c("cluster", "variable", "category", "observed", "expected",
                                "cluster_pct", "overall_pct", "observed_expected_ratio",
                                "adjusted_standardized_residual", "is_mode"))
  # Observed counts must add back up to each cluster's size, per variable.
  per_variable <- res %>%
    dplyr::group_by(cluster, variable) %>%
    dplyr::summarize(total = sum(observed), .groups = "drop")
  sizes <- model_df %>% tidy_rowwise(model, type = "summary") %>%
    dplyr::mutate(cluster = as.integer(cluster)) %>%
    dplyr::select(cluster, size)
  joined <- per_variable %>% dplyr::left_join(sizes, by = "cluster")
  expect_true(all(joined$total == joined$size))

  # A positive residual means over-representation and vice versa.
  over <- res %>% dplyr::filter(observed > expected)
  expect_true(all(over$adjusted_standardized_residual > 0, na.rm = TRUE))
  under <- res %>% dplyr::filter(observed < expected)
  expect_true(all(under$adjusted_standardized_residual < 0, na.rm = TRUE))

  # The Mode flag must agree with the Mode column of the summary table.
  modes <- model_df$model[[1]]$modes
  mode_of_cluster_1 <- modes$`利用目的`[modes$cluster == 1]
  flagged <- res %>% dplyr::filter(cluster == 1, variable == "利用目的", is_mode)
  expect_equal(flagged$category, mode_of_cluster_1)

  # Sorted by absolute residual within each cluster.
  first_cluster <- res %>% dplyr::filter(cluster == 1)
  expect_equal(abs(first_cluster$adjusted_standardized_residual),
               sort(abs(first_cluster$adjusted_standardized_residual), decreasing = TRUE))
})

test_that("the observed/expected ratio is NA rather than Inf when nothing is expected", {
  expect_true(is.na(kmodes_adjusted_residual(1, 0, 0.5, 0.5)))
})

test_that("category composition sums to 100% within each cluster and variable", {
  df <- kmodes_test_df()
  res <- exp_kmodes(df, `利用目的`, `契約タイプ`, `導入経路`, flag, centers = 3, seed = 1,
                    elbow_method_mode = "none") %>%
    tidy_rowwise(model, type = "category_composition")
  expect_equal(colnames(res), c("variable", "cluster", "category", "n", "pct",
                                "cramers_v", "variable_order"))
  totals <- res %>%
    dplyr::group_by(variable, cluster) %>%
    dplyr::summarize(total = sum(pct, na.rm = TRUE), .groups = "drop")
  expect_true(all(abs(totals$total - 1) < 1e-9))
  # Variables are ordered by Cramer's V, most distinguishing first.
  order_lookup <- res %>% dplyr::distinct(variable, cramers_v, variable_order) %>%
    dplyr::arrange(variable_order)
  expect_equal(order_lookup$cramers_v, sort(order_lookup$cramers_v, decreasing = TRUE))
})

test_that("the MCA map returns observations, categories and cluster representatives", {
  df <- kmodes_test_df()
  model_df <- exp_kmodes(df, `利用目的`, `契約タイプ`, `導入経路`, flag, centers = 3, seed = 1,
                         elbow_method_mode = "none", map_sample_size = 200)
  res <- model_df %>% tidy_rowwise(model, type = "map")
  expect_equal(colnames(res), c("row_type", "row_id", "cluster", "variable", "category",
                                "Dim1", "Dim2", "label", "is_representative",
                                "is_characteristic_category", "dim1_pct_variance",
                                "dim2_pct_variance"))
  expect_setequal(unique(res$row_type), c("observation", "category", "cluster_representative"))
  expect_equal(sum(res$row_type == "cluster_representative"), 3)
  expect_true(all(res$is_representative == (res$row_type == "cluster_representative")))
  # Only category rows can be flagged as characteristic.
  expect_true(all(res$row_type[res$is_characteristic_category] == "category"))
  expect_true(!is.na(unique(res$dim1_pct_variance)[[1]]))
})

test_that("a cluster representative is the mean coordinate of its rows, not the Mode", {
  df <- kmodes_test_df()
  res <- exp_kmodes(df, `利用目的`, `契約タイプ`, `導入経路`, flag, centers = 3, seed = 1,
                    elbow_method_mode = "none", map_sample_size = 200) %>%
    tidy_rowwise(model, type = "map")
  observations <- res %>% dplyr::filter(row_type == "observation") %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarize(Dim1 = mean(Dim1), Dim2 = mean(Dim2), .groups = "drop") %>%
    dplyr::arrange(cluster)
  representatives <- res %>% dplyr::filter(row_type == "cluster_representative") %>%
    dplyr::arrange(cluster)
  expect_equal(representatives$Dim1, observations$Dim1)
  expect_equal(representatives$Dim2, observations$Dim2)
})

test_that("the map degrades to an empty typed table instead of failing", {
  # One usable variable is not enough for MCA, but the K-Modes model is fine.
  constant_df <- tibble::tibble(a = rep(c("x", "y"), 30), b = rep("same", 60))
  res <- kmodes_build_mca_map(constant_df, rep(1:2, 30), tibble::tibble(
    variable = character(0), category = character(0),
    adjusted_standardized_residual = numeric(0)))
  expect_equal(nrow(res), 0)
  expect_true("row_type" %in% colnames(res))
  expect_equal(ncol(res), 12)
})

test_that("tidy data keeps the original columns and reports excluded rows", {
  df <- kmodes_test_df()
  res <- exp_kmodes(df, `利用目的`, `契約タイプ`, `導入経路`, centers = 3, seed = 1,
                    elbow_method_mode = "none") %>%
    tidy_rowwise(model, type = "data")
  expect_true(all(colnames(df) %in% colnames(res)))
  expect_true(all(c("Cluster", "Dissimilarity to Mode", "Dissimilarity Rate",
                    "Similarity to Mode", "Silhouette Score", "Is Excluded") %in% colnames(res)))
  # Every original row comes back, and the excluded ones are flagged.
  expect_equal(nrow(res), nrow(df))
  expect_equal(sum(res$`Is Excluded`), 3)
  expect_true(all(is.na(res$Cluster[res$`Is Excluded`])))
  expect_equal(res$`Similarity to Mode`, 1 - res$`Dissimilarity Rate`)
})

test_that("the cohesion table exposes the per-row distance to the Mode", {
  df <- kmodes_test_df()
  res <- exp_kmodes(df, `利用目的`, `契約タイプ`, `導入経路`, centers = 3, seed = 1,
                    elbow_method_mode = "none") %>%
    tidy_rowwise(model, type = "cohesion")
  expect_equal(colnames(res), c("cluster", "row_id", "dissimilarity_to_mode",
                                "dissimilarity_rate", "similarity_to_mode"))
  expect_true(all(res$dissimilarity_to_mode >= 0 & res$dissimilarity_to_mode <= 3))
})

test_that("glance reports the model overview", {
  df <- kmodes_test_df()
  res <- exp_kmodes(df, `利用目的`, `契約タイプ`, `導入経路`, centers = 3, seed = 1,
                    elbow_method_mode = "none") %>%
    glance_rowwise(model)
  expect_true(all(c("centers", "n_variables", "n_used", "excluded_nrow", "excluded_pct",
                    "total_dissimilarity", "avg_dissimilarity") %in% colnames(res)))
  expect_equal(res$centers[[1]], 3)
  expect_equal(res$excluded_nrow[[1]], 3)
})

test_that("invalid input produces a readable error", {
  df <- kmodes_test_df(with_na = FALSE)
  expect_error(exp_kmodes(df, `利用目的`, centers = 3), "at least 2 variables")
  expect_error(exp_kmodes(df, `利用目的`, `契約タイプ`, centers = 1), "2 or larger")
  expect_error(exp_kmodes(df, `利用目的`, `契約タイプ`, centers = 10000),
               "larger than the number of distinct category patterns")
  expect_error(exp_kmodes(df, `利用目的`, `契約タイプ`, numeric_bins = 1), "Number of Bins")

  all_missing <- df
  all_missing$`利用目的` <- NA_character_
  expect_error(exp_kmodes(all_missing, `利用目的`, `契約タイプ`, centers = 2),
               "no row left after removing rows with missing values")

  unsupported <- df
  unsupported$timestamp <- Sys.time()
  expect_error(exp_kmodes(unsupported, `利用目的`, timestamp, centers = 2),
               "does not support the type")
})

test_that("an unknown tidy type is rejected", {
  df <- kmodes_test_df()
  model_df <- exp_kmodes(df, `利用目的`, `契約タイプ`, `導入経路`, centers = 3, seed = 1,
                         elbow_method_mode = "none")
  expect_error(tidy_rowwise(model_df, model, type = "no_such_type"),
               "Unknown tidy type for K-Modes")
})
