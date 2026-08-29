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

test_that("exp_kmodes rejects a Repeat-By column selected as a variable", {
  df <- tibble::tibble(
    repeat_by = rep(c("A", "B"), 20),
    a = rep(c("x", "y"), 20),
    b = rep(c("u", "v"), 20)
  ) %>% dplyr::group_by(repeat_by)

  expect_error(
    exp_kmodes(df, repeat_by, a, b, centers = 2, elbow_method_mode = "none"),
    "Repeat-By column cannot be used"
  )
})

test_that("sampled_nrow is set only when max_nrow actually samples", {
  df <- tibble::tibble(
    a = rep(c("x", "y"), 20),
    b = rep(c("u", "v"), 20)
  )

  not_sampled <- exp_kmodes(
    df, a, b, centers = 2, max_nrow = 100,
    elbow_method_mode = "none", map_sample_size = 2
  )$model[[1]]
  expect_null(not_sampled$sampled_nrow)
  expect_equal(nrow(not_sampled$df_original), nrow(df))

  sampled <- exp_kmodes(
    df, a, b, centers = 2, max_nrow = 10,
    elbow_method_mode = "none", map_sample_size = 2
  )$model[[1]]
  expect_equal(sampled$sampled_nrow, 10)
  expect_equal(nrow(sampled$df_original), 10)
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

test_that("the auto threshold follows the configured number of bins, not a constant", {
  # The threshold used to be a hardcoded 10, which happens to equal
  # numeric_bins' own default -- so at default settings the two agreed and
  # nothing looked wrong. They diverge as soon as the user changes the field.
  # A column with 15 distinct values is the case that exposes it: above the old
  # constant, at or below a raised bin count.
  df <- tibble::tibble(mid = rep(1:15, 8))

  raised <- kmodes_prepare_data(df, "mid", numeric_handling = "auto", numeric_bins = 20)
  expect_equal(raised$numeric_conversion$conversion, "as_category",
               info = "15 distinct values with 20 bins requested are already the categories")
  expect_setequal(unique(raised$prepared$mid), as.character(1:15))

  # Binning into more bins than there are distinct values can only produce bins
  # that are empty by construction, and replaces values the reader recognises
  # with interval labels.
  expect_false(any(grepl("^[[(].*,.*[])]$", unique(raised$prepared$mid))))

  default <- kmodes_prepare_data(df, "mid", numeric_handling = "auto", numeric_bins = 10)
  expect_equal(default$numeric_conversion$conversion, "equal_width",
               info = "15 distinct values into 10 bins is genuine binning")
  expect_equal(dplyr::n_distinct(default$prepared$mid), 10)
})

test_that("a lowered bin count is honoured by the auto threshold", {
  # The mirror-image of the case above: with a hardcoded threshold of 10, an
  # 8-distinct column stayed 8 categories however low the bin count was set.
  df <- tibble::tibble(v = rep(1:8, 25))

  lowered <- kmodes_prepare_data(df, "v", numeric_handling = "auto", numeric_bins = 5)
  expect_equal(lowered$numeric_conversion$conversion, "equal_width")
  expect_equal(dplyr::n_distinct(lowered$prepared$v), 5)

  at_threshold <- kmodes_prepare_data(df, "v", numeric_handling = "auto", numeric_bins = 8)
  expect_equal(at_threshold$numeric_conversion$conversion, "as_category",
               info = "distinct == bins needs no binning")
})

test_that("a non-finite value does not abort the conversion", {
  # Inf is what a ratio column with a zero denominator produces. It used to
  # reach cut() and abort the whole analytics with base R's
  # "'to' must be a finite number", which named nothing the user could act on.
  wide <- c(seq_len(40), Inf)

  prepared <- kmodes_prepare_column(wide, "auto", 10)
  expect_equal(prepared$conversion, "equal_width")
  expect_true(is.na(prepared$values[length(prepared$values)]),
              info = "a non-finite entry carries no category and is treated as missing")
  expect_equal(dplyr::n_distinct(prepared$values[!is.na(prepared$values)]), 10)

  # -Inf and NaN take the same path.
  expect_silent(kmodes_prepare_column(c(seq_len(40), -Inf), "auto", 10))
  expect_silent(kmodes_prepare_column(c(seq_len(40), NaN), "auto", 10))

  # And end to end, through exp_kmodes itself.
  df <- tibble::tibble(cat = rep(c("a", "b", "c"), 20), num = c(Inf, seq_len(59)))
  expect_error(exp_kmodes(df, cat, num, centers = 2, seed = 1), NA)
})

test_that("kmodes_prepare_data preserves a factor's declared level order for display (#37936)", {
  df <- tibble::tibble(
    a = factor(c("High", "Mid", "Low", "High"), levels = c("High", "Mid", "Low")),
    b = c("x", "x", "y", "x")
  )
  prepared <- kmodes_prepare_data(df, c("a", "b"))
  # The algorithm-facing values are unaffected -- still plain character.
  expect_type(prepared$prepared$a, "character")
  # But the display order the factor declared is captured separately.
  expect_equal(prepared$display_levels$a, c("High", "Mid", "Low"))
  # A plain character column has no explicit order to capture.
  expect_null(prepared$display_levels$b)
})

test_that("kmodes_prepare_data captures the natural bin order for equal-width numeric binning (#37936)", {
  df <- tibble::tibble(wide = seq_len(60))
  prepared <- kmodes_prepare_data(df, "wide", numeric_handling = "equal_width", numeric_bins = 4)
  expected_levels <- levels(cut(df$wide, breaks = 4, include.lowest = TRUE))
  expect_equal(prepared$display_levels$wide, expected_levels)
  # Proves the fix matters for this fixture: natural cut() order differs from
  # codepoint/string order (e.g. "(10,11]" would otherwise sort before "(2,3]").
  expect_false(identical(expected_levels, sort(expected_levels)))
})

test_that("kmodes_category_display_levels unions per-variable orders in variable order, deduping shared values (#37936)", {
  prepared_df <- tibble::tibble(
    a = c("High", "Mid", "Low"),
    b = c("Yes", "No", "Yes")
  )
  display_levels <- list(a = c("High", "Mid", "Low"), b = c("Yes", "No"))
  levels_out <- kmodes_category_display_levels(prepared_df, display_levels)
  expect_equal(levels_out, c("High", "Mid", "Low", "Yes", "No"))

  # NULL entries (no declared order) fall back to sort(unique(...)) per
  # variable -- the pre-fix behavior, preserved for columns with no order to
  # honor. A NULL display_levels altogether (old callers) behaves the same.
  levels_fallback <- kmodes_category_display_levels(prepared_df, list(a = NULL, b = NULL))
  expect_equal(levels_fallback, c("High", "Low", "Mid", "No", "Yes"))
  expect_equal(kmodes_category_display_levels(prepared_df, NULL), levels_fallback)
})

test_that("character category sorting normalizes native encodings", {
  values <- c("ア", "あ", "é", "a")
  Encoding(values) <- "unknown"

  expect_equal(kmodes_sort_character(values), c("a", "é", "あ", "ア"))
  expect_equal(kmodes_mode_value(rep(values[1:2], each = 2)), "あ")

  japanese <- rep(c("あ", "ア"), each = 20)
  Encoding(japanese) <- "unknown"
  df <- tibble::tibble(
    japanese = japanese,
    segment = rep(c("x", "y"), each = 20)
  )
  expect_error(
    exp_kmodes(df, japanese, segment, centers = 2, seed = 1,
               elbow_method_mode = "none", map_sample_size = 0),
    NA
  )
})

test_that("report category_order keeps conflicting shared factor orders per variable", {
  df <- tibble::tibble(
    a = factor(c("A", "B", "A", "B"), levels = c("A", "B")),
    b = factor(c("A", "B", "A", "B"), levels = c("B", "A"))
  )
  model_df <- exp_kmodes(df, a, b, centers = 2, seed = 1, elbow_method_mode = "none")

  for (type in c("category_composition", "characteristic_categories")) {
    result <- tidy_rowwise(model_df, model, type = type)
    orders <- result %>%
      dplyr::distinct(variable, category, category_order) %>%
      dplyr::arrange(variable, category_order)
    expect_equal(as.character(orders$category[orders$variable == "a"]), c("A", "B"))
    expect_equal(as.character(orders$category[orders$variable == "b"]), c("B", "A"))
  }
})

test_that("kmodes_mode_value breaks ties deterministically", {
  expect_equal(kmodes_mode_value(c("b", "a", "a", "b")), "a")
  expect_equal(kmodes_mode_value(c("c", "c", "a")), "c")
  expect_equal(kmodes_mode_value(c(NA, NA)), NA)
  # A numeric tie must break on the number, not on its text. table() names are
  # character, so a plain sort() would rank code 10 above code 9 and the Mode
  # picked while fitting would stop matching the label-based Mode flag.
  expect_equal(kmodes_mode_value(c(rep(9L, 3), rep(10L, 3))), 9)
  expect_equal(kmodes_mode_value(c(rep(2L, 2), rep(11L, 2))), 2)
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

test_that("analysis_conditions reports variable count, names, row count, rows removed and cluster count (issue #37682)", {
  df <- kmodes_test_df()
  model_df <- exp_kmodes(df, `利用目的`, `契約タイプ`, `導入経路`, centers = 3, seed = 1,
                         elbow_method_mode = "none")
  res <- model_df %>% tidy_rowwise(model, type = "analysis_conditions")
  expect_equal(colnames(res), c("Metric", "Value"))
  expect_equal(res$Metric, c("Number of Variables", "Variable Names", "Row Count",
                            "Rows Removed", "Number of Clusters"))
  expect_equal(res$Value[[1]], "3")
  expect_true(grepl("利用目的", res$Value[[2]], fixed = TRUE))
  expect_true(grepl("契約タイプ", res$Value[[2]], fixed = TRUE))
  expect_true(grepl("導入経路", res$Value[[2]], fixed = TRUE))
  expect_equal(res$Value[[3]], as.character(nrow(df) - 3))
  expect_equal(res$Value[[4]], "3")
  expect_equal(res$Value[[5]], "3")
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

test_that("the cluster-count table agrees with the clustering the report shows", {
  # The candidate loop used to refit at every k, including the selected one, and landed on
  # its own local optimum -- so the k = centers row described a different, usually worse,
  # clustering than the summary table right above it.
  df <- kmodes_test_df()
  model_df <- exp_kmodes(df, `利用目的`, `契約タイプ`, `導入経路`, flag, small_num, ord,
                         centers = 3, seed = 1, elbow_method_mode = "silhouette",
                         max_centers = 5)
  summary_df <- model_df %>% tidy_rowwise(model, type = "summary")
  selection <- model_df %>% tidy_rowwise(model, type = "silhouette") %>%
    dplyr::filter(center == 3)

  # The worst silhouette across the clusters IS the model's minimum.
  expect_equal(selection$min_silhouette[[1]], min(summary_df$min_silhouette))
  # And the reported average is the size-weighted average of the per-cluster averages.
  expect_equal(selection$avg_silhouette[[1]],
               sum(summary_df$size * summary_df$avg_silhouette) / sum(summary_df$size),
               tolerance = 1e-8)

  elbow_df <- exp_kmodes(df, `利用目的`, `契約タイプ`, `導入経路`, flag, small_num, ord,
                         centers = 3, seed = 1, elbow_method_mode = "elbow", max_centers = 5)
  elbow_summary <- elbow_df %>% tidy_rowwise(model, type = "summary")
  elbow_selection <- elbow_df %>% tidy_rowwise(model, type = "elbow") %>%
    dplyr::filter(center == 3)
  # Total mismatches at the selected k = the mismatches the summary implies.
  n_variables <- elbow_df$model[[1]]$n_variables
  expect_equal(elbow_selection$total_dissimilarity[[1]],
               sum(elbow_summary$size * n_variables * (1 - elbow_summary$avg_matching_rate_to_mode)),
               tolerance = 1e-6)
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
  expect_equal(colnames(res), c("cluster", "variable", "category", "category_order", "observed", "expected",
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
  # `category` is a factor now (#37936, display order fix), so compare on
  # character values -- the flag/value agreement is what this test checks,
  # not the column's class (covered separately below).
  modes <- model_df$model[[1]]$modes
  mode_of_cluster_1 <- modes$`利用目的`[modes$cluster == 1]
  flagged <- res %>% dplyr::filter(cluster == 1, variable == "利用目的", is_mode)
  expect_equal(as.character(flagged$category), mode_of_cluster_1)

  # Sorted by absolute residual within each cluster.
  first_cluster <- res %>% dplyr::filter(cluster == 1)
  expect_equal(abs(first_cluster$adjusted_standardized_residual),
               sort(abs(first_cluster$adjusted_standardized_residual), decreasing = TRUE))
})

test_that("characteristic_categories and category_composition honor a factor's declared level order (#37936)", {
  set.seed(7)
  n <- 120
  df <- tibble::tibble(
    tenure = factor(sample(c("3年以上", "1年-3年", "1年未満", "6ヶ月未満"), n, TRUE),
                    levels = c("6ヶ月未満", "1年未満", "1年-3年", "3年以上")),
    plan = sample(c("Enterprise", "Free", "Standard"), n, TRUE)
  )
  model_df <- exp_kmodes(df, tenure, plan, centers = 2, seed = 1, elbow_method_mode = "none")

  characteristic <- model_df %>% tidy_rowwise(model, type = "characteristic_categories")
  tenure_char_levels <- characteristic %>% dplyr::filter(variable == "tenure") %>%
    dplyr::pull(category)
  expect_s3_class(tenure_char_levels, "factor")
  expect_equal(levels(droplevels(tenure_char_levels)), c("6ヶ月未満", "1年未満", "1年-3年", "3年以上"))
  # Proves the fixture actually exercises the fix: alphabetical/codepoint
  # order would be different from the declared factor level order here.
  expect_false(identical(levels(droplevels(tenure_char_levels)), sort(levels(droplevels(tenure_char_levels)))))

  composition <- model_df %>% tidy_rowwise(model, type = "category_composition")
  tenure_comp_levels <- composition %>% dplyr::filter(variable == "tenure") %>%
    dplyr::pull(category)
  expect_s3_class(tenure_comp_levels, "factor")
  expect_equal(levels(droplevels(tenure_comp_levels)), c("6ヶ月未満", "1年未満", "1年-3年", "3年以上"))

  # A plain character (non-factor) source column still gets a factor OUTPUT
  # column, falling back to alphabetical order (the pre-fix behavior) -- the
  # fix does not depend on every selected variable being a factor.
  plan_comp_levels <- composition %>% dplyr::filter(variable == "plan") %>% dplyr::pull(category)
  expect_s3_class(plan_comp_levels, "factor")
  expect_equal(levels(droplevels(plan_comp_levels)), sort(unique(df$plan)))
})

test_that("category_composition keeps unused declared Factor levels (tam#38122 analog)", {
  # Factor type keeps every declared level, including ones with 0 rows. The
  # composition table used to call table() on as.character(factor), which
  # dropped unused levels before they could become n=0 bars on that variable's
  # Category Composition panel.
  set.seed(7)
  n <- 80
  df <- tibble::tibble(
    tenure = factor(sample(c("3年以上", "1年-3年"), n, TRUE),
                    levels = c("6ヶ月未満", "1年未満", "1年-3年", "3年以上")),
    plan = sample(c("Enterprise", "Free", "Standard"), n, TRUE)
  )
  expect_false("6ヶ月未満" %in% as.character(df$tenure))
  expect_false("1年未満" %in% as.character(df$tenure))

  composition <- exp_kmodes(df, tenure, plan, centers = 2, seed = 1,
                            elbow_method_mode = "none") %>%
    tidy_rowwise(model, type = "category_composition")

  tenure_rows <- composition %>% dplyr::filter(variable == "tenure")
  tenure_cats <- as.character(tenure_rows$category)
  expect_true(all(c("6ヶ月未満", "1年未満", "1年-3年", "3年以上") %in% tenure_cats))
  unused <- tenure_rows %>% dplyr::filter(category %in% c("6ヶ月未満", "1年未満"))
  expect_true(nrow(unused) > 0)
  expect_true(all(unused$n == 0))
  expect_true(all(unused$pct == 0 | is.na(unused$pct)))

  # Unused tenure levels must not be copied onto the sibling character variable.
  plan_cats <- composition %>% dplyr::filter(variable == "plan") %>%
    dplyr::pull(category) %>% as.character()
  expect_false(any(plan_cats %in% c("6ヶ月未満", "1年未満")))
})

test_that("category_composition does not resurrect empty equal-width bins", {
  # display_levels for a cut() numeric also lists empty bins. Those are not
  # user-declared Factor levels; Category Composition must keep omitting them.
  set.seed(7)
  df <- tibble::tibble(
    grp = sample(c("A", "B"), 80, TRUE),
    score = sample(1:5, 80, TRUE)
  )
  composition <- exp_kmodes(df, grp, score, centers = 2, seed = 1,
                            numeric_handling = "equal_width", numeric_bins = 10,
                            elbow_method_mode = "none") %>%
    tidy_rowwise(model, type = "category_composition")
  score_rows <- composition %>% dplyr::filter(variable == "score")
  expect_true(nrow(score_rows) > 0)
  # A category can be n=0 in one cluster and still observed overall. The
  # lock is that a cut() bin which never appears in ANY row stays omitted --
  # 5 distinct scores cannot fill 10 equal-width bins.
  totals <- score_rows %>%
    dplyr::group_by(category) %>%
    dplyr::summarize(total = sum(n), .groups = "drop")
  expect_true(all(totals$total > 0))
  expect_lt(nrow(totals), 10)
})

test_that("the observed/expected ratio is NA rather than Inf when nothing is expected", {
  expect_true(is.na(kmodes_adjusted_residual(1, 0, 0.5, 0.5)))
})

test_that("category composition sums to 100% within each cluster and variable", {
  df <- kmodes_test_df()
  res <- exp_kmodes(df, `利用目的`, `契約タイプ`, `導入経路`, flag, centers = 3, seed = 1,
                    elbow_method_mode = "none") %>%
    tidy_rowwise(model, type = "category_composition")
  expect_equal(colnames(res), c("variable", "cluster", "category", "category_order", "n", "pct",
                                "cramers_v", "variable_order", "original_order"))
  totals <- res %>%
    dplyr::group_by(variable, cluster) %>%
    dplyr::summarize(total = sum(pct, na.rm = TRUE), .groups = "drop")
  expect_true(all(abs(totals$total - 1) < 1e-9))
  # Variables are ordered by Cramer's V, most distinguishing first.
  order_lookup <- res %>% dplyr::distinct(variable, cramers_v, variable_order) %>%
    dplyr::arrange(variable_order)
  expect_equal(order_lookup$cramers_v, sort(order_lookup$cramers_v, decreasing = TRUE))
  # original_order is the order the variables were selected in, so the report can switch back.
  original <- res %>% dplyr::distinct(variable, original_order) %>%
    dplyr::arrange(original_order)
  expect_equal(original$variable, c("利用目的", "契約タイプ", "導入経路", "flag"))
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
