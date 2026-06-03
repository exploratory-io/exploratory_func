# how to run this test:
# devtools::test(filter="kmeans")
context("test kmeans analytics view functions")

test_that("exp_kmeans", {
  df <- mtcars %>% mutate(new_col = c(rep("A", n() - 10), rep("B", 10)))
  df <- df %>% tibble::add_row(cyl=5, mpg=NA, hp=100)
  model_df <- exp_kmeans(df, cyl, mpg, hp, max_nrow=30)
  res <- model_df %>% tidy_rowwise(model, type="summary")
  expect_equal(nrow(res), 3)
  res <- model_df %>% tidy_rowwise(model, type="summary", with_excluded_rows=TRUE)
  expect_equal(nrow(res), 4) # With an additional row for the count of excluded rows.
  res <- model_df %>% tidy_rowwise(model, type="variances")
  res <- model_df %>% tidy_rowwise(model, type="loadings")
  res <- model_df %>% tidy_rowwise(model, type="biplot")
  res <- model_df %>% tidy_rowwise(model, type="data")
  res <- model_df %>% tidy_rowwise(model, type="gathered_data")
  res <- model_df %>% tidy_rowwise(model, type="gathered_data", normalize_data=TRUE, n_sample=20)
  expect_equal(colnames(res),
               c("disp","drat","wt","qsec","vs","am","gear","carb","new_col","cluster","PC1","PC2","PC3","row_id","key",
                 "value"))
})

test_that("exp_kmeans with strange column name", {
  df <- mtcars %>%
    rename(`Cy l` = cyl) %>%
    mutate(new_col = c(rep("A", n() - 10), rep("B", 10)))

  # Add list column and difftime column etc. which used to cause error until we removed it as preprocessing.
  df <- df %>% mutate(posix1=lubridate::ymd_hm("2021-01-01 00:00"),
                      posix2=lubridate::ymd_hm("2021-01-02 00:00"),
                      difftime = posix2-posix1, dur = lubridate::as.duration(difftime),
                      intv = lubridate::as.interval(posix1, posix2),
                      period = lubridate::as.period(intv),
                      str="a,b,c",
                      list=stringr::str_split(str,","))
  df <- df %>% select(-posix1, -posix2, -str) # Remove POSIXct column and character column we used to create those special columns.

  model_df <- exp_kmeans(df, `Cy l`, mpg, hp)
  model_df %>% tidy_rowwise(model, type="variances")
  model_df %>% tidy_rowwise(model, type="loadings")
  model_df %>% tidy_rowwise(model, type="biplot")
  model_df %>% tidy_rowwise(model, type="data")
  model_df %>% tidy_rowwise(model, type="gathered_data")
  res <- model_df %>% tidy_rowwise(model, type="gathered_data", normalize_data=TRUE, n_sample=100) # testing n_sample more than nrow()
  expect_equal(colnames(res),
               c("disp","drat","wt","qsec","vs","am","gear","carb","new_col","cluster","PC1","PC2","PC3","row_id","key",
                 "value"))
})

test_that("exp_kmeans with single column name", {
  model_df <- exp_kmeans(mtcars, mpg)
  model_df %>% tidy_rowwise(model, type="variances")
  # model_df %>% tidy_rowwise(model, type="loadings") # Not used.
  # model_df %>% tidy_rowwise(model, type="biplot") # Will skip for single column case.
  model_df %>% tidy_rowwise(model, type="data")
  model_df %>% tidy_rowwise(model, type="gathered_data")
  res <- model_df %>% tidy_rowwise(model, type="gathered_data", normalize_data=TRUE, n_sample=100) # testing n_sample more than nrow()
  expect_equal(colnames(res),
               c("cyl","disp","hp","drat","wt","qsec","vs","am","gear","carb","cluster","PC1","row_id","key","value"))
})

test_that("exp_kmeans elbow method mode", {
  df <- mtcars %>% mutate(new_col = c(rep("A", n() - 10), rep("B", 10)))
  model_df <- exp_kmeans(df, cyl, mpg, hp, elbow_method_mode=TRUE)
  res <- model_df$model[[1]]$elbow_result
  expect_equal(colnames(res), c("center","totss","tot.withinss","betweenss","iter"))
  # Test the case the rows are fewer and we have to limit the search.
  df <- df %>% head(9)
  model_df <- exp_kmeans(df, cyl, mpg, hp, elbow_method_mode=TRUE)
  res <- model_df$model[[1]]$elbow_result
  # Search should be limited up to 8 (9 - 1).
  expect_equal(nrow(res), 8)
})

test_that("exp_kmeans silhouette method mode", {
  df <- mtcars %>% mutate(new_col = c(rep("A", n() - 10), rep("B", 10)))

  # silhouette_result is attached with correct columns and value ranges
  model_df <- exp_kmeans(df, cyl, mpg, hp, elbow_method_mode = "silhouette")
  res <- model_df$model[[1]]$silhouette_result
  expect_equal(colnames(res), c("center", "avg_silhouette", "min_silhouette", "pct_negative"))
  expect_true(min(res$center) == 2)
  expect_true(all(res$avg_silhouette >= -1 & res$avg_silhouette <= 1))
  expect_true(all(res$min_silhouette >= -1 & res$min_silhouette <= 1))
  expect_true(all(res$pct_negative >= 0 & res$pct_negative <= 1))
  # elbow_result should NOT be attached when silhouette mode is used
  expect_null(model_df$model[[1]]$elbow_result)
})

test_that("exp_kmeans silhouette mode caps k by distinct data points", {
  # Only 3 distinct rows across many duplicates; max_centers default is 10.
  # k must be capped at distinct points - 1 so stats::kmeans() does not error.
  df <- data.frame(
    x = rep(c(1, 2, 3), times = 20),
    y = rep(c(10, 20, 30), times = 20)
  )
  model_df <- exp_kmeans(df, x, y, elbow_method_mode = "silhouette")
  res <- model_df$model[[1]]$silhouette_result
  expect_true(max(res$center) <= 2) # distinct points (3) - 1
})

test_that("exp_kmeans elbow_method_mode = 'elbow' attaches elbow_result only", {
  df <- mtcars %>% mutate(new_col = c(rep("A", n() - 10), rep("B", 10)))
  model_df <- exp_kmeans(df, cyl, mpg, hp, elbow_method_mode = "elbow")
  expect_false(is.null(model_df$model[[1]]$elbow_result))
  expect_null(model_df$model[[1]]$silhouette_result)
})

test_that("exp_kmeans elbow_method_mode = 'none' attaches neither", {
  df <- mtcars %>% mutate(new_col = c(rep("A", n() - 10), rep("B", 10)))
  model_df <- exp_kmeans(df, cyl, mpg, hp, elbow_method_mode = "none")
  expect_null(model_df$model[[1]]$elbow_result)
  expect_null(model_df$model[[1]]$silhouette_result)
})

test_that("exp_kmeans elbow_method_mode backward compatibility: logical TRUE/FALSE", {
  df <- mtcars %>% mutate(new_col = c(rep("A", n() - 10), rep("B", 10)))

  # TRUE should behave the same as "elbow"
  model_df_logical <- exp_kmeans(df, cyl, mpg, hp, elbow_method_mode = TRUE, seed = 42)
  model_df_enum   <- exp_kmeans(df, cyl, mpg, hp, elbow_method_mode = "elbow", seed = 42)
  expect_false(is.null(model_df_logical$model[[1]]$elbow_result))
  expect_null(model_df_logical$model[[1]]$silhouette_result)
  expect_equal(model_df_logical$model[[1]]$elbow_result,
               model_df_enum$model[[1]]$elbow_result)

  # FALSE should behave the same as "none"
  model_df_false <- exp_kmeans(df, cyl, mpg, hp, elbow_method_mode = FALSE)
  expect_null(model_df_false$model[[1]]$elbow_result)
  expect_null(model_df_false$model[[1]]$silhouette_result)
})

# group_by for elbow method is not currently supported. Revive this test when it is.
#test_that("exp_kmeans elbow method mode with group_by", {
#  df <- mtcars %>% mutate(new_col = c(rep("A", n() - 10), rep("B", 10)))
#  df <- df %>% group_by(new_col)
#  model_df <- exp_kmeans(df, cyl, mpg, hp, elbow_method_mode=TRUE, max_centers=3)
#  model_df %>% tidyr::unnest(model)
#})

test_that("compute_silhouette_per_row returns aligned per-row columns", {
  set.seed(1)
  mat <- as.matrix(iris[, 1:4])
  mat <- scale(mat)
  km <- stats::kmeans(mat, centers = 3)
  res <- compute_silhouette_per_row(km$cluster, mat)
  expect_equal(nrow(res), nrow(mat))
  expect_setequal(colnames(res), c("silhouette_score", "nearest_cluster", "cluster_width"))
  expect_true(all(res$silhouette_score >= -1 & res$silhouette_score <= 1))
  expect_true(all(res$nearest_cluster %in% unique(km$cluster)))
  # cluster_width is the per-cluster mean of silhouette_score, broadcast to rows.
  expected_avg <- tapply(res$silhouette_score, km$cluster, mean)
  for (cl in unique(km$cluster)) {
    expect_equal(unique(res$cluster_width[km$cluster == cl]), unname(expected_avg[as.character(cl)]))
  }
  # Passing precomputed dist gives identical result to recomputing it.
  d <- stats::dist(mat)
  res_d <- compute_silhouette_per_row(km$cluster, mat, d)
  expect_equal(res, res_d)
})

test_that("compute_silhouette_per_row returns all-NA for degenerate input (no error)", {
  mat <- matrix(rep(1, 20), ncol = 2)            # all identical points
  res <- compute_silhouette_per_row(rep(1L, 10), mat)  # single cluster
  expect_equal(nrow(res), 10)
  expect_true(all(is.na(res$silhouette_score)))
  expect_true(all(is.na(res$nearest_cluster)))
  expect_true(all(is.na(res$cluster_width)))
})

test_that("iterate_silhouette accepts a precomputed dist and matches recompute", {
  set.seed(1)
  df <- iris[, 1:4]
  mat <- scale(as_numeric_matrix_(df, columns = colnames(df)))
  d <- stats::dist(mat)
  set.seed(1); a <- iterate_silhouette(df, max_centers = 4, normalize_data = TRUE, seed = 1)
  set.seed(1); b <- iterate_silhouette(df, max_centers = 4, normalize_data = TRUE, seed = 1, dist = d)
  expect_equal(a, b)
})

test_that("exp_kmeans attaches per-row silhouette to each model (all elbow modes)", {
  df <- mtcars
  for (mode in list("none", "elbow", "silhouette")) {
    model_df <- exp_kmeans(df, cyl, mpg, hp, centers = 3, elbow_method_mode = mode, max_nrow = 30)
    model <- model_df$model[[1]]
    expect_true(!is.null(model$silhouette))
    expect_equal(nrow(model$silhouette), nrow(model$df))
    expect_setequal(colnames(model$silhouette),
                    c("silhouette_score", "nearest_cluster", "cluster_width"))
  }
})

test_that("tidy_rowwise summary includes per-cluster silhouette aggregates", {
  df <- mtcars
  model_df <- exp_kmeans(df, cyl, mpg, hp, centers = 3, max_nrow = 30)
  res <- model_df %>% tidy_rowwise(model, type = "summary")
  expect_true(all(c("avg_silhouette", "min_silhouette", "pct_negative") %in% colnames(res)))
  non_excluded <- res[!is.na(res$cluster), ]
  expect_true(all(non_excluded$pct_negative >= 0 & non_excluded$pct_negative <= 1))
  expect_true(all(non_excluded$avg_silhouette >= non_excluded$min_silhouette))
})

test_that("tidy_rowwise data includes per-row silhouette columns", {
  df <- mtcars
  model_df <- exp_kmeans(df, cyl, mpg, hp, centers = 3, max_nrow = 30)
  res <- model_df %>% tidy_rowwise(model, type = "data")
  expect_true(all(c("silhouette_score", "nearest_cluster", "cluster_width") %in% colnames(res)))
  expect_true(all(res$silhouette_score >= -1 & res$silhouette_score <= 1))
  # gathered_data must NOT carry the per-row silhouette columns (charts select their own cols).
  g <- model_df %>% tidy_rowwise(model, type = "gathered_data")
  expect_false("silhouette_score" %in% colnames(g))
})

test_that("exp_kmeans with strange column name still yields silhouette columns", {
  df <- mtcars
  df <- df %>% dplyr::rename(`Cy l !#$%` = cyl)
  model_df <- exp_kmeans(df, `Cy l !#$%`, mpg, hp, centers = 3, max_nrow = 30)
  res <- model_df %>% tidy_rowwise(model, type = "data")
  expect_true("silhouette_score" %in% colnames(res))
  smry <- model_df %>% tidy_rowwise(model, type = "summary")
  expect_true("avg_silhouette" %in% colnames(smry))
})

# ---- silhouette_sample_size (#36126: bound the O(n^2) silhouette dist) ----

test_that("silhouette_sample_index returns full index unless sample_size < n", {
  expect_equal(silhouette_sample_index(10, NULL), 1:10)   # NULL -> no cap
  expect_equal(silhouette_sample_index(10, 0), 1:10)      # non-positive -> no cap
  expect_equal(silhouette_sample_index(10, 10), 1:10)     # equal -> no cap
  expect_equal(silhouette_sample_index(10, 20), 1:10)     # larger -> no cap
  set.seed(1)
  idx <- silhouette_sample_index(100, 5)                  # smaller -> subsample
  expect_length(idx, 5)
  expect_true(all(idx %in% 1:100))
  expect_false(is.unsorted(idx))                          # returned sorted
  expect_equal(length(unique(idx)), 5)                    # without replacement
})

test_that("compute_silhouette_per_row with sample_idx scatters to full length, NA off-sample", {
  set.seed(1)
  full_mat <- scale(as.matrix(iris[, 1:4]))
  km <- stats::kmeans(full_mat, centers = 3)
  n <- nrow(full_mat)
  sample_idx <- sort(sample.int(n, 30))
  res <- compute_silhouette_per_row(km$cluster, full_mat[sample_idx, , drop = FALSE],
                                    sample_idx = sample_idx)
  expect_equal(nrow(res), n)                                   # full length, aligned to data
  expect_setequal(colnames(res), c("silhouette_score", "nearest_cluster", "cluster_width"))
  expect_equal(sum(!is.na(res$silhouette_score)), 30)          # only sampled rows scored
  expect_true(all(is.na(res$silhouette_score[-sample_idx])))   # off-sample rows are NA
  on_sample <- res$silhouette_score[sample_idx]
  expect_true(all(on_sample >= -1 & on_sample <= 1))
})

test_that("exp_kmeans bounds the per-row silhouette to silhouette_sample_size (no OOM, aligned)", {
  df <- mtcars  # 32 rows, no NA
  model_df <- exp_kmeans(df, cyl, mpg, hp, centers = 3, max_nrow = NULL,
                         silhouette_sample_size = 10)
  model <- model_df$model[[1]]
  # Per-row silhouette stays positionally aligned to all rows ...
  expect_equal(nrow(model$silhouette), nrow(model$df))
  # ... but only up to silhouette_sample_size rows actually get a (finite) score.
  expect_lte(sum(!is.na(model$silhouette$silhouette_score)), 10)
  expect_gt(sum(!is.na(model$silhouette$silhouette_score)), 0)

  res <- model_df %>% tidy_rowwise(model, type = "data")
  expect_equal(nrow(res), nrow(model$df))
  smry <- model_df %>% tidy_rowwise(model, type = "summary")
  expect_true(all(c("avg_silhouette", "min_silhouette", "pct_negative") %in% colnames(smry)))
})

test_that("exp_kmeans default (large) silhouette_sample_size leaves small data fully scored", {
  df <- mtcars
  model_df <- exp_kmeans(df, cyl, mpg, hp, centers = 3, max_nrow = NULL)  # default 5000 >> 32
  sil <- model_df$model[[1]]$silhouette
  expect_equal(sum(!is.na(sil$silhouette_score)), nrow(sil))  # no subsampling for small data
})

test_that("exp_kmeans silhouette method respects silhouette_sample_size (bounded, no error)", {
  df <- mtcars
  model_df <- exp_kmeans(df, cyl, mpg, hp, centers = 3, max_nrow = NULL,
                         elbow_method_mode = "silhouette", max_centers = 4,
                         silhouette_sample_size = 15)
  sil_res <- model_df$model[[1]]$silhouette_result
  expect_false(is.null(sil_res))
  expect_true(all(c("center", "avg_silhouette", "min_silhouette", "pct_negative") %in% colnames(sil_res)))
})

# #1: Grouped case exercises the else branch (per-group subsample via g_sample_idx).
test_that("exp_kmeans grouped case bounds and aligns per-group silhouette (#36126)", {
  df <- mtcars %>% dplyr::group_by(am)  # two groups: 19 and 13 rows, both > sample_size below
  model_df <- exp_kmeans(df, cyl, mpg, hp, centers = 2, max_nrow = NULL,
                         silhouette_sample_size = 8)
  expect_equal(nrow(model_df), length(unique(mtcars$am)))  # one model row per group
  for (i in seq_len(nrow(model_df))) {
    m <- model_df$model[[i]]
    # Per-row silhouette stays positionally aligned to the group's rows ...
    expect_equal(nrow(m$silhouette), nrow(m$df))
    # ... but at most silhouette_sample_size rows actually get a finite score.
    n_scored <- sum(!is.na(m$silhouette$silhouette_score))
    expect_lte(n_scored, 8)
    expect_gt(n_scored, 0)
  }
})

# #2: The scatter must place each sampled row's value at ITS OWN position, not just
# produce the right count/range. Compare against an independent reference silhouette
# computed directly on the subsample (silhouette() preserves input observation order).
test_that("compute_silhouette_per_row scatters each value to its own row, positionally (#36126)", {
  set.seed(42)
  full_mat <- scale(as.matrix(iris[, 1:4]))
  km <- stats::kmeans(full_mat, centers = 3)
  n <- nrow(full_mat)
  sample_idx <- sort(sample.int(n, 40))

  sub_mat <- full_mat[sample_idx, , drop = FALSE]
  ref_sil <- cluster::silhouette(as.integer(km$cluster[sample_idx]), stats::dist(sub_mat))
  ref_width <- as.numeric(ref_sil[, "sil_width"])
  ref_neighbor <- as.integer(ref_sil[, "neighbor"])
  ref_clusters <- as.integer(ref_sil[, "cluster"])
  ref_clus_avg <- tapply(ref_width, ref_clusters, mean, na.rm = TRUE)
  ref_cluster_width <- as.numeric(ref_clus_avg[as.character(ref_clusters)])

  res <- compute_silhouette_per_row(km$cluster, sub_mat, sample_idx = sample_idx)

  # Exact, position-by-position equality. A scatter that shuffled positions (but kept
  # values in [-1, 1]) would pass the existing count/range checks yet fail here.
  expect_equal(res$silhouette_score[sample_idx], ref_width)
  expect_equal(res$nearest_cluster[sample_idx], ref_neighbor)
  expect_equal(res$cluster_width[sample_idx], ref_cluster_width)
  expect_true(all(is.na(res$silhouette_score[-sample_idx])))  # off-sample untouched
})

# #3: With the seed fixed, the subsample draw (and thus the scores and NA positions)
# must be identical across runs -- otherwise the UI diagnostic would flicker per run.
test_that("exp_kmeans silhouette subsample is reproducible across runs (same seed) (#36126)", {
  run <- function() {
    exp_kmeans(mtcars, cyl, mpg, hp, centers = 3, max_nrow = NULL,
               silhouette_sample_size = 12, seed = 1)$model[[1]]$silhouette
  }
  s1 <- run()
  s2 <- run()
  expect_equal(s1$silhouette_score, s2$silhouette_score)            # identical scores incl. NAs
  expect_equal(which(is.na(s1$silhouette_score)),
               which(is.na(s2$silhouette_score)))                   # identical subsample draw
  expect_equal(sum(!is.na(s1$silhouette_score)), 12)                # subsampling actually happened
})

# #4: When `mat` is supplied, iterate_silhouette must use it (and the supplied dist) and
# ignore df/normalize_data for the matrix build -- this is the kmeans-input/dist consistency
# fix. A bogus df must not change the result; it must match building from the equivalent df.
test_that("iterate_silhouette uses supplied mat/dist and ignores df for the matrix build (#36126)", {
  mat <- scale(as.matrix(mtcars[, c("cyl", "mpg", "hp")]))
  mat[is.nan(mat)] <- 0
  d <- stats::dist(mat)
  df_equiv <- as.data.frame(mat)  # already normalized, so normalize_data=FALSE rebuilds `mat`

  res_built <- iterate_silhouette(df_equiv, max_centers = 4, normalize_data = FALSE, seed = 42)
  res_mat   <- iterate_silhouette(df_equiv, max_centers = 4, normalize_data = FALSE,
                                  seed = 42, mat = mat, dist = d)
  expect_equal(res_mat, res_built)  # supplying mat/dist matches the build-from-df path

  bogus_df <- data.frame(a = seq_len(nrow(mat)), b = rev(seq_len(nrow(mat))))
  res_bogus <- iterate_silhouette(bogus_df, max_centers = 4, normalize_data = TRUE,
                                  seed = 42, mat = mat, dist = d)
  expect_equal(res_bogus, res_mat)  # df is ignored entirely when mat is supplied
})
