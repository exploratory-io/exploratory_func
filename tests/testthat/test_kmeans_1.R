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
