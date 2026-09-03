# how to run this test (requires R/devtools -- NOT executed in this session,
# no R/Rscript binary was available; verified only by careful code tracing
# against hclust_dendrogram_segments()/exp_hclust_variables() -- see the
# tam#38161 design doc for the explicit caveat):
# devtools::test(filter="hclust_variables")
context("test hierarchical clustering of variables")

test_that("exp_hclust_variables basic structure", {
  set.seed(1)
  df <- mtcars
  model_df <- exp_hclust_variables(df, mpg, cyl, disp, hp, drat, wt, qsec,
                                   method = "pearson", linkage_method = "complete",
                                   n_clusters = 3, max_nrow = NULL)

  conditions <- model_df %>% tidy_rowwise(model, type = "summary")
  expect_equal(nrow(conditions), 6)
  expect_true(all(c("Metric", "Value") %in% colnames(conditions)))
  expect_equal(conditions$Value[conditions$Metric == "Number of Variables"], "7")
  expect_equal(conditions$Value[conditions$Metric == "Correlation Method"], "pearson")

  membership <- model_df %>% tidy_rowwise(model, type = "cluster_membership")
  expect_equal(sort(membership$Variable), sort(c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec")))
  expect_equal(nrow(membership), 7)
  expect_true(all(grepl("^Cluster [0-9]+$", membership$Cluster)))
  # cluster ids used must be exactly 1..n_clusters (display-order renumbering
  # never skips or duplicates an id).
  used_ids <- sort(unique(as.integer(sub("Cluster ", "", membership$Cluster))))
  expect_equal(used_ids, 1:3)

  cor_mat <- model_df %>% tidy_rowwise(model, type = "correlation_matrix")
  expect_equal(nrow(cor_mat), 7)
  expect_equal(ncol(cor_mat), 8) # Variable + 7 numeric columns
  # Diagonal is always 1 (self-correlation).
  for (v in c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec")) {
    expect_equal(cor_mat[cor_mat$Variable == v, v], 1)
  }

  segments <- model_df %>% tidy_rowwise(model, type = "dendrogram_segments")
  # n leaves = 7 -> n - 1 = 6 merges -> 3 segments each.
  expect_equal(nrow(segments), 3 * 6)
  expect_true(all(c("merge_step", "x0", "y0", "x1", "y1") %in% colnames(segments)))

  leaves <- model_df %>% tidy_rowwise(model, type = "dendrogram_leaves")
  expect_equal(nrow(leaves), 7)
  expect_equal(sort(leaves$`Display Order`), 1:7)
})

test_that("exp_hclust_variables distance is 1 - correlation", {
  # Two perfectly correlated columns (b = 2*a) must have distance ~0 --
  # cutting into 2 clusters should NOT split them apart from each other,
  # while an unrelated / negatively related third column should end up
  # separate.
  set.seed(42)
  a <- rnorm(200)
  df <- data.frame(
    a = a,
    b = a * 2 + rnorm(200, sd = 0.001), # near-perfectly correlated with a
    c = -a + rnorm(200, sd = 0.001)     # near-perfectly ANTI-correlated with a (1 - r ~ 2, not ~0)
  )
  model_df <- exp_hclust_variables(df, a, b, c, method = "pearson",
                                   linkage_method = "average", n_clusters = 2)
  membership <- model_df %>% tidy_rowwise(model, type = "cluster_membership")
  cl <- setNames(membership$Cluster, membership$Variable)
  # a and b must land in the same cluster (distance ~ 0).
  expect_equal(cl[["a"]], cl[["b"]])
})

test_that("exp_hclust_variables excludes a constant (zero-variance) column", {
  set.seed(7)
  df <- data.frame(
    a = rnorm(50),
    b = rnorm(50),
    c = rnorm(50),
    const = 5 # zero variance -> correlation with everything is NA/undefined
  )
  model_df <- exp_hclust_variables(df, a, b, c, const, method = "pearson", n_clusters = 2)
  conditions <- model_df %>% tidy_rowwise(model, type = "summary")
  expect_equal(conditions$Value[conditions$Metric == "Number of Variables"], "3")
  expect_equal(conditions$Value[conditions$Metric == "Excluded Variables"], "const")

  membership <- model_df %>% tidy_rowwise(model, type = "cluster_membership")
  expect_false("const" %in% membership$Variable)
  expect_equal(nrow(membership), 3)
})

test_that("exp_hclust_variables errors when fewer than 2 usable variables remain", {
  df <- data.frame(a = rnorm(20), const1 = 1, const2 = 2)
  expect_error(
    exp_hclust_variables(df, a, const1, const2, method = "pearson", n_clusters = 2),
    "At least 2 variables"
  )
})

test_that("exp_hclust_variables rejects ward.D2 linkage (not Euclidean-valid on 1 - correlation)", {
  df <- mtcars
  expect_error(
    exp_hclust_variables(df, mpg, cyl, disp, method = "pearson", linkage_method = "ward.D2", n_clusters = 2),
    "Ward"
  )
})

test_that("exp_hclust_variables validates n_clusters bounds", {
  df <- mtcars
  expect_error(
    exp_hclust_variables(df, mpg, cyl, disp, method = "pearson", n_clusters = 1),
    "Number of Clusters"
  )
  expect_error(
    exp_hclust_variables(df, mpg, cyl, disp, method = "pearson", n_clusters = 10),
    "cannot exceed"
  )
})

test_that("exp_hclust_variables requires at least 2 selected variables", {
  df <- mtcars
  expect_error(exp_hclust_variables(df, mpg), "2 or more variables")
})

test_that("exp_hclust_variables each linkage method runs (complete/average/single)", {
  df <- mtcars
  for (lm in c("complete", "average", "single")) {
    model_df <- exp_hclust_variables(df, mpg, cyl, disp, hp, drat, wt,
                                     method = "pearson", linkage_method = lm, n_clusters = 2)
    membership <- model_df %>% tidy_rowwise(model, type = "cluster_membership")
    expect_equal(nrow(membership), 6)
  }
})

test_that("exp_hclust_variables supports auto/spearman/kendall correlation methods", {
  df <- mtcars
  for (m in c("auto", "pearson", "spearman", "kendall")) {
    model_df <- exp_hclust_variables(df, mpg, cyl, disp, hp, method = m, n_clusters = 2)
    membership <- model_df %>% tidy_rowwise(model, type = "cluster_membership")
    expect_equal(nrow(membership), 4)
  }
})

test_that("exp_hclust_variables handles missing values via pairwise-complete correlation", {
  set.seed(3)
  df <- mtcars
  df$mpg[c(1, 3, 5)] <- NA
  df$hp[c(2, 4)] <- NA
  model_df <- exp_hclust_variables(df, mpg, cyl, disp, hp, drat, method = "pearson", n_clusters = 2)
  membership <- model_df %>% tidy_rowwise(model, type = "cluster_membership")
  expect_equal(nrow(membership), 5)
})

test_that("exp_hclust_variables works with exactly 2 variables (n = 2 edge case)", {
  df <- mtcars
  model_df <- exp_hclust_variables(df, mpg, hp, method = "pearson", n_clusters = 2)
  membership <- model_df %>% tidy_rowwise(model, type = "cluster_membership")
  expect_equal(nrow(membership), 2)
  segments <- model_df %>% tidy_rowwise(model, type = "dendrogram_segments")
  expect_equal(nrow(segments), 3) # n=2 -> 1 merge -> 3 segments
})

test_that("exp_hclust_variables respects max_nrow sampling", {
  set.seed(11)
  df <- data.frame(a = rnorm(1000), b = rnorm(1000), c = rnorm(1000))
  model_df <- exp_hclust_variables(df, a, b, c, method = "pearson", n_clusters = 2, max_nrow = 100)
  conditions <- model_df %>% tidy_rowwise(model, type = "summary")
  expect_equal(conditions$Value[conditions$Metric == "Number of Rows"], "100")
})

test_that("exp_hclust_variables works with complex / multibyte / symbol column names", {
  # Canonical stress-test name per tam workflow.md rule 7 / .claude/rules/workflow.md
  # (datablog): every R-generating feature must be verified against a name with
  # spaces, multibyte characters, and symbols.
  stress_name <- "航空 会社 !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表"
  df <- mtcars
  colnames(df)[1] <- stress_name
  model_df <- exp_hclust_variables(df, !!rlang::sym(stress_name), cyl, disp, hp,
                                   method = "pearson", n_clusters = 2)
  membership <- model_df %>% tidy_rowwise(model, type = "cluster_membership")
  expect_true(stress_name %in% membership$Variable)

  cor_mat <- model_df %>% tidy_rowwise(model, type = "correlation_matrix")
  expect_true(stress_name %in% colnames(cor_mat))
  expect_true(stress_name %in% cor_mat$Variable)
})

test_that("hclust_dendrogram_segments produces a valid binary-merge geometry", {
  set.seed(5)
  m <- matrix(rnorm(60), ncol = 6)
  colnames(m) <- letters[1:6]
  cor_mat <- cor(m)
  hc <- stats::hclust(stats::as.dist(1 - cor_mat), method = "complete")
  segs <- hclust_dendrogram_segments(hc)

  n <- length(hc$order)
  expect_equal(nrow(segs), 3 * (n - 1))

  # Every segment's x is within [0, max(hc$height)].
  expect_true(all(segs$x0 >= 0 & segs$x0 <= max(hc$height) + 1e-9))
  expect_true(all(segs$x1 >= 0 & segs$x1 <= max(hc$height) + 1e-9))

  # The vertical connector for the LAST merge step (the root) should span the
  # full leaf y-range on one side or the other -- i.e. its two horizontal
  # branches together must eventually reach y = 1 and y = n somewhere in the
  # segment table (root merge covers everyone).
  expect_true(any(segs$y0 == 1 | segs$y1 == 1))
  expect_true(any(segs$y0 == n | segs$y1 == n))

  # Every merge_step from 1..(n-1) is represented exactly 3 times.
  expect_equal(as.integer(table(segs$merge_step)), rep(3L, n - 1))
})

test_that("exp_hclust_variables with a strange column name (backtick-needing)", {
  df <- mtcars %>% dplyr::rename(`Cy l` = cyl)
  model_df <- exp_hclust_variables(df, `Cy l`, mpg, hp, method = "pearson", n_clusters = 2)
  membership <- model_df %>% tidy_rowwise(model, type = "cluster_membership")
  expect_true("Cy l" %in% membership$Variable)
})
