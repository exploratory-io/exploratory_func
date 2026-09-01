context('Hierarchical clustering analytics')

test_that('exp_hclust returns the dendrogram model contract', {
  result <- mtcars %>% exploratory:::exp_hclust(
    mpg, disp, hp, centers = 3, max_interactive_k = 6,
    map_sample_size = 10, seed = 1
  )
  model <- result$model[[1]]
  nodes <- broom::tidy(model, type = 'dendrogram_nodes')

  expect_s3_class(model, 'hclust_exploratory')
  expect_equal(nrow(nodes), 2L * model$valid_nrow - 1L)
  expect_equal(sum(nodes$node_type == 'leaf'), model$valid_nrow)
  expect_equal(sum(nodes$node_type == 'internal'), model$valid_nrow - 1L)
  expect_equal(nodes$node_id[which.max(nodes$size)], 2L * model$valid_nrow - 2L)
  expect_equal(nodes$size[which.max(nodes$size)], model$valid_nrow)
  expect_equal(nodes$leaf_start[which.max(nodes$size)], 0L)
  expect_equal(nodes$leaf_end[which.max(nodes$size)], model$valid_nrow - 1L)
  expect_equal(sum(!is.na(nodes$metadata_json)), 1L)
  expect_equal(sum(!is.na(nodes$cuts_json)), 1L)
  expect_equal(sort(as.integer(names(model$cuts))), 2:6)
  expect_equal(unname(vapply(model$cuts, length, integer(1))), as.integer(names(model$cuts)))
})

test_that('dendrogram cuts agree with cutree membership for every interactive K', {
  result <- iris %>% exploratory:::exp_hclust(
    Sepal.Length, Sepal.Width, Petal.Length, Petal.Width,
    centers = 4, max_interactive_k = 8, elbow_method_mode = 'none', seed = 3
  )
  model <- result$model[[1]]
  nodes <- model$dendrogram_nodes
  for (k in seq.int(2L, model$max_interactive_k)) {
    roots <- model$cuts[[as.character(k)]]
    roots <- roots[order(nodes$leaf_start[roots + 1L])]
    expected <- model$memberships[[as.character(k)]]
    actual <- integer(model$valid_nrow)
    for (cluster_index in seq_along(roots)) {
      root <- nodes[roots[[cluster_index]] + 1L, ]
      leaves <- model$leaf_order[(root$leaf_start + 1L):(root$leaf_end + 1L)]
      actual[leaves + 1L] <- cluster_index
    }
    expect_equal(actual, expected, info = paste('K =', k))
  }
})

test_that('all supported distance and linkage combinations work', {
  for (distance in c('euclidean', 'manhattan')) {
    for (linkage in c('ward.D2', 'complete', 'average', 'single')) {
      if (linkage == 'ward.D2' && distance == 'manhattan') next
      result <- mtcars %>% exploratory:::exp_hclust(
        mpg, disp, hp, distance = distance, linkage = linkage,
        centers = 3, max_interactive_k = 4, elbow_method_mode = 'none',
        map_sample_size = 8, seed = 1
      )
      expect_equal(result$model[[1]]$distance, distance)
      expect_equal(result$model[[1]]$linkage, linkage)
    }
  }
  expect_error(
    exploratory:::exp_hclust(mtcars, mpg, disp, distance = 'manhattan', linkage = 'ward.D2'),
    'ward.D2 linkage requires the euclidean distance'
  )
})

test_that('missing rows and sampling preserve source row ids', {
  data <- tibble::tibble(x = c(1, 2, NA, 4, 5), y = c(2, 1, 3, Inf, 5))
  result <- data %>% exploratory:::exp_hclust(
    x, y, centers = 2, takeSample = FALSE, elbow_method_mode = 'none', seed = 1
  )
  model <- result$model[[1]]
  output <- broom::tidy(model, type = 'data')
  expect_equal(output$row_id, as.character(seq_len(nrow(data))))
  expect_equal(output$is_excluded, c(FALSE, FALSE, TRUE, TRUE, FALSE))
  expect_equal(broom::tidy(model, type = 'analysis_conditions')$Value[[4]], '2')
})

test_that('gathered data is observation-variable long data and augment works by default', {
  result <- mtcars %>% exploratory:::exp_hclust(
    mpg, disp, hp, centers = 3, elbow_method_mode = 'none', seed = 1
  )
  model <- result$model[[1]]
  gathered <- broom::tidy(model, type = 'gathered_data')

  expect_equal(nrow(gathered), model$valid_nrow * length(model$selected_cols))
  expect_named(gathered, c('row_id', 'cluster', 'key', 'value', 'standardized_value'))
  expect_equal(gathered$row_id[seq_len(length(model$selected_cols))],
               rep(model$row_ids[[1]], length(model$selected_cols)))
  expect_equal(unname(gathered$key[seq_len(length(model$selected_cols))]),
               unname(model$selected_cols))

  augmented <- broom::augment(model)
  expect_equal(nrow(augmented), model$valid_nrow)
  expect_true('.cluster' %in% colnames(augmented))
})

test_that('invalid numeric inputs fail with interpretable messages', {
  expect_error(
    exploratory:::exp_hclust(iris, Species, centers = 2),
    'requires numeric variables'
  )
  expect_error(
    exploratory:::exp_hclust(tibble::tibble(x = rep(1, 4), y = 1:4), x, y, centers = 2),
    'non-constant finite values'
  )
  expect_error(
    exploratory:::exp_hclust(tibble::tibble(x = 1), x, centers = 2),
    'At least two valid rows'
  )
})

# tam#38157: four defects found by the tam-side analytics harness
# (src/test/analytics-harness/specs/hclust.json).
context("exp_hclust defects found by the analytics harness (tam#38157)")

hclust_harness_df <- function() {
  set.seed(38157)
  n <- 60
  base <- data.frame(
    spend = c(rnorm(20, 1200, 90), rnorm(20, 2000, 90), rnorm(20, 2800, 90)),
    visits = c(rnorm(20, 5, 0.6), rnorm(20, 10, 0.6), rnorm(20, 7, 0.6)),
    stay = c(rnorm(20, 23, 2), rnorm(20, 34, 2), rnorm(20, 24, 2))
  )
  base$all_na <- NA_real_
  base$constant <- 7
  base
}

test_that("a variable with no finite value is dropped and named, not blamed on the rows", {
  df <- hclust_harness_df()
  expect_warning(
    model <- df %>% exp_hclust(all_na, spend, visits, centers = 3, max_interactive_k = 5),
    "all_na"
  )
  # The analytics must still run on the variables that DO carry data. Before
  # this, complete.cases() dropped every row and the user was told "At least two
  # valid rows are required" -- blaming the rows for one unusable column.
  fit <- model$model[[1]]
  expect_equal(unname(sort(fit$selected_cols)), c("spend", "visits"))
  expect_equal(fit$valid_nrow, nrow(df))
})

test_that("an all-unusable selection fails naming the columns, not the rows", {
  df <- hclust_harness_df()
  expect_error(
    df %>% exp_hclust(all_na, centers = 2),
    "No usable variable"
  )
  expect_error(df %>% exp_hclust(all_na, centers = 2), "all_na")
})

test_that("a constant variable's error names the column", {
  df <- hclust_harness_df()
  expect_error(
    df %>% exp_hclust(constant, spend, visits, centers = 3),
    "constant"
  )
  # ...and still says what the requirement is.
  expect_error(
    df %>% exp_hclust(constant, spend, visits, centers = 3),
    "non-constant finite values"
  )
})

test_that("the merge distance table follows max_interactive_k, not max_centers", {
  df <- hclust_harness_df()
  # max_centers stays at its default 10 throughout, so any change here is
  # max_interactive_k reaching the table.
  for (k in c(2L, 4L, 8L)) {
    model <- df %>% exp_hclust(spend, visits, stay, centers = 2, max_interactive_k = k)
    md <- model %>% tidy_rowwise(model, type = "merge_distance")
    expect_equal(nrow(md), k - 1L,
                 info = paste("max_interactive_k =", k))
    expect_equal(md$cluster[[1]], "2 → 1")
  }
})

test_that("the chosen cut's silhouette is reported whatever diagnostic sweep was asked for", {
  df <- hclust_harness_df()
  # elbow_method_mode selects which sweep runs OVER k. It says nothing about
  # whether the quality of the cut the user actually chose is reported, and the
  # Cluster Summary that shows it is not gated on the mode.
  for (mode in c("silhouette", "elbow", "none")) {
    model <- df %>% exp_hclust(spend, visits, stay, centers = 3,
                               max_interactive_k = 5, elbow_method_mode = mode)
    summary_df <- model %>% tidy_rowwise(model, type = "summary")
    expect_equal(sum(is.na(summary_df$avg_silhouette)), 0L,
                 info = paste("elbow_method_mode =", mode))
  }
})

test_that("the diagnostic sweep itself still follows the mode", {
  # The fix above must not make every mode compute every sweep.
  df <- hclust_harness_df()
  sil <- df %>% exp_hclust(spend, visits, centers = 3, max_interactive_k = 5,
                           elbow_method_mode = "silhouette")
  elb <- df %>% exp_hclust(spend, visits, centers = 3, max_interactive_k = 5,
                           elbow_method_mode = "elbow")
  expect_gt(nrow(sil %>% tidy_rowwise(model, type = "silhouette")), 0L)
  expect_equal(nrow(elb %>% tidy_rowwise(model, type = "silhouette")), 0L)
  expect_gt(nrow(elb %>% tidy_rowwise(model, type = "elbow")), 0L)
})
