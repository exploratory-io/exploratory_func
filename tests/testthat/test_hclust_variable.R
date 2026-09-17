context('Hierarchical clustering of variables')

test_that('exp_hclust_variable returns the shared dendrogram contract with variable leaves', {
  result <- mtcars %>% exploratory:::exp_hclust_variable(
    mpg, cyl, disp, hp, drat, wt, qsec, centers = 3, max_interactive_k = 6
  )
  model <- result$model[[1]]
  nodes <- broom::tidy(model, type = 'dendrogram_nodes')
  p <- 7L

  expect_s3_class(model, 'hclust_variable_exploratory')
  expect_false(inherits(model, 'hclust_exploratory'))
  expect_equal(nrow(nodes), 2L * p - 1L)
  expect_equal(sum(nodes$node_type == 'leaf'), p)
  expect_equal(nodes$node_id[which.max(nodes$size)], 2L * p - 2L)
  expect_equal(nodes$label[nodes$node_type == 'leaf'],
               c('mpg', 'cyl', 'disp', 'hp', 'drat', 'wt', 'qsec'))
  expect_equal(nodes$row_id[nodes$node_type == 'leaf'], nodes$label[nodes$node_type == 'leaf'])
  expect_equal(sum(!is.na(nodes$metadata_json)), 1L)
  metadata <- jsonlite::fromJSON(nodes$metadata_json[[1]])
  expect_equal(metadata$metadata$leafKind, 'variable')
  expect_equal(metadata$metadata$distanceMethod, 'correlation')
  expect_equal(metadata$metadata$n, p)
  expect_equal(metadata$rootId, 2L * p - 2L)
  expect_equal(sort(as.integer(names(model$cuts))), 2:6)
})

test_that('the distance is 1 - r and the tree matches stats::hclust on it', {
  data <- mtcars[c('mpg', 'disp', 'hp', 'wt', 'qsec')]
  for (method in c('pearson', 'spearman')) {
    for (linkage in c('average', 'complete', 'single')) {
      result <- data %>% exploratory:::exp_hclust_variable(
        dplyr::everything(), centers = 2, cor_method = method, linkage = linkage,
        elbow_method_mode = 'none'
      )
      model <- result$model[[1]]
      expected <- stats::hclust(stats::as.dist(1 - stats::cor(data, method = method)), method = linkage)
      expect_equal(as.numeric(model$hclust$height), as.numeric(expected$height), tolerance = 1e-10,
                   info = paste(method, linkage))
      expect_equal(as.matrix(model$distance_object),
                   1 - stats::cor(data, method = method), tolerance = 1e-10, check.attributes = FALSE)
    }
  }
  # Negatively correlated variables stay apart (1 - r, not 1 - |r|).
  negative <- tibble::tibble(a = 1:20, b = -(1:20) + rep(c(0, 0.1), 10), c = (1:20) + rep(c(0.2, 0), 10))
  model <- exploratory:::exp_hclust_variable(negative, a, b, c, centers = 2,
                                             elbow_method_mode = 'none')$model[[1]]
  data_out <- broom::tidy(model, type = 'data')
  expect_equal(data_out$cluster[data_out$variable == 'a'], data_out$cluster[data_out$variable == 'c'])
  expect_false(data_out$cluster[data_out$variable == 'a'] == data_out$cluster[data_out$variable == 'b'])
})

test_that('dendrogram cuts agree with cutree membership for every interactive K', {
  result <- mtcars %>% exploratory:::exp_hclust_variable(
    dplyr::everything(), centers = 4, max_interactive_k = 10, elbow_method_mode = 'none'
  )
  model <- result$model[[1]]
  nodes <- model$dendrogram_nodes
  for (k in seq.int(2L, model$max_interactive_k)) {
    roots <- model$cuts[[as.character(k)]]
    roots <- roots[order(nodes$leaf_start[roots + 1L])]
    actual <- integer(model$valid_nrow)
    for (cluster_index in seq_along(roots)) {
      root <- nodes[roots[[cluster_index]] + 1L, ]
      leaves <- model$leaf_order[(root$leaf_start + 1L):(root$leaf_end + 1L)]
      actual[leaves + 1L] <- cluster_index
    }
    expect_equal(actual, model$memberships[[as.character(k)]], info = paste('K =', k))
    # Same partition as cutree (labels differ: display order vs cutree order).
    expect_equal(as.integer(table(actual, stats::cutree(model$hclust, k)) > 0) %>% sum(), k,
                 info = paste('K =', k))
  }
})

test_that('report tables have the documented columns and consistent values', {
  model <- mtcars %>% exploratory:::exp_hclust_variable(
    mpg, cyl, disp, hp, drat, wt, qsec, centers = 3
  ) %>% .$model %>% .[[1]]

  summary <- broom::tidy(model, type = 'summary')
  expect_equal(names(summary), c('cluster', 'n_variables', 'variables', 'avg_within_cor',
                                 'avg_silhouette', 'min_silhouette'))
  expect_equal(summary$cluster, 1:3)
  expect_equal(sum(summary$n_variables), 7L)

  conditions <- broom::tidy(model, type = 'analysis_conditions')
  expect_equal(conditions$Metric, c('Number of Variables', 'Variable Names', 'Row Count',
                                    'Number of Clusters', 'Correlation Method', 'Distance',
                                    'Linkage', 'Missing Values'))
  expect_equal(conditions$Value[[3]], '32')

  merge <- broom::tidy(model, type = 'merge_distance')
  expect_equal(nrow(merge), 6L)
  expect_equal(merge$merge_distance, rev(model$hclust$height)[1:6])

  silhouette <- broom::tidy(model, type = 'silhouette')
  expect_equal(silhouette$center, 2:6)

  cor_long <- broom::tidy(model, type = 'cor')
  expect_equal(nrow(cor_long), 49L)
  expect_equal(levels(cor_long$pair.name.x), model$selected_cols[model$leaf_order + 1L])
  expect_equal(cor_long$correlation[cor_long$pair.name.x == 'mpg' & cor_long$pair.name.y == 'wt'],
               stats::cor(mtcars$mpg, mtcars$wt))

  data_out <- broom::tidy(model, type = 'data')
  expect_equal(names(data_out), c('variable', 'cluster', 'display_order', 'avg_cor_own',
                                  'nearest_cluster', 'avg_cor_nearest', 'silhouette'))
  expect_equal(data_out$display_order, 0:6)
  expect_true(all(is.na(data_out$nearest_cluster) | data_out$nearest_cluster != data_out$cluster))
  expect_equal(nrow(broom::tidy(model, type = 'unknown')), 0L)
  expect_equal(broom::glance(model)$n_variables, 7L)

  # The summary list and the data table agree on membership.
  for (cluster_id in summary$cluster) {
    expect_equal(strsplit(summary$variables[[cluster_id]], ', ')[[1]],
                 data_out$variable[data_out$cluster == cluster_id])
  }
})

test_that('silhouette sweep follows elbow_method_mode but the chosen cut is always reported', {
  model <- mtcars %>% exploratory:::exp_hclust_variable(
    mpg, cyl, disp, hp, drat, wt, qsec, centers = 3, elbow_method_mode = 'none'
  ) %>% .$model %>% .[[1]]
  expect_equal(nrow(broom::tidy(model, type = 'silhouette')), 0L)
  expect_true(any(is.finite(broom::tidy(model, type = 'summary')$avg_silhouette)))
  expect_error(exploratory:::exp_hclust_variable(mtcars, mpg, cyl, elbow_method_mode = 'elbow'))
})

test_that('pairwise missing values, dropped and constant variables, and bounds', {
  data <- tibble::tibble(x = c(1, 2, NA, 4, 5, 6), y = c(2, 1, 3, Inf, 5, 7),
                         z = c(6, 5, 4, 3, NA, 1), empty = NA_real_)
  expect_warning(
    model <- exploratory:::exp_hclust_variable(data, x, y, z, empty, centers = 2)$model[[1]],
    'no finite value: empty'
  )
  expect_equal(model$selected_cols, c('x', 'y', 'z'))
  expect_equal(model$cor_mat['x', 'z'], stats::cor(data$x, data$z, use = 'pairwise.complete.obs'))

  expect_error(exploratory:::exp_hclust_variable(tibble::tibble(a = 1:5, b = 1), a, b, centers = 2),
               'b has the same value in every row')
  expect_error(exploratory:::exp_hclust_variable(mtcars, mpg), 'At least two numeric variables')
  expect_error(exploratory:::exp_hclust_variable(mtcars, mpg, wt, centers = 3),
               'centers cannot be greater than the number of variables')
  expect_error(exploratory:::exp_hclust_variable(iris, Sepal.Length, Species), 'requires numeric variables')

  two <- exploratory:::exp_hclust_variable(mtcars, mpg, wt, centers = 2, max_interactive_k = 10)$model[[1]]
  expect_equal(two$max_interactive_k, 2L)
  expect_equal(nrow(broom::tidy(two, type = 'silhouette')), 0L)
  expect_true(all(is.na(broom::tidy(two, type = 'summary')$avg_within_cor)))
  expect_true(all(is.na(broom::tidy(two, type = 'data')$avg_cor_own)))
})

test_that('complex column names survive as leaf labels', {
  name <- '航空 会社 !"#$%&\'()*+, -./:;<=>?@[]^_`{|}~ 表'
  data <- mtcars[c('mpg', 'wt', 'hp')]
  names(data)[[1]] <- name
  model <- exploratory:::exp_hclust_variable(data, dplyr::everything(), centers = 2)$model[[1]]
  nodes <- broom::tidy(model, type = 'dendrogram_nodes')
  expect_true(name %in% nodes$label)
  expect_true(name %in% broom::tidy(model, type = 'data')$variable)
  expect_true(name %in% levels(broom::tidy(model, type = 'cor')$pair.name.x))
})
