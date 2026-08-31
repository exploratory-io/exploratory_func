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
