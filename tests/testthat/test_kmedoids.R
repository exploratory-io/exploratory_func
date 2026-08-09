context('K-Medoids analytics')

test_that('exp_kmedoids returns a pam model and summary data', {
  set.seed(1)
  result <- mtcars %>% exploratory:::exp_kmedoids(mpg, disp, hp, centers = 3, seed = 1)
  model <- result$model[[1]]
  summary_data <- broom::tidy(model, type = 'summary', with_excluded_rows = TRUE)

  expect_s3_class(model, 'pam_exploratory')
  expect_equal(nrow(summary_data), 3)
  expect_equal(sum(summary_data$size), nrow(mtcars))
  expect_true(all(c('avg_distance_to_medoid', 'avg_silhouette', 'medoid_row_id') %in%
    colnames(summary_data)))
})

test_that('exp_kmedoids supports both distance metrics and standardization', {
  euclidean <- iris %>% exploratory:::exp_kmedoids(
    Sepal.Length, Sepal.Width, Petal.Length, Petal.Width,
    centers = 3, distance = 'euclidean', normalize_data = FALSE, seed = 42
  )
  manhattan <- iris %>% exploratory:::exp_kmedoids(
    Sepal.Length, Sepal.Width, Petal.Length, Petal.Width,
    centers = 3, distance = 'manhattan', normalize_data = TRUE, seed = 42
  )

  expect_equal(euclidean$model[[1]]$distance, 'euclidean')
  expect_equal(manhattan$model[[1]]$distance, 'manhattan')
  expect_false(isTRUE(all.equal(
    euclidean$model[[1]]$pam$clustering,
    manhattan$model[[1]]$pam$clustering
  )))
})

test_that('excluded rows are represented without failing the model', {
  data <- tibble::tibble(x = c(1, 2, 3, NA), y = c(1, 2, 4, 5))
  result <- data %>% exploratory:::exp_kmedoids(x, y, centers = 2, seed = 1)
  summary_data <- broom::tidy(result$model[[1]], type = 'summary', with_excluded_rows = TRUE)
  excluded <- summary_data[is.na(summary_data$cluster), , drop = FALSE]

  expect_equal(nrow(excluded), 1)
  expect_equal(excluded$size, 1)
  expect_equal(excluded$pct_size, 0.25)
})

test_that('report tidy types are available', {
  result <- mtcars %>% exploratory:::exp_kmedoids(mpg, disp, hp, centers = 3, seed = 1)
  model <- result$model[[1]]
  types <- c('profile', 'silhouette', 'elbow', 'variable_importance',
    'representative_values', 'distribution', 'cohesion', 'map', 'data')

  output <- lapply(types, function(type) broom::tidy(model, type = type))
  names(output) <- types

  expect_true(all(vapply(output, is.data.frame, logical(1))))
  expect_true(nrow(output$profile) > 0)
  expect_true(nrow(output$variable_importance) > 0)
  expect_true(nrow(output$map) > 0)
  expect_true(all(c('cluster', 'variable', 'value') %in% colnames(output$distribution)))
})

test_that('non-numeric variables are rejected clearly', {
  expect_error(
    iris %>% exploratory:::exp_kmedoids(Species, Sepal.Length, centers = 2),
    'requires numeric variables'
  )
})
