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
  expect_true(all(c('mpg', 'disp', 'hp') %in% colnames(summary_data)))
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
    'representative_values', 'distribution', 'cohesion', 'map',
    'medoid_details', 'counts', 'data')

  output <- lapply(types, function(type) broom::tidy(model, type = type))
  names(output) <- types

  expect_true(all(vapply(output, is.data.frame, logical(1))))
  expect_true(nrow(output$profile) > 0)
  expect_true(nrow(output$variable_importance) > 0)
  expect_true(nrow(output$map) > 0)
  expect_true(any(output$map$row_type == 'vector'))
  expect_true(all(c('cluster', 'variable', 'value') %in% colnames(output$distribution)))
  expect_true(all(c('cluster', 'row_id', 'mpg', 'disp', 'hp') %in%
    colnames(broom::tidy(model, type = 'medoid_details'))))
  expect_equal(nrow(output$counts), 1)
  expect_equal(output$counts$original_nrow, nrow(mtcars))
  expect_true(all(c('row_id', 'cluster', 'is_medoid', 'distance_to_medoid',
    'silhouette_score', 'is_excluded') %in% colnames(output$data)))
})

test_that('non-numeric variables are rejected clearly', {
  expect_error(
    iris %>% exploratory:::exp_kmedoids(Species, Sepal.Length, centers = 2),
    'requires numeric variables'
  )
})

test_that('algorithm selection uses compact PAM output for small data', {
  result <- mtcars %>% exploratory:::exp_kmedoids(
    mpg, disp, hp, centers = 3, algorithm = 'pam', elbow_method_mode = 'none', seed = 7
  )
  model <- result$model[[1]]

  expect_equal(model$requested_algorithm, 'pam')
  expect_equal(model$effective_algorithm, 'pam')
  expect_false(model$is_approximate)
  expect_null(model$pam$diss)
  expect_null(model$pam$data)
  expect_equal(length(model$medoid_indices), 3)
})

test_that('CLARA is available and preserves full-data assignments', {
  result <- iris %>% exploratory:::exp_kmedoids(
    Sepal.Length, Sepal.Width, Petal.Length, Petal.Width,
    centers = 3, algorithm = 'clara', clara_samples = 3,
    clara_sample_size = 50, silhouette_sample_size = 50,
    elbow_method_mode = 'none', map_sample_size = 20, seed = 42
  )
  model <- result$model[[1]]
  output_data <- broom::tidy(model, type = 'data')

  expect_equal(model$effective_algorithm, 'clara')
  expect_true(model$is_approximate)
  expect_equal(length(model$clustering), nrow(iris))
  expect_equal(sum(!is.na(output_data$cluster)), nrow(iris))
  expect_equal(sum(!is.na(output_data$silhouette_score)), length(model$silhouette_widths))
  expect_equal(nrow(broom::tidy(model, type = 'medoid_details')), 3)
})

test_that('auto switches to CLARA and explicit PAM fails safely for large data', {
  set.seed(19)
  data <- tibble::tibble(x = rnorm(5001), y = rnorm(5001))

  auto <- data %>% exploratory:::exp_kmedoids(
    x, y, centers = 3, algorithm = 'auto', elbow_method_mode = 'none',
    silhouette_sample_size = 100, map_sample_size = 20, seed = 19
  )
  expect_equal(auto$model[[1]]$effective_algorithm, 'clara')

  expect_error(
    data %>% exploratory:::exp_kmedoids(
      x, y, centers = 3, algorithm = 'pam', elbow_method_mode = 'none', seed = 19
    ),
    'Select algorithm = "clara"'
  )
})

test_that('diagnostic and map sample sizes bound expensive outputs', {
  result <- mtcars %>% exploratory:::exp_kmedoids(
    mpg, disp, hp, centers = 3, silhouette_sample_size = 10,
    map_sample_size = 8, map_variable_n = 2, elbow_method_mode = 'silhouette', seed = 1
  )
  model <- result$model[[1]]
  map <- broom::tidy(model, type = 'map')
  counts <- broom::tidy(model, type = 'counts')

  expect_lte(sum(map$row_type == 'observation'), 8)
  expect_lte(sum(map$row_type == 'vector') / 2, 2)
  expect_equal(counts$diagnostic_nrow, min(model$valid_nrow, 10))
  expect_equal(counts$map_sample_nrow, min(model$valid_nrow, max(8, model$centers)))
})
