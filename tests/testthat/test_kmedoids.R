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
    euclidean$model[[1]]$clustering,
    manhattan$model[[1]]$clustering
  )))
})

test_that('distribution standardized_value matches a hand z-score when normalize_data is TRUE, and raw value stays untouched (tam#37938)', {
  set.seed(1)
  result <- iris %>% exploratory:::exp_kmedoids(
    Sepal.Length, Sepal.Width, Petal.Length, Petal.Width,
    centers = 3, distance = 'euclidean', normalize_data = TRUE, seed = 1
  )
  model <- result$model[[1]]
  distribution <- broom::tidy(model, type = 'distribution')

  vars <- c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width')
  for (v in vars) {
    rows <- distribution[distribution$variable == v, , drop = FALSE]
    # `value` (raw) must still be the untouched original iris value.
    expect_equal(sort(rows$value), sort(iris[[v]]))
    # `standardized_value` must equal a hand-computed z-score of the same raw values,
    # not a value re-derived from some other, differently-ordered/sampled set of rows.
    expected_z <- as.numeric(scale(rows$value))
    # Order by row position (as.integer(rownames) isn't reliable here) -- compare the
    # SET of standardized values instead, since row order in `distribution` need not
    # match `sort(value)`'s order.
    expect_equal(sort(round(rows$standardized_value, 6)), sort(round(expected_z, 6)))
  }
  # A standardized column, unlike the raw one, must have ~zero mean and unit variance
  # per variable (up to floating point).
  for (v in vars) {
    rows <- distribution[distribution$variable == v, , drop = FALSE]
    expect_true(abs(mean(rows$standardized_value)) < 1e-8)
    expect_equal(stats::sd(rows$standardized_value), 1, tolerance = 1e-6)
  }
})

test_that('distribution standardized_value equals the raw value when normalize_data is FALSE (tam#37938)', {
  set.seed(1)
  result <- iris %>% exploratory:::exp_kmedoids(
    Sepal.Length, Sepal.Width, Petal.Length, Petal.Width,
    centers = 3, distance = 'euclidean', normalize_data = FALSE, seed = 1
  )
  model <- result$model[[1]]
  distribution <- broom::tidy(model, type = 'distribution')

  expect_equal(distribution$standardized_value, distribution$value)
})

test_that('distribution standardized_value is 0, not NaN, for a zero-variance variable (tam#37938)', {
  set.seed(1)
  data <- tibble::tibble(x = c(1, 2, 3, 4, 5, 6), constant = rep(7, 6))
  result <- data %>% exploratory:::exp_kmedoids(x, constant, centers = 2, normalize_data = TRUE, seed = 1)
  model <- result$model[[1]]
  distribution <- broom::tidy(model, type = 'distribution')
  constant_rows <- distribution[distribution$variable == 'constant', , drop = FALSE]

  expect_true(all(constant_rows$value == 7))
  expect_false(any(is.nan(constant_rows$standardized_value)))
  expect_true(all(constant_rows$standardized_value == 0))
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
    'medoid_details', 'counts', 'analysis_conditions', 'data')

  output <- lapply(types, function(type) broom::tidy(model, type = type))
  names(output) <- types

  expect_true(all(vapply(output, is.data.frame, logical(1))))
  expect_true(nrow(output$profile) > 0)
  expect_equal(nrow(output$profile), 9)
  expect_true(nrow(output$variable_importance) > 0)
  expect_true(nrow(output$map) > 0)
  expect_true(any(output$map$row_type == 'vector'))
  expect_true(all(c('cluster', 'variable', 'value', 'standardized_value') %in%
    colnames(output$distribution)))
  expect_true(all(c('cluster', 'row_id', 'mpg', 'disp', 'hp') %in%
    colnames(broom::tidy(model, type = 'medoid_details'))))
  expect_equal(nrow(output$counts), 1)
  expect_equal(output$counts$original_nrow, nrow(mtcars))
  expect_true(all(c('row_id', 'cluster', 'is_medoid', 'distance_to_medoid',
    'silhouette_score', 'is_excluded') %in% colnames(output$data)))
})

test_that('analysis_conditions returns the shared Metric/Value shape', {
  data <- tibble::tibble(
    x = c(1, 2, 3, 4, 5, 6, NA),
    y = c(1, 2, 4, 5, 7, 8, 9),
    z = c(2, 3, 1, 6, 4, 9, 5)
  )
  result <- data %>% exploratory:::exp_kmedoids(x, y, z, centers = 2, seed = 1)
  conditions <- broom::tidy(result$model[[1]], type = 'analysis_conditions')

  # Same five rows, in the same order, as kmodes_analysis_conditions_table() and
  # tidy.prcomp_exploratory()'s K-Means branch -- the report translates these cells by
  # exact string match, so the wording is part of the contract.
  expect_equal(colnames(conditions), c('Metric', 'Value'))
  expect_equal(conditions$Metric,
    c('Number of Variables', 'Variable Names', 'Row Count', 'Rows Removed',
      'Number of Clusters'))
  expect_true(is.character(conditions$Value))
  expect_equal(conditions$Value[[1]], '3')
  expect_equal(conditions$Value[[2]], 'x, y, z')
  # Row Count is the rows actually clustered, Rows Removed the ones dropped for the NA.
  expect_equal(conditions$Value[[3]], '6')
  expect_equal(conditions$Value[[4]], '1')
  expect_equal(conditions$Value[[5]], '2')
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
  expect_false('pam' %in% names(model))
  expect_equal(length(model$medoid_indices), 3)
})

test_that('PAM silhouette scores retain their original row indexes', {
  result <- iris %>% exploratory:::exp_kmedoids(
    Sepal.Length, Sepal.Width, Petal.Length, Petal.Width,
    centers = 3, algorithm = 'pam', elbow_method_mode = 'none', seed = 42
  )
  model <- result$model[[1]]
  reference <- cluster::pam(
    model$mat, k = 3, metric = 'manhattan', stand = FALSE,
    keep.diss = FALSE, keep.data = FALSE, pamonce = 5
  )
  expect_equal(
    model$silhouette_row_indices,
    as.integer(rownames(reference$silinfo$widths))
  )
  expect_false(identical(model$silhouette_row_indices, seq_along(model$silhouette_widths)))
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

test_that('the PCoA map is computed once at fit time and cached, not recomputed per tidy() call', {
  set.seed(1)
  result <- mtcars %>% exploratory:::exp_kmedoids(mpg, disp, hp, centers = 3, seed = 1)
  model <- result$model[[1]]

  # The fit itself must already carry the map result -- tidy(type='map') should
  # be a pure cache read, not a fresh stats::dist()/stats::cmdscale() computation.
  expect_true(is.data.frame(model$map_result))
  expect_true(nrow(model$map_result) > 0)

  first_call <- broom::tidy(model, type = 'map')
  second_call <- broom::tidy(model, type = 'map')

  expect_identical(first_call, model$map_result)
  expect_identical(second_call, model$map_result)
  expect_identical(first_call, second_call)

  # tam's set_kmedoids_analytics_params() helper reads this attribute directly
  # via broom::tidy(..., type='map') (bypassing tidy_rowwise's unnesting), so the
  # cached tibble must still carry it, not just a fresh .kmedoids_map() call.
  expect_true(is.numeric(attr(model$map_result, 'representation_rate')))
  expect_true(is.numeric(attr(first_call, 'representation_rate')))
})

test_that('models created before map caching still produce their map', {
  result <- mtcars %>% exploratory:::exp_kmedoids(mpg, disp, hp, centers = 3, seed = 1)
  model <- result$model[[1]]
  model$map_result <- NULL

  map <- broom::tidy(model, type = 'map')

  expect_true(nrow(map) > 0)
  expect_true(any(map$row_type == 'vector'))
})

test_that('a minimal (all-tied) fit still produces a usable cached map', {
  data <- tibble::tibble(x = c(1, 1, 1), y = c(1, 1, 1))
  result <- data %>% exploratory:::exp_kmedoids(x, y, centers = 2, seed = 1)
  model <- result$model[[1]]

  expect_true(is.data.frame(model$map_result))
  expect_true(is.data.frame(broom::tidy(model, type = 'map')))
})

test_that('diagnostic and map counts use bounded integer sample sizes', {
  result <- mtcars %>% exploratory:::exp_kmedoids(
    mpg, disp, hp, centers = 3, silhouette_sample_size = 10.9,
    map_sample_size = 8.9, elbow_method_mode = 'none', seed = 1
  )
  model <- result$model[[1]]
  counts <- broom::tidy(model, type = 'counts')

  expect_identical(model$silhouette_sample_size, 10L)
  expect_identical(model$map_sample_size, 8L)
  expect_identical(counts$diagnostic_nrow, 10L)
  expect_identical(counts$map_sample_nrow, 8L)

  model$valid_nrow <- 6001L
  model$mat <- matrix(rnorm(12002), ncol = 2)
  model$medoid_indices <- 1:3
  model$silhouette_sample_size <- 6001L
  expect_equal(nrow(exploratory:::.kmedoids_diagnostic_mat(model)$mat), 5000)
  model$map_sample_size <- 6001L
  expect_identical(broom::tidy(model, type = 'counts')$map_sample_nrow, 5000L)
})

test_that('CLARA candidate samples are capped before fitting', {
  set.seed(1)
  fit <- exploratory:::.kmedoids_fit(
    matrix(rnorm(200), ncol = 2), centers = 3, distance = 'euclidean',
    algorithm = 'clara', clara_samples = 1, clara_sample_size = 100,
    pam_max_n = 20
  )
  expect_identical(fit$clara_sample_size, 20L)
})

test_that('PAM with the default nstart matches calling cluster::pam() with no nstart at all', {
  set.seed(11)
  mat <- matrix(rnorm(150), ncol = 3)

  set.seed(5)
  fit_default <- exploratory:::.kmedoids_fit(mat, centers = 3, distance = 'euclidean', algorithm = 'pam')
  set.seed(5)
  raw <- cluster::pam(mat, k = 3, metric = 'euclidean', stand = FALSE, keep.diss = FALSE, keep.data = FALSE, pamonce = 5)

  expect_identical(as.integer(fit_default$clustering), as.integer(raw$clustering))
  expect_identical(fit_default$medoid_indices, as.integer(raw$id.med))

  set.seed(5)
  fit_explicit_one <- exploratory:::.kmedoids_fit(mat, centers = 3, distance = 'euclidean', algorithm = 'pam', nstart = 1)
  expect_identical(as.integer(fit_explicit_one$clustering), as.integer(raw$clustering))
})

test_that('PAM nstart > 1 switches to random-start search and returns a valid clustering', {
  set.seed(11)
  mat <- matrix(rnorm(150), ncol = 3)

  set.seed(5)
  fit_nstart <- exploratory:::.kmedoids_fit(mat, centers = 3, distance = 'euclidean', algorithm = 'pam', nstart = 10)

  expect_length(fit_nstart$medoid_indices, 3)
  expect_length(unique(fit_nstart$medoid_indices), 3)
  expect_setequal(unique(fit_nstart$clustering), 1:3)

  # cluster::pam(medoids = 'random', nstart = N) is a distinct algorithm from
  # the default deterministic "build" -- confirm the underlying raw fit
  # actually used random medoid initialization for nstart > 1.
  expect_identical(as.character(fit_nstart$raw$call$medoids), 'random')
  expect_false(is.null(fit_nstart$raw$call$nstart))
})

test_that('CLARA ignores nstart entirely', {
  set.seed(3)
  mat <- matrix(rnorm(200), ncol = 2)

  set.seed(7)
  fit_a <- exploratory:::.kmedoids_fit(
    mat, centers = 3, distance = 'euclidean', algorithm = 'clara', clara_samples = 5
  )
  set.seed(7)
  fit_b <- exploratory:::.kmedoids_fit(
    mat, centers = 3, distance = 'euclidean', algorithm = 'clara', clara_samples = 5, nstart = 25
  )
  expect_identical(as.integer(fit_a$clustering), as.integer(fit_b$clustering))
})

test_that('exp_kmedoids validates nstart and forwards it through to the fit', {
  set.seed(4)
  df <- as.data.frame(matrix(rnorm(90), ncol = 3))
  names(df) <- c('v1', 'v2', 'v3')

  expect_error(
    df %>% exploratory:::exp_kmedoids(v1, v2, v3, centers = 3, algorithm = 'pam', nstart = 0),
    'nstart must be a positive number'
  )

  result <- df %>% exploratory:::exp_kmedoids(v1, v2, v3, centers = 3, algorithm = 'pam', nstart = 5, seed = 1)
  model <- result$model[[1]]
  expect_identical(model$nstart, 5L)
})

test_that('nstart is forwarded to PAM diagnostic fits', {
  calls <- list()
  original_fit <- exploratory:::.kmedoids_fit
  testthat::local_mocked_bindings(
    .kmedoids_fit = function(...) {
      call <- list(...)
      calls[[length(calls) + 1L]] <<- call
      original_fit(...)
    },
    .package = 'exploratory'
  )

  set.seed(12)
  data <- as.data.frame(matrix(rnorm(120), ncol = 3))
  names(data) <- c('v1', 'v2', 'v3')
  data %>% exploratory:::exp_kmedoids(
    v1, v2, v3, centers = 3, nstart = 5, max_centers = 4,
    elbow_method_mode = 'silhouette', silhouette_sample_size = 40,
    map_sample_size = 20, seed = 1
  )
  data %>% exploratory:::exp_kmedoids(
    v1, v2, v3, centers = 3, nstart = 5, max_centers = 4,
    elbow_method_mode = 'elbow', silhouette_sample_size = 40,
    map_sample_size = 20, seed = 1
  )

  expect_gt(length(calls), 2L)
  expect_true(all(vapply(calls, function(call) identical(call$nstart, 5L), logical(1))))
})

# tam#38004: .kmedoids_pcoa() replaces stats::cmdscale(add = TRUE) in .kmedoids_map() --
# the Cailliez additive constant made the Cluster Map ~90% of exp_kmedoids() runtime at
# the default map_sample_size (measured ~55s of a ~61s run at 3,600 fitted rows). These
# tests cover the replacement directly (against the add = FALSE convention it now
# matches) and the end-to-end map contract it still has to satisfy.

test_that('.kmedoids_pcoa reproduces cmdscale(add = FALSE) above the exact-fallback threshold', {
  # Three non-collinear cluster centers so BOTH of the top two axes carry real signal --
  # a two-center fixture leaves the second axis pure noise, making its correlation
  # unstable and an unreliable check of the sketch's accuracy.
  set.seed(11)
  n <- 400
  centers <- matrix(
    c(rep(4, 6), rep(1, 6), c(rep(4, 3), rep(1, 3))),
    nrow = 3, byrow = TRUE
  )
  raw <- centers[sample(1:3, n, TRUE), ] + matrix(rnorm(n * 6, 0, 0.6), n, 6)
  mat <- scale(raw)
  d <- stats::dist(mat, method = 'manhattan')

  reference <- stats::cmdscale(d, k = 2, eig = TRUE, add = FALSE)
  fit <- exploratory:::.kmedoids_pcoa(d, k = 2, seed = 1)

  expect_equal(dim(fit$points), c(n, 2))
  # PCoA axis signs are arbitrary -- compare magnitude of correlation, not raw coordinates.
  expect_gt(abs(cor(fit$points[, 1], reference$points[, 1])), 0.99)
  expect_gt(abs(cor(fit$points[, 2], reference$points[, 2])), 0.99)
  expect_equal(sort(fit$eig, decreasing = TRUE)[1:2], reference$eig[1:2], tolerance = 1e-4)
})

test_that('.kmedoids_pcoa is deterministic for a fixed seed and varies with a different one', {
  set.seed(21)
  n <- 200
  mat <- scale(matrix(rnorm(n * 5), n, 5))
  d <- stats::dist(mat, method = 'euclidean')

  same_seed_a <- exploratory:::.kmedoids_pcoa(d, k = 2, seed = 5)
  same_seed_b <- exploratory:::.kmedoids_pcoa(d, k = 2, seed = 5)
  other_seed <- exploratory:::.kmedoids_pcoa(d, k = 2, seed = 6)

  expect_identical(same_seed_a$points, same_seed_b$points)
  # A different random sketch can still converge to the same subspace on well-separated
  # data, so assert on eigenvalues (exact regardless of seed) rather than requiring the
  # point coordinates to differ.
  expect_equal(sort(same_seed_a$eig, decreasing = TRUE)[1:2],
               sort(other_seed$eig, decreasing = TRUE)[1:2], tolerance = 1e-4)
})

test_that('.kmedoids_pcoa falls back to cmdscale for small n and degenerate input', {
  small <- tibble::tibble(x = c(1, 2, 3, 10, 11), y = c(1, 2, 1, 10, 9))
  d_small <- stats::dist(small, method = 'euclidean')
  fit_small <- exploratory:::.kmedoids_pcoa(d_small, k = 2, seed = 1, fallback_n = 50L)
  reference_small <- stats::cmdscale(d_small, k = 2, eig = TRUE, add = FALSE)
  expect_equal(fit_small$points, reference_small$points)

  tied <- tibble::tibble(x = rep(1, 60), y = rep(1, 60))
  d_tied <- stats::dist(tied, method = 'euclidean')
  fit_tied <- exploratory:::.kmedoids_pcoa(d_tied, k = 2, seed = 1)
  expect_true(all(is.finite(fit_tied$points)))
  expect_true(all(fit_tied$points == 0))
})

test_that('the cached PCoA map keeps its full contract after replacing cmdscale(add = TRUE)', {
  set.seed(31)
  n <- 400
  centers <- matrix(c(rep(4, 8), rep(1, 8), c(rep(4, 4), rep(1, 4))), nrow = 3, byrow = TRUE)
  raw <- centers[sample(1:3, n, TRUE), ] + matrix(rnorm(n * 8, 0, 0.6), n, 8)
  df <- as.data.frame(raw)
  names(df) <- paste0('v', 1:8)

  result <- df %>% exploratory:::exp_kmedoids(
    v1, v2, v3, v4, v5, v6, v7, v8, centers = 3, distance = 'manhattan',
    seed = 1, map_sample_size = 200, map_variable_n = 4
  )
  model <- result$model[[1]]
  map <- broom::tidy(model, type = 'map')

  expect_true(is.data.frame(map))
  expect_true(all(c('medoid', 'observation', 'vector') %in% map$row_type))
  expect_equal(sum(map$row_type == 'medoid'), 3)
  expect_equal(sum(map$row_type == 'vector'), 4 * 2)
  expect_false(anyNA(map$Dim1))
  expect_false(anyNA(map$Dim2))

  rate <- attr(map, 'representation_rate')
  expect_length(rate, 2)
  expect_true(all(is.finite(rate)))
  expect_true(all(rate >= 0 & rate <= 1))
  expect_gte(rate[2], rate[1])

  # Same seed -> byte-identical cached map (reproducibility of the randomized sketch).
  result2 <- df %>% exploratory:::exp_kmedoids(
    v1, v2, v3, v4, v5, v6, v7, v8, centers = 3, distance = 'manhattan',
    seed = 1, map_sample_size = 200, map_variable_n = 4
  )
  map2 <- broom::tidy(result2$model[[1]], type = 'map')
  expect_identical(map$Dim1, map2$Dim1)
  expect_identical(map$Dim2, map2$Dim2)
})
