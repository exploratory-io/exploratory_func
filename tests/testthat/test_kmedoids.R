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
  # iris$Species is a PLAIN factor, so it gets the unordered-factor message (tam#39875).
  expect_error(
    iris %>% exploratory:::exp_kmedoids(Species, Sepal.Length, centers = 2),
    "Column Species is an unordered factor. Use Set Value Order with 'Ordered' checked, or use K-Modes for unordered categories.",
    fixed = TRUE
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
  # representation_rate is each axis' OWN share; the cumulative pair is a separate attribute (tam#39564).
  cumulative <- attr(map, 'cumulative_representation_rate')
  expect_length(cumulative, 2)
  expect_lte(sum(rate), 1 + 1e-9)
  expect_equal(cumulative, c(rate[1], rate[1] + rate[2]))
  expect_gte(cumulative[2], cumulative[1])

  # Same seed -> byte-identical cached map (reproducibility of the randomized sketch).
  result2 <- df %>% exploratory:::exp_kmedoids(
    v1, v2, v3, v4, v5, v6, v7, v8, centers = 3, distance = 'manhattan',
    seed = 1, map_sample_size = 200, map_variable_n = 4
  )
  map2 <- broom::tidy(result2$model[[1]], type = 'map')
  expect_identical(map$Dim1, map2$Dim1)
  expect_identical(map$Dim2, map2$Dim2)
})

test_that('too few rows for the requested cluster count is reported in the caller\'s terms', {
  # cluster::pam() aborts with 'number of cluster centres must lie between 1
  # and nrow(x)-1', which names an argument the Analytics UI never shows and
  # counts rows AFTER missing-value removal -- so the number in the message
  # does not match what the user sees in the table either.
  df <- tibble::tibble(a = c(1, 2, 3), b = c(4, 5, 6))

  expect_error(
    exploratory:::exp_kmedoids(df, a, b, centers = 3, seed = 1),
    'must be greater than the number of clusters')

  # The boundary is nrow > centers, not nrow >= centers: pam needs at least one
  # non-medoid row to assign.
  expect_error(exploratory:::exp_kmedoids(df, a, b, centers = 2, seed = 1), NA)
})

test_that('a stale clara_sample_size does not block a PAM run', {
  # The Analytics dialog keeps the CLARA fields populated after the user
  # switches the algorithm away from CLARA, and PAM never reads this value.
  # Validating it unconditionally failed a legitimate run with an error naming
  # a parameter that was no longer on screen.
  df <- tibble::tibble(a = rnorm(60), b = rnorm(60))

  expect_error(
    exploratory:::exp_kmedoids(df, a, b, centers = 3, algorithm = 'pam',
                               clara_sample_size = 2, seed = 1),
    NA)

  # 'auto' resolves to pam or clara from the row count inside the fit, and
  # supplies its own sample size when it picks clara -- so it must not be
  # validated up front either.
  expect_error(
    exploratory:::exp_kmedoids(df, a, b, centers = 3, algorithm = 'auto',
                               clara_sample_size = 2, seed = 1),
    NA)

  # Non-regression: when CLARA is the chosen algorithm the value IS read, and
  # an unusable one is still rejected.
  expect_error(
    exploratory:::exp_kmedoids(df, a, b, centers = 3, algorithm = 'clara',
                               clara_sample_size = 2, seed = 1),
    'clara_sample_size must be greater than the number of clusters')
})

test_that('silhouette_sample_size is only validated when a silhouette is computed', {
  df <- tibble::tibble(a = rnorm(60), b = rnorm(60))

  for (mode in c('none', 'elbow')) {
    expect_error(
      exploratory:::exp_kmedoids(df, a, b, centers = 3, elbow_method_mode = mode,
                                 silhouette_sample_size = 2, seed = 1),
      NA,
      info = paste('nothing reads silhouette_sample_size in mode', mode))
  }

  # Non-regression: the silhouette mode does read it.
  expect_error(
    exploratory:::exp_kmedoids(df, a, b, centers = 3, elbow_method_mode = 'silhouette',
                               silhouette_sample_size = 2, seed = 1),
    'silhouette_sample_size must be greater than the number of clusters')
})

test_that('profile variable_order groups variables by peak cluster, regardless of profile_top_n (tam#38492)', {
  # The Cluster Profile radar orders its category axis by variable_order so that each
  # cluster's characteristic variables form one contiguous arc. profile_top_n keeps only
  # each cluster's most characteristic variables, so a variable can survive for some
  # clusters and not others -- the order must therefore be decided BEFORE that filter,
  # or the peak cluster is read off a partial set and the arc breaks apart.
  set.seed(11)
  n <- 400
  n_var <- 12
  n_cluster <- 3
  group <- sample(seq_len(n_cluster), n, replace = TRUE)
  values <- matrix(3, n, n_var)
  for (i in seq_len(n)) {
    block <- ((group[i] - 1) * 4 + 1):((group[i] - 1) * 4 + 4)
    values[i, block] <- values[i, block] + 1.2
  }
  noisy <- values + matrix(stats::rnorm(n * n_var, 0, 0.8), n, n_var)
  df <- as.data.frame(matrix(round(pmin(5, pmax(1, as.vector(noisy)))), n, n_var))
  names(df) <- paste0('V', sprintf('%02d', seq_len(n_var)))

  fit <- function(show_all, top_n) {
    result <- df %>% exploratory:::exp_kmedoids(
      !!!rlang::syms(names(df)),
      centers = n_cluster, seed = 1,
      profile_show_all = show_all, profile_top_n = top_n
    )
    broom::tidy(result$model[[1]], type = 'profile')
  }
  order_of <- function(profile) {
    unique(profile[order(profile$variable_order), ][['variable']])
  }
  # Independent oracle: the cluster whose standardized mean is highest for that variable,
  # taken from every (variable, cluster) pair -- never from the filtered frame.
  full <- fit(TRUE, 12)
  peak <- full %>%
    dplyr::group_by(variable) %>%
    dplyr::slice_max(standardized_mean, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()
  runs_around_axis <- function(variables) {
    clusters <- peak$cluster[match(variables, peak$variable)]
    sum(clusters != c(clusters[-1], clusters[1]))
  }

  narrowed <- fit(FALSE, 4)
  expect_lt(nrow(narrowed), nrow(full))            # the filter really did narrow the frame
  expect_true(any(table(narrowed$variable) < n_cluster))  # ... per (variable, cluster), not per variable

  expect_equal(order_of(narrowed), order_of(full))
  expect_equal(runs_around_axis(order_of(narrowed)), dplyr::n_distinct(peak$cluster))
})

test_that('profile variable_order keeps the peak cluster primary when a z-score exceeds 10', {
  rows <- tibble::tibble(
    variable = rep(c('early_peak', 'late_peak'), each = 2),
    cluster = rep(1:2, 2),
    standardized_mean = c(0.1, 0, 0, 100)
  )

  ordered <- exploratory:::.kmedoids_profile_variable_order(rows)

  expect_equal(ordered$variable, c('early_peak', 'late_peak'))
  expect_equal(ordered$order, c(1, 2))
})

test_that('profile carries each cluster\'s row count as cluster_size (tam#39474)', {
  data <- tibble::tibble(
    a = c(1, 1.1, 0.9, 1, 1.2, 0.8, 10, 10.1, 9.9, 10),
    b = c(5, 5.1, 4.9, 5, 5.2, 4.8, 0, 0.1, -0.1, 0)
  )
  model <- exploratory:::exp_kmedoids(data, a, b, centers = 2, elbow_method_mode = 'none', seed = 1)$model[[1]]
  rows <- broom::tidy(model, type = 'profile')
  expect_true('cluster_size' %in% names(rows))
  expect_true(is.integer(rows$cluster_size))
  sizes <- rows %>% dplyr::distinct(cluster, cluster_size) %>% dplyr::arrange(cluster)
  expect_equal(sizes$cluster_size, as.integer(table(model$clustering)))
  expect_equal(sum(sizes$cluster_size), nrow(data))
})

# ---------------------------------------------------------------------------------------------
# Ordered-factor (ordinal) variables, tam#39875
# ---------------------------------------------------------------------------------------------

.kmed_agree_levels <- c('Strongly disagree', 'Disagree', 'Neutral', 'Agree', 'Strongly agree')
.kmed_room_levels <- c('Small', 'Medium', 'Large')

# Two well-separated archetypes (12 rows each). Level POSITIONS are hand-written so the tests
# never go through factor()/as.integer() to learn what the expected position is.
.kmed_likert_positions <- function() {
  list(
    agree = c(c(1, 2, 1, 2, 1, 2, 2, 1, 1, 2, 1, 2), c(5, 4, 5, 4, 4, 5, 5, 4, 4, 5, 5, 4)),
    room = c(c(1, 1, 2, 1, 2, 1, 1, 2, 1, 1, 2, 1), c(3, 3, 2, 3, 3, 2, 3, 3, 3, 2, 3, 3)),
    x = c(seq(1, 3.2, length.out = 12), seq(7, 9.2, length.out = 12))
  )
}

.kmed_likert_fixture <- function() {
  pos <- .kmed_likert_positions()
  tibble::tibble(
    id = seq_along(pos$x),
    q_agree = factor(.kmed_agree_levels[pos$agree], levels = .kmed_agree_levels, ordered = TRUE),
    q_room = factor(.kmed_room_levels[pos$room], levels = .kmed_room_levels, ordered = TRUE),
    x = pos$x
  )
}

# Independent expectation: (position - 1) / (L - 1), L == 1 -> 0, position found with match().
.kmed_expected_encoding <- function(col) {
  levs <- levels(col)
  position <- match(as.character(col), levs)
  if (length(levs) == 1) return(ifelse(is.na(position), NA_real_, 0))
  (position - 1) / (length(levs) - 1)
}

test_that('ordered-only variables fit, are encoded to 0-1 positions, and keep levels on the model (tam#39875)', {
  data <- .kmed_likert_fixture()
  pos <- .kmed_likert_positions()
  model <- exploratory:::exp_kmedoids(data, q_agree, q_room, centers = 2, seed = 1)$model[[1]]

  expect_s3_class(model, 'pam_exploratory')
  expect_equal(model$ordinal_levels, list(q_agree = .kmed_agree_levels, q_room = .kmed_room_levels))
  expect_equal(unname(model$mat[, 'q_agree']), (pos$agree - 1) / 4)
  expect_equal(unname(model$mat[, 'q_room']), (pos$room - 1) / 2)
  # Ordinal columns are NEVER standardized, even though normalize_data defaults to TRUE.
  expect_true(all(model$mat >= 0 & model$mat <= 1))
  # original_fit_mat keeps the raw positions 1..L.
  expect_true(is.numeric(model$original_fit_mat))
  expect_equal(unname(model$original_fit_mat[, 'q_agree']), pos$agree)
  expect_equal(unname(model$original_fit_mat[, 'q_room']), pos$room)
  # The two archetypes land in two different clusters.
  expect_equal(length(unique(model$clustering[1:12])), 1)
  expect_equal(length(unique(model$clustering[13:24])), 1)
  expect_false(model$clustering[[1]] == model$clustering[[13]])
})

test_that('mixed numeric + ordered: numeric is z-scored, ordinal stays 0-1 (tam#39875)', {
  data <- .kmed_likert_fixture()
  pos <- .kmed_likert_positions()
  model <- exploratory:::exp_kmedoids(data, q_agree, x, q_room, centers = 2, seed = 1)$model[[1]]

  expect_equal(unname(model$mat[, 'x']), as.numeric(scale(pos$x)))
  expect_equal(unname(model$mat[, 'q_agree']), (pos$agree - 1) / 4)
  expect_equal(unname(model$mat[, 'q_room']), (pos$room - 1) / 2)
  # original_fit_mat: raw numeric value for x, positions for the ordinal columns.
  expect_equal(unname(model$original_fit_mat[, 'x']), pos$x)
  expect_equal(unname(model$original_fit_mat[, 'q_agree']), pos$agree)
  expect_equal(names(model$ordinal_levels), c('q_agree', 'q_room'))

  raw <- exploratory:::exp_kmedoids(data, q_agree, x, q_room, centers = 2, seed = 1,
    normalize_data = FALSE)$model[[1]]
  expect_equal(unname(raw$mat[, 'x']), pos$x)
  expect_equal(unname(raw$mat[, 'q_agree']), (pos$agree - 1) / 4)
})

test_that('a plain factor is rejected with the unordered-factor message (tam#39875)', {
  data <- .kmed_likert_fixture()
  data$q_plain <- factor(as.character(data$q_agree))
  expect_error(
    exploratory:::exp_kmedoids(data, q_agree, q_plain, x, centers = 2),
    "Column q_plain is an unordered factor. Use Set Value Order with 'Ordered' checked, or use K-Modes for unordered categories.",
    fixed = TRUE
  )
})

test_that('character, logical and Date columns get the numeric-or-ordered message (tam#39875)', {
  data <- .kmed_likert_fixture()
  msg <- 'K-Medoids requires numeric or ordered factor variables.'
  data$ch <- as.character(data$q_agree)
  data$lg <- data$x > 5
  data$dt <- as.Date('2026-01-01') + seq_len(nrow(data))
  for (col in c('ch', 'lg', 'dt')) {
    expect_error(
      exploratory:::exp_kmedoids(data, q_agree, !!rlang::sym(col), centers = 2),
      msg, fixed = TRUE
    )
  }
})

test_that('values outside the levels become NA, are excluded and are counted in Rows Removed (tam#39875)', {
  data <- .kmed_likert_fixture()
  answers <- as.character(data$q_agree)
  answers[c(3, 15)] <- "Don't know"
  data$q_agree <- factor(answers, levels = .kmed_agree_levels, ordered = TRUE)
  expect_equal(sum(is.na(data$q_agree)), 2)

  model <- exploratory:::exp_kmedoids(data, q_agree, x, q_room, centers = 2, seed = 1)$model[[1]]
  expect_equal(model$excluded_nrow, 2)
  expect_equal(model$valid_nrow, 22)
  expect_equal(nrow(model$mat), 22)
  expect_equal(nrow(model$original_fit_mat), 22)
  expect_equal(model$valid_indices, setdiff(1:24, c(3, 15)))

  conditions <- broom::tidy(model, type = 'analysis_conditions')
  expect_equal(conditions$Value[conditions$Metric == 'Rows Removed'], '2')
  expect_equal(conditions$Value[conditions$Metric == 'Row Count'], '22')

  summary_data <- broom::tidy(model, type = 'summary', with_excluded_rows = TRUE)
  excluded <- summary_data[is.na(summary_data$cluster), , drop = FALSE]
  expect_equal(excluded$size, 2)
})

test_that('unused middle and unused top levels keep their positions (tam#39875)', {
  levs <- c('a', 'b', 'c', 'd', 'e')
  # 'c' (middle) and 'e' (top) are never answered; L stays 5.
  answers <- c('a', 'b', 'a', 'b', 'a', 'b', 'd', 'd', 'd', 'b', 'a', 'd')
  data <- tibble::tibble(
    q = factor(answers, levels = levs, ordered = TRUE),
    x = c(1, 2, 1, 2, 1, 2, 9, 9.5, 9, 2, 1, 9.5)
  )
  expect_false(any(c('c', 'e') %in% answers))
  model <- exploratory:::exp_kmedoids(data, q, x, centers = 2, seed = 1)$model[[1]]

  expected_position <- c(1, 2, 1, 2, 1, 2, 4, 4, 4, 2, 1, 4)
  expect_equal(unname(model$original_fit_mat[, 'q']), expected_position)
  expect_equal(unname(model$mat[, 'q']), (expected_position - 1) / 4)
  expect_equal(unname(model$mat[, 'q']), .kmed_expected_encoding(data$q))
  expect_equal(max(model$mat[, 'q']), 0.75) # top used level is 'd' = 3/4, not 1
  expect_equal(model$ordinal_levels$q, levs)
})

test_that('a single-level ordered column encodes to 0 (tam#39875)', {
  data <- .kmed_likert_fixture()
  data$q_one <- factor(rep('Only', nrow(data)), levels = 'Only', ordered = TRUE)
  model <- exploratory:::exp_kmedoids(data, q_agree, q_one, x, centers = 2, seed = 1)$model[[1]]
  expect_true(all(model$mat[, 'q_one'] == 0))
  expect_true(all(model$original_fit_mat[, 'q_one'] == 1))
})

test_that('summary and medoid_details show level labels for ordinal medoid columns (tam#39875)', {
  data <- .kmed_likert_fixture()
  pos <- .kmed_likert_positions()
  model <- exploratory:::exp_kmedoids(data, q_agree, x, q_room, centers = 2, seed = 1)$model[[1]]
  medoids <- model$medoid_indices

  for (type in c('summary', 'medoid_details')) {
    tbl <- broom::tidy(model, type = type)
    expect_true(is.character(tbl$q_agree), info = type)
    expect_true(is.character(tbl$q_room), info = type)
    expect_true(is.numeric(tbl$x), info = type)
    expect_equal(tbl$q_agree, .kmed_agree_levels[pos$agree[medoids]], info = type)
    expect_equal(tbl$q_room, .kmed_room_levels[pos$room[medoids]], info = type)
    expect_equal(tbl$x, pos$x[medoids], info = type)
    expect_true(all(tbl$q_agree %in% .kmed_agree_levels), info = type)
  }
})

test_that('representative_values and profile carry ordinal labels, positions and lower medians (tam#39875)', {
  # Cluster B has an EVEN size (4) with positions 4,4,5,5 -> lower median is position 4 ('Agree'),
  # never the non-level 4.5. Cluster A (6 rows) is clearly separate.
  data <- tibble::tibble(
    q = factor(.kmed_agree_levels[c(1, 1, 2, 1, 2, 1, 4, 4, 5, 5)], levels = .kmed_agree_levels,
      ordered = TRUE),
    x = c(1, 1.1, 0.9, 1, 1.2, 0.8, 9, 9.1, 8.9, 9)
  )
  model <- exploratory:::exp_kmedoids(data, q, x, centers = 2, elbow_method_mode = 'none',
    seed = 1)$model[[1]]
  big <- model$clustering[[10]]
  expect_equal(sum(model$clustering == big), 4)
  small <- setdiff(1:2, big)

  for (type in c('representative_values', 'profile')) {
    tbl <- broom::tidy(model, type = type)
    expect_true(all(c('is_ordinal', 'medoid_label', 'cluster_median_label',
      'overall_median_label') %in% colnames(tbl)), info = type)
    expect_true(is.logical(tbl$is_ordinal), info = type)
    expect_true(is.character(tbl$medoid_label), info = type)
    expect_true(is.numeric(tbl$medoid), info = type)
    expect_true(is.numeric(tbl$cluster_median), info = type)

    q_big <- tbl[tbl$variable == 'q' & tbl$cluster == big, , drop = FALSE]
    expect_true(q_big$is_ordinal, info = type)
    expect_equal(unname(q_big$cluster_median), 4, info = type)            # lower median position
    expect_equal(q_big$cluster_median_label, 'Agree', info = type)
    expect_equal(unname(q_big$cluster_mean), 4.5, info = type)            # mean stays an average position
    expect_equal(q_big$medoid_label, .kmed_agree_levels[unname(q_big$medoid)], info = type)
    # overall: positions 1,1,2,1,2,1,4,4,5,5 -> sorted 1 1 1 1 2 2 4 4 5 5, n = 10, lower median = 2.
    expect_equal(unname(q_big$overall_median), 2, info = type)
    expect_equal(q_big$overall_median_label, 'Disagree', info = type)
    expect_equal(unname(q_big$overall_mean), 2.6, info = type)

    q_small <- tbl[tbl$variable == 'q' & tbl$cluster == small, , drop = FALSE]
    expect_equal(unname(q_small$cluster_median), 1, info = type)          # 1,1,1,1,2,2 -> lower median 1
    expect_equal(q_small$cluster_median_label, 'Strongly disagree', info = type)

    x_rows <- tbl[tbl$variable == 'x', , drop = FALSE]
    expect_false(any(x_rows$is_ordinal), info = type)
    expect_true(all(is.na(x_rows$medoid_label)), info = type)
    expect_true(all(is.na(x_rows$cluster_median_label)), info = type)
    expect_true(all(is.na(x_rows$overall_median_label)), info = type)
  }
})

test_that('distribution standardized_value is a per-column z-score for ordinal columns when normalizing (tam#39875)', {
  data <- .kmed_likert_fixture()
  pos <- .kmed_likert_positions()
  model <- exploratory:::exp_kmedoids(data, q_agree, x, centers = 2, seed = 1,
    normalize_data = TRUE)$model[[1]]
  distribution <- broom::tidy(model, type = 'distribution')

  agree_rows <- distribution[distribution$variable == 'q_agree', , drop = FALSE]
  expect_equal(agree_rows$value, pos$agree)                       # value = original positions
  expect_equal(agree_rows$standardized_value, as.numeric(scale(pos$agree)))
  expect_equal(mean(agree_rows$standardized_value), 0, tolerance = 1e-8)
  expect_equal(stats::sd(agree_rows$standardized_value), 1, tolerance = 1e-8)
  x_rows <- distribution[distribution$variable == 'x', , drop = FALSE]
  expect_equal(x_rows$standardized_value, as.numeric(scale(pos$x)))

  raw <- exploratory:::exp_kmedoids(data, q_agree, x, centers = 2, seed = 1,
    normalize_data = FALSE)$model[[1]]
  raw_dist <- broom::tidy(raw, type = 'distribution')
  expect_equal(raw_dist$standardized_value, raw_dist$value)
  expect_equal(raw_dist$value[raw_dist$variable == 'q_agree'], pos$agree)
})

test_that('distribution z-score of a constant ordinal column is 0, not NaN (tam#39875)', {
  data <- .kmed_likert_fixture()
  data$q_one <- factor(rep('Only', nrow(data)), levels = 'Only', ordered = TRUE)
  model <- exploratory:::exp_kmedoids(data, q_agree, q_one, x, centers = 2, seed = 1)$model[[1]]
  distribution <- broom::tidy(model, type = 'distribution')
  expect_false(any(is.nan(distribution$standardized_value)))
  expect_true(all(distribution$standardized_value[distribution$variable == 'q_one'] == 0))
})

test_that('analysis_conditions gains "Number of Ordinal Variables" only when an ordinal variable is used (tam#39875)', {
  data <- .kmed_likert_fixture()
  mixed <- broom::tidy(
    exploratory:::exp_kmedoids(data, q_agree, x, q_room, centers = 2, seed = 1)$model[[1]],
    type = 'analysis_conditions'
  )
  expect_true('Number of Ordinal Variables' %in% mixed$Metric)
  expect_equal(mixed$Value[mixed$Metric == 'Number of Ordinal Variables'], '2')
  expect_equal(mixed$Value[mixed$Metric == 'Number of Variables'], '3')
  expect_true(all(c('Number of Variables', 'Variable Names', 'Row Count', 'Rows Removed',
    'Number of Clusters') %in% mixed$Metric))

  numeric_only <- broom::tidy(
    exploratory:::exp_kmedoids(data, x, id, centers = 2, seed = 1)$model[[1]],
    type = 'analysis_conditions'
  )
  expect_equal(numeric_only$Metric,
    c('Number of Variables', 'Variable Names', 'Row Count', 'Rows Removed', 'Number of Clusters'))
})

test_that('a complex column name works end to end for an ordered factor (tam#39875)', {
  weird <- '航空 会社 !"#$%&\'()*+, -./:;<=>?@[]^_\'{|}~ 表'
  data <- .kmed_likert_fixture()
  names(data)[names(data) == 'q_agree'] <- weird
  model <- exploratory:::exp_kmedoids(data, !!rlang::sym(weird), x, centers = 2, seed = 1)$model[[1]]
  pos <- .kmed_likert_positions()

  expect_equal(names(model$ordinal_levels), weird)
  expect_equal(unname(model$mat[, weird]), (pos$agree - 1) / 4)
  for (type in c('summary', 'medoid_details')) {
    tbl <- broom::tidy(model, type = type)
    expect_true(weird %in% colnames(tbl), info = type)
    expect_true(is.character(tbl[[weird]]), info = type)
    expect_true(all(tbl[[weird]] %in% .kmed_agree_levels), info = type)
  }
  for (type in c('profile', 'representative_values', 'distribution', 'variable_importance')) {
    tbl <- broom::tidy(model, type = type)
    expect_true(weird %in% tbl$variable, info = type)
  }
  profile <- broom::tidy(model, type = 'profile')
  expect_true(all(profile$is_ordinal[profile$variable == weird]))
  conditions <- broom::tidy(model, type = 'analysis_conditions')
  expect_match(conditions$Value[conditions$Metric == 'Variable Names'], weird, fixed = TRUE)
  for (type in c('map', 'cohesion', 'counts', 'data', 'silhouette')) {
    expect_true(is.data.frame(broom::tidy(model, type = type)), info = type)
  }
})

test_that('PCoA map, variable importance and silhouette run on mixed data (tam#39875)', {
  # Replicated past the 50-row exact-cmdscale threshold so the randomized PCoA path runs too.
  base <- .kmed_likert_fixture()
  data <- dplyr::bind_rows(base, base, base)
  data$x <- data$x + rep(c(0, 0.05, 0.1), each = nrow(base))
  for (rows in list(base, data)) {
    model <- exploratory:::exp_kmedoids(rows, q_agree, x, q_room, centers = 2, seed = 1,
      elbow_method_mode = 'silhouette', max_centers = 4)$model[[1]]
    map <- broom::tidy(model, type = 'map')
    expect_true(nrow(map) > 0)
    expect_true(all(is.finite(map$Dim1)) && all(is.finite(map$Dim2)))
    rate <- attr(map, 'representation_rate')
    expect_equal(length(rate), 2)
    expect_true(all(rate >= 0))
    expect_true(sum(rate) <= 1 + 1e-9)

    importance <- broom::tidy(model, type = 'variable_importance')
    expect_equal(sort(importance$variable), sort(c('q_agree', 'x', 'q_room')))
    expect_true(all(is.finite(importance$eta_squared)))
    expect_true(all(importance$eta_squared >= 0 & importance$eta_squared <= 1))

    silhouette <- broom::tidy(model, type = 'silhouette')
    expect_true(nrow(silhouette) > 0)
    expect_true(is.finite(model$silhouette_avg))
  }
})

test_that('models saved before ordinal support (no ordinal_levels / original_fit_mat) still tidy (tam#39875)', {
  model <- mtcars %>% exploratory:::exp_kmedoids(mpg, disp, hp, centers = 3, seed = 1)
  model <- model$model[[1]]
  current <- lapply(c('summary', 'profile', 'representative_values', 'distribution', 'medoid_details'),
    function(type) broom::tidy(model, type = type))
  model$ordinal_levels <- NULL
  model$original_fit_mat <- NULL
  model$map_result <- NULL
  legacy <- lapply(c('summary', 'profile', 'representative_values', 'distribution', 'medoid_details'),
    function(type) broom::tidy(model, type = type))
  for (i in seq_along(current)) {
    expect_equal(legacy[[i]], current[[i]])
  }
  expect_equal(broom::tidy(model, type = 'analysis_conditions')$Metric,
    c('Number of Variables', 'Variable Names', 'Row Count', 'Rows Removed', 'Number of Clusters'))
})

test_that('numeric-only output is identical to the pre-ordinal implementation apart from the new columns (tam#39875)', {
  golden <- readRDS(testthat::test_path('kmedoids_numeric_golden.rds'))
  new_columns <- list(
    profile = c('is_ordinal', 'medoid_label', 'cluster_median_label', 'overall_median_label'),
    representative_values = c('is_ordinal', 'medoid_label', 'cluster_median_label',
      'overall_median_label')
  )
  fixture <- iris
  fixture$n_int <- as.integer(round(fixture$Petal.Width * 10))
  fixture$const <- 1
  for (mode in c('normalized', 'raw')) {
    model <- exploratory:::exp_kmedoids(
      fixture, Sepal.Length, Petal.Length, n_int, const, centers = 3,
      normalize_data = identical(mode, 'normalized'), seed = 1,
      elbow_method_mode = 'silhouette', max_centers = 5
    )$model[[1]]
    expected <- golden[[mode]]
    expect_identical(model$ordinal_levels, list())
    expect_identical(model$clustering, expected$clustering)
    expect_identical(model$medoid_indices, expected$medoid_indices)
    # The golden was written on macOS ARM; scale() / colMeans() / BLAS differ in the last bits
    # on other platforms (Jenkins Linux), so floating-point values compare with a tight tolerance.
    # Integer outputs above (clustering, medoids) and every class below stay exact.
    expect_equal(unclass(model$mat), expected$mat, tolerance = 1e-10)
    expect_equal(model$original_fit_mat, expected$original_fit_mat, tolerance = 1e-10)
    for (type in c('summary', 'analysis_conditions', 'profile', 'variable_importance',
                   'representative_values', 'distribution', 'cohesion', 'counts', 'data',
                   'silhouette', 'medoid_details')) {
      actual <- broom::tidy(model, type = type)
      before <- expected[[type]]
      # Same columns in the same relative order once the new columns are dropped...
      kept <- setdiff(colnames(actual), new_columns[[type]])
      expect_identical(kept, colnames(before), info = paste(mode, type))
      # ...with the same classes, and the same values up to floating-point noise, in every old column.
      # PAM now fits on grid-snapped distances (.kmedoids_pam_input), which moves silhouette
      # widths by ~1e-9 relative, hence 1e-8 here rather than the 1e-10 used for the matrices.
      expect_identical(lapply(as.data.frame(actual)[kept], class), lapply(as.data.frame(before), class),
        info = paste(mode, type))
      expect_equal(as.data.frame(actual)[kept], as.data.frame(before), tolerance = 1e-8,
        info = paste(mode, type))
    }
    # Numeric-only rows: the new columns exist but are inert.
    for (type in names(new_columns)) {
      actual <- broom::tidy(model, type = type)
      expect_true(all(new_columns[[type]] %in% colnames(actual)), info = type)
      expect_false(any(actual$is_ordinal), info = type)
      expect_true(all(is.na(actual$medoid_label)), info = type)
      expect_true(all(is.na(actual$cluster_median_label)), info = type)
      expect_true(all(is.na(actual$overall_median_label)), info = type)
    }
    # The map is deterministic up to eigenvector sign.
    map <- broom::tidy(model, type = 'map')
    expect_equal(abs(map$Dim1), abs(expected$map$Dim1))
    expect_equal(abs(map$Dim2), abs(expected$map$Dim2))
    expect_equal(attr(map, 'representation_rate'), attr(expected$map, 'representation_rate'))
  }
})

test_that('PAM input is snapped onto a power-of-two grid so the FastPAM swap sums are exact (tam#39875)', {
  # cluster::pam(pamonce = 5) looped forever when the rounding noise of its swap estimate beat
  # its tolerance. On grid values every sum it forms is exact, so the swap loop must end.
  set.seed(3)
  mat <- cbind(g = rep(c(0, 1), each = 20), x = c(stats::rnorm(20, 0, 0.1), stats::rnorm(20, 5, 0.1)))

  snapped <- exploratory:::.kmedoids_pam_input(mat, 'manhattan')
  expect_equal(dim(snapped), dim(mat))
  expect_lt(max(abs(snapped - mat)), 1e-9)
  step <- exploratory:::.kmedoids_exact_grid_step(4 * nrow(mat) * ncol(mat) * 2 * max(abs(mat)))
  expect_true(all(snapped / step == round(snapped / step)))

  # Euclidean distances go through sqrt(), so the distance matrix itself is snapped.
  snapped_dist <- exploratory:::.kmedoids_pam_input(mat, 'euclidean')
  expect_s3_class(snapped_dist, 'dist')
  plain_dist <- stats::dist(mat)
  expect_lt(max(abs(as.numeric(snapped_dist) - as.numeric(plain_dist))), 1e-9)
  step_dist <- exploratory:::.kmedoids_exact_grid_step(4 * nrow(mat) * max(plain_dist))
  expect_true(all(as.numeric(snapped_dist) / step_dist == round(as.numeric(snapped_dist) / step_dist)))

  # .kmedoids_fit must hand pam() the snapped input, not the raw matrix.
  fit_src <- paste(deparse(exploratory:::.kmedoids_fit), collapse = '\n')
  expect_true(grepl('.kmedoids_pam_input(mat, distance)', fit_src, fixed = TRUE))
})

test_that('K-Medoids finishes on well-separated data that made the FastPAM swap loop forever (tam#39875)', {
  skip_if_not_installed('callr')
  # Seeds 4, 5, 6 and 8 of this exact shape hung exp_kmedoids() forever with pamonce = 5 on the
  # raw data. Run them in a child R process with a timeout so a regression fails here instead
  # of hanging the whole suite.
  pkg_dir <- normalizePath(testthat::test_path('..', '..'))
  res <- tryCatch(callr::r(function(pkg_dir) {
    if (file.exists(file.path(pkg_dir, 'DESCRIPTION'))) {
      suppressMessages(pkgload::load_all(pkg_dir, quiet = TRUE))
    } else {
      # R CMD check runs the tests outside the source tree; use the installed package.
      suppressMessages(library(exploratory))
    }
    clusters <- list()
    for (s in c(4, 5, 6, 8)) {
      for (distance in c('manhattan', 'euclidean')) {
        set.seed(s)
        df <- tibble::tibble(
          g = factor(rep(c('a', 'b'), each = 20), levels = c('a', 'b'), ordered = TRUE),
          x = c(stats::rnorm(20, 0, 0.1), stats::rnorm(20, 5, 0.1))
        )
        m <- exp_kmedoids(df, g, x, centers = 2, seed = 1, distance = distance)$model[[1]]
        clusters[[paste(s, distance)]] <- sort(unique(m$clustering))
      }
    }
    unique(unname(clusters))
  }, args = list(pkg_dir = pkg_dir), libpath = .libPaths(), timeout = 300),
  error = function(e) conditionMessage(e))
  expect_identical(res, list(1:2))
})
