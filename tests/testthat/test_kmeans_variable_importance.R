context('K-Means Characteristic Variables (tam#38160)')

# K-Means shares its "Characteristic Variables" computation with K-Medoids
# (cluster_variable_importance_anova() in cluster_variable_importance.R) -- both cluster
# purely numeric variables, so the same one-way-ANOVA-per-variable eta-squared statistic
# applies to both. K-Modes uses a DIFFERENT statistic (Cramer's V) because it clusters purely
# categorical variables -- not covered here.

test_that('tidy(type="variable_importance") returns one row per clustering variable with the expected columns', {
  set.seed(1)
  model_df <- mtcars %>% exp_kmeans(mpg, disp, hp, centers = 3, seed = 1)
  model <- model_df$model[[1]]
  vi <- broom::tidy(model, type = 'variable_importance')

  expect_true(is.data.frame(vi))
  expect_equal(nrow(vi), 3)
  expect_setequal(colnames(vi), c('variable', 'eta_squared', 'test_statistic', 'p_value'))
  expect_setequal(vi$variable, c('mpg', 'disp', 'hp'))
  # eta-squared is a proportion of variance explained: must be within [0, 1].
  expect_true(all(vi$eta_squared >= 0 & vi$eta_squared <= 1))
  # A real, non-degenerate ANOVA F statistic/p-value should be produced for every variable
  # here (3 clusters, 32 rows, no zero-variance/constant columns).
  expect_true(all(!is.na(vi$test_statistic)))
  expect_true(all(!is.na(vi$p_value)))
})

test_that('eta_squared/F/p match an independent hand computation against the real fitted cluster assignment', {
  # Cross-checks the PACKAGE's own R implementation against stats::aov() computed directly
  # in the test, rather than re-deriving the same formula a second time (which would risk
  # encoding the same bug twice) -- per testing.md#r-codegen-fix-live-execution-required's
  # sibling guidance for R-that-inspects-data.
  set.seed(7)
  model_df <- iris %>% exp_kmeans(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width,
                                  centers = 3, seed = 7)
  model <- model_df$model[[1]]
  vi <- broom::tidy(model, type = 'variable_importance')

  cluster_ids <- factor(model$kmeans$cluster)
  vars <- c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width')
  for (v in vars) {
    value <- model$df[[v]]
    fit <- stats::aov(value ~ cluster_ids)
    tab <- summary(fit)[[1]]
    expected_f <- tab[['F value']][[1]]
    expected_p <- tab[['Pr(>F)']][[1]]
    # eta-squared = SS_between / SS_total, independently derived here via the ANOVA table
    # (SS for "cluster_ids" over SS "Sum Sq" total) instead of the package's own tapply-based
    # between/total computation, so the two derivations don't share a possible shared bug.
    expected_eta <- tab[['Sum Sq']][[1]] / sum(tab[['Sum Sq']])

    row <- vi[vi$variable == v, , drop = FALSE]
    expect_equal(nrow(row), 1)
    expect_equal(row$test_statistic, expected_f, tolerance = 1e-8)
    expect_equal(row$p_value, expected_p, tolerance = 1e-8)
    expect_equal(row$eta_squared, expected_eta, tolerance = 1e-8)
  }
})

test_that('a zero-variance (constant) clustering variable gets eta_squared = 0, not NaN/NA', {
  set.seed(1)
  data <- tibble::tibble(x = c(1, 2, 3, 10, 11, 12), constant = rep(5, 6))
  model_df <- data %>% exp_kmeans(x, constant, centers = 2, seed = 1, normalize_data = FALSE)
  model <- model_df$model[[1]]
  vi <- broom::tidy(model, type = 'variable_importance')

  constant_row <- vi[vi$variable == 'constant', , drop = FALSE]
  expect_equal(nrow(constant_row), 1)
  expect_false(is.nan(constant_row$eta_squared))
  expect_false(is.na(constant_row$eta_squared))
  expect_equal(constant_row$eta_squared, 0)
})

test_that('single-column K-Means (allow_single_column) still returns a one-row variable_importance', {
  set.seed(1)
  model_df <- mtcars %>% exp_kmeans(mpg, centers = 2, seed = 1)
  model <- model_df$model[[1]]
  vi <- broom::tidy(model, type = 'variable_importance')

  expect_equal(nrow(vi), 1)
  expect_equal(vi$variable, 'mpg')
  expect_true(vi$eta_squared >= 0 && vi$eta_squared <= 1)
})

test_that('variable_importance survives a complex/stress-test column name (tam workflow.md rule 7)', {
  set.seed(1)
  weird_name <- "航空 会社 !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表"
  df <- mtcars
  colnames(df)[colnames(df) == 'cyl'] <- weird_name
  model_df <- df %>% exp_kmeans(!!rlang::sym(weird_name), mpg, hp, centers = 3, seed = 1)
  model <- model_df$model[[1]]
  vi <- broom::tidy(model, type = 'variable_importance')

  expect_true(weird_name %in% vi$variable)
  expect_equal(nrow(vi), 3)
  expect_true(all(vi$eta_squared >= 0 & vi$eta_squared <= 1))
})

test_that('a grouped (Repeat By) K-Means model computes variable_importance per group, from that group\'s own data', {
  set.seed(1)
  df <- mtcars %>% dplyr::mutate(grp = ifelse(cyl == 4, 'a', 'b'))
  model_df <- df %>% dplyr::group_by(grp) %>%
    exp_kmeans(mpg, hp, centers = 2, seed = 1, max_nrow = NULL)

  for (i in seq_len(nrow(model_df))) {
    model <- model_df$model[[i]]
    vi <- broom::tidy(model, type = 'variable_importance')
    expect_equal(nrow(vi), 2)
    expect_setequal(vi$variable, c('mpg', 'hp'))
    # Each group's variable_importance must be computed from THAT group's own rows,
    # not leak in the other group's data -- cross-check nrow(model$df) matches the group size.
    expect_equal(nrow(model$df), sum(df$grp == model_df$grp[[i]]))
  }
})

test_that('variable_importance returns an empty, correctly-typed tibble for a pure-PCA fit (no kmeans attached)', {
  # tidy.prcomp_exploratory(type="variable_importance") must not error for a fit that never
  # went through exp_kmeans() -- e.g. a plain do_prcomp() result, or an old saved K-Means
  # model missing the tam#37681-era fields.
  fit <- iris %>% do_prcomp(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, seed = 1)
  model <- fit$model[[1]]
  vi <- broom::tidy(model, type = 'variable_importance')

  expect_true(is.data.frame(vi))
  expect_equal(nrow(vi), 0)
  expect_setequal(colnames(vi), c('variable', 'eta_squared', 'test_statistic', 'p_value'))
})

test_that('.kmeans_variable_importance is a thin wrapper reusing cluster_variable_importance_anova (no logic duplication)', {
  set.seed(1)
  model_df <- mtcars %>% exp_kmeans(mpg, disp, hp, centers = 3, seed = 1)
  model <- model_df$model[[1]]

  wrapper_result <- exploratory:::.kmeans_variable_importance(model)
  direct_result <- exploratory:::cluster_variable_importance_anova(
    exploratory:::as_numeric_matrix_(model$df, columns = model$selected_cols),
    model$kmeans$cluster
  )
  expect_equal(wrapper_result, direct_result)
})
