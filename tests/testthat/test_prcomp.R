# how to run this test:
# devtools::test(filter="prcomp")
context("test prcomp functions")

test_that("do_prcomp", {
  df <- mtcars %>% mutate(new_col = c(rep("A", n() - 10), rep("B", 10)))
  model_df <- do_prcomp(df, cyl, mpg, hp, max_nrow=30)
  model_df %>% tidy_rowwise(model, type="variances")
  model_df %>% tidy_rowwise(model, type="loadings")
  model_df %>% tidy_rowwise(model, type="biplot")
  model_df %>% tidy_rowwise(model, type="screeplot")
  res <- model_df %>% tidy_rowwise(model, type="data")
  expect_equal(colnames(res),
               c("mpg","cyl","disp","hp","drat","wt","qsec","vs","am","gear","carb","new_col","PC1","PC2","PC3"))
})

test_that("do_prcomp with strange column name", {
  df <- mtcars %>%
    rename(`Cy l` = cyl) %>%
    mutate(new_col = c(rep("A", n() - 10), rep("B", 10)))
  model_df <- do_prcomp(df, `Cy l`, mpg, hp)
  model_df %>% tidy_rowwise(model, type="variances")
  model_df %>% tidy_rowwise(model, type="loadings")
  model_df %>% tidy_rowwise(model, type="biplot")
  model_df %>% tidy_rowwise(model, type="screeplot")
  res <- model_df %>% tidy_rowwise(model, type="data")
  expect_equal(colnames(res),
               c("mpg","Cy l","disp","hp","drat","wt","qsec","vs","am","gear","carb","new_col","PC1","PC2","PC3"))
})

test_that("prcomp_report_config returns expected thresholds", {
  cfg <- exploratory:::prcomp_report_config()
  expect_equal(cfg$loading_salient, 0.40)
  expect_equal(cfg$dominant_contribution, 0.40)
  expect_equal(cfg$dominant_ratio, 1.5)
  expect_equal(cfg$representation_high, 0.70)
  expect_equal(cfg$representation_mostly, 0.50)
  expect_equal(cfg$representation_partial, 0.30)
  expect_equal(cfg$cumulative_high, 0.80)
  expect_equal(cfg$cumulative_mid, 0.60)
  expect_equal(cfg$scale_ratio_warning, 10)
  expect_equal(cfg$na_exclusion_warning, 0.20)
  expect_equal(cfg$next_gain_threshold, 0.20)
})

test_that("classify_pca_component_pattern classifies all five patterns", {
  f <- exploratory:::classify_pca_component_pattern
  r <- f(loadings = c(a=0.9, b=0.2, c=0.1), contributions = c(a=0.6, b=0.3, c=0.1))
  expect_equal(r$status, "single_variable"); expect_equal(r$label, "Single Variable"); expect_equal(r$dominant_variable, "a")
  r <- f(loadings = c(a=0.8, b=0.7, c=-0.6), contributions = c(a=0.34, b=0.33, c=0.33))
  expect_equal(r$status, "contrast"); expect_equal(r$positive_variables, "a,b"); expect_equal(r$negative_variables, "c")
  r <- f(loadings = c(a=0.7, b=0.6, c=0.5, d=0.1), contributions = c(a=0.3, b=0.3, c=0.3, d=0.1))
  expect_equal(r$status, "common_direction")
  r <- f(loadings = setNames(rep(0.3, 7), letters[1:7]), contributions = setNames(rep(1/7, 7), letters[1:7]))
  expect_equal(r$status, "diffuse")
  r <- f(loadings = c(a=0.5, b=0.2, c=0.1), contributions = c(a=0.39, b=0.35, c=0.26))
  expect_equal(r$status, "mixed")
})

test_that("select_pca_related_variables respects threshold, min 2, max 5, ordering, signs", {
  f <- exploratory:::select_pca_related_variables
  r <- f(loadings = c(a=0.9, b=-0.7, c=0.5, d=0.45, e=0.44, f=0.41, g=0.1),
         contributions = c(a=.3,b=.2,c=.15,d=.13,e=.12,f=.08,g=.02))
  expect_equal(r$display_text, "+a, -b, +c, +d, +e")
  r <- f(loadings = c(a=0.9, b=0.2, c=0.1), contributions = c(a=.7,b=.2,c=.1))
  expect_equal(r$display_text, "+a, +b")
})

test_that("do_prcomp attaches report data, sign stabilization, retained resolution", {
  df <- mtcars
  model_df <- df %>% do_prcomp(mpg, cyl, disp, hp, drat, wt)
  fit <- model_df$model[[1]]
  expect_true(!is.null(fit$parallel))
  expect_true(is.numeric(fit$recommended_components))
  expect_true(fit$retained_components >= 1)
  expect_true(!is.null(fit$input_diagnostics))
  d <- fit$input_diagnostics
  expect_true(all(c("original_row_count","analyzed_row_count","excluded_row_count",
                    "excluded_row_rate","excluded_variables","variable_sd","scale_ratio") %in% names(d)))
  # sign stabilization: strongest |correlation| per PC is non-negative
  cleaned <- df[, c("mpg","cyl","disp","hp","drat","wt")]
  cors <- cor(cleaned, fit$x)
  strongest <- apply(cors, 2, function(col) col[which.max(abs(col))])
  expect_true(all(strongest >= 0))
  # explicit retained + clamp
  m2 <- df %>% do_prcomp(mpg, cyl, disp, retained_components = 99)
  expect_equal(m2$model[[1]]$retained_components, 3L)
  m3 <- df %>% do_prcomp(mpg, cyl, disp, retained_components = 2)
  expect_equal(m3$model[[1]]$retained_components, 2L)
})

test_that("new report tidy types return expected columns and tokens", {
  model_df <- mtcars %>% do_prcomp(mpg, cyl, disp, hp, drat, wt)
  res <- model_df %>% tidy_rowwise(model, type = "analysis_conditions")
  expect_equal(colnames(res), c("Metric", "Value", "Description", "status"))
  expect_true(all(c("Row Count","Number of Variables","Normalization","SD Ratio (Max/Min)") %in% res$Metric))
  expect_false("Rows vs Variables" %in% res$Metric)
  expect_false("Rows Used" %in% res$Metric)
  expect_false("Variables Used" %in% res$Metric)
  expect_false("Number of Rows" %in% res$Metric)
  # #37268: empty excluded-variables cell is "None" (JA: なし), not "-".
  excluded_row <- res[res$Metric == "Excluded Variables", , drop = FALSE]
  if (nrow(excluded_row) == 1 && identical(excluded_row$status, "na")) {
    expect_equal(excluded_row$Value, "None")
  }
  res <- model_df %>% tidy_rowwise(model, type = "parallel_screeplot")
  expect_equal(colnames(res), c("Component", "Eigenvalue", "Random Data Eigenvalue"))
  res <- model_df %>% tidy_rowwise(model, type = "variances_judged")
  expect_equal(colnames(res), c("Component","Eigenvalue","% Variance","Cummulated % Variance",
                                "Parallel Analysis","Kaiser Criterion","Selected",
                                "parallel_status","kaiser_status","selected_status"))
  expect_true(all(res$parallel_status %in% c("adopted","not_adopted","na")))
  expect_true(all(res$selected_status %in% c("adopted","not_adopted")))
  m2 <- mtcars %>% do_prcomp(mpg, cyl, disp, normalize_data = FALSE)
  r2 <- m2 %>% tidy_rowwise(model, type = "variances_judged")
  expect_true(all(r2$kaiser_status == "na"))
})

test_that("new report tidy types return empty typed tibbles for kmeans fits", {
  km <- mtcars %>% exploratory:::exp_kmeans(mpg, cyl, disp, centers = 2)
  ac <- km %>% tidy_rowwise(model, type = "analysis_conditions")
  expect_equal(colnames(ac), c("Metric", "Value", "Description", "status"))
  expect_equal(nrow(ac), 0)
  ps <- km %>% tidy_rowwise(model, type = "parallel_screeplot")
  expect_equal(colnames(ps), c("Component", "Eigenvalue", "Random Data Eigenvalue"))
  expect_equal(nrow(ps), 0)
  vj <- km %>% tidy_rowwise(model, type = "variances_judged")
  expect_equal(colnames(vj), c("Component","Eigenvalue","% Variance","Cummulated % Variance",
                               "Parallel Analysis","Kaiser Criterion","Selected",
                               "parallel_status","kaiser_status","selected_status"))
  expect_equal(nrow(vj), 0)
})

test_that("component_profiles / loadings_signed / contributions", {
  model_df <- mtcars %>% do_prcomp(mpg, cyl, disp, hp, drat, wt, retained_components = 3)
  res <- model_df %>% tidy_rowwise(model, type = "component_profiles")
  expect_equal(colnames(res), c("Component","Eigenvalue","% Variance","Cummulated % Variance",
                                "Related Variables","Pattern",
                                "pattern_status","dominant_variable","positive_variables","negative_variables"))
  expect_equal(nrow(res), 3)
  expect_true(all(res$pattern_status %in% c("single_variable","common_direction","contrast","diffuse","mixed")))
  res <- model_df %>% tidy_rowwise(model, type = "loadings_signed")
  expect_equal(colnames(res), c("Variable","Component","Loading"))
  expect_true(any(res$Loading < 0))
  res <- model_df %>% tidy_rowwise(model, type = "contributions")
  expect_equal(colnames(res), c("Variable","Component","Contribution","Variance Contribution"))
  sums <- res %>% dplyr::group_by(Component) %>% dplyr::summarize(s = sum(Contribution)) %>% dplyr::pull(s)
  expect_true(all(abs(sums - 100) < 1e-6))
  # Each component's Variance Contribution segments sum to that component's % variance, and all
  # segments together sum to the cumulative variance explained (issue #37132).
  var_ratio <- model_df$model[[1]]$sdev^2 / sum(model_df$model[[1]]$sdev^2) * 100
  vc_sums <- res %>% dplyr::group_by(Component) %>%
    dplyr::summarize(s = sum(`Variance Contribution`)) %>% dplyr::pull(s)
  expect_true(all(abs(vc_sums - var_ratio[seq_along(vc_sums)]) < 1e-6))
})

test_that("component_profiles / loadings_signed / contributions empty for kmeans fits", {
  km <- mtcars %>% exploratory:::exp_kmeans(mpg, cyl, disp, centers = 2)
  cp <- km %>% tidy_rowwise(model, type = "component_profiles")
  expect_equal(colnames(cp), c("Component","Eigenvalue","% Variance","Cummulated % Variance",
                               "Related Variables","Pattern",
                               "pattern_status","dominant_variable","positive_variables","negative_variables"))
  expect_equal(nrow(cp), 0)
  ls <- km %>% tidy_rowwise(model, type = "loadings_signed")
  expect_equal(colnames(ls), c("Variable","Component","Loading"))
  expect_equal(nrow(ls), 0)
  ct <- km %>% tidy_rowwise(model, type = "contributions")
  expect_equal(colnames(ct), c("Variable","Component","Contribution","Variance Contribution"))
  expect_equal(nrow(ct), 0)
})

test_that("loadings_signed_wide returns a wide table with %-labeled PC headers (#37130)", {
  model_df <- mtcars %>% do_prcomp(mpg, cyl, disp, hp, drat, wt)
  res <- model_df %>% tidy_rowwise(model, type = "loadings_signed_wide")
  # One row per input variable; first column is Variable, rest are the PC columns.
  expect_equal(res$Variable, c("mpg","cyl","disp","hp","drat","wt"))
  pc_cols <- setdiff(colnames(res), "Variable")
  expect_equal(length(pc_cols), 6L) # 6 variables -> 6 components
  # Headers carry the contribution % e.g. "PC1 (43.1%)".
  expect_true(all(grepl("^PC[0-9]+ \\([0-9]+\\.[0-9]%\\)$", pc_cols)))
  # The %-labels use the SAME basis as variances_judged (sdev^2 / sum * 100).
  vj <- model_df %>% tidy_rowwise(model, type = "variances_judged")
  expected_labels <- paste0("PC", seq_len(nrow(vj)), " (",
                            format(round(vj$`% Variance`, 1), nsmall = 1, trim = TRUE), "%)")
  expect_equal(pc_cols, expected_labels)
  # Signed loadings: negatives are expected on non-dominant variables.
  expect_true(any(unlist(res[, pc_cols]) < 0))
})

test_that("loadings_signed_wide is empty for kmeans fits (#37130)", {
  km <- mtcars %>% exploratory:::exp_kmeans(mpg, cyl, disp, centers = 2)
  lw <- km %>% tidy_rowwise(model, type = "loadings_signed_wide")
  expect_equal(colnames(lw), "Variable")
  expect_equal(nrow(lw), 0)
})

test_that("variable_map / representation", {
  model_df <- mtcars %>% do_prcomp(mpg, cyl, disp, hp, drat, wt, retained_components = 2)
  res <- model_df %>% tidy_rowwise(model, type = "variable_map")
  expect_true(all(c("PC1","PC2","measure_name","Representation 2D") %in% colnames(res)))
  res <- model_df %>% tidy_rowwise(model, type = "representation")
  expect_true(all(c("Variable","PC1","PC2","Retained","Judgement","judgement_status") %in% colnames(res)))
  expect_true(all(res$judgement_status %in% c("high","mostly","partial","low")))
  # cumulative representation monotone non-decreasing across PC columns
  pc_cols <- grep("^PC[0-9]+$", colnames(res), value = TRUE)
  m <- as.matrix(res[, pc_cols])
  expect_true(all(apply(m, 1, function(r) all(diff(r) >= -1e-9))))
})

test_that("variable_map / representation empty for kmeans fits", {
  km <- mtcars %>% exploratory:::exp_kmeans(mpg, cyl, disp, centers = 2)
  vm <- km %>% tidy_rowwise(model, type = "variable_map")
  expect_true(all(c("PC1","PC2","measure_name","Representation 2D") %in% colnames(vm)))
  expect_equal(nrow(vm), 0)
  rp <- km %>% tidy_rowwise(model, type = "representation")
  expect_true(all(c("Variable","Retained","Judgement","judgement_status") %in% colnames(rp)))
  expect_equal(nrow(rp), 0)
})

# ---------------------------------------------------------------------------
# A6 robustness sweep: grouped / strange-names / 2-var / kmeans / old-model.
# The report tidy types (issue #37019) must degrade gracefully across these.
# ---------------------------------------------------------------------------

# Canonical strange/multibyte stress name (project convention). No backticks.
PRCOMP_STRESS_NAME <- "航空 会社 !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表"

# The eight report tidy types + their required columns (empty case = 0 rows, same cols).
PRCOMP_REPORT_TYPE_COLS <- list(
  analysis_conditions = c("Metric", "Value", "Description", "status"),
  parallel_screeplot  = c("Component", "Eigenvalue", "Random Data Eigenvalue"),
  variances_judged    = c("Component", "Eigenvalue", "% Variance", "Cummulated % Variance",
                          "Parallel Analysis", "Kaiser Criterion", "Selected",
                          "parallel_status", "kaiser_status", "selected_status"),
  component_profiles  = c("Component", "Eigenvalue", "% Variance", "Cummulated % Variance",
                          "Related Variables", "Pattern",
                          "pattern_status", "dominant_variable", "positive_variables", "negative_variables"),
  loadings_signed     = c("Variable", "Component", "Loading"),
  contributions       = c("Variable", "Component", "Contribution", "Variance Contribution"),
  variable_map        = c("measure_name", "PC1", "PC2", "Measures", "Representation 2D"),
  representation       = c("Variable", "Retained", "Judgement", "judgement_status")
)

test_that("report tidy types run per-group and preserve the group column (Repeat By)", {
  model_df <- mtcars %>% dplyr::group_by(am) %>% do_prcomp(mpg, disp, hp, wt)
  # One model row per group.
  expect_equal(nrow(model_df), length(unique(mtcars$am)))
  for (ty in c("component_profiles", "variances_judged", "loadings_signed", "variable_map", "representation")) {
    res <- model_df %>% tidy_rowwise(model, type = ty)
    # group column preserved and both groups present
    expect_true("am" %in% colnames(res), info = ty)
    expect_gt(nrow(res), 0)
    expect_equal(sort(unique(res$am)), sort(unique(mtcars$am)), info = ty)
    # the type's required output columns are all present alongside the group column
    expect_true(all(PRCOMP_REPORT_TYPE_COLS[[ty]] %in% colnames(res)), info = ty)
  }
})

test_that("report tidy types pass strange / multibyte variable names through intact", {
  d <- mtcars[, c("mpg", "cyl", "disp")]
  colnames(d) <- c("Cy l", PRCOMP_STRESS_NAME, "disp")
  # select all three columns (names contain spaces/symbols, so use tidyselect everything()).
  model_df <- d %>% do_prcomp(dplyr::everything())

  ls <- model_df %>% tidy_rowwise(model, type = "loadings_signed")
  expect_true(PRCOMP_STRESS_NAME %in% ls$Variable)
  expect_true("Cy l" %in% ls$Variable)

  vm <- model_df %>% tidy_rowwise(model, type = "variable_map")
  expect_true(PRCOMP_STRESS_NAME %in% vm$measure_name)
  expect_true("Cy l" %in% vm$measure_name)

  ct <- model_df %>% tidy_rowwise(model, type = "contributions")
  expect_true(PRCOMP_STRESS_NAME %in% ct$Variable)

  rp <- model_df %>% tidy_rowwise(model, type = "representation")
  expect_true(PRCOMP_STRESS_NAME %in% rp$Variable)

  # component_profiles surfaces variable names inside the Related Variables text; the
  # stress name (or "Cy l") must appear intact somewhere in that column.
  cp <- model_df %>% tidy_rowwise(model, type = "component_profiles")
  expect_gt(nrow(cp), 0)
  related_blob <- paste(cp$`Related Variables`, collapse = " | ")
  expect_true(grepl(PRCOMP_STRESS_NAME, related_blob, fixed = TRUE) ||
                grepl("Cy l", related_blob, fixed = TRUE))
})

test_that("report tidy types handle 2-variable input", {
  model_df <- mtcars %>% do_prcomp(mpg, cyl)

  vj <- model_df %>% tidy_rowwise(model, type = "variances_judged")
  expect_equal(nrow(vj), 2) # 2 components

  cp <- model_df %>% tidy_rowwise(model, type = "component_profiles")
  expect_gte(nrow(cp), 1) # at least one retained component

  vm <- model_df %>% tidy_rowwise(model, type = "variable_map")
  # two variables -> two origin rows + two endpoint rows
  expect_equal(nrow(vm), 4)
  expect_true(all(c("PC1", "PC2", "measure_name", "Representation 2D") %in% colnames(vm)))

  rp <- model_df %>% tidy_rowwise(model, type = "representation")
  pc_cols <- grep("^PC[0-9]+$", colnames(rp), value = TRUE)
  expect_equal(length(pc_cols), 2)   # exactly PC1, PC2 (not padded)
  expect_equal(nrow(rp), 2)          # two variables
  # cumulative representation monotone non-decreasing across the two PC columns
  m <- as.matrix(rp[, pc_cols])
  expect_true(all(apply(m, 1, function(r) all(diff(r) >= -1e-9))))
})

test_that("all 8 report tidy types return 0-row typed tibbles for a kmeans fit", {
  km <- mtcars %>% exploratory:::exp_kmeans(mpg, cyl, disp, hp, centers = 2)
  for (ty in names(PRCOMP_REPORT_TYPE_COLS)) {
    res <- km %>% tidy_rowwise(model, type = ty)
    expect_equal(nrow(res), 0, info = ty)
    expect_true(all(PRCOMP_REPORT_TYPE_COLS[[ty]] %in% colnames(res)), info = ty)
  }
})

test_that("all 8 report tidy types return 0-row typed tibbles for an old saved model (report fields stripped)", {
  model_df <- mtcars %>% do_prcomp(mpg, cyl, disp, hp)
  fit <- model_df$model[[1]]
  # Simulate a model saved before the #37019 report data existed.
  fit$parallel <- NULL
  fit$input_diagnostics <- NULL
  fit$retained_components <- NULL
  fit$retained_is_auto <- NULL
  fit$kaiser_components <- NULL
  fit$recommended_components <- NULL
  fit$normalize_data <- NULL
  for (ty in names(PRCOMP_REPORT_TYPE_COLS)) {
    res <- exploratory:::tidy.prcomp_exploratory(fit, type = ty)
    expect_equal(nrow(res), 0, info = ty)
    expect_true(all(PRCOMP_REPORT_TYPE_COLS[[ty]] %in% colnames(res)), info = ty)
  }
})

test_that("coefficients tidy type returns rotation weights (long, signed)", {
  model_df <- mtcars %>% do_prcomp(mpg, cyl, disp, hp, drat, wt)
  res <- model_df %>% tidy_rowwise(model, type = "coefficients")
  expect_equal(colnames(res), c("Variable", "Component", "Coefficient", "Score Coefficient"))
  expect_true(any(res$Coefficient < 0))            # signed eigenvector weights
  expect_true(nrow(res) == 6 * length(unique(res$Component)))  # vars x components
  # Default scale preserves component variance, so the score coefficient IS the rotation. (#27224)
  expect_equal(res$`Score Coefficient`, res$Coefficient, tolerance = 1e-12)
  # kmeans fit -> empty
  km <- mtcars %>% exploratory:::exp_kmeans(mpg, cyl, disp, centers = 2)
  r2 <- km %>% tidy_rowwise(model, type = "coefficients")
  expect_equal(nrow(r2), 0)
  expect_equal(colnames(r2), c("Variable", "Component", "Coefficient", "Score Coefficient"))
})

# ---------------------------------------------------------------------------
# Polychoric correlation support (issue #37294)
# ---------------------------------------------------------------------------

# Deterministic ordinal (1-5 Likert) fixture driven by two latent factors, so the polychoric
# solution has a known two-component structure.
make_ordinal_survey <- function(n = 300, seed = 37294) {
  set.seed(seed)
  latent_a <- rnorm(n)
  latent_b <- rnorm(n)
  to_five <- function(z) as.integer(cut(z, breaks = c(-Inf, -1, -0.3, 0.3, 1, Inf), labels = FALSE))
  data.frame(
    q1 = to_five(latent_a + rnorm(n, 0, 0.6)), q2 = to_five(latent_a + rnorm(n, 0, 0.6)),
    q3 = to_five(latent_a + rnorm(n, 0, 0.6)), q4 = to_five(latent_b + rnorm(n, 0, 0.6)),
    q5 = to_five(latent_b + rnorm(n, 0, 0.6)), q6 = to_five(latent_b + rnorm(n, 0, 0.6))
  )
}

test_that("do_prcomp cor_type='polychoric' fits from the correlation matrix", {
  df <- make_ordinal_survey()
  model_df <- df %>% do_prcomp(q1, q2, q3, q4, q5, q6, cor_type = "polychoric")
  fit <- model_df$model[[1]]
  expect_equal(fit$correlation_type, "polychoric")
  expect_true(isTRUE(fit$is_categorical_correlation))
  # Eigen-decomposition of a correlation matrix: the eigenvalues sum to the variable count.
  expect_equal(sum(fit$sdev^2), 6, tolerance = 1e-8)
  expect_equal(dim(fit$x), c(nrow(df), 6L))
  expect_equal(colnames(fit$rotation), paste0("PC", 1:6))
})

test_that("do_prcomp polychoric scores are scale(data) %*% eigenvectors (issue #37294)", {
  df <- make_ordinal_survey()
  fit <- (df %>% do_prcomp(q1, q2, q3, q4, q5, q6, cor_type = "polychoric"))$model[[1]]
  # The approximation the issue specifies. Sign stabilization sweeps rotation and x together,
  # so the identity survives it.
  expected <- scale(as.matrix(df)) %*% fit$rotation
  expect_equal(unname(expected), unname(fit$x), tolerance = 1e-10)
})

test_that("do_prcomp polychoric report branches all render", {
  df <- make_ordinal_survey()
  model_df <- df %>% do_prcomp(q1, q2, q3, q4, q5, q6, cor_type = "polychoric")
  for (ty in c("variances", "loadings", "biplot", "screeplot", "analysis_conditions",
               "parallel_screeplot", "variances_judged", "component_profiles",
               "loadings_signed", "loadings_signed_wide", "contributions", "coefficients",
               "variable_map", "representation", "data")) {
    res <- model_df %>% tidy_rowwise(model, type = ty)
    expect_gt(nrow(res), 0)
  }
})

test_that("do_prcomp polychoric loadings come from the polychoric solution, not a fresh Pearson", {
  df <- make_ordinal_survey()
  fit <- (df %>% do_prcomp(q1, q2, q3, q4, q5, q6, cor_type = "polychoric"))$model[[1]]
  loadings <- exploratory:::prcomp_signed_loadings(fit)
  # Component loading of a correlation-matrix PCA == eigenvector * sqrt(eigenvalue).
  expect_equal(unname(loadings),
               unname(fit$rotation %*% diag(fit$sdev, nrow = length(fit$sdev))), tolerance = 1e-10)
  # And it is NOT the Pearson cross-correlation the pre-#37294 branches recomputed.
  pearson_loadings <- cor(as.matrix(df), fit$x)
  expect_false(isTRUE(all.equal(unname(loadings), unname(pearson_loadings), tolerance = 1e-6)))
})

test_that("do_prcomp accepts ordered factor columns under polychoric", {
  df <- make_ordinal_survey()
  numeric_fit <- (df %>% do_prcomp(q1, q2, q3, q4, q5, q6, cor_type = "polychoric"))$model[[1]]
  for (column in names(df)) df[[column]] <- factor(df[[column]], levels = 1:5, ordered = TRUE)
  model_df <- df %>% do_prcomp(q1, q2, q3, q4, q5, q6, cor_type = "polychoric")
  factor_fit <- model_df$model[[1]]
  # Category coding turns the ordered factor back into the same 1..5 codes.
  expect_equal(factor_fit$sdev, numeric_fit$sdev, tolerance = 1e-10)
  # The branches that used to call cor() directly would have failed on a factor column.
  expect_equal(nrow(model_df %>% tidy_rowwise(model, type = "loadings_signed_wide")), 6)
  expect_equal(nrow(model_df %>% tidy_rowwise(model, type = "representation")), 6)
})

test_that("do_prcomp analysis_method / cor_diagnostics tidy types", {
  df <- make_ordinal_survey()
  poly <- df %>% do_prcomp(q1, q2, q3, q4, q5, q6, cor_type = "polychoric")
  method <- poly %>% tidy_rowwise(model, type = "analysis_method")
  expect_equal(method$Item, c("Correlation", "Normalization", "Target Variables", "Data Rows"))
  expect_equal(unique(method$correlation_type), "polychoric")
  # Booleans must be the literal strings the client's isTrue() accepts.
  expect_equal(unique(method$correlation_is_auto), "FALSE")
  expect_equal(unique(method$has_diagnostics), "TRUE")
  expect_equal(unique(method$degraded_from), "")
  expect_equal(nrow(poly %>% tidy_rowwise(model, type = "cor_diagnostics")), 6)

  pearson <- df %>% do_prcomp(q1, q2, q3, q4, q5, q6, cor_type = "pearson")
  method_pearson <- pearson %>% tidy_rowwise(model, type = "analysis_method")
  expect_equal(unique(method_pearson$correlation_type), "pearson")
  expect_equal(unique(method_pearson$has_diagnostics), "FALSE")
  # Pearson shows no diagnostics section, but the columns keep their shape.
  diagnostics <- pearson %>% tidy_rowwise(model, type = "cor_diagnostics")
  expect_equal(nrow(diagnostics), 0)
  expect_equal(colnames(diagnostics), c("Diagnostic", "Judgement", "Description", "status"))
})

test_that("do_prcomp cor_type='auto' picks polychoric for ordinal data and Pearson for continuous", {
  set.seed(37294)
  n <- 300
  latent_a <- rnorm(n); latent_b <- rnorm(n)
  to_four <- function(z) as.integer(cut(z, breaks = c(-Inf, -0.6, 0, 0.6, Inf), labels = FALSE))
  ordinal <- data.frame(a = to_four(latent_a + rnorm(n, 0, 0.5)), b = to_four(latent_a + rnorm(n, 0, 0.5)),
                        c = to_four(latent_b + rnorm(n, 0, 0.5)), e = to_four(latent_b + rnorm(n, 0, 0.5)))
  ordinal_fit <- (ordinal %>% do_prcomp(a, b, c, e))$model[[1]]
  expect_equal(ordinal_fit$correlation_type, "polychoric")
  expect_true(isTRUE(ordinal_fit$correlation_is_auto))

  continuous <- data.frame(a = rnorm(n), b = rnorm(n), c = rnorm(n), e = rnorm(n))
  continuous_fit <- (continuous %>% do_prcomp(a, b, c, e))$model[[1]]
  expect_equal(continuous_fit$correlation_type, "pearson")
  expect_false(isTRUE(continuous_fit$is_categorical_correlation))
})

test_that("do_prcomp Pearson output is unchanged by the polychoric support (issue #37294)", {
  set.seed(37294)
  n <- 200
  df <- data.frame(a = rnorm(n), b = rnorm(n), c = rnorm(n), d = rnorm(n))
  df$b <- df$b + 0.7 * df$a
  df$d <- df$d - 0.5 * df$c
  fit <- (df %>% do_prcomp(a, b, c, d))$model[[1]]
  reference <- prcomp(df, scale. = TRUE)
  # Same decomposition as a plain prcomp() on the raw data (up to the sign stabilization sweep).
  expect_equal(fit$sdev, reference$sdev, tolerance = 1e-10)
  expect_equal(abs(unname(fit$rotation)), abs(unname(reference$rotation)), tolerance = 1e-10)
  # The Pearson path keeps recomputing the loadings from the data, so no cache is attached.
  expect_null(fit$signed_loadings)
})

test_that("do_prcomp rejects nominal categorical variables", {
  df <- make_ordinal_survey()
  df$region <- factor(sample(c("East", "West", "North"), nrow(df), replace = TRUE))
  expect_error(df %>% do_prcomp(q1, q2, region), "EXP-ANA-35")
})

test_that("do_prcomp polychoric works with Repeat By", {
  df <- make_ordinal_survey()
  df$grp <- rep(c("A", "B"), each = nrow(df) / 2)
  model_df <- df %>% dplyr::group_by(grp) %>% do_prcomp(q1, q2, q3, q4, q5, q6, cor_type = "polychoric")
  expect_equal(nrow(model_df), 2)
  # Every facet must run on the same correlation, or the groups' loadings are incomparable.
  expect_equal(unique(vapply(model_df$model, function(f) f$correlation_type, character(1))), "polychoric")
  expect_equal(nrow(model_df %>% tidy_rowwise(model, type = "variances_judged")), 12)
})

test_that("exp_kmeans is unaffected by the PCA correlation-type support", {
  km <- mtcars %>% exploratory:::exp_kmeans(mpg, cyl, disp, centers = 2)
  fit <- km$model[[1]]
  # with_report_data = FALSE keeps the raw-data prcomp() path and adds no method metadata.
  expect_null(fit$correlation_type)
  expect_null(fit$is_categorical_correlation)
  expect_null(fit$signed_loadings)
})

# ---------------------------------------------------------------------------
# Principal component score scale (issue #27224)
# ---------------------------------------------------------------------------

test_that("do_prcomp defaults to preserve_variance and attaches the canonical scores", {
  fit <- (mtcars %>% do_prcomp(mpg, cyl, disp, hp, drat, wt))$model[[1]]
  expect_equal(fit$score_scale, "preserve_variance")
  expect_equal(unname(fit$scores), unname(fit$x), tolerance = 1e-12)
  # The default score SDs are the component SDs, not 1 -- that is the whole point of the option.
  expect_equal(unname(apply(fit$scores, 2, sd)), unname(fit$sdev), tolerance = 1e-8)
})

test_that("unit_variance standardizes every component to SD 1 without changing the solution", {
  ref <- (mtcars %>% do_prcomp(mpg, cyl, disp, hp, drat, wt))$model[[1]]
  fit <- (mtcars %>% do_prcomp(mpg, cyl, disp, hp, drat, wt, score_scale = "unit_variance"))$model[[1]]
  expect_equal(fit$score_scale, "unit_variance")
  expect_equal(unname(apply(fit$scores, 2, sd)), rep(1, length(fit$sdev)), tolerance = 1e-8)
  # Only the scale of the OUTPUT scores changes: eigenvalues, coefficients and the canonical
  # score matrix are identical to the default run.
  expect_equal(fit$sdev, ref$sdev, tolerance = 1e-12)
  expect_equal(fit$rotation, ref$rotation, tolerance = 1e-12)
  expect_equal(fit$x, ref$x, tolerance = 1e-12)
  # scores * sdev == x, i.e. the scaling is exactly the documented sweep.
  expect_equal(unname(sweep(fit$scores, 2, fit$sdev, "*")), unname(fit$x), tolerance = 1e-10)
})

test_that("unit_variance scores are built AFTER sign stabilization (#27224)", {
  fit <- (mtcars %>% do_prcomp(mpg, cyl, disp, hp, drat, wt, score_scale = "unit_variance"))$model[[1]]
  # Sign stabilization flips fit$x; scaling it afterwards keeps the same sign, so every score
  # column must correlate POSITIVELY with the canonical (already flipped) score column.
  correlations <- vapply(seq_len(ncol(fit$scores)), function(i) cor(fit$scores[, i], fit$x[, i]), numeric(1))
  expect_true(all(correlations > 0))
  # And the report's sign contract still holds: the strongest-loading variable loads positively.
  loadings <- cor(mtcars[, c("mpg", "cyl", "disp", "hp", "drat", "wt")], fit$x)
  strongest <- vapply(seq_len(ncol(loadings)), function(i) loadings[which.max(abs(loadings[, i])), i], numeric(1))
  expect_true(all(strongest >= 0))
})

test_that("data / biplot tidy types output the selected score scale (#27224)", {
  preserve <- mtcars %>% do_prcomp(mpg, cyl, disp, hp, drat, wt)
  unitvar  <- mtcars %>% do_prcomp(mpg, cyl, disp, hp, drat, wt, score_scale = "unit_variance")
  d_preserve <- preserve %>% tidy_rowwise(model, type = "data")
  d_unitvar  <- unitvar  %>% tidy_rowwise(model, type = "data")
  expect_equal(sd(d_preserve$PC1), preserve$model[[1]]$sdev[1], tolerance = 1e-8)
  expect_equal(sd(d_unitvar$PC1), 1, tolerance = 1e-8)
  # Same solution, different score scale: the two PC1 columns are perfectly correlated.
  expect_equal(cor(d_preserve$PC1, d_unitvar$PC1), 1, tolerance = 1e-10)

  # Biplot observation rows carry the same scale (loading rows are appended after the scores).
  b_preserve <- preserve %>% tidy_rowwise(model, type = "biplot")
  b_unitvar  <- unitvar  %>% tidy_rowwise(model, type = "biplot")
  obs_preserve <- b_preserve$PC1[is.na(b_preserve$measure_name)]
  obs_unitvar  <- b_unitvar$PC1[is.na(b_unitvar$measure_name)]
  expect_equal(sd(obs_preserve), preserve$model[[1]]$sdev[1], tolerance = 1e-8)
  expect_equal(sd(obs_unitvar), 1, tolerance = 1e-8)
})

test_that("score-independent report outputs are identical under both score scales (#27224)", {
  preserve <- mtcars %>% do_prcomp(mpg, cyl, disp, hp, drat, wt)
  unitvar  <- mtcars %>% do_prcomp(mpg, cyl, disp, hp, drat, wt, score_scale = "unit_variance")
  for (ty in c("variances", "variances_judged", "loadings", "loadings_signed", "loadings_signed_wide",
               "contributions", "component_profiles", "variable_map", "representation",
               "screeplot", "parallel_screeplot")) {
    expect_equal(preserve %>% tidy_rowwise(model, type = ty),
                 unitvar %>% tidy_rowwise(model, type = ty), info = ty)
  }
})

test_that("old saved models without $scores fall back to the canonical scores (#27224)", {
  model_df <- mtcars %>% do_prcomp(mpg, cyl, disp, hp)
  reference <- model_df %>% tidy_rowwise(model, type = "data")
  model_df$model[[1]]$scores <- NULL      # model saved before #27224
  model_df$model[[1]]$score_scale <- NULL
  legacy <- model_df %>% tidy_rowwise(model, type = "data")
  expect_equal(legacy, reference)
  # The conditions table and the coefficients table degrade to the pre-#27224 meaning too.
  conditions <- model_df %>% tidy_rowwise(model, type = "analysis_conditions")
  expect_equal(conditions$Value[conditions$Metric == "Score Scale"], "Preserve Component Variance")
  coefficients <- model_df %>% tidy_rowwise(model, type = "coefficients")
  expect_equal(coefficients$`Score Coefficient`, coefficients$Coefficient, tolerance = 1e-12)
})

test_that("exp_kmeans is pinned to preserve_variance (#27224)", {
  km <- mtcars %>% exploratory:::exp_kmeans(mpg, cyl, disp, centers = 2)
  fit <- km$model[[1]]
  expect_equal(fit$score_scale, "preserve_variance")
  expect_equal(unname(fit$scores), unname(fit$x), tolerance = 1e-12)
})

test_that("unit_variance refuses to standardize a degenerate component (EXP-ANA-36, #27224)", {
  set.seed(27224)
  n <- 100
  df <- data.frame(a = rnorm(n), b = rnorm(n))
  df$c <- df$a + df$b # exactly collinear -> the last component has ~0 standard deviation
  # The default scale still works; only unit variance is impossible.
  expect_silent(df %>% do_prcomp(a, b, c))
  expect_error(df %>% do_prcomp(a, b, c, score_scale = "unit_variance"), "EXP-ANA-36")
})

test_that("do_prcomp rejects an unknown score_scale value (#27224)", {
  expect_error(mtcars %>% do_prcomp(mpg, cyl, disp, score_scale = "spss"))
})

test_that("analysis_conditions carries the Score Scale row (#27224)", {
  preserve <- (mtcars %>% do_prcomp(mpg, cyl, disp, hp)) %>% tidy_rowwise(model, type = "analysis_conditions")
  expect_true("Score Scale" %in% preserve$Metric)
  expect_equal(preserve$Value[preserve$Metric == "Score Scale"], "Preserve Component Variance")
  expect_equal(preserve$Description[preserve$Metric == "Score Scale"],
               "How principal-component scores are scaled.")
  expect_equal(preserve$status[preserve$Metric == "Score Scale"], "ok")
  # Sits between Normalization and SD Ratio, per the spec's Metric ordering.
  expect_equal(which(preserve$Metric == "Score Scale"), which(preserve$Metric == "Normalization") + 1L)
  expect_equal(which(preserve$Metric == "SD Ratio (Max/Min)"), which(preserve$Metric == "Score Scale") + 1L)

  unitvar <- (mtcars %>% do_prcomp(mpg, cyl, disp, hp, score_scale = "unit_variance")) %>%
    tidy_rowwise(model, type = "analysis_conditions")
  expect_equal(unitvar$Value[unitvar$Metric == "Score Scale"], "Unit Variance")
})

test_that("coefficients carry rotation / sdev as the score coefficient under unit_variance (#27224)", {
  model_df <- mtcars %>% do_prcomp(mpg, cyl, disp, hp, drat, wt, score_scale = "unit_variance")
  fit <- model_df$model[[1]]
  res <- model_df %>% tidy_rowwise(model, type = "coefficients")
  expect_equal(colnames(res), c("Variable", "Component", "Coefficient", "Score Coefficient"))
  # Component coefficient stays the (sign-stabilized) rotation; score coefficient is rotation/sdev.
  expected <- sweep(fit$rotation, 2, fit$sdev, "/")
  got <- res %>%
    dplyr::mutate(Component = as.character(Component)) %>%
    dplyr::arrange(match(Variable, rownames(expected)), match(Component, colnames(expected)))
  expect_equal(got$Coefficient, as.vector(t(fit$rotation)), tolerance = 1e-10)
  expect_equal(got$`Score Coefficient`, as.vector(t(expected)), tolerance = 1e-10)
  # Verifying the meaning: applying the score coefficients to the standardized data reproduces
  # the SD-1 scores the Data tab shows -- this is SPSS's Component Score Coefficient Matrix.
  standardized <- scale(as.matrix(mtcars[, rownames(expected)]))
  expect_equal(unname(standardized %*% expected), unname(fit$scores), tolerance = 1e-8)
})

test_that("polychoric fits honor the score scale too (#27224)", {
  df <- make_ordinal_survey()
  fit <- (df %>% do_prcomp(q1, q2, q3, q4, q5, q6,
                           cor_type = "polychoric", score_scale = "unit_variance"))$model[[1]]
  expect_equal(fit$score_scale, "unit_variance")
  expect_equal(unname(sweep(fit$scores, 2, fit$sdev, "*")), unname(fit$x), tolerance = 1e-10)
  # Approximate scores: dividing by the eigenvalue-derived sdev lands close to, but not exactly at,
  # SD 1 -- documented behavior (the report says so), so assert a bound rather than an exact 1.
  # Measured on this fixture: 0.976 .. 1.124, i.e. a max deviation of 0.124.
  expect_true(all(abs(apply(fit$scores, 2, sd) - 1) < 0.2))
})
