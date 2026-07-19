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
  expect_true(all(c("Rows Used","Variables Used","Normalization","SD Ratio (Max/Min)") %in% res$Metric))
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
  expect_equal(colnames(res), c("Variable", "Component", "Coefficient"))
  expect_true(any(res$Coefficient < 0))            # signed eigenvector weights
  expect_true(nrow(res) == 6 * length(unique(res$Component)))  # vars x components
  # kmeans fit -> empty
  km <- mtcars %>% exploratory:::exp_kmeans(mpg, cyl, disp, centers = 2)
  r2 <- km %>% tidy_rowwise(model, type = "coefficients")
  expect_equal(nrow(r2), 0)
  expect_equal(colnames(r2), c("Variable", "Component", "Coefficient"))
})
