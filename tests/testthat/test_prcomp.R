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
