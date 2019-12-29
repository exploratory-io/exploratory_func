# how to run this test:
# devtools::test(filter="kmeans")
context("test kmeans analytics view functions")
test_that("exp_kmeans", {
  df <- mtcars %>% mutate(new_col = c(rep("A", n() - 10), rep("B", 10)))
  model_df <- exp_kmeans(df, cyl, mpg, hp, max_nrow=30)
  model_df %>% tidy(model, type="variances")
  model_df %>% tidy(model, type="loadings")
  model_df %>% tidy(model, type="biplot")
  model_df %>% tidy(model, type="data")
  res <- model_df %>% tidy(model, type="gathered_data")
  res <- model_df %>% tidy(model, type="gathered_data", normalize_data=TRUE, n_sample=20)
})

test_that("exp_kemans with strange column name", {
  df <- mtcars %>%
    rename(`Cy l` = cyl) %>%
    mutate(new_col = c(rep("A", n() - 10), rep("B", 10)))
  model_df <- exp_kmeans(df, `Cy l`, mpg, hp)
  model_df %>% tidy(model, type="variances")
  model_df %>% tidy(model, type="loadings")
  model_df %>% tidy(model, type="biplot")
  model_df %>% tidy(model, type="data")
  model_df %>% tidy(model, type="gathered_data")
  res <- model_df %>% tidy(model, type="gathered_data", normalize_data=TRUE, n_sample=100) # testing n_sample more than nrow()
})

test_that("exp_kemans with single column name", {
  model_df <- exp_kmeans(mtcars, mpg)
  model_df %>% tidy(model, type="variances")
  # model_df %>% tidy(model, type="loadings") # Not used.
  # model_df %>% tidy(model, type="biplot") # Will skip for single column case.
  model_df %>% tidy(model, type="data")
  model_df %>% tidy(model, type="gathered_data")
  res <- model_df %>% tidy(model, type="gathered_data", normalize_data=TRUE, n_sample=100) # testing n_sample more than nrow()
})

test_that("exp_kmeans elbow method mode", {
  df <- mtcars %>% mutate(new_col = c(rep("A", n() - 10), rep("B", 10)))
  model_df <- exp_kmeans(df, cyl, mpg, hp, elbow_method_mode=TRUE)
  model_df %>% tidyr::unnest(model)
})

test_that("exp_kmeans elbow method mode with group_by", {
  df <- mtcars %>% mutate(new_col = c(rep("A", n() - 10), rep("B", 10)))
  df <- df %>% group_by(new_col)
  model_df <- exp_kmeans(df, cyl, mpg, hp, elbow_method_mode=TRUE, max_centers=3)
  model_df %>% tidyr::unnest(model)
})
