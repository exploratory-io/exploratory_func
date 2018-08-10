# how to run this test:
# devtools::test(filter="kmeans")
context("test kmeans analytics view functions")

test_that("exp_kmeans", {
  df <- mtcars %>% mutate(new_col = c(rep("A", n() - 10), rep("B", 10)))
  model_df <- exp_kmeans(df, cyl, mpg, hp)
  model_df %>% tidy(model, type="variances")
  model_df %>% tidy(model, type="loadings")
  model_df %>% tidy(model, type="biplot")
  model_df %>% tidy(model, type="data")
  res <- model_df %>% tidy(model, type="gathered_data")
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
})
