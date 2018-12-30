# how to run this test:
# devtools::test(filter="prcomp")
context("test prcomp functions")

test_that("do_prcomp", {
  df <- mtcars %>% mutate(new_col = c(rep("A", n() - 10), rep("B", 10)))
  model_df <- do_prcomp(df, cyl, mpg, hp, max_nrow=30)
  model_df %>% tidy(model, type="variances")
  model_df %>% tidy(model, type="loadings")
  model_df %>% tidy(model, type="biplot")
  model_df %>% tidy(model, type="data")
})

test_that("do_prcomp with strange column name", {
  df <- mtcars %>%
    rename(`Cy l` = cyl) %>%
    mutate(new_col = c(rep("A", n() - 10), rep("B", 10)))
  model_df <- do_prcomp(df, `Cy l`, mpg, hp)
  model_df %>% tidy(model, type="variances")
  model_df %>% tidy(model, type="loadings")
  model_df %>% tidy(model, type="biplot")
  model_df %>% tidy(model, type="data")
})
