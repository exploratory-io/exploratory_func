# how to run this test:
# devtools::test(filter="prcomp")
context("test prcomp functions")

test_that("do_prcomp", {
  model_df <- do_prcomp(mtcars, cyl, mpg, hp)
  model_df %>% tidy(model, type="variances")
  model_df %>% tidy(model, type="loadings")
  model_df %>% tidy(model, type="biplot")
  model_df %>% tidy(model, type="data")
})

test_that("do_prcomp with strange column name", {
  df <- mtcars %>% rename('Cy l' = cyl)
  model_df <- do_prcomp(mtcars, `Cy l`, mpg, hp)
  model_df %>% tidy(model, type="variances")
  model_df %>% tidy(model, type="loadings")
  model_df %>% tidy(model, type="biplot")
  model_df %>% tidy(model, type="data")
})
