# how to run this test:
# devtools::test(filter="factanal")
context("test factor analysis function, exp_factanal")

test_that("exp_factanal", {
  df <- mtcars %>% mutate(new_col = c(rep("A", n() - 10), rep("B", 10)))

  check_output <- function(model_df) {
    res <- model_df %>% glance_rowwise(model, pretty.name=TRUE)
    expect_equal(colnames(res),
                 c("Number of Factors", "Total Variance", "Chi-Square", "P Value", "Degree of Freedom", "Number of Rows"))
    res <- model_df %>% tidy_rowwise(model, type="variances")
    expect_equal(colnames(res),
                 c("SS loadings", "Proportion Var", "Cumulative Var", "Proportion Explained", "Cumulative Proportion", "Factor", "% Variance", "Cummulated % Variance"))
    res <- model_df %>% tidy_rowwise(model, type="loadings")
    expect_equal(colnames(res),
                 c("variable", "factor", "value"))
    res <- model_df %>% tidy_rowwise(model, type="biplot")
    expect_equal(colnames(res),
                 c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb", "new_col", ".factor_1", ".factor_2", ".variable", ".factor_2_variable"))
    res <- model_df %>% tidy_rowwise(model, type="screeplot")
    expect_equal(colnames(res),
                 c("factor", "eigenvalue"))
    res <- model_df %>% tidy_rowwise(model, type="data")
    expect_equal(colnames(res),
                 c("mpg","cyl","disp","hp","drat","wt","qsec","vs","am","gear","carb","new_col","Factor 1","Factor 2"))
  }

  model_df <- exp_factanal(df, cyl, mpg, hp, max_nrow=30, fm="minres") 
  check_output(model_df)
  model_df <- exp_factanal(df, cyl, mpg, hp, max_nrow=30, fm="ml")
  check_output(model_df)
  # model_df <- exp_factanal(df, cyl, mpg, hp, max_nrow=30, fm="pa") # TODO: This gives error "NaNs produced"
  # check_output(model_df)
  model_df <- exp_factanal(df, cyl, mpg, hp, max_nrow=30, fm="ols")
  check_output(model_df)
  model_df <- exp_factanal(df, cyl, mpg, hp, max_nrow=30, fm="wls")
  check_output(model_df)
  model_df <- exp_factanal(df, cyl, mpg, hp, max_nrow=30, fm="gls")
  check_output(model_df)
  model_df <- exp_factanal(df, cyl, mpg, hp, max_nrow=30, fm="minchi")
  check_output(model_df)
  model_df <- exp_factanal(df, cyl, mpg, hp, max_nrow=30, fm="minrank")
  check_output(model_df)
  # model_df <- exp_factanal(df, cyl, mpg, hp, max_nrow=30, fm="alpha") # TODO: This gives error "NaNs produced"
  # check_output(model_df)
})

test_that("exp_factanal with strange column name and all-NA column", {
  df <- mtcars %>%
    rename(`Cy l` = cyl) %>%
    mutate(new_col = c(rep("A", n() - 10), rep("B", 10))) %>%
    mutate(na_col = NA)
  model_df <- exp_factanal(df, `Cy l`, mpg, hp, na_col)
  res <- model_df %>% glance_rowwise(model, pretty.name=TRUE)
  expect_equal(colnames(res),
               c("Number of Factors", "Total Variance", "Chi-Square", "P Value", "Degree of Freedom", "Number of Rows"))
  res <- model_df %>% tidy_rowwise(model, type="variances")
  expect_equal(colnames(res),
               c("SS loadings", "Proportion Var", "Cumulative Var", "Proportion Explained", "Cumulative Proportion", "Factor", "% Variance", "Cummulated % Variance"))
  res <- model_df %>% tidy_rowwise(model, type="loadings")
  expect_equal(colnames(res),
               c("variable", "factor", "value"))
  res <- model_df %>% tidy_rowwise(model, type="biplot")
  expect_equal(colnames(res),
               c("mpg", "Cy l", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb", "new_col", ".factor_1", ".factor_2", ".variable", ".factor_2_variable"))
  res <- model_df %>% tidy_rowwise(model, type="screeplot")
  expect_equal(colnames(res),
               c("factor", "eigenvalue"))
  res <- model_df %>% tidy_rowwise(model, type="data")
  expect_equal(colnames(res),
               c("mpg","Cy l","disp","hp","drat","wt","qsec","vs","am","gear","carb","new_col","Factor 1","Factor 2"))
})
