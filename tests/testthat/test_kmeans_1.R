# how to run this test:
# devtools::test(filter="kmeans")
context("test kmeans analytics view functions")

test_that("exp_kmeans", {
  df <- mtcars %>% mutate(new_col = c(rep("A", n() - 10), rep("B", 10)))
  model_df <- exp_kmeans(df, cyl, mpg, hp, max_nrow=30)
  model_df %>% tidy_rowwise(model, type="variances")
  model_df %>% tidy_rowwise(model, type="loadings")
  model_df %>% tidy_rowwise(model, type="biplot")
  model_df %>% tidy_rowwise(model, type="data")
  res <- model_df %>% tidy_rowwise(model, type="gathered_data")
  res <- model_df %>% tidy_rowwise(model, type="gathered_data", normalize_data=TRUE, n_sample=20)
  expect_equal(colnames(res),
               c("disp","drat","wt","qsec","vs","am","gear","carb","new_col","cluster","PC1","PC2","PC3","row_id","key",
                 "value"))
})

test_that("exp_kmeans with strange column name", {
  df <- mtcars %>%
    rename(`Cy l` = cyl) %>%
    mutate(new_col = c(rep("A", n() - 10), rep("B", 10)))

  # Add list column and difftime column etc. which used to cause error until we removed it as preprocessing.
  df <- df %>% mutate(posix1=lubridate::ymd_hm("2021-01-01 00:00"),
                      posix2=lubridate::ymd_hm("2021-01-02 00:00"),
                      difftime = posix2-posix1, dur = as.duration(difftime),
                      intv = as.interval(posix1, posix2),
                      period = as.period(intv),
                      str="a,b,c",
                      list=stringr::str_split(str,","))
  df <- df %>% select(-posix1, -posix2, -str) # Remove POSIXct column and character column we used to create those special columns.

  model_df <- exp_kmeans(df, `Cy l`, mpg, hp)
  model_df %>% tidy_rowwise(model, type="variances")
  model_df %>% tidy_rowwise(model, type="loadings")
  model_df %>% tidy_rowwise(model, type="biplot")
  model_df %>% tidy_rowwise(model, type="data")
  model_df %>% tidy_rowwise(model, type="gathered_data")
  res <- model_df %>% tidy_rowwise(model, type="gathered_data", normalize_data=TRUE, n_sample=100) # testing n_sample more than nrow()
  expect_equal(colnames(res),
               c("disp","drat","wt","qsec","vs","am","gear","carb","new_col","cluster","PC1","PC2","PC3","row_id","key",
                 "value"))
})

test_that("exp_kmeans with single column name", {
  model_df <- exp_kmeans(mtcars, mpg)
  model_df %>% tidy_rowwise(model, type="variances")
  # model_df %>% tidy_rowwise(model, type="loadings") # Not used.
  # model_df %>% tidy_rowwise(model, type="biplot") # Will skip for single column case.
  model_df %>% tidy_rowwise(model, type="data")
  model_df %>% tidy_rowwise(model, type="gathered_data")
  res <- model_df %>% tidy_rowwise(model, type="gathered_data", normalize_data=TRUE, n_sample=100) # testing n_sample more than nrow()
  expect_equal(colnames(res),
               c("cyl","disp","hp","drat","wt","qsec","vs","am","gear","carb","cluster","PC1","row_id","key","value"))
})

test_that("exp_kmeans elbow method mode", {
  df <- mtcars %>% mutate(new_col = c(rep("A", n() - 10), rep("B", 10)))
  model_df <- exp_kmeans(df, cyl, mpg, hp, elbow_method_mode=TRUE)
  res <- model_df %>% tidyr::unnest(model)
  expect_equal(colnames(res), c("center","totss","tot.withinss","betweenss","iter"))
})

# group_by for elbow method is not currently supported. Revive this test when it is.
#test_that("exp_kmeans elbow method mode with group_by", {
#  df <- mtcars %>% mutate(new_col = c(rep("A", n() - 10), rep("B", 10)))
#  df <- df %>% group_by(new_col)
#  model_df <- exp_kmeans(df, cyl, mpg, hp, elbow_method_mode=TRUE, max_centers=3)
#  model_df %>% tidyr::unnest(model)
#})
