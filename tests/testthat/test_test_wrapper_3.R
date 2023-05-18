
context("tests for wrappers of Kraskal-Wallis tests")

test_that("test exp_kruskal", {
  model_df <- exp_kruskal(mtcars, mpg, gear)
  ret <- model_df %>% tidy_rowwise(model, type="pairs")
  ret <- model_df %>% tidy_rowwise(model, type="prob_dist")
  expect_true("p.value" %in% colnames(ret))

  ret <- model_df %>% tidy_rowwise(model, type="model")
  expect_true("Eta Squared" %in% colnames(ret))
  expect_true(ret$`Eta Squared` > 0.424)
  expect_true(ret$`Eta Squared` < 0.425)
  
  ret <- model_df %>% tidy_rowwise(model, type="data_summary")
  expect_equal(colnames(ret),
               c("gear","Rows","Mean","Conf Low","Conf High","Std Error of Mean","Std Deviation",
                 "Minimum","Maximum"))
})

test_that("test exp_kruskal with group_by", {
  model_df <- mtcars %>%
    group_by(am) %>%
    mutate(gear = factor(gear)) %>% exp_kruskal(mpg, gear)
  ret <- model_df %>% tidy_rowwise(model, type="prob_dist")
  ret <- model_df %>% tidy_rowwise(model, type="model")
  ret <- model_df %>% tidy_rowwise(model, type="data_summary")
  expect_equal(colnames(ret),
               c("am","gear","Rows","Mean","Conf Low","Conf High","Std Error of Mean","Std Deviation",
                 "Minimum","Maximum"))
})

test_that("test exp_kruskal with group-level error", {
  df <- tibble::tibble(group=c(1,1,2,2),category=c("a","a","b","b"),value=c(1,2,1,2))
  model_df <- df %>% dplyr::group_by(`group`) %>% exp_kruskal(`value`, `category`)
  ret <- model_df %>% tidy_rowwise(model, type='model')
  expect_equal(colnames(ret),
               c("group","Note"))
  ret <- model_df %>% tidy_rowwise(model, type='prob_dist')
  expect_equal(nrow(ret), 0)
})


test_that("test exp_kruskal with pairs_adjust types.", {
  model_df <- mtcars %>% exp_kruskal(mpg, cyl, pairs_adjust = "bonferroni")
  ret <- model_df %>% tidy_rowwise(model, type="pairs")
  expect_equal(nrow(ret), 3) # for cyl=4, 6, 8.

  model_df <- mtcars %>% exp_kruskal(mpg, cyl, pairs_adjust = "sidak")
  ret <- model_df %>% tidy_rowwise(model, type="pairs")
  expect_equal(nrow(ret), 3)

  model_df <- mtcars %>% exp_kruskal(mpg, cyl, pairs_adjust = "holm")
  ret <- model_df %>% tidy_rowwise(model, type="pairs")
  expect_equal(nrow(ret), 3)

  model_df <- mtcars %>% exp_kruskal(mpg, cyl, pairs_adjust = "hochberg")
  ret <- model_df %>% tidy_rowwise(model, type="pairs")
  expect_equal(nrow(ret), 3)

  model_df <- mtcars %>% exp_kruskal(mpg, cyl, pairs_adjust = "none")
  ret <- model_df %>% tidy_rowwise(model, type="pairs")
  expect_equal(nrow(ret), 3)
})


