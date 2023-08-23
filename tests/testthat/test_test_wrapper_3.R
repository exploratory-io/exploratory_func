
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

  # Test with bucketed data.
  range.text <- c( rep("1 - 2", 8), rep("2 - 3", 8), rep("3 - 4", 8), rep("4 - 5", 8))
  model_df <- mtcars %>% mutate(range= range.text) %>% exp_kruskal(mpg, range, pairs_adjust = "bonferroni")
  ret <- model_df %>% tidy_rowwise(model, type="pairs")
  expect_equal(nrow(ret), 6) # Combination of 4 range groups.
  expect_equal(colnames(ret), c("Group 1", "Group 2", "Z Value", "P Value", "Method")) 
  # Make sure it is in "1 - 2" format, not in "1" or "1 - 2 - 3".
  expect_equal(ret$`Group 1`, c("1 - 2", "1 - 2", "2 - 3", "1 - 2", "2 - 3", "3 - 4")) 
  # Make sure it is in "1 - 2" format, not in "1" or "1 - 2 - 3".
  expect_equal(ret$`Group 2`, c("2 - 3", "3 - 4", "3 - 4", "4 - 5", "4 - 5", "4 - 5")) 

})


