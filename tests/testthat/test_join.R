# how to run this test:
# devtools::test(filter="join")

lower_col <- c("a","b","c", "d1a", "1eb","f1c")
upper_col <- c("A","B","C", "D1a", "1EB", "F1C", "G", "H")
value_lower_col <- c(1,2,3,4,5,6)
value_upper_col <- c(8,7,6,5,4,3,2,1)
lower_df <- data.frame(lower_col, value_lower_col)
upper_df <- data.frame(upper_col, value_upper_col)


context("test case insensitive join functions")

test_that("left_join with case insensitive", {
  df1 <- lower_df %>% left_join(upper_df, by = c("lower_col" = "upper_col"), ignorecase = TRUE)
  # > df1
  # lower_col value_lower_col value_upper_col
  # 1         a               1               8
  # 2         b               2               7
  # 3         c               3               6
  # 4       d1a               4               5
  # 5       1eb               5               4
  # 6       f1c               6               3
  expect_equal(unlist(df1 %>% filter(lower_col == "d1a") %>% select(value_upper_col))[[1]], 5)
})

test_that("left_join with case sensitive", {
  df2 <- lower_df %>% left_join(upper_df, by = c("lower_col" = "upper_col"))
  # > df2
  #   lower_col value_lower_col value_upper_col
  # 1         a               1              NA
  # 2         b               2              NA
  # 3         c               3              NA
  # 4       d1a               4              NA
  # 5       1eb               5              NA
  # 6       f1c               6              NA
  result <- df2 %>% filter(lower_col == "d1a") %>% select(value_upper_col)
  expect_equal(is.na(result$value_upper_col),TRUE)
})

test_that("right_join with case insensitive", {
  df3 <- lower_df %>% right_join(upper_df, by = c("lower_col" = "upper_col"), ignorecase = TRUE)
  # > df3
  #   lower_col value_lower_col value_upper_col
  # 1         a               1               8
  # 2         b               2               7
  # 3         c               3               6
  # 4       d1a               4               5
  # 5       1eb               5               4
  # 6       f1c               6               3
  # 7      <NA>              NA               2
  # 8      <NA>              NA               1
  expect_equal(unlist(df3 %>% filter(lower_col == "d1a") %>% select(value_upper_col))[[1]], 5)
  expect_equal(unlist(df3 %>% filter(lower_col == "d1a") %>% select(value_lower_col))[[1]], 4)
})

test_that("right_join with case sensitive", {
  df4 <- lower_df %>% right_join(upper_df, by = c("lower_col" = "upper_col"))
  #> df4
  #   lower_col value_lower_col value_upper_col
  # 1         A              NA               8
  # 2         B              NA               7
  # 3         C              NA               6
  # 4       D1a              NA               5
  # 5       1EB              NA               4
  # 6       F1C              NA               3
  # 7         G              NA               2
  # 8         H              NA               1
  expect_equal(unlist(df4 %>% filter(lower_col == "D1a") %>% select(value_upper_col))[[1]], 5)
  result <- df4 %>% filter(lower_col == "D1a") %>% select(value_lower_col)
  expect_equal(is.na(result$value_lower_col),TRUE)
})

test_that("full_join with case insensitive", {
  df5 <- lower_df %>% full_join(upper_df, by = c("lower_col" = "upper_col"), ignorecase = TRUE)
  # > df5
  #   lower_col value_lower_col value_upper_col
  # 1         a               1               8
  # 2         b               2               7
  # 3         c               3               6
  # 4       d1a               4               5
  # 5       1eb               5               4
  # 6       f1c               6               3
  # 7      <NA>              NA               2
  # 8      <NA>              NA               1
  expect_equal(unlist(df5 %>% filter(lower_col == "d1a") %>% select(value_upper_col))[[1]], 5)
  expect_equal(unlist(df5 %>% filter(lower_col == "d1a") %>% select(value_lower_col))[[1]], 4)
})

test_that("full_join with case sensitive", {
  df6 <- lower_df %>% full_join(upper_df, by = c("lower_col" = "upper_col"))
  # > df6
  #    lower_col value_lower_col value_upper_col
  # 1          a               1              NA
  # 2          b               2              NA
  # 3          c               3              NA
  # 4        d1a               4              NA
  # 5        1eb               5              NA
  # 6        f1c               6              NA
  # 7          A              NA               8
  # 8          B              NA               7
  # 9          C              NA               6
  # 10       D1a              NA               5
  # 11       1EB              NA               4
  # 12       F1C              NA               3
  # 13         G              NA               2
  # 14         H              NA               1 
  expect_equal(unlist(df6 %>% filter(lower_col == "D1a") %>% select(value_upper_col))[[1]], 5)
  result <- df6 %>% filter(lower_col == "D1a") %>% select(value_lower_col)
  expect_equal(is.na(result$value_lower_col),TRUE)
})

test_that("inner_join with case insensitive", {
  df7 <- lower_df %>% inner_join(upper_df, by = c("lower_col" = "upper_col"), ignorecase = TRUE)
  # > df7
  #   lower_col value_lower_col value_upper_col
  # 1         a               1               8
  # 2         b               2               7
  # 3         c               3               6
  # 4       d1a               4               5
  # 5       1eb               5               4
  # 6       f1c               6               3
  expect_equal(unlist(df7 %>% filter(lower_col == "d1a") %>% select(value_upper_col))[[1]], 5)
  expect_equal(unlist(df7 %>% filter(lower_col == "d1a") %>% select(value_lower_col))[[1]], 4)
})

test_that("inner_join with case sensitive", {
  df8 <- lower_df %>% inner_join(upper_df, by = c("lower_col" = "upper_col"))
  # > df8
  # [1] lower_col       value_lower_col value_upper_col
  # <0 rows> (or 0-length row.names)
  expect_equal(nrow(df8),0)
})

test_that("semi_join with case insensitive", {
  df9 <- lower_df %>% semi_join(upper_df, by = c("lower_col" = "upper_col"), ignorecase = TRUE)
  # > df9
  # lower_col value_lower_col
  # 1         a               1
  # 2         b               2
  # 3         c               3
  # 4       d1a               4
  # 5       1eb               5
  # 6       f1c               6
  expect_equal(unlist(df9 %>% filter(lower_col == "d1a") %>% select(value_lower_col))[[1]], 4)
})

test_that("semi_join with case sensitive", {
  df10 <- lower_df %>% semi_join(upper_df, by = c("lower_col" = "upper_col"))
  #> df10
  #[1] lower_col       value_lower_col
  #<0 rows> (or 0-length row.names)  expect_equal(nrow(df10),0)
  expect_equal(nrow(df10),0)
})

test_that("anti_join with case insensitive", {
  df11 <- lower_df %>% anti_join(upper_df, by = c("lower_col" = "upper_col"), ignorecase = TRUE)
  # > df11
  # [1] lower_col       value_lower_col
  # <0 rows> (or 0-length row.names)  expect_equal(unlist(df11 %>% filter(lower_col == "d1a") %>% select(value_lower_col))[[1]], 4)
  expect_equal(nrow(df11),0)
})

test_that("anti_join with case sensitive", {
  df12 <- lower_df %>% anti_join(upper_df, by = c("lower_col" = "upper_col"))
  # > df12
  #   lower_col value_lower_col
  # 1         a               1
  # 2         b               2
  # 3         c               3
  # 4       d1a               4
  # 5       1eb               5
  # 6       f1c               6
  expect_equal(unlist(df12 %>% filter(lower_col == "d1a") %>% select(value_lower_col))[[1]], 4)
})
