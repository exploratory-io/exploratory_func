# how to run this test:
# devtools::test(filter="join")

lower_col <- c("a","b","c", "d1a", "1eb","f1c")
upper_col <- c("A","B","C", "D1a", "1EB", "F1C", "G", "H")
value_lower_col <- c(1,2,3,4,5,6)
value_upper_col <- c(8,7,6,5,4,3,2,1)
other_lower_col <- c('aa','bb','cc','dd','ee', 'ff')
other_upper_col <- c('AA','BB','CC','DD','EE', 'FF', "GG", "HH")

lower_df <- data.frame(lower_col, value_lower_col, other_lower_col)
upper_df <- data.frame(upper_col, value_upper_col, other_upper_col)


context("test case insensitive join functions")

test_that("left_join with case insensitive", {
  df1 <- lower_df %>% left_join(upper_df, by = c("lower_col" = "upper_col"),
                                ignorecase = TRUE,
                                target_columns = c("upper_col","value_upper_col"))
  # lower_col value_lower_col other_lower_col value_upper_col
  # 1         a               1              aa               8
  # 2         b               2              bb               7
  # 3         c               3              cc               6
  # 4       d1a               4              dd               5
  # 5       1eb               5              ee               4
  # 6       f1c               6              ff               3
  expect_equal(unlist(df1 %>% filter(lower_col == "d1a") %>% select(value_upper_col))[[1]], 5)
})

test_that("left_join with case sensitive", {
  df2 <- lower_df %>% left_join(upper_df, by = c("lower_col" = "upper_col"))
  #lower_col value_lower_col other_lower_col value_upper_col other_upper_col
  # 1         a               1              aa              NA            <NA>
  # 2         b               2              bb              NA            <NA>
  # 3         c               3              cc              NA            <NA>
  # 4       d1a               4              dd              NA            <NA>
  # 5       1eb               5              ee              NA            <NA>
  # 6       f1c               6              ff              NA            <NA>
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

test_that("left_join with NA on target", {
  source_df <- readr::read_csv("a, b, c
1, 2, 3
2, 3, 4
3, 4, 5
4, 5, 6
5, 6, 7
NA,7, 8
NA,8, 9
NA,9,10")
  target_df <- readr::read_csv("a, d
1, a
2, b
3, c
4, e
5, f
NA,g
NA,h
NA,i
NA,j")
  result <- source_df %>% exploratory::left_join(target_df, dplyr::join_by(`a` == `a`))

# result looks like this and it does not match na with target data frame.
# a,b,c,d
# 1,2,3,a
# 2,3,4,b
# 3,4,5,c
# 4,5,6,e
# 5,6,7,f
# NA,7,8, NA
# NA,8,9, NA
# NA,9,10, NA

  expect_equal(nrow(result),8)
  expect_equal(ncol(result),4)
  expect_equal(nrow(dplyr::filter(result, is.na(a))),3)

  result <- source_df %>% exploratory::right_join(target_df, dplyr::join_by(`a` == `a`))

#  a	b	c	d
#  1	2	3	a
#  2	3	4	b
#  3	4	5	c
#  4	5	6	e
#  5	6	7	f
#  NA NA NA g
#  NA NA NA h
#  NA NA NA i
#  NA NA nA j

  expect_equal(nrow(result),9)
  expect_equal(ncol(result),4)
  expect_equal(nrow(dplyr::filter(result, is.na(a))),4)

  result <- source_df %>% exploratory::inner_join(target_df, dplyr::join_by(`a` == `a`))

  # a	b	c	d
  # 1	2	3	a
  # 2	3	4	b
  # 3	4	5	c
  # 4	5	6	e
  # 5	6	7	f

  expect_equal(nrow(result),5)
  expect_equal(ncol(result),4)
  expect_equal(nrow(dplyr::filter(result, is.na(a))),0)

  result <- source_df %>% exploratory::full_join(target_df, dplyr::join_by(`a` == `a`))

  # a   b   c  d
  # 1   2   3  a
  # 2   3   4  b
  # 3   4   5  c
  # 4   5   6  e
  # 5   6   7  f
  # NA  7   8  NA
  # NA  8   9  NA
  # NA  9   10 NA
  # NA  NA  NA g
  # NA  NA  NA h
  # NA  NA  NA i
  # NA  NA  NA j

  expect_equal(nrow(result),12)
  expect_equal(ncol(result),4)
  expect_equal(nrow(dplyr::filter(result, is.na(a))),7)

})

test_that("cross_join with selected column", {
  source <- readr::read_csv('"ID","製品製品名"
1,"机"
2,"ライト"
3,"椅子"')

 target <- readr::read_csv('"ID","色","適用"
1,"赤","明るめ"
2,"青","暗め"
3,"白","普通"')

  df <- cross_join(source, target, target_columns = (c("適用")), exclude_target_columns=TRUE)
  # > df
  # A tibble: 9 × 5
  # ID.x 製品製品名  ID.y 色
  # <dbl> <chr>      <dbl> <chr>
  # 1     1 机             1 赤
  # 2     1 机             2 青
  # 3     1 机             3 白
  # 4     2 ライト         1 赤
  # 5     2 ライト         2 青
  # 6     2 ライト         3 白
  # 7     3 椅子           1 赤
  # 8     3 椅子           2 青
  # 9     3 椅子           3 白
  expect_equal(nrow(df), 9)
  expect_equal(ncol(df), 4)
  expect_equal(stringr::str_detect(colnames(df), "適用"), c(FALSE, FALSE, FALSE, FALSE))

})

test_that("column suffix argument with case insensitive", {
  df13 <- mtcars %>% left_join(mtcars, by = c("gear" = "gear", "cyl" = "cyl"), suffix = c("1", "2"), ignorecase = TRUE)
  expect_equal("mpg1" %in% colnames(df13) , TRUE)
  expect_equal("mpg2" %in% colnames(df13) , TRUE)
})

test_that("column suffix argument with case insensitive and empty source suffix", {
  df14 <- mtcars %>% left_join(mtcars, by = c("gear" = "gear", "cyl" = "cyl"), suffix = c("", "_1"), ignorecase = TRUE)
  expect_equal("mpg" %in% colnames(df14) , TRUE)
  expect_equal("mpg_1" %in% colnames(df14) , TRUE)
})

test_that("column suffix argument with case insensitive and empty source suffix and exclude_selected_columns", {
  df15 <- mtcars %>% left_join(mtcars, by = c("gear" = "gear", "cyl" = "cyl"), suffix = c("", "_1"), ignorecase = TRUE, target_columns = c("mpg", "am", "vs"), exclude_target_columns = TRUE)
  expect_equal("carb" %in% colnames(df15) , TRUE)
  expect_equal("carb_1" %in% colnames(df15) , TRUE)
})

test_that("group by target data frame case", {
  target <- mtcars %>% group_by(mpg)
  df16 <- mtcars %>% select(-mpg) %>% left_join(target, by = c("gear" = "gear", "cyl" = "cyl"), suffix = c("", "_1"), ignorecase = TRUE, target_columns = c("mpg", "am", "vs"), exclude_target_columns = TRUE)
  expect_equal("mpg" %nin% colnames(df16) , TRUE)
  expect_equal("mpg_1" %nin% colnames(df16) , TRUE)
})

# ---- #33983: dplyr `multiple` / `relationship` pass-through ----
context("test join multiple / relationship pass-through (#33983)")

mx <- data.frame(id = c(1, 2, 3, NA), xv = c("a", "b", "c", "d"), stringsAsFactors = FALSE)
my <- data.frame(id = c(1, 1, 2, NA, NA), yv = c("p", "q", "r", "s", "t"), stringsAsFactors = FALSE)

test_that("left_join forwards multiple = first / last / any / all like dplyr", {
  for (m in c("first", "last", "any", "all")) {
    res <- left_join(mx, my, by = "id", multiple = m)
    expected <- dplyr::left_join(mx, my, by = "id", multiple = m, na_matches = "never")
    expect_equal(res, expected, info = m)
  }
  expect_equal(nrow(left_join(mx, my, by = "id", multiple = "first")), 4)  # NA never matches: x rows 1,2,3,NA
  expect_equal(left_join(mx, my, by = "id", multiple = "first")$yv[1], "p")
  expect_equal(left_join(mx, my, by = "id", multiple = "last")$yv[1], "q")
})

test_that("left_join default is unchanged (all matches kept)", {
  expect_equal(nrow(left_join(mx, my, by = "id")), 5)  # id 1 matches two rows; NA never matches
  expect_equal(left_join(mx, my, by = "id"), dplyr::left_join(mx, my, by = "id", na_matches = "never"))
})

test_that("inner_join / right_join / full_join forward multiple", {
  expect_equal(inner_join(mx, my, by = "id", multiple = "first"), dplyr::inner_join(mx, my, by = "id", multiple = "first", na_matches = "never"))
  expect_equal(nrow(inner_join(mx, my, by = "id", multiple = "first")), 2)
  expect_equal(inner_join(mx, my, by = "id", multiple = "last"), dplyr::inner_join(mx, my, by = "id", multiple = "last", na_matches = "never"))
  expect_equal(right_join(mx, my, by = "id", multiple = "first"), dplyr::right_join(mx, my, by = "id", multiple = "first", na_matches = "never"))
  expect_equal(full_join(mx, my, by = "id", multiple = "last"), dplyr::full_join(mx, my, by = "id", multiple = "last", na_matches = "never"))
})

test_that("multiple works with ignorecase", {
  cx <- data.frame(k = c("A", "b", "C"), xv = 1:3, stringsAsFactors = FALSE)
  cy <- data.frame(k = c("a", "A", "B", "c"), yv = c("p", "q", "r", "s"), stringsAsFactors = FALSE)
  res_all <- left_join(cx, cy, by = "k", ignorecase = TRUE)
  expect_equal(nrow(res_all), 4)
  res_first <- left_join(cx, cy, by = "k", ignorecase = TRUE, multiple = "first")
  expect_equal(nrow(res_first), 3)
  expect_equal(res_first$yv, c("p", "r", "s"))
  res_last <- left_join(cx, cy, by = "k", ignorecase = TRUE, multiple = "last")
  expect_equal(res_last$yv, c("q", "r", "s"))
  res_inner <- inner_join(cx, cy, by = "k", ignorecase = TRUE, multiple = "first")
  expect_equal(nrow(res_inner), 3)
  # right/full also go through the case-insensitive helper
  expect_equal(nrow(right_join(cx, cy, by = "k", ignorecase = TRUE, multiple = "first")), 4)
  expect_equal(nrow(full_join(cx, cy, by = "k", ignorecase = TRUE, multiple = "first")), 4)
})

test_that("multiple works with target_columns, na_matches and suffix", {
  res <- left_join(mx, my, by = "id", multiple = "first", target_columns = c("id", "yv"))
  expect_equal(nrow(res), 4)
  res_na <- left_join(mx, my, by = "id", multiple = "first", na_matches = "na")
  expect_equal(res_na, dplyr::left_join(mx, my, by = "id", multiple = "first", na_matches = "na"))
  expect_equal(res_na$yv[4], "s")
  expect_true(is.na(left_join(mx, my, by = "id", multiple = "first")$yv[4]))
  sx <- data.frame(id = 1:2, v = c("a", "b"), stringsAsFactors = FALSE)
  sy <- data.frame(id = c(1, 1, 2), v = c("p", "q", "r"), stringsAsFactors = FALSE)
  res_sfx <- left_join(sx, sy, by = "id", multiple = "last", suffix = c("_src", "_tgt"))
  expect_equal(names(res_sfx), c("id", "v_src", "v_tgt"))
  expect_equal(res_sfx$v_tgt, c("q", "r"))
})

test_that("multiple works with join_by inequality key", {
  ix <- data.frame(a = c(5, 10), stringsAsFactors = FALSE)
  iy <- data.frame(b = c(1, 3, 7), stringsAsFactors = FALSE)
  res <- left_join(ix, iy, by = dplyr::join_by(a >= b), multiple = "last")
  expect_equal(nrow(res), 2)
  expect_equal(res$b, c(3, 7))
})

test_that("relationship is forwarded (proves pass-through)", {
  expect_error(left_join(mx, my, by = "id", relationship = "many-to-one"))
  expect_equal(nrow(left_join(mx, my, by = "id", relationship = "many-to-many")), 5)
  expect_error(left_join(mx, my, by = "id", ignorecase = TRUE, relationship = "many-to-one"))
})

test_that("multiple works for a column named with symbols and multibyte chars", {
  nm <- "航空 会社 !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表"
  sx <- stats::setNames(data.frame(c(1, 2), c("a", "b"), stringsAsFactors = FALSE), c(nm, "xv"))
  sy <- stats::setNames(data.frame(c(1, 1, 2), c("p", "q", "r"), stringsAsFactors = FALSE), c(nm, "yv"))
  res <- left_join(sx, sy, by = nm, multiple = "first")
  expect_equal(nrow(res), 2)
  expect_equal(res$yv, c("p", "r"))
  res_ic <- left_join(sx, sy, by = nm, multiple = "last", ignorecase = TRUE)
  expect_equal(res_ic$yv, c("q", "r"))
})

test_that("semi_join / anti_join with ignorecase still work (helper unchanged for them)", {
  cx <- data.frame(k = c("A", "b", "z"), stringsAsFactors = FALSE)
  cy <- data.frame(k = c("a", "B"), stringsAsFactors = FALSE)
  expect_equal(nrow(semi_join(cx, cy, by = "k", ignorecase = TRUE)), 2)
  expect_equal(nrow(anti_join(cx, cy, by = "k", ignorecase = TRUE)), 1)
})

test_that("a stray unknown argument is still silently ignored (no wholesale dots forwarding)", {
  expect_equal(nrow(left_join(mx, my, by = "id", some_unknown_arg = 1)), 5)
})
