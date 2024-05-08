context("check util functions")
test_that("bind_rows_safe", {
  df1 <- tibble::tibble(a...1=1, b=1)
  df2 <- tibble::tibble(a...1=2, c=2)
  res <- dplyr::bind_rows(df1, df2)
  expect_equal(colnames(res), c("a", "b", "c")) # To check if this fix is still necessary when dplyr is upgraded in future.
  res <- exploratory:::bind_rows_safe(df1, df2) # This is an internal function.
  expect_equal(colnames(res), c("a...1", "b", "c"))
})

test_that("bind_rows", {
  library(dplyr)
  res <- mtcars %>% exploratory::bind_rows(list(acars = mtcars, bcars = mtcars), id_column_name="dataf", current_df_name="firstMtcars")
  expect_equal(unique(res$dataf), c("firstMtcars", "acars", "bcars"))
  res2 <- mtcars %>% exploratory::bind_rows(mtcars, mtcars, id_column_name="dataf", force_data_type = TRUE)
  expect_equal(unique(res2$dataf), c(1,2,3))
  # For data1, test1 column is factor data type
  data1 <- data.frame(person = c("A","B","C"),
                      test1 = as.factor(c(1,4,5)),
                      test2 = c(14,25,10),
                      test3 = c(12.5,16.0,4),
                      test4 = c(16,23,21),
                      test5 = as.factor(c(49,36,52)))
  # for data2, test2 column is character data type
  data2 <- data.frame(person = c("D","E","F"),
                      test1 = c(8,7,2),
                      test2 = c(14,25,10),
                      test3 = c(6.5,12.0,19.5),
                      test4 = as.factor(c(15,21,29)),
                      test5 = as.factor(c(54,51,36)))
  # if this is dplyr::bind_rows, it fails because of factor vs character data type mismatch.
  # but exploratory::bind_rows works if force_data_type is set as TRUE.
  res3 <- exploratory::bind_rows(data1, data2, force_data_type = TRUE)
  expect_equal(unique(res3$person), c("A","B","C","D","E","F"))
  # test data frames without dedicated names
  mtcars1 <- mtcars
  mtcars2 <- mtcars
  mtcars3 <- mtcars
  res4 <- mtcars1 %>% exploratory::bind_rows(mtcars2, mtcars3, current_df_name = "mtcars1", id_column_name = "ID")
  expect_equal(unique(res4$ID), c("mtcars1","mtcars2","mtcars3"))
  # backward compatibility check
  res5 <- mtcars1 %>% exploratory::bind_rows(mtcars2, mtcars3, .id = "ID")
  expect_equal(unique(res5$ID), c("1","2","3"))
  # col_index_as_col_name
  res6 <- mtcars1 %>% exploratory::bind_rows(mtcars2, mtcars2, .id = "ID", use_col_index_as_col_name = TRUE)
  expect_equal(colnames(res6),c("ID", stringr::str_c("X",c(1:11))))
})

test_that("union", {
  library(dplyr)
  # For data1, test1 column is factor data type
  data1 <- data.frame(person = c("A","B","C"),
                      test1 = as.factor(c(1,4,5)),
                      test2 = c(14,25,10),
                      test3 = c(12.5,16.0,4),
                      test4 = c(16,23,21),
                      test5 = as.factor(c(49,36,52)))
  # for data2, test2 column is character data type
  data2 <- data.frame(person = c("A","D","F"),
                      test1 = c(1,7,2),
                      test2 = c(14,25,10),
                      test3 = c(12.5,12.0,19.5),
                      test4 = as.factor(c(16,21,29)),
                      test5 = as.factor(c(49,51,36)))
  res <- exploratory::union(x = data1, y = data2, force_data_type = TRUE)
  expect_equal(nrow(res), 5)
})

test_that("union_all", {
  library(dplyr)
  # For data1, test1 column is factor data type
  data1 <- data.frame(person = c("A","B","C"),
                      test1 = as.factor(c(1,4,5)),
                      test2 = c(14,25,10),
                      test3 = c(12.5,16.0,4),
                      test4 = c(16,23,21),
                      test5 = as.factor(c(49,36,52)))
  # for data2, test2 column is character data type
  data2 <- data.frame(person = c("A","D","F"),
                      test1 = c(1,7,2),
                      test2 = c(14,25,10),
                      test3 = c(12.5,12.0,19.5),
                      test4 = as.factor(c(16,21,29)),
                      test5 = as.factor(c(49,51,36)))
  res <- exploratory::union_all(x = data1, y = data2, force_data_type = TRUE)
  expect_equal(nrow(res), 6)
})

test_that("intersect", {
  library(dplyr)
  # For data1, test1 column is factor data type
  data1 <- data.frame(person = c("A","B","C"),
                      test1 = as.factor(c(1,4,5)),
                      test2 = c(14,25,10),
                      test3 = c(12.5,16.0,4),
                      test4 = c(16,23,21),
                      test5 = as.factor(c(49,36,52)))
  # for data2, test2 column is character data type
  data2 <- data.frame(person = c("A","D","F"),
                      test1 = c(1,7,2),
                      test2 = c(14,25,10),
                      test3 = c(12.5,12.0,19.5),
                      test4 = as.factor(c(16,21,29)),
                      test5 = as.factor(c(49,51,36)))
  res <- exploratory::intersect(x = data1, y = data2, force_data_type = TRUE)
  expect_equal(nrow(res), 1)
})

test_that("setdiff", {
  library(dplyr)
  # For data1, test1 column is factor data type
  data1 <- data.frame(person = c("A","B","C"),
                      test1 = as.factor(c(1,4,5)),
                      test2 = c(14,25,10),
                      test3 = c(12.5,16.0,4),
                      test4 = c(16,23,21),
                      test5 = as.factor(c(49,36,52)))
  # for data2, test2 column is character data type
  data2 <- data.frame(person = c("A","D","F"),
                      test1 = c(1,7,2),
                      test2 = c(14,25,10),
                      test3 = c(12.5,12.0,19.5),
                      test4 = as.factor(c(16,21,29)),
                      test5 = as.factor(c(49,51,36)))
  res <- exploratory::setdiff(x = data1, y = data2, force_data_type = TRUE)
  expect_equal(nrow(res), 2)
})

test_that("test pivot with empty data frame", {
  # Create an empty data frame
  df <- mtcars %>% filter(gear > 100)
  df <- pivot(df, row_cols=c("gear"), col_cols=c("cyl"))
  # it should return below 0 row df.
  # > df
  # A tibble: 0 × 1
  # … with 1 variable: gear <dbl>
  expect_equal(nrow(df), 0)
  expect_equal(ncol(df), 1)
})

test_that("exp_cut", {
  df <- exploratory::read_delim_file("https://www.dropbox.com/s/ok8m7cpa5cw2lw3/airline_2013_10_tricky.csv?dl=1" , ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE)
  res <- df %>% mutate(c1=exp_cut(ARR_DELAY))
  expect_equal(class(res$c1),"factor")
})

test_that("exp_cut upper/lower range support", {
  expect_equal(levels(exp_cut(4*1:25, breaks=5)), c("[4,23.2]"  , "(23.2,42.4]","(42.4,61.6]","(61.6,80.8]","(80.8,100]" ))
  expect_equal(levels(exp_cut(4*1:25, breaks=5, lower.range=20, upper.range=80)), c( "[-Inf,20]","(20,32]"  ,"(32,44]",  "(44,56]"  ,"(56,68]",  "(68,80]"  ,"(80, Inf]"))
  expect_equal(levels(exp_cut(4*1:25, breaks=5, lower.range=20, upper.range=80, include.outside.range=F)), c("[20,32]","(32,44]","(44,56]","(56,68]","(68,80]"))
  expect_equal(levels(exp_cut(4*1:25, breaks=5, lower.range=20)), c("[-Inf,20]","(20,36]"  ,"(36,52]"  ,"(52,68]" , "(68,84]",  "(84,100]" ))
  expect_equal(levels(exp_cut(4*1:25, breaks=5, lower.range=20, include.outside.range=F)), c("[20,36]" ,"(36,52]", "(52,68]", "(68,84]" ,"(84,100]"))
  expect_equal(levels(exp_cut(4*1:25, breaks=5, upper.range=80)), c("[4,19.2]"  , "(19.2,34.4]","(34.4,49.6]","(49.6,64.8]","(64.8,80]" , "(80,Inf]" ))
  expect_equal(levels(exp_cut(4*1:25, breaks=5, upper.range=80, include.outside.range=F)), c("[4,19.2]",   "(19.2,34.4]","(34.4,49.6]","(49.6,64.8]","(64.8,80]" ))


  expect_equal(levels(exp_cut(c(4*1:25, Inf, -Inf), breaks=5)), c("[-Inf,4]",   "(4,36]",     "(36,68]",    "(68,100]",   "(100, Inf]"))
  expect_equal(levels(exp_cut(c(4*1:25, Inf, -Inf), breaks=5, lower.range=20, upper.range=80)),  c("[-Inf,20]", "(20,40]" ,  "(40,60]" ,  "(60,80]",   "(80, Inf]"))
  expect_equal(levels(exp_cut(c(4*1:25, Inf, -Inf), breaks=5, lower.range=20, upper.range=80, include.outside.range=F)), c("[-Inf,20]", "(20,40]" ,  "(40,60]" ,  "(60,80]",   "(80, Inf]"))
  expect_equal(levels(exp_cut(c(4*1:25, Inf, -Inf), breaks=5, lower.range=20)), c("[-Inf,20]",   "(20,46.7]",   "(46.7,73.3]", "(73.3,100]",  "(100, Inf]"))
  expect_equal(levels(exp_cut(c(4*1:25, Inf, -Inf), breaks=5, lower.range=20, include.outside.range=F)), c("[-Inf,20]",   "(20,46.7]",   "(46.7,73.3]", "(73.3,100]",  "(100, Inf]"))
  expect_equal(levels(exp_cut(c(4*1:25, Inf, -Inf), breaks=5, upper.range=80)), c("[-Inf,4]",    "(4,29.3]",    "(29.3,54.7]", "(54.7,80]",   "(80, Inf]"))
  expect_equal(levels(exp_cut(c(4*1:25, Inf, -Inf), breaks=5, upper.range=80, include.outside.range=F)), c("[-Inf,4]",    "(4,29.3]",    "(29.3,54.7]", "(54.7,80]",   "(80, Inf]"))
})

test_that("_tam_cut_by_step", {
  # Without range.

  # Actual value range: 1-21
  # Step = 5
  expect_equal(levels(exp_cut_by_step(1:21, 5)), c("(-Inf,1]" ,"(1,6]"  ,  "(6,11]",   "(11,16]" , "(16,21]"  ))
  expect_equal(levels(exp_cut_by_step(1:21, 5, include.outside.range=F)), c("(1,6]"  , "(6,11]",  "(11,16]" ,"(16,21]"))
  expect_equal(levels(exp_cut_by_step(1:21, 5, right=F)), c("[1,6)" ,   "[6,11)" ,  "[11,16)" , "[16,21)" , "[21,Inf)"))
  expect_equal(levels(exp_cut_by_step(1:21, 5, include.outside.range=F, right=F)), c("[1,6)" ,  "[6,11)" , "[11,16)", "[16,21)"))

  # Actual value range: 1-20
  # Step = 5
  # The last bucket should include value 20.
  expect_equal(levels(exp_cut_by_step(1:20, 5)), c("(-Inf,1]", "(1,6]" ,   "(6,11]"  , "(11,16]" , "(16,21]" ))
  expect_equal(levels(exp_cut_by_step(1:20, 5, include.outside.range=F)), c("(1,6]"  , "(6,11]",  "(11,16]" ,"(16,21]"))
  expect_equal(levels(exp_cut_by_step(1:20, 5, right=F)), c("[1,6)"  ,  "[6,11)"  , "[11,16)" , "[16,21)",  "[21,Inf)"))
  expect_equal(levels(exp_cut_by_step(1:20, 5, include.outside.range=F, right=F)), c("[1,6)" ,  "[6,11)",  "[11,16)", "[16,21)"))

  # With step less than 1.
  # Actual value range: 1-10
  # Step = 0.8
  expect_equal(levels(exp_cut_by_step(1:10, 0.8)), c("(-Inf,1]", "(1,1.8]",    "(1.8,2.6]",  "(2.6,3.4]",  "(3.4,4.2]",  "(4.2,5]",    "(5,5.8]",    "(5.8,6.6]",  "(6.6,7.4]",  "(7.4,8.2]",  "(8.2,9]",    "(9,9.8]","(9.8,10.6]" ))
  expect_equal(levels(exp_cut_by_step(1:10, 0.8, include.outside.range=F)), c("(1,1.8]",   "(1.8,2.6]", "(2.6,3.4]", "(3.4,4.2]", "(4.2,5]",   "(5,5.8]",   "(5.8,6.6]", "(6.6,7.4]", "(7.4,8.2]", "(8.2,9]",   "(9,9.8]","(9.8,10.6]"  ))
  expect_equal(levels(exp_cut_by_step(1:10, 0.8, right=F)), c( "[1,1.8)",    "[1.8,2.6)" , "[2.6,3.4)" , "[3.4,4.2)" , "[4.2,5)"  ,  "[5,5.8)" ,   "[5.8,6.6)"  ,"[6.6,7.4)" , "[7.4,8.2)" , "[8.2,9)" ,   "[9,9.8)"   , "[9.8,10.6)", "[10.6,Inf)" ))
  expect_equal(levels(exp_cut_by_step(1:10, 0.8, include.outside.range=F, right=F)), c("[1,1.8)" ,   "[1.8,2.6)"  ,"[2.6,3.4)",  "[3.4,4.2)" , "[4.2,5)" ,   "[5,5.8)"  ,  "[5.8,6.6)" , "[6.6,7.4)" , "[7.4,8.2)" , "[8.2,9)"  ,  "[9,9.8)"  ,  "[9.8,10.6)"   ))

  # With range.

  # Actual value range: 1-20
  # Range: 5-15
  # Step = 5
  expect_equal(levels(exp_cut_by_step(1:20, 5, 5, 15)), c("(-Inf,5]" , "(5,10]"  ,  "(10,15]" ,  "(15, Inf]"))
  expect_equal(levels(exp_cut_by_step(1:20, 5, 5, 15, include.outside.range=F)), c(  "(5,10]" , "(10,15]"))
  expect_equal(levels(exp_cut_by_step(1:20, 5, 5, 15, right=F)), c("[-Inf,5)" , "[5,10)"  ,  "[10,15)"  , "[15, Inf)"))
  expect_equal(levels(exp_cut_by_step(1:20, 5, 5, 15, include.outside.range=F, right=F)), c("[5,10)" , "[10,15)"))

  # Actual value range: 1-20
  # Range: 0-25
  # Step = 5
  expect_equal(levels(exp_cut_by_step(1:20, 5, 0, 25)), c( "(0,5]" ,  "(5,10]" , "(10,15]", "(15,20]", "(20,25]"))
  expect_equal(levels(exp_cut_by_step(1:20, 5, 0, 25, include.outside.range=F)), c(  "(0,5]"  , "(5,10]",  "(10,15]", "(15,20]" ,"(20,25]"))
  expect_equal(levels(exp_cut_by_step(1:20, 5, 0, 25, right=F)), c("[0,5)" ,  "[5,10)" , "[10,15)", "[15,20)", "[20,25)"))
  expect_equal(levels(exp_cut_by_step(1:20, 5, 0, 25, include.outside.range=F, right=F)), c("[0,5)" ,  "[5,10)",  "[10,15)" ,"[15,20)" ,"[20,25)"))

  # Actual value range: 1-21
  # Range: 1-21
  # Step = 5
  expect_equal(levels(exp_cut_by_step(1:21, 5, 1, 21)), c("(-Inf,1]", "(1,6]" ,   "(6,11]",   "(11,16]" , "(16,21]" ))
  expect_equal(levels(exp_cut_by_step(1:21, 5, 1, 21, include.outside.range=F)), c(  "(1,6]"  , "(6,11]",  "(11,16]", "(16,21]"))
  expect_equal(levels(exp_cut_by_step(1:21, 5, 1, 21, right=F)), c("[1,6)"   , "[6,11)" ,  "[11,16)",  "[16,21)" , "[21,Inf)"))
  expect_equal(levels(exp_cut_by_step(1:21, 5, 1, 21, include.outside.range=F, right=F)), c("[1,6)"  , "[6,11)",  "[11,16)", "[16,21)"))

  # With step less than 1.
  # Actual value range: -1 - 10
  # Range: 0-8
  # Step = 0.8
  expect_equal(levels(exp_cut_by_step(-1:10, 0.8, 0, 8)), c( "(-Inf,0]" , "(0,0.8]"  , "(0.8,1.6]", "(1.6,2.4]", "(2.4,3.2]" ,"(3.2,4]" ,  "(4,4.8]"  , "(4.8,5.6]", "(5.6,6.4]", "(6.4,7.2]", "(7.2,8]"  , "(8, Inf]"  ))
  expect_equal(levels(exp_cut_by_step(-1:10, 0.8, 0, 8, include.outside.range=F)), c(  "(0,0.8]"  , "(0.8,1.6]", "(1.6,2.4]", "(2.4,3.2]" ,"(3.2,4]" ,  "(4,4.8]"  , "(4.8,5.6]", "(5.6,6.4]" ,"(6.4,7.2]", "(7.2,8]"     ))
  expect_equal(levels(exp_cut_by_step(-1:10, 0.8, 0, 8, right=F)), c("[-Inf,0)"  ,"[0,0.8)" ,  "[0.8,1.6)", "[1.6,2.4)", "[2.4,3.2)", "[3.2,4)"  , "[4,4.8)"  , "[4.8,5.6)" ,"[5.6,6.4)", "[6.4,7.2)", "[7.2,8)" ,  "[8, Inf)" ))
  expect_equal(levels(exp_cut_by_step(-1:10, 0.8, 0, 8, include.outside.range=F, right=F)), c("[0,0.8)",   "[0.8,1.6)" ,"[1.6,2.4)", "[2.4,3.2)" ,"[3.2,4)"  , "[4,4.8)" ,  "[4.8,5.6)" ,"[5.6,6.4)", "[6.4,7.2)", "[7.2,8)"  ))

  # Include infinite values.
  expect_equal(levels(exp_cut_by_step(c(1:10,-Inf,Inf,NA, NaN), 4)), c("(-Inf,1]", "(1,5]",    "(5,9]",  "(9,13]"  ,   "(13, Inf]"))
  # Should be the same result as above.
  expect_equal(levels(exp_cut_by_step(c(1:10,-Inf,Inf,NA, NaN), 4, include.outside.range=F)), c("(-Inf,1]", "(1,5]",    "(5,9]", "(9,13]"  ,   "(13, Inf]"))

  # Include infinite values with range.
  expect_equal(levels(exp_cut_by_step(c(1:10,-Inf,Inf,NA, NaN), 4, 0, 8)), c("(-Inf,0]", "(0,4]",    "(4,8]",    "(8, Inf]"))
  # Should be the same result as above.
  expect_equal(levels(exp_cut_by_step(c(1:10,-Inf,Inf,NA, NaN), 4, 0, 8, include.outside.range=F)), c("(-Inf,0]", "(0,4]",    "(4,8]",    "(8, Inf]"))

  # Edge case test with no step param.
  expect_equal(levels(exp_cut_by_step(c())), NULL)
  expect_equal(levels(exp_cut_by_step(c(), include.outside.range=F)), NULL)
  expect_equal(levels(exp_cut_by_step(c(NA))), NULL)
  expect_equal(levels(exp_cut_by_step(c(NA), include.outside.range=F)), NULL)
  expect_equal(levels(exp_cut_by_step(c(NaN))), c("[-Inf, Inf]"))
  expect_equal(levels(exp_cut_by_step(c(NaN), include.outside.range=F)), c("[-Inf, Inf]"))
  expect_equal(levels(exp_cut_by_step(c(Inf))), c("[-Inf, Inf]"))
  expect_equal(levels(exp_cut_by_step(c(Inf), include.outside.range=F)), c("[-Inf, Inf]"))
  expect_equal(levels(exp_cut_by_step(c(-Inf))), c("[-Inf, Inf]"))
  expect_equal(levels(exp_cut_by_step(c(-Inf), include.outside.range=F)), c("[-Inf, Inf]"))
  expect_equal(levels(exp_cut_by_step(c(0,0,0,0))), c("(-0.001,0]" ,"(0,0.001]"))
  expect_equal(levels(exp_cut_by_step(c(0,0,0,0), include.outside.range=F)), c("(-0.001,0]" ,"(0,0.001]"))
  expect_equal(levels(exp_cut_by_step(c(4,4,4))), c("(3.996,4]", "(4,4.004]"))
  expect_equal(levels(exp_cut_by_step(c(4,4,4), include.outside.range=F)), c("(3.996,4]", "(4,4.004]"))
  expect_equal(levels(exp_cut_by_step(c(4,4,4,4,4))), c("(3.996,4]", "(4,4.004]"))
  expect_equal(levels(exp_cut_by_step(c(4,4,4,4,4), include.outside.range=F)), c("(3.996,4]", "(4,4.004]"))
  expect_equal(levels(exp_cut_by_step(c(-Inf, Inf, NA, NaN))), c("[-Inf, Inf]"))
  expect_equal(levels(exp_cut_by_step(c(-Inf, Inf, NA, NaN), include.outside.range=F)), c("[-Inf, Inf]"))

  # Edge case test with step param.
  expect_equal(levels(exp_cut_by_step(c(), step=5)), NULL)
  expect_equal(levels(exp_cut_by_step(c(), step=5, include.outside.range=F)), NULL)
  expect_equal(levels(exp_cut_by_step(c(NA), step=5)), NULL)
  expect_equal(levels(exp_cut_by_step(c(NA), step=5, include.outside.range=F)),NULL)
  expect_equal(levels(exp_cut_by_step(c(NaN), step=5)), c("[-Inf, Inf]"))
  expect_equal(levels(exp_cut_by_step(c(NaN), step=5, include.outside.range=F)), c("[-Inf, Inf]"))
  expect_equal(levels(exp_cut_by_step(c(Inf), step=5)), c("[-Inf, Inf]"))
  expect_equal(levels(exp_cut_by_step(c(Inf), step=5, include.outside.range=F)), c("[-Inf, Inf]"))
  expect_equal(levels(exp_cut_by_step(c(-Inf), step=5)), c("[-Inf, Inf]"))
  expect_equal(levels(exp_cut_by_step(c(-Inf), step=5, include.outside.range=F)), c("[-Inf, Inf]"))
  expect_equal(levels(exp_cut_by_step(c(0,0,0,0), step=5)), c("(-0.001,0]" ,"(0,0.001]"))
  expect_equal(levels(exp_cut_by_step(c(0,0,0,0), step=5, include.outside.range=F)), c("(-0.001,0]" ,"(0,0.001]"))
  expect_equal(levels(exp_cut_by_step(c(4,4,4), step=5)), c("(3.996,4]", "(4,4.004]"))
  expect_equal(levels(exp_cut_by_step(c(4,4,4), step=5, include.outside.range=F)), c("(3.996,4]", "(4,4.004]"))
  expect_equal(levels(exp_cut_by_step(c(4,4,4,4,4), step=5)), c("(3.996,4]", "(4,4.004]"))
  expect_equal(levels(exp_cut_by_step(c(4,4,4,4,4), step=5, include.outside.range=F)), c("(3.996,4]", "(4,4.004]"))
  expect_equal(levels(exp_cut_by_step(c(-Inf, Inf, NA, NaN), step=5)), c("[-Inf, Inf]"))
  expect_equal(levels(exp_cut_by_step(c(-Inf, Inf, NA, NaN), step=5, include.outside.range=F)), c("[-Inf, Inf]"))
})



test_that("test upper_gather", {
  mat <- matrix(seq(20),nrow=5, ncol=4)
  mat[[15]] <- NA # inject NA
  # use col03 to break sorted state
  colnames(mat) <- c("col 1","col 2","col 03", "col 4")
  result <- upper_gather(mat)
  expect_equal( typeof(result[[2]]), "character")
  expect_equal(result[[1]], sort(result[[1]]))
  expect_equal(result[result[[1]]==3 & result[[2]]=="col 4", 3][[1]], 18)
  expect_equal(result[result[[1]]==2 & result[[2]]=="col 03", 3][[1]], 12)
  expect_equal(nrow(result), 6)
})

test_that("upper_gather with 2 by 2 matrix", {
  ret <- upper_gather(
    structure(
      c(1, 0.952601667396786, 0.952601667396786, 1),
      .Dim = c(2L,2L),
      .Dimnames = list(
        c("ARR_DELAY", "DEP_DELAY"),
        c("ARR_DELAY", "DEP_DELAY"))))
  expect_equal(ret,
               structure(
                 list(
                   Var1 = "ARR_DELAY", Var2 = "DEP_DELAY", value = 0.952601667396786),
                 .Names = c("Var1", "Var2", "value"),
                 row.names = c(NA, -1L),
                 class = "data.frame")
               )
})

test_that("test upper_gather with vector", {
  mat <- seq(6)
  names <- paste("entity", seq(4))
  result <- upper_gather(mat,names)
  expect_equal(result$Var1, sort(result$Var1))
  expect_equal(as.numeric(result[result[[1]]=="entity 1" & result[[2]]=="entity 3",3]), 2)
  expect_equal(as.numeric(result[result[[1]]=="entity 1" & result[[2]]=="entity 4",3]), 3)
  expect_equal(as.numeric(result[result[[1]]=="entity 2" & result[[2]]=="entity 3",3]), 4)
  expect_equal(as.numeric(result[result[[1]]=="entity 2" & result[[2]]=="entity 4",3]), 5)
  expect_equal(as.numeric(result[result[[1]]=="entity 3" & result[[2]]=="entity 4",3]), 6)
  expect_equal(nrow(result), 6)
})

test_that("test upper_gather with vector diag true", {
  mat <- seq(6)
  names <- paste("entity", seq(4))
  result <- upper_gather(mat,names, diag=1)
  expect_equal(nrow(result), 10)
})

test_that("sparse_cast", {
  test_df <- data.frame(
    row = rep(paste("row", 6-seq(5)), each=4),
    col = rep(paste("col", seq(4)), 5),
    val = rep(c(NA,1,0,0), 5)
  )
  colnames(test_df) <- c("ro w", "co l", "va l")
  mat <- sparse_cast(test_df, "ro w", "co l", "va l")

  expect_equal(dim(mat), c(5, 4))
  expect_equal(dimnames(mat), list(paste("row", seq(5)), paste("col", seq(4))))

  mat <- sparse_cast(test_df, "ro w", "co l")
  expect_equal(dim(mat), c(5, 4))
})

test_that("test avoid_conflict", {
  origin <- c("name1", "name1.new", "name2")
  new <- c("name1", "name2")
  ret <- avoid_conflict(origin, new)
  expect_equal(ret, c("name1.new.new", "name2.new"))
})

test_that("test grouped_by", {
  loadNamespace("dplyr")
  test_df <- data.frame(
    col1=c(rep(paste("col1", seq(2)), 4), NA, NA), # inject NA for test
    col2=rep(paste("col2", seq(2)), each=5),
    col3=paste("col3", seq(10))
  )
  colnames(test_df) <- c("col 1", "col 2", "col 3")
  test_df
  df <- dplyr::group_by(test_df, `col 1`, `col 2`)
  ret <- grouped_by(df)
  expect_equal(ret, c("col 1", "col 2"))
})

test_that("test simple_cast colnames are sorted", {
  test_df <- data.frame(
    rowname = rep(c("row1", "row02", "row3", "row004"), each=3),
    colname = rep(c("col1", "col2", "col03"), 4),
    val = seq(12),
    stringsAsFactors = FALSE
  )
  mat <- simple_cast(test_df, "rowname", "colname", "val")
  expect_equal(test_df[test_df$rowname=="row3" & test_df$colname=="col03",3][[1]], mat["row3", "col03"])
})

test_that("test simple_cast colnames, rownames with na", {
  test_df <- data.frame(
    rowname = rep(c("row1", "row02", NA, "row004"), each=3),
    colname = rep(c("col1", NA, "col03"), 4),
    val = seq(12),
    stringsAsFactors = FALSE
  )
  mat <- simple_cast(test_df, "rowname", "colname", "val")
  expect_equal(dim(mat), c(3, 2))
  expect_equal(test_df[test_df$rowname=="row02" & test_df$colname=="col1",3][[1]], mat["row02", "col1"])
})

test_that("test simple_cast larger than max int (2^31)", {
  test_df <- data.frame(
    rval = seq(2^16),
    cval = seq(2^16),
    val = rep(0, 2^16),
    stringsAsFactors = FALSE
  )
  expect_error(simple_cast(test_df, "rval", "cval", "val"), "Data is too large to make a matrix for calculation.")
})

test_that("test sparse_cast", {
  test_df <- data.frame(
    rowname = rep(c("row1", "row02", "row3"), each=3),
    colname = c("col1", "col02", "col5", "col02", "col3", "col1", "col02", "col4", "col5"),
    val = seq(9),
    stringsAsFactors = FALSE
  )
  mat <- sparse_cast(test_df, "rowname", "colname", "val")

  for(rindex in seq(9)){
    row <- test_df[rindex, "rowname"]
    col <- test_df[rindex, "colname"]
    val <- test_df[rindex, "val"]
    expect_equal(mat[row, col], val)
  }
})

test_that("test sparse_cast with na label", {
  test_df <- data.frame(
    rowname = rep(c("row1", "row02", NA), each=3),
    colname = c("col1", "col02", NA, "col02", "col3", "col1", "col02", "col4", "col5"),
    val = seq(9),
    stringsAsFactors = FALSE
  )
  mat <- sparse_cast(test_df, "rowname", "colname", "val")
  expect_equal(dim(mat), c(2, 3))
})

test_that("test simple_cast with undefined column names", {
  test_df <- data.frame(
    rowname = rep(c("row1", "row02", NA), each=3),
    colname = c("col1", "col02", NA, "col02", "col3", "col1", "col02", "col4", "col5"),
    val = seq(9),
    stringsAsFactors = FALSE
  )
  expect_error({
    simple_cast(test_df, "row", "colname", "val")
  }, "row is not in column names")
  expect_error({
    simple_cast(test_df, "rowname", "col", "val")
  }, "col is not in column names")
  expect_error({
    simple_cast(test_df, "rowname", "colname", "valname")
  }, "valname is not in column names")
})

test_that("test sparse_cast with undefined column names", {
  test_df <- data.frame(
    rowname = rep(c("row1", "row02", NA), each=3),
    colname = c("col1", "col02", NA, "col02", "col3", "col1", "col02", "col4", "col5"),
    val = seq(9),
    stringsAsFactors = FALSE
  )
  expect_error({
    sparse_cast(test_df, "row", "colname", "val")
  }, "row is not in column names")
  expect_error({
    sparse_cast(test_df, "rowname", "col", "val")
  }, "col is not in column names")
  expect_error({
    sparse_cast(test_df, "rowname", "colname", "valname")
  }, "valname is not in column names")
})

test_that("test sparse_cast without val", {
  test_df <- data.frame(
    rowname = rep(c("row1", "row02", "row3"), each=3),
    colname = c("col1", "col02", "col5", "col02", "col3", "col1", "col02", "col4", "col5"),
    stringsAsFactors = FALSE
  )
  mat <- sparse_cast(test_df, "rowname", "colname")

  for(rindex in seq(9)){
    row <- test_df[rindex, "rowname"]
    col <- test_df[rindex, "colname"]
    expect_equal(mat[row, col], TRUE)
  }
})

test_that("test sparse_cast with fun.aggregate", {
  test_df <- data.frame(
    rowname = rep(c("row1", "row02", "row3"), each=3),
    colname = c("col1", "col1", "col5", "col02", "col3", "col1", "col02", "col4", "col5"),
    val = seq(9),
    stringsAsFactors = FALSE
  )
  mat <- sparse_cast(test_df, "rowname", "colname", "val", fun.aggregate=mean)

  expect_equal(mat["row1", "col1"], 1.5)
})

test_that("test mat_to_df", {
  nc <- 4
  nr <- 5
  mat <- matrix(seq(nc*nr), ncol=nc, nrow=nr)
  colnames(mat) <- paste("cname", seq(nc))
  rownames(mat) <- paste("rname", seq(nr))
  ret <- mat_to_df(mat, c("aa", "bb", "value"))
  expect_true(is.character(ret$aa))
  expect_true(is.character(ret$bb))
  expect_true(!is.unsorted(ret$aa))
})

test_that("test mat_to_df output when no colnames are set.", {
  nc <- 4
  nr <- 5
  mat <- matrix(seq(nc*nr), ncol=nc, nrow=nr)
  ret <- mat_to_df(mat, c("aa", "bb", "value"))
  # Values should be pure numeric string, as opposed to "V1", "V2", which as.data.frame() adds by default.
  expect_true(all(stringr::str_detect(ret$bb,"^[0-9]+$")))
  # The same check for the first column of the output just in case.
  expect_true(all(stringr::str_detect(ret$aa,"^[0-9]+$")))
  expect_true(is.character(ret$bb))
  expect_true(!is.unsorted(ret$aa))
})

test_that("test mat_to_df with column names with numeric characters only", {
  nc <- 4
  nr <- 5
  mat <- matrix(seq(nc*nr), ncol=nc, nrow=nr)

  # Set numeric-character-only column names for test.
  colnames(mat) <- as.character(seq(nc))
  rownames(mat) <- as.character(seq(nr))

  ret <- mat_to_df(mat, c("aa", "bb", "value"))
  expect_true(is.character(ret$aa))
  expect_true(is.character(ret$bb))
  expect_true(!is.unsorted(ret$aa))
})

test_that("test floor", {
  ret <- exploratory::floor(c(3, 3.0001, 2.9999, NA))
  expect_equal(ret, c(3, 3, 2, NA))
  ret <- exploratory::floor(c(3.1, 3.10001, 3.09999, NA), digits=1)
  expect_equal(ret, c(3.1, 3.1, 3, NA))
  ret <- exploratory::floor(c(30, 31, 29.9999, NA), digits=-1)
  expect_equal(ret, c(30, 30, 20, NA))
})

test_that("test ceiling", {
  ret <- exploratory::ceiling(c(3, 3.0001, 2.9999, NA))
  expect_equal(ret, c(3, 4, 3, NA))
  ret <- exploratory::ceiling(c(3.1, 3.10001, 3.09999, NA), digits=1)
  expect_equal(ret, c(3.1, 3.2, 3.1, NA))
  ret <- exploratory::ceiling(c(30, 31, 29.9999, NA), digits=-1)
  expect_equal(ret, c(30, 40, 30, NA))
})

test_that("test %nin%", {
  ret <- c(1,3,NA,2) %nin% c(3, NA)
  expect_equal(ret, c(T,F,F,T))
})

test_that("list_n", {
  test_list <- list(seq(1), seq(2), seq(3))
  ret <- list_n(test_list)
  expect_equal(ret, c(1, 2, 3))
})

test_that("list_extract", {
  test_list <- list(seq(1), seq(2), seq(3))
  def_ret <- list_extract(test_list)
  expect_equal(def_ret, c(1, 1, 1))

  # index over
  over_ret <- list_extract(test_list, 3)
  expect_equal(over_ret, c(NA, NA, 3))

  # index minus
  minus_ret <- list_extract(test_list, -2)
  expect_equal(minus_ret, c(NA, 1, 2))

  # index minus over
  minus_ret <- list_extract(test_list, -5)
  expect_equal(minus_ret, c(NA, NA, NA))

  test_df_list <- list(data.frame(1), data.frame(1, second=2), data.frame(1, 2, 3))
  def_ret <- list_extract(test_df_list)
  expect_equal(def_ret, c(1, 1, 1))

  # index over
  over_ret <- list_extract(test_df_list, 3)
  expect_equal(over_ret, c(NA, NA, 3))

  # index minus
  minus_ret <- list_extract(test_df_list, -2)
  expect_equal(minus_ret, c(NA, 1, 2))

  # index minus over
  minus_ret <- list_extract(test_df_list, -5)
  expect_equal(minus_ret, c(NA, NA, NA))

  # index text
  minus_ret <- list_extract(test_df_list, "second")
  expect_equal(minus_ret, c(NA, 2, NA))
})

test_that("as_numeric_matrix", {
  test_df <- data.frame(
    date1 = lubridate::ymd("1990:10:11") + seq(10),
    date2 = lubridate::ymd("1991:08:11") - seq(10)
  )
  expect_warning({
    ret <- exploratory:::as_numeric_matrix_(test_df, columns = c("date1", "date2"))
    expect_true(all(is.na(ret)))
  })
})

test_that("as_numeric_matrix", {
  test_df <- data.frame(
    char1 = as.character(seq(10)),
    char2 = as.character(0 - seq(10))
  )
  ret <- exploratory:::as_numeric_matrix_(test_df, columns = c("char1", "char2"))
  expect_true(all(!is.na(ret)))
})

test_that("as_numeric_matrix to group", {
  test_df <- data.frame(
    date1 = as.character(seq(20)),
    date2 = as.character(0 - seq(20)),
    group = paste(rep(c(1, 2), each = 10))
  )
  ret <- test_df %>%
    dplyr::group_by(group) %>%
    exploratory:::as_numeric_matrix_(columns = c("date1", "date2"))
  expect_equal(dim(ret), c(20, 2))
})

test_that("evaluate_select", {
  test_df <- data.frame(
    col1 = as.character(seq(10)),
    col2 = as.character(0 - seq(10))
  )
  ret <- evaluate_select(test_df, c("dplyr::starts_with('col')"))
  expect_equal(ret, c("col1", "col2"))
})

test_that("evaluate_select negative test", { # TODO: we should move out from old lazy eval scheme.
  test_df <- data.frame(
    col1 = as.character(seq(10)),
    col2 = as.character(0 - seq(10))
  )
  expect_error({
    evaluate_select(test_df, c("co1"))
  }) # Error message here is not consistent between linux ("") and others ("undefined columns selected"). Just verifying it results in error.
  expect_error({
    evaluate_select(test_df, c("dplyr::starts_with('something')"))
  }) # Error message here is not consistent between linux ("") and others ("no column selected"). Just verifying it results in error.
})

test_that("list_to_text should return NA", {
  test_df <- data.frame(
    col1 = as.character(seq(10))
  )

  test_list <- replicate(10, list(replicate(5, letters[2])))
  test_list[[1]] <- NA
  test_list[[2]] <- character(0)
  test_list[[3]] <- c(NA, "b")

  test_df[["test_list"]] <- test_list
  ret <- dplyr::mutate(test_df, text = list_to_text(test_list) )

  expect_equal(ret[["text"]], c("NA", "", "NA, b", rep("b, b, b, b, b", 7)))
})

test_that("list_concat", {
  list1 <- list(
    NA,
    NA,
    character(0),
    c(3, 5)
  )

  ret <- list_concat(list1, collapse = TRUE)
  expect_equal(length(ret), 1)
  expect_equal(ret[[1]], c(NA, NA, "3", "5"))
})

test_that("list_concat with multiple list", {
  list1 <- list(
    NA,
    NA,
    character(0),
    c(3, 5)
  )

  list2 <- list(
    NA,
    c("a", "c"),
    character(0),
    c(6)
  )

  list3 <- list(
    NA,
    c(1, 3),
    c("a", "c"),
    c(6)
  )

  ret1 <- list_concat(list1, list2, list3, collapse = FALSE)
  expect_equal(ret1[[1]], c(NA, NA, NA))
  expect_equal(ret1[[2]], c(NA, "a", "c", "1", "3"))
  expect_equal(ret1[[3]], c("a", "c"))
  expect_equal(ret1[[4]], c(3, 5, 6, 6))

  ret1_collapse <- list_concat(list1, list2, list3, collapse = TRUE)

  expect_equal(length(ret1_collapse), 1)
  expect_equal(ret1_collapse[[1]], c(NA, NA, NA, NA, "a", "c", "1", "3", "a", "c", "3", "5", "6", "6"))
})

test_that("test expand_args", {
  func <- function(..., def = "defalut"){
    caller <- match.call()
    expand_args(caller, exclude = "def")
  }

  ret <- func(aaa = "aa\"a",
              cc_list = list("c", "c"),
              fml = ~as.formula("~c()"),
              chars = c("chars", "chars2"),
              "no args",
              def = "not default")

  expect_equal(ret, "aaa = \"aa\"a\", cc_list = list(\"c\", \"c\"), fml = ~as.formula(\"~c()\"), chars = c(\"chars\", \"chars2\"), \"no args\"")
})

test_that("move_col", {
  test_data <- data.frame(
    a = seq(3),
    b = seq(3),
    c = seq(3),
    d = seq(3),
    e = seq(3),
    f = seq(3),
    g = seq(3)
  )

  left_to_right <- move_col(test_data, "c", 6)
  expect_equal(colnames(left_to_right), c("a", "b", "d", "e", "f", "c", "g"))

  right_to_left <- move_col(test_data, "f", 2)
  expect_equal(colnames(right_to_left), c("a", "f", "b", "c", "d", "e", "g"))
})

test_that("unixtime_to_datetime", {
  data <- c(300, 900, NA)

  unix_ret <- unixtime_to_datetime(data)
  unix_ans <- as.POSIXct(data, origin="1970-01-01", tz = "GMT")
  expect_equal(unix_ret, unix_ans)

  data <- c("300", "900", NA)

  unix_ret <- unixtime_to_datetime(data)
  unix_ans <- as.POSIXct(as.numeric(data), origin="1970-01-01", tz = "GMT")
  expect_equal(unix_ret, unix_ans)


})

test_that("append_colnames", {
  test_df <- data.frame(col1 = seq(3), col2 = seq(3))

  ret <- exploratory:::append_colnames(test_df, "a.", ".b")

  expect_equal(colnames(ret), c("a.col1.b", "a.col2.b"))
})

test_that("test pivot", {
  set.seed(1)
  test_df <- data.frame(
    group = c(rep(letters[1:2], each = 50),"a"),
    cat1 = c(letters[round(runif(100)*5)+1], NA),
    cat2 = c(letters[round(runif(100)*3)+1], "a"),
    cat3 = c(letters[round(runif(100)*3)+1], "a"),
    value = c(letters[round(runif(100)*3)+1], "a"),
    dateCol = c(rep(lubridate::ymd("2022-01-01"),101)),
    posixctCol = c(rep(lubridate::ymd_hms("2022-01-01 00:00:00"),101)),
    intCol = as.integer(1:101),
    num3 = c(NA, seq(100))
  )
  test_df <- test_df %>% dplyr::mutate(dateCol = dplyr::case_when(cat2 == "a" ~ as.Date(NA), TRUE ~ dateCol))
  test_df <- test_df %>% dplyr::mutate(posixctCol = dplyr::case_when(cat2 == "b" ~ as.POSIXct(NA), TRUE ~ posixctCol))
  test_df <- test_df %>% dplyr::mutate(intCol = dplyr::case_when(cat2 == "b" ~ NA_integer_, TRUE ~ intCol))

  pivoted <- pivot(test_df, row_cols=c("cat1","cat3"), col_cols=c("cat2"))
  #pivoted <- pivot(test_df, cat1+cat3 ~ cat2)
  expect_true("cat1" %in% colnames(pivoted))
  expect_true("cat3" %in% colnames(pivoted))

  pivoted_with_val <- pivot(test_df, row_cols=c("cat1"), col_cols=c("cat2","cat3"), value = num3, fun.aggregate=mean, fill = 0)
  expect_true(all(!is.na(pivoted_with_val)))

  # add test case for fill argument is not 0 or 1.
  pivoted_with_val2 <- pivot(test_df, row_cols=c("cat1"), col_cols=c("cat2","cat3"), value = num3, fun.aggregate=mean, fill = 10)
  expect_equal(sum(pivoted_with_val2$a_a), 152)


  pivoted_with_na <- pivot(test_df, row_cols=c("cat1"), col_cols=c("cat2", "cat3"), value = num3, fun.aggregate=mean, na.rm = FALSE)
  expect_true(any(is.na(pivoted_with_na)))

  pivoted_with_cols_sep <- pivot(test_df, row_cols=c("cat1"), col_cols=c("cat2", "cat3"), value = num3, fun.aggregate=mean, na.rm = FALSE, cols_sep='-')
  expect_true("b-a" %in% colnames(pivoted_with_cols_sep)) # make sure column names are separated by '-'.

  pivoted_with_na_ratio <- pivot(test_df, row_cols=c("cat1"), col_cols=c("cat2") , value = num3, fun.aggregate=na_ratio, na.rm = TRUE)
  expect_true(any(pivoted_with_na_ratio %>% select(a,b,c,d) !=0)) # Verify that NA is detected.

  pivoted_with_na <- pivot(test_df, row_cols=c("cat1"), col_cols=c("value"),fun.aggregate=mean, na.rm = FALSE) # test for the case where original df has value column
  expect_true(ncol(pivoted_with_na)>1) # make sure resulting column is not the only one row.

  # value column as categorical column: mode
  pivoted_with_categorical <- pivot(test_df, row_cols = c("cat1"), col_cols = c("cat2"), value = cat3, fun.aggregate = get_mode, na.rm = TRUE)
  expect_true(nrow(pivoted_with_categorical)>1)

  # value column as categorical column: n_distinct
  pivoted_with_categorical <- pivot(test_df, row_cols = c("cat1"), col_cols = c("cat2"), value = cat3, fun.aggregate = n_distinct, na.rm = TRUE)
  expect_true(nrow(pivoted_with_categorical)>1)

  # value column as categorical column: min
  pivoted_with_categorical <- pivot(test_df, row_cols = c("cat1"), col_cols = c("cat2"), value = cat3, fun.aggregate = min, na.rm = TRUE)
  expect_true(nrow(pivoted_with_categorical)>1)

  # value column as categorical column: max
  pivoted_with_categorical <- pivot(test_df, row_cols = c("cat1"), col_cols = c("cat2"), value = cat3, fun.aggregate = max, na.rm = TRUE)
  expect_true(nrow(pivoted_with_categorical)>1)

  # value column as categorical column: na_count
  pivoted_with_categorical <- pivot(test_df, row_cols = c("cat1"), col_cols = c("cat2"), value = cat3, fun.aggregate = na_count, na.rm = TRUE)
  expect_true(nrow(pivoted_with_categorical)>1)

  # value column as categorical column: na_ratio
  pivoted_with_categorical <- pivot(test_df, row_cols = c("cat1"), col_cols = c("cat2"), value = cat3, fun.aggregate = na_ratio, na.rm = TRUE)
  expect_true(nrow(pivoted_with_categorical)>1)

  # value column as categorical column: non_na_count
  pivoted_with_categorical <- pivot(test_df, row_cols = c("cat1"), col_cols = c("cat2"), value = cat3, fun.aggregate = non_na_count, na.rm = TRUE)
  expect_true(nrow(pivoted_with_categorical)>1)

  # value column as categorical column: non_na_ratio
  pivoted_with_categorical <- pivot(test_df, row_cols = c("cat1"), col_cols = c("cat2"), value = cat3, fun.aggregate = non_na_ratio, na.rm = TRUE)
  expect_true(nrow(pivoted_with_categorical)>1)

  # value column as Date column: mode
  pivoted_with_date <- pivot(test_df, row_cols = c("cat1"), col_cols = c("cat2"), value = dateCol, fun.aggregate = get_mode, na.rm = TRUE)
  expect_true(nrow(pivoted_with_date)>1)

  # value column as Date column:median
  pivoted_with_date <- pivot(test_df, row_cols = c("cat1"), col_cols = c("cat2"), value = dateCol, fun.aggregate = median, na.rm = TRUE)
  expect_true(nrow(pivoted_with_date)>1)

  # value column as Date column:min
  pivoted_with_date <- pivot(test_df, row_cols = c("cat1"), col_cols = c("cat2"), value = dateCol, fun.aggregate = min, na.rm = TRUE)
  expect_true(nrow(pivoted_with_date)>1)

  # value column as Date column:max
  pivoted_with_date <- pivot(test_df, row_cols = c("cat1"), col_cols = c("cat2"), value = dateCol, fun.aggregate = max, na.rm = TRUE)
  expect_true(nrow(pivoted_with_date)>1)

  # value column as Date column: n_distinct
  pivoted_with_date <- pivot(test_df, row_cols = c("cat1"), col_cols = c("cat2"), value = dateCol, fun.aggregate = n_distinct, na.rm = TRUE)
  expect_true(nrow(pivoted_with_date)>1)

  # value column as Date column: na_count
  pivoted_with_date <- pivot(test_df, row_cols = c("cat1"), col_cols = c("cat2"), value = dateCol, fun.aggregate = na_count, na.rm = TRUE)
  expect_true(nrow(pivoted_with_date)>1)

  # value column as Date column: na_ratio
  pivoted_with_date <- pivot(test_df, row_cols = c("cat1"), col_cols = c("cat2"), value = dateCol, fun.aggregate = na_ratio, na.rm = TRUE)
  expect_true(nrow(pivoted_with_date)>1)

  # value column as Date column: non_na_count
  pivoted_with_date <- pivot(test_df, row_cols = c("cat1"), col_cols = c("cat2"), value = dateCol, fun.aggregate = non_na_count, na.rm = TRUE)
  expect_true(nrow(pivoted_with_date)>1)

  # value column as Date column: non_na_ratio
  pivoted_with_date <- pivot(test_df, row_cols = c("cat1"), col_cols = c("cat2"), value = dateCol, fun.aggregate = non_na_ratio, na.rm = TRUE)
  expect_true(nrow(pivoted_with_date)>1)

  # value column as POSIXct column: mode
  pivoted_with_posixct <- pivot(test_df, row_cols = c("cat1"), col_cols = c("cat2"), value = posixctCol, fun.aggregate = get_mode, na.rm = TRUE)
  expect_true(nrow(pivoted_with_posixct)>1)

  # value column as POSIXct column: median
  pivoted_with_posixct <- pivot(test_df, row_cols = c("cat1"), col_cols = c("cat2"), value = posixctCol, fun.aggregate = median, na.rm = TRUE)
  expect_true(nrow(pivoted_with_posixct)>1)

  # value column as POSIXct column: min
  pivoted_with_posixct <- pivot(test_df, row_cols = c("cat1"), col_cols = c("cat2"), value = posixctCol, fun.aggregate = min, na.rm = TRUE)
  expect_true(nrow(pivoted_with_posixct)>1)

  # value column as POSIXct column: max
  pivoted_with_posixct <- pivot(test_df, row_cols = c("cat1"), col_cols = c("cat2"), value = posixctCol, fun.aggregate = max, na.rm = TRUE)
  expect_true(nrow(pivoted_with_posixct)>1)

  # value column as POSIXct column: first
  pivoted_with_posixct <- pivot(test_df, row_cols = c("cat1"), col_cols = c("cat2"), value = posixctCol, fun.aggregate = first, na.rm = TRUE)
  expect_true(nrow(pivoted_with_posixct)>1)

  # value column as POSIXct column: last
  pivoted_with_posixct <- pivot(test_df, row_cols = c("cat1"), col_cols = c("cat2"), value = posixctCol, fun.aggregate = last, na.rm = TRUE)
  expect_true(nrow(pivoted_with_posixct)>1)

  # value column as POSIXct column: n_distinct
  pivoted_with_posixct <- pivot(test_df, row_cols = c("cat1"), col_cols = c("cat2"), value = posixctCol, fun.aggregate = n_distinct, na.rm = TRUE)
  expect_true(nrow(pivoted_with_posixct)>1)

  # value column as POSIXct column: na_count
  pivoted_with_posixct <- pivot(test_df, row_cols = c("cat1"), col_cols = c("cat2"), value = posixctCol, fun.aggregate = na_count, na.rm = TRUE)
  expect_true(nrow(pivoted_with_posixct)>1)

  # value column as POSIXct column: na_ratio
  pivoted_with_posixct <- pivot(test_df, row_cols = c("cat1"), col_cols = c("cat2"), value = posixctCol, fun.aggregate = na_ratio, na.rm = TRUE)
  expect_true(nrow(pivoted_with_posixct)>1)

  # value column as POSIXct column: non_na_count
  pivoted_with_posixct <- pivot(test_df, row_cols = c("cat1"), col_cols = c("cat2"), value = posixctCol, fun.aggregate = non_na_count, na.rm = TRUE)
  expect_true(nrow(pivoted_with_posixct)>1)

  # value column as POSIXct column: non_na_ratio
  pivoted_with_posixct <- pivot(test_df, row_cols = c("cat1"), col_cols = c("cat2"), value = posixctCol, fun.aggregate = non_na_ratio, na.rm = TRUE)
  expect_true(nrow(pivoted_with_posixct)>1)

  # value column as integer column
  pivoted_with_int <- pivot(test_df, row_cols = c("cat1"), col_cols = c("cat2"), value = intCol, fun.aggregate = sum, na.rm = TRUE)
  expect_true(nrow(pivoted_with_int)>1)

})

test_that("test pivot with NA", {
  test_df_na <- data.frame(
    carrier = c("AA", "AA", "UA"),
    state = c("CA", "NY", "CA"),
    num = c(1,1,1)
  )
  pivoted <- test_df_na %>% pivot(row_cols=c("state"), col_cols=c("carrier"), value = num)
  expect_true(any(is.na(pivoted)))
})

test_that("test pivot with Date", {
  test_df <- data.frame(
    dt = rep(lubridate::today() + seq(3), each = 5),
    col = rep(seq(5), 3),
    val = seq(15)
  )

  pivoted <- pivot(test_df, row_cols=c("dt"), col_cols=c("col"), value = val)
  expect_true(pivoted$dt %>% inherits("Date"))
})

test_that("test pivot with Date funcs", {
  test_df <- data.frame(
    dt = rep(as.Date("2019-12-20") + seq(3)*10, each = 5),
    col = rep(seq(5), 3),
    val = seq(15)
  )

  pivoted <- pivot(test_df, row_cols=c(dt_wday="dt"), row_funs=c("wday"), col_cols=c("dt"), col_funs=c("week"), value = val)
  expect_true(all(pivoted$dt_wday %in% c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")))
  expect_equal(colnames(pivoted), c("dt_wday","2", "3", "52"))
})

test_that("test pivot with Date funcs with fill", { # There was a case where error happened with this case with fill (commit 0745f491).
  test_df <- data.frame(
    dt = rep(as.Date("2019-12-20") + seq(3)*10, each = 5),
    col = rep(seq(5), 3),
    val = seq(15)
  )

  pivoted <- pivot(test_df, row_cols=c(dt_wday="dt"), row_funs=c("wday"), col_cols=c("dt"), col_funs=c("week"), value = val, fill=0)
  expect_true(all(pivoted$dt_wday %in% c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")))
  expect_equal(colnames(pivoted), c("dt_wday","2", "3", "52"))
})

test_that("test pivot with POSIXct", {
  test_df <- data.frame(
    dt = rep(lubridate::now() + seq(3), each = 5),
    col = rep(seq(5), 3),
    val = seq(15)
  )

  pivoted <- pivot(test_df, row_cols=c("dt"), col_cols=c("col"), value = val)
  expect_true(pivoted$dt %>% inherits("POSIXct"))
})

test_that("test pivot with group_by and dirty colum names", {
  test_df <- data.frame(
    group = c(rep(letters[1:2], each = 50),"a"),
    cat1 = c(letters[round(runif(100)*5)+1], NA),
    cat2 = c(letters[round(runif(100)*3)+1], "a"),
    cat3 = c(letters[round(runif(100)*3)+1], "a"),
    num3 = c(NA, seq(100))
  )
  colnames(test_df) <- c("group", "cat 1", "cat-2", "cat 3", "Num 3")

  grouped_pivoted <- test_df %>%
    dplyr::group_by(group) %>%
    pivot(row_cols=c("cat 1", "cat-2"), col_cols=c("cat 3"))
  expect_true("group" %in% colnames(grouped_pivoted))
  expect_equal("group", grouped_by(grouped_pivoted))
})

test_that("test to_same_type for factor", {
  original <- factor(c("bb", "bb", "aa"), levels = c("bb", "aa"))

  to_replace <- c("aa", "aa", "bb")
  ret <- exploratory:::to_same_type(to_replace, original)
  expect_equal(ret, factor(c("aa", "aa", "bb"), levels = c("bb", "aa")))

  to_replace <- factor(c("aa", "aa", "bb"), levels = c("aa", "bb"))
  ret <- exploratory:::to_same_type(to_replace, original)
  expect_equal(ret, factor(c("aa", "aa", "bb"), levels = c("bb", "aa")))
})

test_that("test fill_mat_NA", {
  test_mat <- matrix(seq(10), nrow = 2)
  indice <- c(2, 5)
  ret <- exploratory:::fill_mat_NA(indice, test_mat)
  expect_true(any(is.na(ret)))
})


test_that("test na_count", {
  data <- c("a", "b", NA, "c", NA, "d", "e", "f", NA, NA)
  ret <- na_count(data)
  expect_true(ret == 4)
})

test_that("test na_pct", {
  data <- c("a", "b", NA, "c", NA, "d", "e", "f", NA, NA)
  ret <- na_pct(data)
  expect_true(ret == 40)
})

test_that("test na_ratio", {
  data <- c("a", "b", NA, "c", NA, "d", "e", "f", NA, NA)
  ret <- na_ratio(data)
  expect_true(ret == 0.4)
})

test_that("test non_na_count", {
  data <- c("a", "b", NA, "c", NA, "d", "e", "f", NA, NA)
  ret <- non_na_count(data)
  expect_true(ret == 6)
})

test_that("test non_na_pct", {
  data <- c("a", "b", NA, "c", NA, "d", "e", "f", NA, NA)
  ret <- non_na_pct(data)
  expect_true(ret == 60)
})

test_that("test non_na_ratio", {
  data <- c("a", "b", NA, "c", NA, "d", "e", "f", NA, NA)
  ret <- non_na_ratio(data)
  expect_true(ret == 0.6)
})

test_that("test extract_from_date", {
  data <- as.Date(c("2018-01-23", "2018-01-25", NA, NA))
  ret <- extract_from_date(data, type="year")
  expect_equal(ret, c(2018,2018,NA,NA))
  ret <- extract_from_date(data, type="fltoyear")
  expect_equal(ret, as.Date(c("2018-01-01","2018-01-01",NA,NA)))
})

test_that("test weekend", {
  data <- as.Date(c("2019-06-16", "2019-06-08", "2019-06-26", NA, NA))
  ret <- weekend(data)
  expect_equal(ret, as.factor(c("Weekend","Weekend", "Weekday", NA, NA)))
})

test_that("test %in_or_all%", {
  ret <- c(1,2,3) %in_or_all% c(1,2)
  expect_equal(ret, c(TRUE, TRUE, FALSE))
  ret <- c(1,2,3) %in_or_all% NULL
  expect_equal(ret, c(TRUE, TRUE, TRUE))
  ret <- c(1,2,3) %in_or_all% c()
  expect_equal(ret, c(TRUE, TRUE, TRUE))
  ret <- c('a','b','c') %in_or_all% stringr::str_to_lower(c()) # Test for our case-insensitive filter condition.
  expect_equal(ret, c(TRUE, TRUE, TRUE))
})

test_that("test %equal_or_all%", {
  df <- data.frame(x  = c (1,2,3))
  ret <- df %>% dplyr::filter(x %equal_or_all% 2)
  expect_equal(nrow(ret), 1)
  df <- data.frame(x  = c (1,2,3))
  ret <- df %>% dplyr::filter(x %equal_or_all% NULL)
  expect_equal(nrow(ret), 3)
  df <- data.frame(x  = c (1,2,3))
  ret <- df %>% dplyr::filter(x %equal_or_all% "")
  expect_equal(nrow(ret), 3)
})

test_that("test %not_equal_or_all%", {
  df <- data.frame(x  = c (1,2,3))
  ret <- df %>% dplyr::filter(x %not_equal_or_all% 2)
  expect_equal(nrow(ret), 2)
  df <- data.frame(x  = c (1,2,3))
  ret <- df %>% dplyr::filter(x %not_equal_or_all% NULL)
  expect_equal(nrow(ret), 3)
  df <- data.frame(x  = c (1,2,3))
  ret <- df %>% dplyr::filter(x %not_equal_or_all% "")
  expect_equal(nrow(ret), 3)
})

test_that("test %greater_or_all%", {
  df <- data.frame(x  = c (1,2,3))
  ret <- df %>% dplyr::filter(x %greater_or_all% 2)
  expect_equal(nrow(ret), 1)
  df <- data.frame(x  = c (1,2,3))
  ret <- df %>% dplyr::filter(x %greater_or_all% NULL)
  expect_equal(nrow(ret), 3)
  df <- data.frame(x  = c (1,2,3))
  ret <- df %>% dplyr::filter(x %greater_or_all% "")
  expect_equal(nrow(ret), 3)
})

test_that("test %greater_or_equal_or_all%", {
  df <- data.frame(x  = c (1,2,3))
  ret <- df %>% dplyr::filter(x %greater_or_equal_or_all% 2)
  expect_equal(nrow(ret), 2)
  df <- data.frame(x  = c (1,2,3))
  ret <- df %>% dplyr::filter(x %greater_or_equal_or_all% NULL)
  expect_equal(nrow(ret), 3)
  df <- data.frame(x  = c (1,2,3))
  ret <- df %>% dplyr::filter(x %greater_or_equal_or_all% "")
  expect_equal(nrow(ret), 3)
})

test_that("test %less_or_all%", {
  df <- data.frame(x  = c (1,2,3))
  ret <- df %>% dplyr::filter(x %less_or_all% 2)
  expect_equal(nrow(ret), 1)
  df <- data.frame(x  = c (1,2,3))
  ret <- df %>% dplyr::filter(x %less_or_all% NULL)
  expect_equal(nrow(ret), 3)
  df <- data.frame(x  = c (1,2,3))
  ret <- df %>% dplyr::filter(x %less_or_all% "")
  expect_equal(nrow(ret), 3)
})

test_that("test %less_or_equal_or_all%", {
  df <- data.frame(x  = c (1,2,3))
  ret <- df %>% dplyr::filter(x %less_or_equal_or_all% 2)
  expect_equal(nrow(ret), 2)
  df <- data.frame(x  = c (1,2,3))
  ret <- df %>% dplyr::filter(x %less_or_equal_or_all% NULL)
  expect_equal(nrow(ret), 3)
  df <- data.frame(x  = c (1,2,3))
  ret <- df %>% dplyr::filter(x %less_or_equal_or_all% "")
  expect_equal(nrow(ret), 3)
})

test_that("test mase", {
  data <- data.frame(`actual` = c(1,2,NA,2,2.5,3,NA,4),
                     `predicted` = c(1,2,3,3,3,2,4,4),
                     `is_test_data` = c(F,F,F,F,F,T,T,T)) %>%
    dplyr::rename(`ac tual` = actual,
                  `pre dicted` = predicted,
                  `is test data` = is_test_data)

  ret <- data %>% dplyr::summarize(`ma se` = exploratory::mase(`ac tual`, `pre dicted`, `is test data`))
  expect_equal(ret$`ma se`, 0.667, tolerance=0.01)
})

test_that("test true_count", {
  ret <- true_count(c(T,T,T,F,F,NA))
  expect_equal(ret, 3)
})

test_that("test false_count", {
  ret <- false_count(c(T,T,T,F,F,NA))
  expect_equal(ret, 2)
})

test_that("test get_confint", {
  mean_vals <- c(0,1,NA)
  sd_vals <- c(1,1,1)
  ret <- get_confint(mean_vals, sd_vals, conf_int = 0.975)
  expect_equal(ret, c(1.959964, 2.959964, NA), tolerance=0.0001)
})

test_that("test str_clean", {
  ret <- str_clean(c("  not a very  tidy sentence ", " dirty  text ", NA))
  expect_equal(ret, c("not a very tidy sentence", "dirty text", NA))
})

test_that("test str_count_all", {
  # TODO: NA handling in remove.zero = TRUE case.
  ret <- str_count_all(c("  not a very  tidy sentence ", " dirty  text ", NA), c("very", "dirty"), remove.zero = FALSE)
  expect_equal(ret, list(data.frame(.count=c(1,0), .pattern=c("very","dirty"), stringsAsFactors = F),
                         data.frame(.count=c(0,1), .pattern=c("very","dirty"), stringsAsFactors = F),
                         data.frame(.count=c(NA_integer_,NA_integer_), .pattern=c("very","dirty"), stringsAsFactors = F)))
})

test_that("test safe_slice", {
  mat <- matrix(c(1,NA,3,NA,5,6,7,8,NA), 3,3)
  ret1 <- safe_slice(mat,1)
  expect_equal(ret1, matrix(c(1,NA,7),1,3))
  ret2 <- safe_slice(mat,1, remove=TRUE)
  expect_equal(ret2, matrix(c(NA,3,5,6,8,NA),2,3))
})

test_that("test sameple_df_index", {
  df <-data.frame(x=1:99,y=1:99)
  test_index <- sample_df_index(df, 0.3, seed=1)
  expect_equal(length(test_index), 29)
  test_index <- sample_df_index(df, 0.3, ordered=TRUE)
  expect_equal(length(test_index), 30)
})

test_that("test sameple_rows", {
  df <-data.frame(x=c(1,NA,3),y=c(2,3,NA))
  df <-setNames(df,c("x 1", "col 2"))
  ret <- df %>% sample_rows(2)
  expect_equal(nrow(ret), 2)
  ret <- df %>% sample_rows(5)
  expect_equal(nrow(ret), 3)
})

test_that("test sameple_rows with group_by", {
  ret <- mtcars %>% group_by(am, vs) %>% sample_rows(3, seed=1)
  expect_equal(nrow(ret), 12) # 3 rows times 4 groups
  expect_equal(grouped_by(ret), c("am", "vs")) # group_by columns should be kept
  # Set of columns should be preserved, though group_by columns are brought to the beginning, which is a known not-so-great behavior.
  expect_true(all(colnames(ret) %in% c("vs","am","mpg","cyl","disp","hp","drat","wt","qsec","gear","carb")))
})

test_that("test unnest_without_empty", {
  df <- data.frame(x=c(1,2,3))
  # create empty row in y
  df$y <- lapply(c(1,0,2),function(x){rep(1,x)})
  # TODO: regular unnest returns the same result as unnest_without_empty.
  # not sure what was the case unnest_without_empty was required at this point.
  # maybe we don't need unnest_without_empty anymore??
  ret <- df %>% unnest_without_empty(y)
  expect_equal(ret$x, c(1,3,3))
  expect_equal(ret$y, c(1,1,1))

  # create empty row in y
  df$y <- lapply(c(1,0,2),function(x){data.frame(z=rep(1,x), w=rep(2,x))})
  # TODO: regular unnest returns the same result as unnest_without_empty.
  # not sure what was the case unnest_without_empty was required at this point.
  # maybe we don't need unnest_without_empty anymore??
  ret <- df %>% unnest_without_empty(y)
  expect_equal(ret$x, c(1,3,3))
  expect_equal(ret$z, c(1,1,1))
  expect_equal(ret$w, c(2,2,2))
})

test_that("r_squared", {
  res <- r_squared(c(1,2,3,4,5), c(3,4,2,4,7))
  expect_equal(res, -0.3, tolerance = 0.001)
})

test_that("excel_numeric_to_date", {
  res <- exploratory::excel_numeric_to_date(50000L) # test integer input
  expect_equal(res, as.Date("2036-11-21"))
})

test_that("excel_numeric_to_datetime", {
  res <- exploratory::excel_numeric_to_datetime(42370.5, tz = "GMT")
  expect_equal(res, as.POSIXct("2016-01-01 12:00:00",tz = "GMT"))
  res <- exploratory::excel_numeric_to_datetime("42370.5", tz = "GMT")
  expect_equal(res, as.POSIXct("2016-01-01 12:00:00",tz = "GMT"))

})

test_that("one_hot", {
  # numeric column case
  df <- data.frame(x=c(1,1,2,3))
  res <- df %>% one_hot(x)
  expect_equal(res$x_1, c(1,1,0,0))

  # character column case
  df <- data.frame(x=c("A", "A", "B", "C"))
  res <- df %>% one_hot(x)
  expect_equal(res$x_A, c(1,1,0,0))
})

test_that("get_mode", {
  # numeric column case
  x <- c(1,2,2,3,NA,NA,NA)
  res <- get_mode(x)
  expect_true(is.na(res))
  res <- get_mode(x, na.rm = TRUE)
  expect_equal(res, 2)

  # logical column case
  x <- c(F,T,T,NA,NA,NA)
  res <- get_mode(x)
  expect_true(is.na(res))
  res <- get_mode(x, na.rm = TRUE)
  expect_equal(res, T)

  # character column case
  x <- c("A","B","B","C",NA,NA,NA)
  res <- get_mode(x)
  expect_true(is.na(res))
  res <- get_mode(x, na.rm = TRUE)
  expect_equal(res, "B")

  # factor column case
  x <- factor(c("A","B","B","C",NA,NA,NA))
  res <- get_mode(x)
  expect_true(is.na(res))
  res <- get_mode(x, na.rm = TRUE)
  expect_equal(as.character(res), "B")

  # Date column case
  x <-c(as.Date(c("2019-01-01","2019-01-02","2019-01-02","2019-01-03")),NA,NA,NA)
  res <- get_mode(x)
  expect_true(is.na(res))
  res <- get_mode(x, na.rm = TRUE)
  expect_equal(as.character(res), "2019-01-02")
})

test_that("get_unknown_category_rows_index", {
  train_df <- data.frame(x=c('a','b','c'),
                         y=c('a','b','c'))
  test_df <- data.frame(y=c('b','e','c'), # Intentionally switched column order to test case where orders do not match.
                        x=c('a','c','d'))
  unknown_vector <- exploratory:::get_unknown_category_rows_index_vector(test_df, train_df)
  unknown_index <- exploratory:::get_row_numbers_from_index_vector(unknown_vector)
  expect_equal(unknown_index,c(2,3))
  restored <- exploratory:::restore_na(c('a','b','c'), c(2,4))
  expect_equal(restored ,c('a',NA,'b',NA,'c'))
  restored <- exploratory:::restore_na(c('a','b','c'), c(1,3,5,7))
  expect_equal(restored ,c(NA,'a',NA,'b',NA,'c',NA))
})

test_that("summarize_group", {
 df <- mtcars %>% exploratory::summarize_group(group_cols = c(cyl="cyl", mpg_int10="mpg"), group_funs = c("none", "asintby10"), count = n())
 expect_equal(nrow(df),5)
 df2 <- mtcars %>% exploratory::summarize_group(group_cols = NULL, group_funs = NULL, count = n())
 expect_equal(nrow(df2),1)
})


test_that("sum_if", {
  df <- mtcars %>% exploratory::summarize_group(group_cols = c(cyl="cyl"), group_funs = c("none"),  custom = exploratory::sum_if(hp, mpg > 15, na.rm = T))
  expect_equal(df %>% dplyr::pull(custom), c(909, 856, 1454))
})

test_that("sum_if_ratio", {
  df <- mtcars %>% exploratory::summarize_group(group_cols = c(cyl="cyl"), group_funs = c("none"),  custom = exploratory::sum_if_ratio(hp, mpg > 15, na.rm = T))
  expect_equal(df %>% dplyr::pull(custom), c(1, 1, 1454/2929)) # 909/909, 856/856, 1454/2929
})

test_that("sum_if_pct", {
  df <- mtcars %>% exploratory::summarize_group(group_cols = c(cyl="cyl"), group_funs = c("none"),  custom = exploratory::sum_if_pct(hp, mpg > 15, na.rm = T))
  expect_equal(df %>% dplyr::pull(custom), c(100, 100, 100 * 1454/2929)) # 100 * 909/909, 100 * 856/856, 100 * 1454/2929
})

test_that("count_if", {
  df <- mtcars %>% exploratory::summarize_group(group_cols = c(cyl="cyl"), group_funs = c("none"),  custom = exploratory::count_if(hp, mpg > 15, na.rm = F))
  expect_equal(df %>% dplyr::pull(custom), c(11, 7, 8))
})

test_that("count_if_ratio", {
  df <- mtcars %>% exploratory::summarize_group(group_cols = c(cyl="cyl"), group_funs = c("none"),  custom = exploratory::count_if_ratio(hp, mpg > 15, na.rm = F))
  expect_equal(df %>% dplyr::pull(custom), c(1, 1, 8/14)) # 11/11, 7/7, 8/14
})

test_that("count_if_pct", {
  df <- mtcars %>% exploratory::summarize_group(group_cols = c(cyl="cyl"), group_funs = c("none"),  custom = exploratory::count_if_pct(hp, mpg > 15, na.rm = F))
  expect_equal(df %>% dplyr::pull(custom), c(100, 100, 800/14)) # 100 *11/11, 100*7/7, 100*8/14
})

test_that("count_unique_if", {
  df <- mtcars %>% exploratory::summarize_group(group_cols = c(cyl="cyl"), group_funs = c("none"),  custom = exploratory::count_unique_if(hp, mpg > 15, na.rm = F))
  expect_equal(df %>% dplyr::pull(custom), c(10, 4, 4))
})

test_that("count_unique_if_ratio", {
  df <- mtcars %>% exploratory::summarize_group(group_cols = c(cyl="cyl"), group_funs = c("none"),  custom = exploratory::count_unique_if_ratio(hp, mpg > 15, na.rm = F))
  expect_equal(df %>% dplyr::pull(custom), c(1, 1, 4/9)) # 10/10, 4/4, 4/9
})

test_that("count_unique_if_pct", {
  df <- mtcars %>% exploratory::summarize_group(group_cols = c(cyl="cyl"), group_funs = c("none"),  custom = exploratory::count_unique_if_pct(hp, mpg > 15, na.rm = F))
  expect_equal(df %>% dplyr::pull(custom), c(100, 100, 100 * 4/9)) # 10/10, 4/4, 4/9
})

test_that("count_rows", {
  df <- mtcars %>% exploratory::summarize_group(group_cols = c(cyl="cyl"), group_funs = c("none"),  custom = exploratory::count_rows())
  expect_equal(df %>% dplyr::pull(custom), c(11, 7, 14))
})

test_that("count_unique", {
  df <- mtcars %>% exploratory::summarize_group(group_cols = c(cyl="cyl"), group_funs = c("none"),  custom = exploratory::count_unique(hp))
  expect_equal(df %>% dplyr::pull(custom), c(10, 4, 9))
})

test_that("average_if", {
  df <- mtcars %>% exploratory::summarize_group(group_cols = c(cyl="cyl"), group_funs = c("none"),  custom = exploratory::average_if(hp, mpg > 15, na.rm = T))
  expect_equal(df %>% mutate(custom = round(custom)) %>% dplyr::pull(custom), c(83, 122, 182))
})

test_that("average_if_ratio", {
  df <- mtcars %>% exploratory::summarize_group(group_cols = c(cyl="cyl"), group_funs = c("none"),  custom = exploratory::average_if_ratio(hp, mpg > 15, na.rm = T))
  expect_equal(df %>% mutate(custom = round(custom, digit=2)) %>% dplyr::pull(custom), c(1, 1, 0.87))
})

test_that("average_if_pct", {
  df <- mtcars %>% exploratory::summarize_group(group_cols = c(cyl="cyl"), group_funs = c("none"),  custom = exploratory::average_if_pct(hp, mpg > 15, na.rm = T))
  expect_equal(df %>% mutate(custom = round(custom, digit=0)) %>% dplyr::pull(custom), c(100, 100, 87))
})

test_that("mean_if", {
  df <- mtcars %>% exploratory::summarize_group(group_cols = c(cyl="cyl"), group_funs = c("none"),  custom = exploratory::mean_if(hp, mpg > 15, na.rm = F))
  expect_equal(df %>% mutate(custom = round(custom)) %>% dplyr::pull(custom), c(83, 122, 182))
})

test_that("mean_if_ratio", {
  df <- mtcars %>% exploratory::summarize_group(group_cols = c(cyl="cyl"), group_funs = c("none"),  custom = exploratory::mean_if_ratio(hp, mpg > 15, na.rm = T))
  expect_equal(df %>% mutate(custom = round(custom, digit=2)) %>% dplyr::pull(custom), c(1, 1, 0.87))
})

test_that("mean_if_pct", {
  df <- mtcars %>% exploratory::summarize_group(group_cols = c(cyl="cyl"), group_funs = c("none"),  custom = exploratory::mean_if_pct(hp, mpg > 15, na.rm = T))
  expect_equal(df %>% mutate(custom = round(custom, digit=0)) %>% dplyr::pull(custom), c(100, 100, 87))
})

test_that("median_if", {
  df <- mtcars %>% exploratory::summarize_group(group_cols = c(cyl="cyl"), group_funs = c("none"),  custom = exploratory::median_if(hp, mpg > 15, gear > 3))
  expect_equal(df %>% dplyr::pull(custom), c(78.5, 123.0, 264))
})

test_that("median_if_ratio", {
  df <- mtcars %>% exploratory::summarize_group(group_cols = c(cyl="cyl"), group_funs = c("none"),  custom = exploratory::median_if_ratio(hp, mpg > 15, gear > 3))
  expect_equal(df %>% mutate(custom = round(custom, digit=2)) %>% dplyr::pull(custom), c(0.86, 1.12, 1.37))
})

test_that("median_if_pct", {
  df <- mtcars %>% exploratory::summarize_group(group_cols = c(cyl="cyl"), group_funs = c("none"),  custom = exploratory::median_if_pct(hp, mpg > 15, gear > 3))
  expect_equal(df %>% mutate(custom = round(custom, digit=0)) %>% dplyr::pull(custom), c(86, 112, 137))
})


test_that("min_if", {
  df <- mtcars %>% exploratory::summarize_group(group_cols = c(cyl="cyl"), group_funs = c("none"),  custom = exploratory::min_if(hp, mpg > 15, gear > 2, na.rm = T))
  expect_equal(df %>% dplyr::pull(custom), c(52, 105, 150))
})

test_that("min_if_ratio", {
  df <- mtcars %>% exploratory::summarize_group(group_cols = c(cyl="cyl"), group_funs = c("none"),  custom = exploratory::min_if_ratio(hp, mpg > 15, gear > 2, na.rm = T))
  expect_equal(df %>% dplyr::pull(custom), c(1, 1, 1))
})

test_that("min_if_pct", {
  df <- mtcars %>% exploratory::summarize_group(group_cols = c(cyl="cyl"), group_funs = c("none"),  custom = exploratory::min_if_pct(hp, mpg > 15, gear > 2, na.rm = T))
  expect_equal(df %>% dplyr::pull(custom), c(100, 100, 100))
})

test_that("max_if", {
  df <- mtcars %>% exploratory::summarize_group(group_cols = c(cyl="cyl"), group_funs = c("none"),  custom = exploratory::max_if(hp, mpg > 15, gear > 2, na.rm = T))
  expect_equal(df %>% dplyr::pull(custom), c(113, 175, 264))
})

test_that("max_if_ratio", {
  df <- mtcars %>% exploratory::summarize_group(group_cols = c(cyl="cyl"), group_funs = c("none"),  custom = exploratory::max_if_ratio(hp, mpg > 15, gear > 2, na.rm = T))
  expect_equal(df %>% mutate(custom = round(custom, digit=2)) %>% dplyr::pull(custom), c(1, 1, 0.79))
})

test_that("max_if_pct", {
  df <- mtcars %>% exploratory::summarize_group(group_cols = c(cyl="cyl"), group_funs = c("none"),  custom = exploratory::max_if_pct(hp, mpg > 15, gear > 2, na.rm = T))
  expect_equal(df %>% mutate(custom = round(custom, digit=0)) %>% dplyr::pull(custom), c(100, 100, 79))
})

test_that("summarize_row", {
 df <- airquality %>% mutate(total = summarize_row(across(where(is.numeric)), mean, na.rm=TRUE))
 expect_equal(colnames(df), c("Ozone", "Solar.R", "Wind", "Temp", "Month", "Day", "total"))
 # Test the default function (mean).
 df <- airquality %>% mutate(total = summarize_row(across(where(is.numeric))))
 expect_equal(colnames(df), c("Ozone", "Solar.R", "Wind", "Temp", "Month", "Day", "total"))
})

test_that("revert_factor_cols_to_logical", {
  df <- data.frame(col1 = I(factor(c(TRUE, FALSE, NA))),
                   col2 = I(forcats::fct_rev(factor(c(TRUE,FALSE,NA)))),
                   col3 = I(factor(c("A","B","C"))))

  res <- revert_factor_cols_to_logical(df)
  expect_equal(res$col1, c(TRUE, FALSE, NA))
  expect_equal(res$col2, c(TRUE, FALSE, NA))
})

test_that("is_integer", {
  expect_true(exploratory:::is_integer(c(0,1,2,3,4,5)))
  expect_false(exploratory:::is_integer(c(0,1.5,2,3,4,5)))
})

test_that("week", {
  dates <- lubridate::ymd(c(20190101,20190107,20190108,20190131,20190201,20190207,20190208,20190701,20190707,20190708,20190731,20191201,20191207,20191208,20191231))
  expect_equal(exploratory::week(dates), c(1,1,2,5,5,6,6,26,27,27,31,48,49,49,53))
  expect_equal(exploratory::week(dates, unit="quarter"), c(1,1,2,5,5,6,6,1,1,2,5,9,10,10,14))
  expect_equal(exploratory::week(dates, unit="month"), c(1,1,2,5,1,1,2,1,1,2,5,1,1,2,5))
})

test_that("confint_mean", {
  # confint_radius is the base implementation.
  v <- 1:100
  expect_equal(exploratory::confint_radius(v), exploratory::confint_mean(v))
})

test_that("calc_confint_mean", {
  # confint_radius is the base implementation.
  v <- 1:100
  expect_equal(exploratory::confint_radius(v), exploratory::calc_confint_mean(sd(v), length(v)))
})

test_that("confint_ratio", {
  # prop_confint_radius is the base implementation.
  v <- 1:100 %% 3 ==0
  expect_equal(exploratory::prop_confint_radius(v), exploratory::confint_ratio(v))
})

test_that("calc_confint_ratio", {
  # prop_confint_radius is the base implementation.
  v <- 1:100 %% 3 ==0
  expect_equal(exploratory::prop_confint_radius(v), exploratory::calc_confint_ratio(sum(v)/length(v), length(v)))
})

test_that("map_platform_locale", {
  ret <- exploratory:::map_platform_locale("Japanese_Japan", from="windows", to="unix")
  expect_equal(ret, "ja_JP.UTF-8")
  ret <- exploratory:::map_platform_locale("ja_JP", from="unix", to="windows")
  expect_equal(ret, "Japanese_Japan")
  ret <- exploratory:::map_platform_locale("English_United States", from="windows", to="unix")
  expect_equal(ret, "en_US.UTF-8")
  ret <- exploratory:::map_platform_locale("en_US", from="unix", to="windows")
  expect_equal(ret, "English_United States")
})

test_that("mutate_predictors", {
  df <- tibble::tibble(x=1, t=as.Date("2020-01-01"), y=4, z=8, w=8)
  res <- df %>% exploratory:::mutate_predictors(c("x", "t", "y", "z", "w"), list(x="log", list(t_mon="mon", t_wday="wday", t_week_of_quarter="week_of_quarter"), y="log2", z=function(x){log(x, base=2)}, w=rlang::expr(log(., base=2))))
  expect_true(all(c("x", "t_mon", "t_wday", "t_week_of_quarter", "y", "z", "w") %in% colnames(res)))
})

test_that("cumsum_decayed", {
  res <- cumsum_decayed(c(1,1,1,1), 0.5)
  expect_equal(res, c(1, 1.5, 1.75, 1.875))
})

test_that("auroc", {
  res <- auroc(c(0.1, 0.2, 0.3, 0.4), c(F, T, F, T))
  expect_equal(res, 0.75)
  res <- auroc(c(0.1, 0.2, 0.3, 0.4, NA, 0.6), c(F, T, F, T, F, NA))
  expect_equal(res, 0.75)
})

test_that("merge_vars", {
  a <- c(1,2,3)
  b <- c(2,3,4,5)
  var1 <- var(c(a,b))
  var2 <- merge_vars(c(var(a), var(b)), c(mean(a), mean(b)), c(length(a), length(b)))
  expect_equal(var1, var2)
})

test_that("merge_sds", {
  a <- c(1,2,3)
  b <- c(2,3,4,5)
  sd1 <- sd(c(a,b))
  sd2 <- merge_sds(c(sd(a), sd(b)), c(mean(a), mean(b)), c(length(a), length(b)))
  expect_equal(sd1, sd2)
})

test_that("years_between", {
  age <- years_between(lubridate::ymd_hms("2000-01-01 13:00:00"), lubridate::ymd_hms("2001-01-01 13:00:00"))
  expect_equal(age, 1)
})

test_that("months_between", {
  age <- months_between(lubridate::ymd("2000-01-01"), lubridate::ymd("2001-01-01"))
  expect_equal(age, 12)
})

test_that("weeks_between", {
  age <- weeks_between(lubridate::ymd("2000-01-01"), lubridate::ymd("2001-01-01"))
  expect_equal(age, 52.2857142857143)
})

test_that("days_between", {
  age <- days_between(lubridate::ymd("2000-01-01"), lubridate::ymd("2001-01-01"))
  expect_equal(age, 366)
})

test_that("hours_between", {
  age <- hours_between(lubridate::ymd_hms("2000-01-01 13:00:00"), lubridate::ymd_hms("2000-01-01 19:30:00"))
  expect_equal(age, 6.5)
})

test_that("minutes_between", {
  age <- minutes_between(lubridate::ymd_hms("2000-01-01 13:00:00"), lubridate::ymd_hms("2000-01-01 14:30:00"))
  expect_equal(age, 90)
})

test_that("seconds_between", {
  age <- seconds_between(lubridate::ymd_hms("2000-01-01 13:00:00"), lubridate::ymd_hms("2000-01-01 13:10:00"))
  expect_equal(age, 600)
})

test_that("last_date", {
  res <- last_date(lubridate::ymd("2000-01-01"))
  expect_equal(res, lubridate::ymd("2000-01-31"))
  res <- last_date(lubridate::ymd_hms("2000-01-01 1:00:00"))
  expect_equal(res, lubridate::ymd_hms("2000-01-31 00:00:00"))
  res <- last_date(lubridate::ymd("2000-01-01"), previous = TRUE)
  expect_equal(res, lubridate::ymd("1999-12-31"))
  res <- last_date(lubridate::ymd_hms("2000-01-01 1:00:00"), previous = TRUE)
  expect_equal(res, lubridate::ymd_hms("1999-12-31 00:00:00"))
})

test_that("ts_lag", {
  t <- as.Date(c("2020-01-01", "2021-01-01", "2021-01-08", "2021-02-01"))
  y <- c(1, 2, 3, 4)
  res <- ts_lag(t, y)
  expect_equal(res, c(NA, 1, 1, 1))
  res <- ts_lag(t, y, unit="quarter")
  expect_equal(res, c(NA, 1, 1, 1))
  res <- ts_lag(t, y, unit="month")
  expect_equal(res, c(NA, 1, 1, 2))
  res <- ts_lag(t, y, unit="week")
  expect_equal(res, c(NA, 1, 2, 3))
  res <- ts_lag(t, y, unit="week", na_fill_type="none")
  expect_equal(res, c(NA, NA, 2, NA))
  res <- ts_lag(t, y, unit="week", na_fill_type="next")
  expect_equal(res, c(1, 2, 2, 4))
  res <- ts_lag(t, y, unit="week", n=2)
  expect_equal(res, c(NA, 1, 1, 3))
})

test_that("ts_diff", {
  t <- as.Date(c("2020-01-01", "2021-01-01", "2021-01-08", "2021-02-01"))
  y <- c(1, 2, 3, 4)
  res <- ts_diff(t, y)
  expect_equal(res, c(NA, 1, 2, 3))
  res <- ts_diff(t, y, unit="quarter")
  expect_equal(res, c(NA, 1, 2, 3))
  res <- ts_diff(t, y, unit="month")
  expect_equal(res, c(NA, 1, 2, 2))
  res <- ts_diff(t, y, unit="week")
  expect_equal(res, c(NA, 1, 1, 1))
  res <- ts_diff(t, y, unit="week", na_fill_type="none")
  expect_equal(res, c(NA, NA, 1, NA))
  res <- ts_diff(t, y, unit="week", na_fill_type="next")
  expect_equal(res, c(0, 0, 1, 0))
  res <- ts_diff(t, y, unit="week", n=2)
  expect_equal(res, c(NA, 1, 2, 1))
})

test_that("ts_diff_ratio", {
  t <- as.Date(c("2020-01-01", "2021-01-01", "2021-01-08", "2021-02-01"))
  y <- c(1, 2, 3, 4)
  res <- ts_diff_ratio(t, y)
  expect_equal(res, c(NA, 1, 2, 3))
  res <- ts_diff_ratio(t, y, unit="quarter")
  expect_equal(res, c(NA, 1, 2, 3))
  res <- ts_diff_ratio(t, y, unit="month")
  expect_equal(res, c(NA, 1, 2, 1))
  res <- ts_diff_ratio(t, y, unit="week")
  expect_equal(res, c(NA, 1, 0.5, 1/3))
  res <- ts_diff_ratio(t, y, unit="week", na_fill_type="none")
  expect_equal(res, c(NA, NA, 0.5, NA))
  res <- ts_diff_ratio(t, y, unit="week", na_fill_type="next")
  expect_equal(res, c(0, 0, 0.5, 0))
  res <- ts_diff_ratio(t, y, unit="week", n=2)
  expect_equal(res, c(NA, 1, 2, 1/3))
})

test_that("is_japanese_holiday", {
  current_option <- getOption("lubridate.week.start")
  # reset the week stat day to Monday (1) to test the fix
  options(lubridate.week.start = 1)
  expect_equal(exploratory::is_japanese_holiday("2019-10-14"), TRUE)
  expect_equal(exploratory::is_japanese_holiday("2019-10-08"), FALSE)
  expect_equal(exploratory::is_japanese_holiday(c("2019-10-14", "2019-10-08", NA)), c(TRUE, FALSE, FALSE))

  # Make sure the option is not reset by exploratory::is_japanese_holiday
  expect_equal(getOption("lubridate.week.start"),1)
  # reset
  options(lubridate.week.start = current_option)
})

test_that("mutate_group", {
  library(lubridate)
  df <- mtcars %>% exploratory::mutate_group(group_cols = c(cyl="cyl", mpg_int10="mpg"), group_funs = c("none", "asintby10"), mpg_cummean = cummean(mpg), mpg_cumsum = cumsum(mpg))
  expect_equal(head(df)$mpg_cummean[[1]],22.8)
  expect_equal(head(df)$mpg_cummean[[2]],23.6)
  expect_equal(head(df)$cyl[[1]], 4) # cyl is sorted so first line should be 4
  expect_equal(head(df)$mpg_int10[[1]], 20) # mpg_int10 is sorted so first line should be 20
  expect_equal(head(df, 12)$cyl[[12]], 6) # cyl is sorted and next group (6) starts from line 12
  expect_equal(head(df, 12)$mpg_int10[[12]], 10) # mpg_int10 is sorted and the value for the line 12 is 10
  expect_equal(head(df)$mpg_cumsum[[1]],22.8)
  expect_equal(head(df)$mpg_cumsum[[2]],47.2)
  df2 <- mtcars %>% exploratory::mutate_group(group_cols = c(cyl="cyl", mpg_int10="mpg"), group_funs = c("none", "asintby10"), wt_cummean = cummean(wt), wt_cumsum = cumsum(wt))
  expect_equal(head(df2)$wt_cummean[[1]],2.32)
  print(head(df2)$wt_cummean[[2]])
  expect_equal(round(head(df2)$wt_cummean[[2]], digits = 2) ,2.76)
  expect_equal(head(df2)$wt_cumsum[[1]],2.32)
  expect_equal(head(df2)$wt_cumsum[[2]],5.51)
  tmp <- tempfile(fileext = ".parquet")
  empDF <- exploratory::read_parquet_file("https://www.dropbox.com/s/n0jkv4wu9dpb4se/Employee_Data_win_calc.parquet?dl=1")

    # group by Date - floor to year
  df3 <- empDF %>% exploratory::mutate_group(group_cols = c(`hired_date_year` = "hired_date"),group_funs = c("rtoyear"),salary_cumsum = cumsum(salary))
  expect_equal(head(df3)$hired_date_year[[1]], as.Date("1976-01-01"))
  expect_equal(head(df3)$salary_cumsum[[3]], 13872)

    # group by Date - floor to half year
  df4 <- empDF %>% exploratory::mutate_group(group_cols = c(`hired_date_halfyear` = "hired_date"),group_funs = c("rtohalfyear"),salary_cumsum = cumsum(salary))
  expect_equal(head(df4)$hired_date_halfyear[[1]], as.Date("1976-07-01"))
  expect_equal(head(df4)$salary_cumsum[[3]], 10169)

  # group by Date - floor to quarter
  df5 <- empDF %>% exploratory::mutate_group(group_cols = c(`hired_date_q` = "hired_date"),group_funs = c("rtoq"),salary_cumsum = cumsum(salary))
  expect_equal(head(df5)$hired_date_q[[2]], as.Date("1977-04-01"))
  expect_equal(head(df5)$salary_cumsum[[4]], 13872)

    # group by Date - floor to bi-month
  df6 <- empDF %>% exploratory::mutate_group(group_cols = c(`hired_date_bimon` = "hired_date"),group_funs = c("rtobimon"),salary_cumsum = cumsum(salary))
  expect_equal(head(df6)$hired_date_bimon[[2]], as.Date("1977-05-01"))
  expect_equal(head(df6)$salary_cumsum[[5]], 19586)

  # group by Date - floor to month
  df7 <- empDF %>% exploratory::mutate_group(group_cols = c(`hired_date_mon` = "hired_date"),group_funs = c("rtomon"),salary_cumsum = cumsum(salary))
  expect_equal(head(df7)$hired_date_mon[[2]], as.Date("1977-06-01"))
  expect_equal(head(df7)$salary_cumsum[[6]], 19045)

  # group by Date - floor to week
  df8 <- empDF %>% exploratory::mutate_group(group_cols = c(`hired_date_week` = "hired_date"),group_funs = c("rtoweek"),salary_cumsum = cumsum(salary))
  expect_equal(head(df8)$hired_date_week[[2]], as.Date("1977-06-12"))
  expect_equal(head(df8,7)$salary_cumsum[[7]], 16856)

  # group by Date - floor to day
  df9 <- empDF %>% exploratory::mutate_group(group_cols = c(`hired_date_day` = "hired_date"),group_funs = c("rtoday"),salary_cumsum = cumsum(salary))
  expect_equal(head(df9)$hired_date_day[[2]], as.Date("1977-06-13"))
  expect_equal(head(df9)$salary_cumsum[[3]], 10169)

  # group by Date - extract year
  df10 <- empDF %>% exploratory::mutate_group(group_cols = c(`hired_date_year` = "hired_date"),group_funs = c("year"),salary_cumsum = cumsum(salary))
  expect_equal(head(df10)$hired_date_year[[1]], 1976)
  expect_equal(head(df10)$salary_cumsum[[1]], 19246)

  # group by Date - extract half year
  df11 <- empDF %>% exploratory::mutate_group(group_cols = c(`hired_date_halfyear` = "hired_date"),group_funs = c("halfyear"),salary_cumsum = cumsum(salary))
  expect_equal(head(df11)$hired_date_halfyear[[1]], 1)
  expect_equal(head(df11)$salary_cumsum[[1]], 2090)

  # group by Date - extract quarter
  df12 <- empDF %>% exploratory::mutate_group(group_cols = c(`hired_date_quarter` = "hired_date"),group_funs = c("quarter"),salary_cumsum = cumsum(salary))
  expect_equal(head(df12)$hired_date_quarter[[1]], 1)
  expect_equal(head(df12)$salary_cumsum[[1]], 2909)

  # group by Date - extract bi-month
  df13 <- empDF %>% exploratory::mutate_group(group_cols = c(`hired_date_bimon` = "hired_date"),group_funs = c("bimon"),salary_cumsum = cumsum(salary))
  expect_equal(head(df13)$hired_date_bimon[[2]], 1)
  expect_equal(head(df13)$salary_cumsum[[1]], 2909)

  # group by Date - extract month (number)
  df14 <- empDF %>% exploratory::mutate_group(group_cols = c(`hired_date_mon` = "hired_date"),group_funs = c("mon"),salary_cumsum = cumsum(salary))
  expect_equal(head(df14)$hired_date_mon[[2]], 1)
  expect_equal(head(df14)$salary_cumsum[[1]], 2426)

  # group by Date - extract month name (short)
  df15 <- empDF %>% exploratory::mutate_group(group_cols = c(`hired_date_monname` = "hired_date"),group_funs = c("monname"),salary_cumsum = cumsum(salary))
  expect_equal(as.character(head(df15)$hired_date_monname[[2]]), "Jan")
  expect_equal(head(df15)$salary_cumsum[[1]], 2426)

  # group by Date - extract month name (long)
  df16 <- empDF %>% exploratory::mutate_group(group_cols = c(`hired_date_monnamelong` = "hired_date"),group_funs = c("monnamelong"),salary_cumsum = cumsum(salary))
  expect_equal(as.character(head(df16)$hired_date_monnamelong[[2]]), "January")
  expect_equal(head(df16)$salary_cumsum[[1]], 2426)

  # group by Date - extract month name (year and month)
  df16b <- empDF %>% exploratory::mutate_group(group_cols = c(`hired_date_monthname_with_year` = "hired_date"),group_funs = c("monthname_with_year"),salary_cumsum = cumsum(salary))
  expect_equal(as.character(head(df16b)$hired_date_monthname_with_year[[2]]), "1977-06")
  expect_equal(head(df16b)$salary_cumsum[[1]], 19246)

  # group by Date - extract week
  df17 <- empDF %>% exploratory::mutate_group(group_cols = c(`hired_date_week` = "hired_date"),group_funs = c("week"),salary_cumsum = cumsum(salary))
  expect_equal(head(df17)$hired_date_week[[2]], 1)
  expect_equal(head(df17)$salary_cumsum[[3]], 31304)

  # group by Date - extract week (Starts from Sun)
  df18 <- empDF %>% exploratory::mutate_group(group_cols = c(`hired_date_epiweek` = "hired_date"),group_funs = c("epiweek"),salary_cumsum = cumsum(salary))
  expect_equal(head(df18)$hired_date_epiweek[[2]], 1)
  expect_equal(head(df18)$salary_cumsum[[3]], 21246)

  # group by Date - extract week (Starts from Mon)
  df19 <- empDF %>% exploratory::mutate_group(group_cols = c(`hired_date_isoweek` = "hired_date"),group_funs = c("isoweek"),salary_cumsum = cumsum(salary))
  expect_equal(head(df19)$hired_date_isoweek[[2]], 1)
  expect_equal(head(df19)$salary_cumsum[[3]], 31304)

  # group by Date - extract week of quarter
  df19 <- empDF %>% exploratory::mutate_group(group_cols = c(`hired_date_week_of_quarter` = "hired_date"),group_funs = c("week_of_quarter"),salary_cumsum = cumsum(salary))
  expect_equal(head(df19)$hired_date_week_of_quarter[[2]], 1)
  expect_equal(head(df19)$salary_cumsum[[3]], 15048)

  # group by Date - extract week of Month
  df20 <- empDF %>% exploratory::mutate_group(group_cols = c(`hired_date_week_of_month` = "hired_date"),group_funs = c("week_of_month"),salary_cumsum = cumsum(salary))
  expect_equal(head(df20)$hired_date_week_of_month[[2]], 1)
  expect_equal(head(df20)$salary_cumsum[[3]], 15505)

  # group by Date - extract day of year
  df21 <- empDF %>% exploratory::mutate_group(group_cols = c(`hired_date_dayofyera` = "hired_date"),group_funs = c("dayofyear"),salary_cumsum = cumsum(salary))
  expect_equal(head(df21)$hired_date_dayofyera[[2]], 1)
  expect_equal(head(df21)$salary_cumsum[[3]], 5484)

  # group by Date - extract day of quarter
  df22 <- empDF %>% exploratory::mutate_group(group_cols = c(`hired_date_dayofquarter` = "hired_date"),group_funs = c("dayofquarter"),salary_cumsum = cumsum(salary))
  expect_equal(head(df22)$hired_date_dayofquarter[[2]], 1)
  expect_equal(head(df22)$salary_cumsum[[3]], 14385)

  # group by Date - extract day of month
  df23 <- empDF %>% exploratory::mutate_group(group_cols = c(`hired_date_day` = "hired_date"),group_funs = c("day"),salary_cumsum = cumsum(salary))
  expect_equal(head(df23)$hired_date_day[[2]], 1)
  expect_equal(head(df23)$salary_cumsum[[3]], 22548)

  # group by Date - extract day of week
  df24 <- empDF %>% exploratory::mutate_group(group_cols = c(`hired_date_dayofweek` = "hired_date"),group_funs = c("dayofweek"),salary_cumsum = cumsum(salary))
  expect_equal(head(df24)$hired_date_dayofweek[[2]], 1)
  expect_equal(head(df24)$salary_cumsum[[3]], 8399)

  # group by Date - extract week day
  df25 <- empDF %>% exploratory::mutate_group(group_cols = c(`hired_date_wday` = "hired_date"),group_funs = c("wday"),salary_cumsum = cumsum(salary))
  expect_equal(as.character(head(df25)$hired_date_wday[[2]]), "Sun")
  expect_equal(head(df25)$salary_cumsum[[3]], 8399)

  # group by Date - extract week day
  df26 <- empDF %>% exploratory::mutate_group(group_cols = c(`hired_date_wdaylong` = "hired_date"),group_funs = c("wdaylong"),salary_cumsum = cumsum(salary))
  expect_equal(as.character(head(df26)$hired_date_wdaylong[[2]]), "Sunday")
  expect_equal(head(df26)$salary_cumsum[[3]], 8399)

  # group by Date - extract weekend
  df27 <- empDF %>% exploratory::mutate_group(group_cols = c(`hired_date_weekend` = "hired_date"),group_funs = c("weekend"),salary_cumsum = cumsum(salary))
  expect_equal(as.character(head(df27)$hired_date_weekend[[2]]), "Weekday")
  expect_equal(head(df27)$salary_cumsum[[3]], 13213)

  # group by job and sort by hired_date (asc)
  df28 <- empDF %>% exploratory::mutate_group(group_cols = c("job"), group_funs = c("none"), sort_cols = c("hired_date"), sort_funs = c("none"), salary_cumsum = cumsum(salary))
  expect_equal(df28$salary_cumsum[[1]], 10312) # since this is cumsum, row 1 is the first value for the group "health care"
  expect_equal(df28$salary_cumsum[[131]], 986268) # since this is cumsum, row 131 is the total value for the group "health care"

  # group by job and sort by hired_date (desc)
  df29 <- empDF %>% exploratory::mutate_group(group_cols = c("job"), group_funs = c("none"), sort_cols = c("hired_date"), sort_funs = c("desc"), salary_cumsum = cumsum(salary))
  expect_equal(df29$salary_cumsum[[1]], 5538) # since this is cumsum, row 1 is the first value for the group "health care" and this must be different from df28 case.
  expect_equal(df29$salary_cumsum[[131]], 986268) # since this is cumsum, row 131 is the max value for the group "health care" and this must be same as df28 case.

  # without group by, just sort by hired_date (desc)
  df30 <- empDF %>% exploratory::mutate_group(sort_cols = c("hired_date"), sort_funs = c("desc"), salary_cumsum = cumsum(salary))
  expect_equal(df30$salary_cumsum[[1]], 12504) # since this is cumsum without group by, row 1 value must be different from df29 case.
  expect_equal(df30$salary_cumsum[[131]], 681061) # since this is cumsum without group by, row 131 value must be different from df29 case.


})


test_that("recode and recode_factor", {
  empDF <- exploratory::read_parquet_file("https://www.dropbox.com/s/n0jkv4wu9dpb4se/Employee_Data_win_calc.parquet?dl=1")
  # partial recode and reverse order
  result <- empDF %>% mutate(job = exploratory::recode_factor(job, "リサーチサイエンティスト" = "A", "リサーチディレクター" = "B", "人事" = "C", reverse_order = TRUE))
  expect_equal(levels(result$job), c("製造ディレクター", "営業担当", "営業幹部", "C", "B", "A", "ラボ技術者", "マネージャー", "ヘルスケア担当"))
  # partial recode and keep order
  result2 <- empDF %>% mutate(job = exploratory::recode_factor(job, "リサーチサイエンティスト" = "A", "リサーチディレクター" = "B", "人事" = "C"))
  expect_equal(levels(result2$job), c("ヘルスケア担当", "マネージャー", "ラボ技術者", "A", "B", "C", "営業幹部", "営業担当","製造ディレクター"))
  # full recode and reverse order
  result3 <- empDF %>% mutate(job_level = exploratory::recode_factor(job_level, `1` = "A", `2` = "B", `3` = "C", `4` = "D", `5` = "E", reverse_order = TRUE))
  expect_equal(levels(result3$job_level), c("E", "D", "C", "B", "A"))
  # full recode and keep order
  result4 <- empDF %>% mutate(job_level = exploratory::recode_factor(job_level, `1` = "A", `2` = "B", `3` = "C", `4` = "D", `5` = "E"))
  expect_equal(levels(result4$job_level), c("A", "B", "C", "D", "E"))
  # type covert is set as TRUE so the result should be numeric 1,2,3 instead of character "1","2","3".
  result5 <- empDF %>% mutate(business_travel = exploratory::recode(business_travel, "たまに" = "1", "なし" = "2", "頻繁" = "3", type_convert = TRUE))
  expect_equal(exploratory:::get_unique_values(result5$business_travel, 100), c(1,2,3))
  # type covert is set as TRUE so the result should be Date 2023-01-01,2023-01-02,2023-01-03 instead of character "2023-01-01","2023-01-02", "2023-01-03".
  result6 <- empDF %>% mutate(business_travel = exploratory::recode(business_travel, "たまに" = "2023-01-01", "なし" = "2023-01-02", "頻繁" = "2023-01-03", type_convert = TRUE))
  expect_equal(exploratory:::get_unique_values(result6$business_travel, 100), c(lubridate::ymd("2023-01-01"),lubridate::ymd("2023-01-02"),lubridate::ymd("2023-01-03")))
  result7 <- empDF %>% mutate(job_level = exploratory::recode_factor(job_level, `1` = "10", `4` = "40"))
  expect_equal(levels(result7$job_level), c("10", "2", "3", "40", "5"))
  # type covert is set as TRUE so the result should be numeric 1,2,3 instead of character "1","2","3".
  result8 <- empDF %>% mutate(business_travel = exploratory::recode(business_travel, "たまに" = "1", "なし" = "2", .default = "0", type_convert = TRUE))
  expect_equal(exploratory:::get_unique_values(result8$business_travel, 100), c(0,1,2))
  result9 <- empDF %>% mutate(job_level = exploratory::recode_factor(job_level, `1` = "10", `4` = "40", .default = "99"))
  expect_equal(levels(result9$job_level), c("10", "99", "40"))


  # Test recoding "." to something else.
    # Test recoding "." to something else.
  test.df <- tibble(text=c("a", "b", ".", "."), value=1:4)
  test.df.result <- test.df %>% dplyr::mutate(text = exploratory::recode(text, `.` = "abc", a="xyz"))
  expect_equal(test.df.result$text, c("xyz", "b", "abc", "abc"))
  test.df.result <- test.df %>% dplyr::mutate(text = exploratory::recode_factor(text, `.` = "abc", a="xyz"))
  expect_equal(levels(test.df.result$text), c("abc", "xyz", "b"))
})

test_that("separate_japanese_address", {
  #  Address 1. Tokyo-To Shinjuku-ku Hyakunin-cho 1-2
  #  Address 2. Tokyo-To Shibuya-ku Shoto 2-3
  df <- data.frame(address2 = c("\u6771\u4EAC\u90FD\u65B0\u5BBF\u533A\u767E\u4EBA\u753A\u0031\u002D\u0032", "\u6771\u4EAC\u90FD\u6E0B\u8C37\u533A\u677E\u6FE4\u0032\u002D\u0033"))
  df2 <- exploratory::separate_japanese_address(df, address2, prefecture_col = "TODOFUKEN", city_col = "SHIKUCHOSON", street_col = "BANCHI")
  # The prefecure is Tokyo for both first line and second line.
  # The city for the first line is Shinjuku-ku and the city for the second line is Shibuya-ku
  # The street for the fist line is Hyakunin-cho 1-2 and the street for the second line is Shoto 2-3.
  check <- df2 %>% dplyr::select(-address2)
  answer <- tibble::tibble(TODOFUKEN = c("\u6771\u4EAC\u90FD", "\u6771\u4EAC\u90FD"),  SHIKUCHOSON = c("\u65B0\u5BBF\u533A", "\u6E0B\u8C37\u533A"), BANCHI = c("\u767E\u4EBA\u753A\u0031\u002D\u0032", "\u677E\u6FE4\u0032\u002D\u0033"))
  expect_true(isTRUE(all.equal(check, answer)), TRUE)
})

test_that("format_cut_output function", {
  x <- cut(c(1,2,3,4,5), breaks=3, dig.lab=10)
  formatted <- format_cut_output(x)
  expect_equal(formatted, factor(c("1.00 - 2.33","1.00 - 2.33","2.33 - 3.67","3.67 - 5.00","3.67 - 5.00")))

  # Negative test
  expect_equal(c(NA,NA,NA), format_cut_output(c(NA,NA,NA), decimal.digits=2))
  expect_equal(NULL, format_cut_output(c(), decimal.digits=2))
})

test_that("construct_new_labels function.", {
  new.labels <- c("a", "b", "c")
  base.labels <- c("1", "2", "3", "4", "5")
  expect_equal(exploratory:::construct_new_labels(base.labels, new.labels), c("a", "b", "c", "4", "5"))

  new.labels <- c("a", "b", "c", "d", "e", "f", "g")
  base.labels <- c("1", "2", "3", "4", "5")
  expect_equal(exploratory:::construct_new_labels(base.labels, new.labels), c("a", "b", "c", "d", "e"))

  new.labels <- c("a", "b", "c", "d", "e")
  base.labels <- c("1", "2", "3", "4", "5")
  expect_equal(exploratory:::construct_new_labels(base.labels, new.labels), c("a", "b", "c", "d", "e"))

  new.labels <- NA
  base.labels <- c("1", "2", "3", "4", "5")
  expect_equal(exploratory:::construct_new_labels(base.labels, new.labels), c("1", "2", "3", "4", "5"))

})

test_that("likert_sigma", {
  res <- likert_sigma(c(rep(1,13),rep(2,43),rep(3,21),rep(4,13),rep(5,10),rep(NA_integer_, 5)))
  # Values from the original 1932 paper - https://stats.stackexchange.com/questions/237828/how-did-likert-calculate-sigma-values-in-his-original-1932-paper
  expected <- c(rep(-1.6272701,13),rep(-0.4252946,43),rep(0.4322558,21),rep(0.9857673,13),rep(1.7549833,10),rep(NA_real_, 5))
  expect_equal(res, expected, tolerance=1e-7)

  # Case where input does not start with 1.
  res <- likert_sigma(c(5, 4, 3, 5))
  expected <- c(0.7978846, -0.3246628, -1.2711063, 0.7978846)
  expect_equal(res, expected, tolerance=1e-7)

  # Work with factor.
  res <- likert_sigma(factor(c("e","d","c","e"), levels=c("a","b","c","d","e")))
  expected <- c(0.7978846, -0.3246628, -1.2711063, 0.7978846)
  expect_equal(res, expected, tolerance=1e-7)

  # Work with logical
  res <- likert_sigma(c(TRUE, FALSE, TRUE))
  expected <- c(0.5453997, -1.0907993, 0.5453997)
  expect_equal(res, expected, tolerance=1e-7)

  # Test the case where ratios happens to not add up to exactly 1 due to precision.
  df <- exploratory::read_delim_file("https://www.dropbox.com/s/ok8m7cpa5cw2lw3/airline_2013_10_tricky.csv?dl=1" , ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE) %>%
    filter(`DAY OF MONTH`>3)
  res <- df %>% mutate(sigma=likert_sigma(`DAY OF MONTH`))
  expect_true(all(!is.na(res$sigma))) # The output should not have NAs caused by precision issue.
})

test_that("renamne_with", {
  df <- mtcars;
  colnames(df) <- c("Q1_abc", "Q2_ddd", "Q3_ert", "Q4_dggh", "Q5_eere", "Q6_bbb", "Q7_dadfa", "Q8_1234", "Q9_erere", "Q10", "Q10_ABC")
  df2 <- df %>% exploratory::rename_with(.fn = ~ stringr::word(.x, 1, sep = "\\s*\\_\\s*"))
  expect_equal(colnames(df2), c("Q1","Q2","Q3","Q4","Q5","Q6", "Q7","Q8","Q9","Q10...10","Q10...11"))

})

test_that("get_average_moving_range", {
  res <- exploratory::get_average_moving_range(c(NA,1,8,3,7,5,NA,9,1,3,2,NA))
  expect_equal(res, 4.125)
})

test_that("cumsum", {
  x <- c(NA, 5, 3, 6, NA, NA, 7, NA, 4, 9)
  expected <- c(NA, 5, 8, 14, NA, NA, 21, NA, 25, 34)
  expect_equal(exploratory::cumsum(x), expected)
  expected <- as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
  expect_equal(exploratory::cumsum(x, skip.na=FALSE), expected)
})

test_that("cummean", {
  x <- c(NA, 5, 3, 6, NA, NA, 7, NA, 4, 9)
  expected <- c(NA, 5, 4, 4.67, NA, NA, 5.25, NA, 5, 5.67)
  expect_equal(exploratory::cummean(x), expected, tolerance=1e-2)
  expected <- as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
  expect_equal(exploratory::cummean(x, skip.na=FALSE), expected)
})

test_that("cummin", {
  x <- c(NA, 5, 3, 6, NA, NA, 7, NA, 4, 9)
  expected <- c(NA, 5, 3, 3, NA, NA, 3, NA, 3, 3)
  expect_equal(exploratory::cummin(x), expected)
  expected <- as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
  expect_equal(exploratory::cummin(x, skip.na=FALSE), expected)
})

test_that("cummax", {
  x <- c(NA, 5, 3, 6, NA, NA, 7, NA, 4, 9)
  expected <- c(NA, 5, 5, 6, NA, NA, 7, NA, 7, 9)
  expect_equal(exploratory::cummax(x), expected)
  expected <- as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
  expect_equal(exploratory::cummax(x, skip.na=FALSE), expected)
})

test_that("cumprod", {
  x <- c(NA, 5, 3, 6, NA, NA, 7, NA, 4, 9)
  expected <- c(NA, 5, 15, 90, NA, NA, 630, NA, 2520, 22680)
  expect_equal(exploratory::cumprod(x), expected)
  expected <- as.numeric(c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
  expect_equal(exploratory::cumprod(x, skip.na=FALSE), expected)
})

test_that("cumall", {
  x <- c(NA, T, T, T, NA, NA, F, NA, T, F)
  expected <- c(NA, T, T, T, NA, NA, F, NA, F, F)
  expect_equal(exploratory::cumall(x), expected)
  expected <- as.logical(c(NA, NA, NA, NA, NA, NA, F, F, F, F))
  expect_equal(exploratory::cumall(x, skip.na=FALSE), expected)
})

test_that("cumany", {
  x <- c(NA, F, F, F, NA, NA, T, NA, F, T)
  expected <- c(NA, F, F, F, NA, NA, T, NA, T, T)
  expect_equal(exploratory::cumany(x), expected)
  expected <- as.logical(c(NA, NA, NA, NA, NA, NA, T, T, T, T))
  expect_equal(exploratory::cumany(x, skip.na=FALSE), expected)
})
