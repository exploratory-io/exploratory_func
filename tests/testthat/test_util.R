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

test_that("case_when", {
  test_df <- data.frame(
    salary = c(1,2,3),
    marital_status = c("single","divoerced","married")
  )
  df <- test_df %>% dplyr::mutate(
    calc1 = exploratory::case_when(
      marital_status == "single" ~ as.character(salary *2),
      TRUE ~ as.character(NA),
      type_convert = TRUE
    )
  )
  expect_true(class(df$calc1) == "numeric")
  df <- test_df %>% dplyr::mutate(
    calc1 = exploratory::case_when(
      marital_status == "single" ~ as.character(salary *2),
      TRUE ~ as.character(NA)
    )
  )
  expect_true(class(df$calc1) == "character")
})

test_that("pivot_longer", {
  test_df <- data.frame(
    country = c("Japan", "US", "UK"),
    `2024-01-01` = c(1,2,3),
    `2024-02-01` = c(2,3,4),
    `2024-03-01` = c(3,4,5)
  )
  colnames(test_df) <- c("country", "2024-01-01", "2024-02-01", "2024-03-01")
  df <- test_df %>% exploratory::pivot_longer(
    cols = c(`2024-01-01`, `2024-02-01`, `2024-03-01`),
    values_to = 'value',
    names_to = c("date"),
    values_drop_na = TRUE,
    names_repair = 'unique',
    type_convert = TRUE)
  expect_true(class(df$date) == "Date")
  df <- test_df %>% exploratory::pivot_longer(
    cols = c(`2024-01-01`, `2024-02-01`, `2024-03-01`),
    values_to = 'value',
    names_to = c("date"),
    values_drop_na = TRUE,
    names_repair = 'unique')
  expect_true(class(df$date) == "character")
});


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

test_that("test pivot with group_by and same group column as row column", {
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
    pivot(row_cols=c("group"), col_cols=c("cat 3"))
  expect_true("group" %in% colnames(grouped_pivoted))
  expect_equal(0, length(grouped_by(grouped_pivoted)))
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
  # Test with numeric single value
  df <- data.frame(x = c(1, 2, 3))
  ret <- df %>% dplyr::filter(x %equal_or_all% 2)
  expect_equal(nrow(ret), 1)

  # Test with NULL (should return all rows)
  ret <- df %>% dplyr::filter(x %equal_or_all% NULL)
  expect_equal(nrow(ret), 3)

  # Test with empty string (should return all rows)
  ret <- df %>% dplyr::filter(x %equal_or_all% "")
  expect_equal(nrow(ret), 3)

  # Test with single Date value
  df <- data.frame(x = c(lubridate::ymd("2024-01-01"),
                         lubridate::ymd("2024-01-02"),
                         lubridate::ymd("2024-01-03")))
  ret <- df %>% dplyr::filter(x %equal_or_all% lubridate::ymd("2024-01-02"))
  expect_equal(nrow(ret), 1)

  # Test with NULL for Date (should return all rows)
  ret <- df %>% dplyr::filter(x %equal_or_all% NULL)
  expect_equal(nrow(ret), 3)

  # Test with multiple values (fallback to %in%)
  ret <- df %>% dplyr::filter(x %equal_or_all% c(lubridate::ymd("2024-01-01"),
                                                 lubridate::ymd("2024-01-03")))
  expect_equal(nrow(ret), 2)
})

test_that("test %not_equal_or_all%", {
  # Test with numeric single value
  df <- data.frame(x = c(1, 2, 3))
  ret <- df %>% dplyr::filter(x %not_equal_or_all% 2)
  expect_equal(nrow(ret), 2)

  # Test with NULL (should return all rows)
  ret <- df %>% dplyr::filter(x %not_equal_or_all% NULL)
  expect_equal(nrow(ret), 3)

  # Test with empty string (should return all rows)
  ret <- df %>% dplyr::filter(x %not_equal_or_all% "")
  expect_equal(nrow(ret), 3)

  # Test with single Date value
  df <- data.frame(x = c(lubridate::ymd("2024-01-01"),
                         lubridate::ymd("2024-01-02"),
                         lubridate::ymd("2024-01-03")))
  ret <- df %>% dplyr::filter(x %not_equal_or_all% lubridate::ymd("2024-01-02"))
  expect_equal(nrow(ret), 2)

  # Test with NULL for Date (should return all rows)
  ret <- df %>% dplyr::filter(x %not_equal_or_all% NULL)
  expect_equal(nrow(ret), 3)

  # Test with multiple values (fallback to %nin%)
  ret <- df %>% dplyr::filter(x %not_equal_or_all% c(lubridate::ymd("2024-01-01"),
                                                     lubridate::ymd("2024-01-03")))
  expect_equal(nrow(ret), 1)
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
  df <- data.frame(x = c(lubridate::ymd("2024-01-01"), lubridate::ymd("2024-01-02"), lubridate::ymd("2024-01-03")))
  ret <- df %>% dplyr::filter(x %greater_or_all% lubridate::ymd("2024-01-02"))
  expect_equal(nrow(ret),1)
  ret <- df %>% dplyr::filter(x %greater_or_all% NULL)
  expect_equal(nrow(ret),3)
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
  df <- data.frame(x = c(lubridate::ymd("2024-01-01"), lubridate::ymd("2024-01-02"), lubridate::ymd("2024-01-03")))
  ret <- df %>% dplyr::filter(x %greater_or_equal_or_all% lubridate::ymd("2024-01-02"))
  expect_equal(nrow(ret),2)
  ret <- df %>% dplyr::filter(x %greater_or_equal_or_all% NULL)
  expect_equal(nrow(ret),3)
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
  df <- data.frame(x = c(lubridate::ymd("2024-01-01"), lubridate::ymd("2024-01-02"), lubridate::ymd("2024-01-03")))
  ret <- df %>% dplyr::filter(x %less_or_all% lubridate::ymd("2024-01-02"))
  expect_equal(nrow(ret),1)
  ret <- df %>% dplyr::filter(x %less_or_all% NULL)
  expect_equal(nrow(ret),3)
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
  df <- data.frame(x = c(lubridate::ymd("2024-01-01"), lubridate::ymd("2024-01-02"), lubridate::ymd("2024-01-03")))
  ret <- df %>% dplyr::filter(x %less_or_equal_or_all% lubridate::ymd("2024-01-02"))
  expect_equal(nrow(ret),2)
  ret <- df %>% dplyr::filter(x %less_or_equal_or_all% NULL)
  expect_equal(nrow(ret),3)
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





























test_that("summarize_row", {
 df <- airquality %>% mutate(total = summarize_row(across(where(is.numeric)), mean, na.rm=TRUE))
 expect_equal(colnames(df), c("Ozone", "Solar.R", "Wind", "Temp", "Month", "Day", "total"))
 # Test the default function (mean).
 df <- airquality %>% mutate(total = summarize_row(across(where(is.numeric))))
 expect_equal(colnames(df), c("Ozone", "Solar.R", "Wind", "Temp", "Month", "Day", "total"))
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

test_that("calc_confint_mean", {
  # confint_radius is the base implementation.
  v <- 1:100
  expect_equal(exploratory::confint_radius(v), exploratory::calc_confint_mean(sd(v), length(v)))
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

  # NA handling fix.
  t <- as.Date(c('2024-02-11', '2024-02-18', '2024-02-25', '2024-03-03', '2024-03-10', '2024-03-17', '2024-03-24', '2024-03-31', '2024-04-07', '2024-04-14', '2024-04-21', '2024-04-28', '2024-05-05', '2024-05-12'))
  y <- c(267, 1628, 1829, 1742, 2209, 1773, 2124, 2070, 2814, 2402, 2359, 1346, 1905, 943)
  res <- ts_lag(t, y, unit="month", n=1)
  # Make sure the 8th value is 1829, not 943.
  expect_equal(res, c(NA, NA, NA, NA, NA, 267, 1628, 1829, 1742, 2209, 1773, 2124, 2070, 2814))
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

test_that("between", {
  x <- c(1,2,3,4,5)
  expected <- c(T,T,T,F,F)
  actual <- exploratory::between(x, 1,3)
  expect_equal(actual, expected)
  expected2 <- c(T,T,T,T,T)
  actual2 <- exploratory::between(x, NULL, NULL)
  expect_equal(actual2, expected2)
  x_date <- c(lubridate::ymd("2024-01-01"),
              lubridate::ymd("2024-01-02"),
              lubridate::ymd("2024-01-03"),
              lubridate::ymd("2024-01-04"))
  expected3 <- c(T,T,T,F)
  actual3 <- exploratory::between(x_date, lubridate::ymd("2024-01-01"), lubridate::ymd("2024-01-03"))
  expect_equal(actual3, expected3)

})


test_that("to_same_type", {
  # factor
  orig_factor <- factor(c("a", "b", "c"), levels = c("a", "b", "c"))
  result <- exploratory:::to_same_type(c("a", "b"), orig_factor)
  expect_true(is.factor(result))
  expect_equal(levels(result), levels(orig_factor))

  # factor with values not in original levels
  result2 <- exploratory:::to_same_type(c("x", "y"), orig_factor)
  expect_true(is.factor(result2))

  # Date
  orig_date <- as.Date("2024-01-01")
  result <- exploratory:::to_same_type("2024-06-15", orig_date)
  expect_true(inherits(result, "Date"))
  expect_equal(result, as.Date("2024-06-15"))

  # POSIXct
  orig_posixct <- as.POSIXct("2024-01-01 12:00:00", tz = "UTC")
  result <- exploratory:::to_same_type("2024-06-15 08:30:00", orig_posixct)
  expect_true(inherits(result, "POSIXct"))

  # numeric
  orig_numeric <- 1.5
  result <- exploratory:::to_same_type("3.14", orig_numeric)
  expect_true(is.numeric(result))
  expect_equal(result, 3.14)

  # character
  orig_char <- "hello"
  result <- exploratory:::to_same_type(123, orig_char)
  expect_true(is.character(result))

  # logical
  orig_logical <- TRUE
  result <- exploratory:::to_same_type(1, orig_logical)
  expect_true(is.logical(result))
  expect_equal(result, TRUE)

  # integer
  orig_int <- 1L
  result <- exploratory:::to_same_type(2.5, orig_int)
  expect_true(is.integer(result))

  # NULL original
  result <- exploratory:::to_same_type(c(1, 2, 3), NULL)
  expect_equal(result, c(1, 2, 3))
})

test_that("excel_numeric_to_date", {
  skip_if_not_installed("janitor")
  # Excel serial date 1 in modern system is 1900-01-01
  result <- excel_numeric_to_date(1, date_system = "modern")
  expect_true(inherits(result, "Date"))
  expect_equal(result, as.Date("1900-01-01"))

  # A known date: 44927 = 2023-01-01 in modern system
  result2 <- excel_numeric_to_date(44927, date_system = "modern")
  expect_equal(result2, as.Date("2023-01-01"))

  # Integer input should also work (the wrapper applies as.numeric)
  result3 <- excel_numeric_to_date(44927L, date_system = "modern")
  expect_equal(result3, as.Date("2023-01-01"))
})


test_that("confint_radius", {
  set.seed(42)
  x <- c(10, 20, 30, 40, 50)
  n <- length(x)
  s <- sd(x)

  # level = 0.95
  expected_95 <- qt(0.975, df = n - 1) * s / sqrt(n)
  result_95 <- confint_radius(x, level = 0.95)
  expect_equal(result_95, expected_95)

  # level = 0.99
  expected_99 <- qt(0.995, df = n - 1) * s / sqrt(n)
  result_99 <- confint_radius(x, level = 0.99)
  expect_equal(result_99, expected_99)

  # higher confidence level should give larger radius
  expect_true(result_99 > result_95)

  # with NA values
  x_na <- c(10, 20, NA, 40, 50)
  result_na <- confint_radius(x_na, level = 0.95)
  n_na <- sum(!is.na(x_na))
  s_na <- sd(x_na, na.rm = TRUE)
  expected_na <- qt(0.975, df = n_na - 1) * s_na / sqrt(n_na)
  expect_equal(result_na, expected_na)
})

test_that("prop_confint_radius", {
  x <- c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE)
  n <- length(x)
  p <- sum(x) / n

  # level = 0.95
  expected_95 <- qnorm(0.975) * sqrt(p * (1 - p) / n)
  result_95 <- prop_confint_radius(x, level = 0.95)
  expect_equal(result_95, expected_95)

  # level = 0.99
  expected_99 <- qnorm(0.995) * sqrt(p * (1 - p) / n)
  result_99 <- prop_confint_radius(x, level = 0.99)
  expect_equal(result_99, expected_99)

  # higher confidence level should give larger radius
  expect_true(result_99 > result_95)
})

test_that("get_score", {
  actual <- c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)
  predicted <- c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE)

  result <- exploratory:::get_score(actual, predicted)

  # manual calculation
  tp <- sum(predicted & actual)       # 2

  fp <- sum(predicted & !actual)      # 1
  tn <- sum(!predicted & !actual)     # 3
  fn <- sum(!predicted & actual)      # 2

  expect_equal(result$true_positive, tp)
  expect_equal(result$false_positive, fp)
  expect_equal(result$true_negative, tn)
  expect_equal(result$false_negative, fn)
  expect_equal(result$test_size, length(actual))

  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  accuracy <- (tp + tn) / length(actual)
  f_score <- 2 * (precision * recall) / (precision + recall)
  specificity <- tn / (tn + fp)

  expect_equal(result$precision, precision)
  expect_equal(result$recall, recall)
  expect_equal(result$accuracy_rate, accuracy)
  expect_equal(result$misclassification_rate, 1 - accuracy)
  expect_equal(result$f_score, f_score)
  expect_equal(result$specificity, specificity)
})

test_that("get_optimized_score", {
  set.seed(123)
  actual_val <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)
  pred_prob <- c(0.9, 0.8, 0.7, 0.3, 0.2, 0.1, 0.6, 0.4, 0.85, 0.15)

  # optimize by f_score
  result <- exploratory:::get_optimized_score(actual_val, pred_prob, threshold = "f_score")
  expect_true(is.data.frame(result))
  expect_true("threshold" %in% names(result))
  expect_true("f_score" %in% names(result))
  expect_true(result$threshold >= 0 && result$threshold <= 1)

  # optimize by accuracy_rate
  result2 <- exploratory:::get_optimized_score(actual_val, pred_prob, threshold = "accuracy_rate")
  expect_true("accuracy_rate" %in% names(result2))

  # invalid threshold should error
  expect_error(exploratory:::get_optimized_score(actual_val, pred_prob, threshold = "invalid"))
})




test_that("unixtime_to_datetime", {
  # epoch 0 should be 1970-01-01 00:00:00 GMT
  result <- unixtime_to_datetime(0)
  expect_true(inherits(result, "POSIXct"))
  expect_equal(as.Date(result), as.Date("1970-01-01"))

  # known timestamp: 1672531200 = 2023-01-01 00:00:00 UTC
  result2 <- unixtime_to_datetime(1672531200)
  expect_equal(as.Date(result2), as.Date("2023-01-01"))
  expect_equal(lubridate::hour(result2), 0)

  # string input should also work (as.numeric is called internally)
  result3 <- unixtime_to_datetime("1672531200")
  expect_equal(result3, result2)
})

test_that("create_model_meta", {
  df <- data.frame(
    y = c(1, 2, 3, 4, 5),
    x1 = c(10, 20, 30, 40, 50),
    x2 = factor(c("a", "b", "a", "b", "a")),
    stringsAsFactors = FALSE
  )
  formula <- y ~ x1 + x2
  result <- exploratory:::create_model_meta(df, formula)

  expect_true(is.list(result))
  expect_true(!is.null(result$terms))
  expect_true(!is.null(result$types))
  expect_true(!is.null(result$flevels))

  # types should contain x1 and x2
  expect_equal(result$types[["x1"]], "double")
  expect_equal(result$types[["x2"]], "factor")

  # flevels should contain levels for x2
  expect_equal(result$flevels[["x2"]], c("a", "b"))

  # x1 should not be in flevels (not a factor)
  expect_true(is.null(result$flevels[["x1"]]))
})

test_that("add_response", {
  # create a simple glm model
  df <- data.frame(
    y = c(0, 0, 1, 1, 0, 1, 1, 0),
    x = c(1, 2, 3, 4, 5, 6, 7, 8)
  )
  model <- glm(y ~ x, data = df, family = binomial)

  # create data with .fitted column (as augment would produce)
  data_with_fitted <- data.frame(
    x = c(1, 5, 8),
    .fitted = c(-0.5, 0.0, 0.5)
  )
  result <- exploratory:::add_response(data_with_fitted, model)
  expect_true("predicted_response" %in% names(result))
  # predicted_response should be linkinv of .fitted (logistic function)
  expected_vals <- model$family$linkinv(data_with_fitted$.fitted)
  expect_equal(result$predicted_response, expected_vals)
  # logistic values should be between 0 and 1
  expect_true(all(result$predicted_response >= 0 & result$predicted_response <= 1))

  # custom label
  result2 <- exploratory:::add_response(data_with_fitted, model, response_label = "prob")
  expect_true("prob" %in% names(result2))

  # empty data frame
  empty_df <- data.frame(x = numeric(0), .fitted = numeric(0))
  result3 <- exploratory:::add_response(empty_df, model)
  expect_equal(nrow(result3), 0)
  expect_true("predicted_response" %in% names(result3))
})
