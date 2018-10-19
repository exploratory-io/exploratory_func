context("check util functions")

test_that("test pivot with empty data frame", {
  df <- data.frame()
  expect_error({
    pivot(df, row ~ col)
  }, "Input data frame is empty.")
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
  expect_equal(result[result[,1]=="entity 1" & result[,2]=="entity 3",3], 2)
  expect_equal(result[result[,1]=="entity 1" & result[,2]=="entity 4",3], 3)
  expect_equal(result[result[,1]=="entity 2" & result[,2]=="entity 3",3], 4)
  expect_equal(result[result[,1]=="entity 2" & result[,2]=="entity 4",3], 5)
  expect_equal(result[result[,1]=="entity 3" & result[,2]=="entity 4",3], 6)
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
  expect_true(is.character(ret[,1]))
  expect_true(is.character(ret[,2]))
  expect_true(!is.unsorted(ret[,1]))
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
    ret <- as_numeric_matrix_(test_df, columns = c("date1", "date2"))
    expect_true(all(is.na(ret)))
  })
})

test_that("as_numeric_matrix", {
  test_df <- data.frame(
    char1 = as.character(seq(10)),
    char2 = as.character(0 - seq(10))
  )
  ret <- as_numeric_matrix_(test_df, columns = c("char1", "char2"))
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
    as_numeric_matrix_(columns = c("date1", "date2"))
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

test_that("evaluate_select negative test", {
  test_df <- data.frame(
    col1 = as.character(seq(10)),
    col2 = as.character(0 - seq(10))
  )
  expect_error({
    evaluate_select(test_df, c("co1"))
  }, "undefined columns selected")
  expect_error({
    evaluate_select(test_df, c("dplyr::starts_with('something')"))
  }, "no column selected")
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

  expect_equal(ret[["text"]], c(rep(NA, 3), rep("b, b, b, b, b", 7)))
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

})

test_that("append_colnames", {
  test_df <- data.frame(col1 = seq(3), col2 = seq(3))

  ret <- append_colnames(test_df, "a.", ".b")

  expect_equal(colnames(ret), c("a.col1.b", "a.col2.b"))
})

test_that("test pivot", {
  test_df <- data.frame(
    group = c(rep(letters[1:2], each = 50),"a"),
    cat1 = c(letters[round(runif(100)*5)+1], NA),
    cat2 = c(letters[round(runif(100)*3)+1], "a"),
    cat3 = c(letters[round(runif(100)*3)+1], "a"),
    num3 = c(NA, seq(100))
  )

  pivoted <- pivot(test_df, cat1+cat3 ~ cat2)
  expect_true("cat1_cat3" %in% colnames(pivoted))

  pivoted_with_val <- pivot(test_df, cat1 ~ cat2 + cat3, value = num3, fun.aggregate=mean, fill = 0)
  expect_true(all(!is.na(pivoted_with_val)))

  pivoted_with_na <- pivot(test_df, cat1 ~ cat2 + cat3, value = num3, fun.aggregate=mean, na.rm = FALSE)
  expect_true(any(is.na(pivoted_with_na)))

})

test_that("test pivot with Date", {
  test_df <- data.frame(
    dt = rep(lubridate::today() + seq(3), each = 5),
    col = rep(seq(5), 3),
    val = seq(15)
  )

  pivoted <- pivot(test_df, dt ~ col, value = val)
  expect_true(pivoted$dt %>% inherits("Date"))
})

test_that("test pivot with POSIXct", {
  test_df <- data.frame(
    dt = rep(lubridate::now() + seq(3), each = 5),
    col = rep(seq(5), 3),
    val = seq(15)
  )

  pivoted <- pivot(test_df, dt ~ col, value = val)
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
    pivot(`cat 1`+ `cat-2` ~ `cat 3`)
  expect_true("group" %in% colnames(grouped_pivoted))
  expect_equal("group", grouped_by(grouped_pivoted))
})

test_that("test same_type for factor", {
  original <- factor(c("bb", "bb", "aa"), levels = c("bb", "aa"))

  to_replace <- c("aa", "aa", "bb")
  ret <- same_type(to_replace, original)
  expect_equal(ret, factor(c("aa", "aa", "bb"), levels = c("bb", "aa")))

  to_replace <- factor(c("aa", "aa", "bb"), levels = c("aa", "bb"))
  ret <- same_type(to_replace, original)
  expect_equal(ret, factor(c("aa", "aa", "bb"), levels = c("bb", "aa")))
})

test_that("test fill_mat_NA", {
  test_mat <- matrix(seq(10), nrow = 2)
  indice <- c(2, 5)
  ret <- fill_mat_NA(indice, test_mat)
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

test_that("test %in_or_all%", {
  ret <- c(1,2,3) %in_or_all% c(1,2)
  expect_equal(ret, c(TRUE, TRUE, FALSE))
  ret <- c(1,2,3) %in_or_all% NULL
  expect_equal(ret, c(TRUE, TRUE, TRUE))
  ret <- c(1,2,3) %in_or_all% c()
  expect_equal(ret, c(TRUE, TRUE, TRUE))
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

test_that("test sameple_rows", {
  df <-data.frame(x=c(1,NA,3),y=c(2,3,NA)) 
  df <-setNames(df,c("x 1", "col 2"))
  ret <- df %>% sample_rows(2)
  expect_equal(nrow(ret), 2)
  ret <- df %>% sample_rows(5)
  expect_equal(nrow(ret), 3)
})

test_that("test unnest_without_empty", {
  df <- data.frame(x=c(1,2,3))
  # create empty row in y
  df$y <- lapply(c(1,0,2),function(x){rep(1,x)})
  # TODO: regular unnest returns the same result as unnest_without_empty.
  # not sure what was the case unnest_without_empty was required at this point.
  # maybe we don't need unnest_without_empty anymore??
  ret <- df %>% unnest_without_empty(y)
  expect_equal(ret, data.frame(x=c(1,3,3),y=c(1,1,1)))

  # create empty row in y
  df$y <- lapply(c(1,0,2),function(x){data.frame(z=rep(1,x), w=rep(2,x))})
  # TODO: regular unnest returns the same result as unnest_without_empty.
  # not sure what was the case unnest_without_empty was required at this point.
  # maybe we don't need unnest_without_empty anymore??
  ret <- df %>% unnest_without_empty(y)
  expect_equal(ret, data.frame(x=c(1,3,3),z=c(1,1,1),w=c(2,2,2)))
})

test_that("r_squared", {
  res <- r_squared(c(1,2,3,4,5), c(3,4,2,4,7))
  expect_equal(res, -0.3, tolerance = 0.001)
})

test_that("excel_numeric_to_date", {
  res <- exploratory::excel_numeric_to_date(50000L) # test integer input
  expect_equal(res, as.Date("2036-11-21"))
})
