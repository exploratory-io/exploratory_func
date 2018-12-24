context("test system functions")

test_that("test clean_data_frame",{
  # create df with dupicated columns names and data frame type column
  df <- data.frame(a = 1:5, a = 2:6)
  colnames(df)<-c("a", "a")
  df$b <- data.frame(c = 3:7, d = 4:8)

  result <- clean_data_frame(df)
  # data frame column should be flattened out and the result data frame should have 4 columns.
  expect_equal(length(colnames(result)), 4)
  expect_equal(colnames(result), c("a", "a.1", "b.c", "b.d"))
})

test_that("test parse_html_tables",{
  result <- parse_html_tables('https://www.cbinsights.com/research-unicorn-companies')
  expect_equal(length(result), 1)
})

test_that("test parse_html_tables with japanese euc-jp table",{
  result <- parse_html_tables('http://download.exploratory.io/test/table_eucjp.html', 'EUC-JP')
  expect_equal(length(result), 1)
})

test_that("test parse_html_tables with japanese shift_jis table",{
  result <- parse_html_tables('http://download.exploratory.io/test/table_sjis.html', 'SHIFT_JIS')
  expect_equal(length(result), 1)
})


test_that("test scrape_html_table",{
  result <- scrape_html_table('https://www.cbinsights.com/research-unicorn-companies', 1, TRUE)
  expect_equal(ncol(result), 6)
  # may change if the web page is updated
  # seems it changes quite often, excluding this check.
  #expect_equal(nrow(result), 166)
})


test_that("test scrape_html_table with japanese euc-jp table",{
  result <- scrape_html_table('http://download.exploratory.io/test/table_eucjp.html', 1, TRUE, 'EUC-JP')
  expect_equal(ncol(result), 2)
  expect_equal(nrow(result), 3)
})

test_that("test scrape_html_table with japanese shift_jis table",{
  result <- scrape_html_table('http://download.exploratory.io/test/table_sjis.html', 1, TRUE, 'SHIFT_JIS')
  expect_equal(ncol(result), 2)
  expect_equal(nrow(result), 3)
})

test_that("test source check conflict case", {
  # this fails in devtools::check() because it can't find the pass because ../../R/model_builder.R can't be found
  # but this works in devtools::test(), so this needs condition to check the existance of the file.
  if(file.exists("../../R/model_builder.R")){
    filenames <- c("../../R/model_builder.R", "../../R/don't_exist.R")

    # suppress file doesn't exist warning
    suppressWarnings({ret <- checkSourceConflict(filenames)})
    expect_true(!is.null(ret[[filenames[[1]]]]$names))
    expect_true(is.null(ret[[filenames[[2]]]]$names))

    expect_true(is.null(ret[[filenames[[1]]]]$error))
    expect_true(!is.null(ret[[filenames[[2]]]]$error))
  } else {
    testthat::skip("../../R/model_builder.R doesn't exist")
  }
})

test_that("test statecode",{

  abbs <- c("NY", "CA", "IL", "DC", "DC")
  num_codes <- c("36", "06", "17", "11", "11")
  names <- c("New York","California","Illinois","District of Columbia","District of Columbia")
  namesWithDifferentCases <- c("new york","califorNIA","ILLINOIS", "districtOf columbia","washington D.C.")
  divisions <- c("Middle Atlantic","Pacific", "East North Central", "South Atlantic", "South Atlantic")
  regions <- c("Northeast","West","North Central", "South", "South")

  expect_equal(names, statecode(abbs, "name"))
  expect_equal(divisions, statecode(abbs, "division"))
  expect_equal(regions, statecode(abbs, "region"))
  expect_equal(abbs, statecode(names, "alpha_code"))
  expect_equal(num_codes, statecode(names, "num_code"))
  # with different cases
  expect_equal(abbs, statecode(namesWithDifferentCases, "alpha_code"))
  # format test
  expect_equal(names, statecode(namesWithDifferentCases, "name"))
})

test_that("test select_columns",{
  df <- data.frame(year=c(2014, 2015, 2016), sales=c(400, 500, 600), profit=c(200, 200, 300))
  # it selects only year and profit
  df1 <- df %>% exploratory::select_columns('year2', 'year1', 'year', 'profit', 'sales1')
  expect_equal(ncol(df1), 2)
  expect_equal(colnames(df1), c('year', 'profit'))

  df2 <- df %>% exploratory::select_columns('year')
  expect_true(is.data.frame(df2))
})

test_that("test select_columns with one column", {
  df <- structure(
    list(
      id = c("cus_AFEeV9EMHRXGeS", "cus_AA00SSatqya7uv", "cus_ARStNcs5xADH7a"),
      object = c("customer", "customer", "customer")
    ),
    .Names = c("id", "object"), class = "data.frame", row.names = c(443L, 609L, 131L)
  )

  col <- exploratory::select_columns(exploratory::clean_data_frame(df), "id") %>% colnames()
  expect_equal(col, "id")
})

test_that("test select_columns with exclude option",{
  df <- data.frame(year=c(2014, 2015, 2016), sales=c(400, 500, 600), profit=c(200, 200, 300))
  # It selects only sales. year and profit will be EXCLUDED
  df1 <- df %>% exploratory::select_columns('year2', 'year1', 'year', 'profit', 'sales1', exclude=TRUE)
  expect_equal(ncol(df1), 1)
  expect_equal(colnames(df1), c('sales'))
  expect_true(is.data.frame(df1))

})

test_that("countycode", {
  ret1 <- countycode(c("California", "CA"),c("San Francisco", "San Francisco"))
  expect_equal(ret1, c("06075", "06075"))
  ret2 <- countycode(c("MD", "MD", "MD"),c("Baltimore", "Baltimore City", "City of Baltimore"))
  expect_equal(ret2, c("24005", "24510", "24510"))
})

test_that("js_glue_transformer", {
  exploratory_env <- new.env()
  exploratory_env$v <- c(T,F,NA)
  res <- glue_exploratory("@{v}", .transformer=js_glue_transformer)
  expect_equal(as.character(res), "true, false, null")
  exploratory_env$v <- 1
  res <- glue_exploratory("{a: {x: @{v}}}", .transformer=js_glue_transformer)
  expect_equal(as.character(res), "{a: {x: 1}}")
})

test_that("odbc_glue_transformer", {
  exploratory_env <- new.env()
  exploratory_env$v <- c(1,2,3)
  res <- glue_exploratory("@{ v }", .transformer=odbc_glue_transformer)
  expect_equal(as.character(res), "1, 2, 3")
  exploratory_env$v <- c("a","b","c")
  res <- glue_exploratory("@{ `v` }", .transformer=odbc_glue_transformer)
  expect_equal(as.character(res), "'a', 'b', 'c'") # Not sure if this behavior works for all types of databases.
})

test_that("bigquery_glue_transformer", {
  exploratory_env <- new.env()
  exploratory_env$v <- c(1,2,3)
  res <- glue_exploratory("@{ v }", .transformer=bigquery_glue_transformer)
  expect_equal(as.character(res), "1, 2, 3")
  exploratory_env$v <- c("a","b","c")
  res <- glue_exploratory("@{ `v` }", .transformer=odbc_glue_transformer)
  expect_equal(as.character(res), "'a', 'b', 'c'") # Not sure if this behavior works for all types of databases.
})
