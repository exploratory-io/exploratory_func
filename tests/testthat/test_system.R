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
  ret <- checkSourceConflict("../../R/model_builder.R")
  expect_true(any(ret %in% "build_data"))
})

