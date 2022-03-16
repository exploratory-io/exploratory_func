context("test system functions")
test_that("test clean_data_frame",{
  # create df with dupicated columns names and data frame type column
  df <- data.frame(a = 1:5, a = 2:6)
  colnames(df)<-c("a", "a")
  df$b <- data.frame(c = 3:7, d = 4:8)
  result <- clean_data_frame(df)
  # data frame column should be flattened out and the result data frame should have 4 columns.
  expect_equal(length(colnames(result)), 4)
  colnames(result)
  expect_equal(colnames(result), c("a", "a.1", "b.c", "b.d"))

  df2 <- data.frame(a = 1:5)
  colnames(df2)<-c("country \\ year")
  result2 <- clean_data_frame(df2)
  expect_equal(colnames(result2), c("country  year"))

  # Make sure clean_data_frame drops row names.
  # Use mtcars for the test because it has row names.
  df3a <- mtcars
  df3a <- clean_data_frame(df3a)
  df3b <- mtcars
  row.names(df3b) <- NULL
  # Compare the row names between data frames.
  # One data frame is processed by clean_data_frame.
  # The other one is processed manually.
  expect_equal(row.names(df3a), row.names(df3b))
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

if (FALSE) { # Disabled for now since this test is susceptible to webpage change and unstable.
test_that("test scrape_html_table",{
  result <- scrape_html_table('https://www.cbinsights.com/research-unicorn-companies', 1, TRUE)
  expect_equal(ncol(result), 6)
  # may change if the web page is updated
  # seems it changes quite often, excluding this check.
  #expect_equal(nrow(result), 166)
})
}


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
  exploratory_env$.config <- new.env()

  exploratory_env$v <- c('a"',"b'","c")
  res <- exploratory:::glue_exploratory("@{ `v` }", .transformer=exploratory:::js_glue_transformer)
  expect_equal(as.character(res), '"a\\\"", "b\'", "c"') # default quote case.

  res <- exploratory:::glue_exploratory("@{`v`, quote=''}", .transformer=exploratory:::js_glue_transformer)
  expect_equal(as.character(res), "a\", b', c") # No quote case.

  exploratory_env$.config$v <- new.env()
  exploratory_env$.config$v$quote <- "" # Made the default no quote.
  res <- exploratory:::glue_exploratory("@{`v`}", .transformer=exploratory:::js_glue_transformer)
  expect_equal(as.character(res), "a\", b', c") # No quote result

  rm("v", envir=exploratory_env$.config) # clear config.

  exploratory_env$v <- c(T,F,NA)
  res <- exploratory:::glue_exploratory("@{v}", .transformer=exploratory:::js_glue_transformer)
  expect_equal(as.character(res), "true, false, null")

  # Empty vector case.
  exploratory_env$v <- as.character(c())
  res <- exploratory:::glue_exploratory("@{v}", .transformer=exploratory:::js_glue_transformer)
  expect_equal(as.character(res), "")

  exploratory_env$v <- 1
  exploratory_env$w <- 2
  exploratory_env$x <- 1000000
  res <- exploratory:::glue_exploratory("{a: {x: @{v}}, b:@{w}, c:@{x}}", .transformer=exploratory:::js_glue_transformer)
  expect_equal(as.character(res), "{a: {x: 1}, b:2, c:1000000}")

  exploratory_env$stock_symbols <- c("AAPL", "GOOG")
  res <- exploratory:::glue_exploratory("{stock:{$in:[@{stock_symbols}]}}", .transformer=exploratory:::js_glue_transformer)
  expect_equal(as.character(res), "{stock:{$in:[\"AAPL\", \"GOOG\"]}}")

  exploratory_env$stock_symbols <- c()
  res <- exploratory:::glue_exploratory("{stock:{$in:[@{stock_symbols}]}}", .transformer=exploratory:::js_glue_transformer)
  expect_equal(as.character(res), "{stock:{$in:[]}}", "message")

  exploratory_env$number_range <- c(-10, 20)
  res <- exploratory:::glue_exploratory("{salary:{$gte:@{number_range[1]}, $lt:@{number_range[2]}}}", .transformer=exploratory:::js_glue_transformer)
  expect_equal(as.character(res), "{salary:{$gte:-10, $lt:20}}")
})

test_that("sql_glue_transformer", {
  exploratory_env <- new.env()
  exploratory_env$.config <- new.env()

  exploratory_env$v <- c(1,2,3)
  res <- exploratory:::glue_exploratory("@{ v }", .transformer=exploratory:::sql_glue_transformer)
  expect_equal(as.character(res), "1, 2, 3")

  exploratory_env$v <- c('a"',"b'","c")
  res <- exploratory:::glue_exploratory("@{ `v` }", .transformer=exploratory:::sql_glue_transformer)
  expect_equal(as.character(res), "'a\"', 'b''', 'c'") # Not sure if this behavior works for all types of databases.

  exploratory_env$v <- c("a","b","c")
  res <- exploratory:::glue_exploratory("@{`v`, quote=FALSE}", .transformer=exploratory:::sql_glue_transformer)
  expect_equal(as.character(res), "a, b, c") # No quote case.

  exploratory_env$v <- c('a"',"b'","c")
  res <- exploratory:::glue_exploratory("@{ `v`, quote=\"\", escape=\"'\" }", .transformer=exploratory:::sql_glue_transformer)
  expect_equal(as.character(res), "a\", b'', c") # No quote but with escape.

  exploratory_env$dept_names <- c("Sales","HR","CEO's secretary", "Data Science\\Statistics")
  exploratory_env$empid_above <- 1100
  res <- exploratory:::glue_exploratory("select * from emp where deptname in (@{dept_names}) and empid > @{empid_above}", .transformer=exploratory:::sql_glue_transformer)
  expect_equal(as.character(res), "select * from emp where deptname in ('Sales', 'HR', 'CEO''s secretary', 'Data Science\\Statistics') and empid > 1100")

  exploratory_env$dept_names <- as.character(c())
  res <- exploratory:::glue_exploratory("select * from emp where deptname in (@{dept_names}) and empid > @{empid_above}", .transformer=exploratory:::sql_glue_transformer)
  expect_equal(as.character(res), "select * from emp where deptname in (NULL) and empid > 1100")

  exploratory_env$number_limit <- 1000000
  res <- exploratory:::glue_exploratory("select top @{number_limit} * from emp", .transformer=exploratory:::sql_glue_transformer)
  expect_equal(as.character(res), "select top 1000000 * from emp")

  exploratory_env$number_range <- c(-10, 20)
  res <- exploratory:::glue_exploratory("select * from emp where salary between @{number_range[1]} and @{number_range[2]}", .transformer=exploratory:::sql_glue_transformer)
  expect_equal(as.character(res), "select * from emp where salary between -10 and 20")
})

test_that("bigquery_glue_transformer", {
  exploratory_env <- new.env()
  exploratory_env$.config <- new.env()

  exploratory_env$v <- c(1,2,3)
  res <- exploratory:::glue_exploratory("@{ v }", .transformer=exploratory:::bigquery_glue_transformer)
  expect_equal(as.character(res), "1, 2, 3")

  exploratory_env$v <- c("a","b","c")
  res <- exploratory:::glue_exploratory("@{ `v` }", .transformer=exploratory:::bigquery_glue_transformer)
  expect_equal(as.character(res), "'a', 'b', 'c'") # Not sure if this behavior works for all types of databases.
  res <- exploratory:::glue_exploratory("@{ `v` , quote = FALSE }", .transformer=exploratory:::bigquery_glue_transformer)
  expect_equal(as.character(res), "a, b, c") # No quote case

  exploratory_env$dept_names <- c("Sales","HR","CEO's secretary", "Data Science\\Statistics")
  exploratory_env$empid_above <- 1100
  res <- exploratory:::glue_exploratory("select * from emp where deptname in (@{dept_names}) and empid > @{empid_above}", .transformer=exploratory:::bigquery_glue_transformer)
  expect_equal(as.character(res), "select * from emp where deptname in ('Sales', 'HR', 'CEO\\'s secretary', 'Data Science\\\\Statistics') and empid > 1100")

  exploratory_env$dept_names <- as.character(c())
  res <- exploratory:::glue_exploratory("select * from emp where deptname in (@{dept_names}) and empid > @{empid_above}", .transformer=exploratory:::bigquery_glue_transformer)
  expect_equal(as.character(res), "select * from emp where deptname in (NULL) and empid > 1100")

  exploratory_env$number_limit <- 1000000
  res <- exploratory:::glue_exploratory("select * from emp limit @{number_limit}", .transformer=exploratory:::bigquery_glue_transformer)
  expect_equal(as.character(res), "select * from emp limit 1000000")


})

test_that("glue_salesforce", {
  res <- exploratory:::glue_salesforce("${1+1}")
  expect_equal(as.character(res), "2")

  exploratory_env <- new.env()
  exploratory_env$.config <- new.env()
  exploratory_env$number_limit <- 1

  res <- exploratory:::glue_salesforce(exploratory:::glue_exploratory("${1+1 + @{number_limit}}", .transformer=exploratory:::salesforce_glue_transformer))
  expect_equal(as.character(res), "3")

})

test_that("prefecturecode", {

  df <- readRDS(url("https://www.dropbox.com/s/eygfwy9mo7xn9xb/prefecturecode_testdata.rds?raw=1"))

  res <- exploratory::prefecturecode(df$hiragana, output_type="name")
  expect_equal(FALSE, any(is.na(res)))

  res <- exploratory::prefecturecode(df$kanji.with.todofuken, output_type="name")
  expect_equal(FALSE, any(is.na(res)))
  # Test case for Hokkaido (Kanji), Tokyo (Kanji), and Osaka (Hiragana)
  df <- data.frame(a=c("\u5317\u6D77\u9053", "\u6771\u4eac", "", NA, "\u304a\u304a\u3055\u304b"))
  result <- exploratory::prefecturecode(df$a, output_type = "code")
  expect_equal(result,c("01", "13", NA, NA, "27"))


  res <- exploratory::prefecturecode(df$kanji, output_type="name")
  expect_equal(FALSE, any(is.na(res)))

  res <- exploratory::prefecturecode(df$romaji.wikipedia, output_type="name")
  expect_equal(FALSE, any(is.na(res)))

  res <- exploratory::prefecturecode(df$romaji.normalized, output_type="name")
  expect_equal(FALSE, any(is.na(res)))

  res <- exploratory::prefecturecode(df$romaji.test, output_type="name")
  expect_equal(FALSE, any(is.na(res)))

})

test_that("geocode_japan_prefecture", {
  df <- readRDS(url("https://www.dropbox.com/s/eygfwy9mo7xn9xb/prefecturecode_testdata.rds?raw=1"))

  res <- exploratory::geocode_japan_prefecture(df, "kanji")
  expect_equal(FALSE, any(is.na(res$longitude)))
  expect_equal(FALSE, any(is.na(res$latitude)))
})

test_that("city_code_japan", {
  # Data: tibble(
  #  x=c("Hokkaido", "Tokyo-to", "Kanagawa-ken", "Kanagawa-ken"), 
  #  y=c("Sapporo-shi Shiraishi-ku", "Inagi-shi", "Ashigarashimo-gun Hakone-machi", "Hakone-machi"))  
  # (In all Japanese Kanji chars).
  df <- tibble(
    x=c("\u5317\u6d77\u9053", "\u6771\u4eac\u90fd", "\u795e\u5948\u5ddd\u770c", "\u795e\u5948\u5ddd\u770c"), 
    y=c("\u672d\u5e4c\u5e02\u767d\u77f3\u533a", "\u7a32\u57ce\u5e02", "\u8db3\u67c4\u4e0b\u90e1\u7bb1\u6839\u753a", "\u7bb1\u6839\u753a"))  
  res <- exploratory::city_code_japan(df$x, df$y)
  expect_equal(FALSE, any(is.na(res)))
  expect_equal("01104", res[1])
  expect_equal("13225", res[2])
  # It should resolve the city code from the city name with "gun".
  expect_equal("14382", res[3])
  # It should resolve the city code from the city name without "gun".
  expect_equal("14382", res[4])

})

test_that("geocode_japan_city", {
  # Data: tibble(x=c("Hokkaido", "Tokyo-to"), y=c("Sapporo-shi Shiraishi-ku", "Inagi-shi"))  (In all Japanese Kanji chars).
  df <- tibble(x=c("\u5317\u6d77\u9053", "\u6771\u4eac\u90fd"), y=c("\u672d\u5e4c\u5e02\u767d\u77f3\u533a", "\u7a32\u57ce\u5e02"))  
  df$code <- exploratory::city_code_japan(df$x, df$y)
  res <- exploratory::geocode_japan_city(df, "code")
  expect_equal(FALSE, any(is.na(res$longitude)))
  expect_equal(FALSE, any(is.na(res$latitude)))
})

test_that("read_parquet_file", {
  df <- read_parquet_file("https://dl.dropbox.com/s/sjkgk9gj0vemq36/sample.parquet")
  expect_equal(TRUE, is.data.frame(df))
})

test_that("read_parquet_file should be to read the parquet file with an invalid UTF-8 encoding.", {
  # Make sure that the current arrow version (5.0) can read this parquet file.
  # arrow 3.0/4.0, cannot read this parquet file and throw an error
  # "Invalid UTF-8 payload" but it is fixed in 5.0.
  df <- read_parquet_file("https://dl.dropbox.com/s/9yp6yk1jjnd8dz0/invalid_utf8_payload_test.parquet")
  expect_equal(TRUE, is.data.frame(df))
})

test_that("test filter_cascade",{
  library(stringr)
  df <- readRDS(url("https://www.dropbox.com/s/p2vmd79ly1zugh9/airbnb_nyc_filter_7.rds?dl=1"))
  df <- df %>% filter_cascade(detect_outlier(reviews_per_month, "iqr") == "Normal", cut(reviews_per_month, breaks = 5, dig.lab = 10) %in% c("(0.21,0.31]"))
  expect_equal(nrow(df), 2661)
})
