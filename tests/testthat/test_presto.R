context("test presto helpers")

test_that("splitPrestoStatements: empty / null / whitespace", {
  expect_equal(splitPrestoStatements(NULL), character(0))
  expect_equal(splitPrestoStatements(""), character(0))
  expect_equal(splitPrestoStatements("   \n  "), character(0))
  expect_equal(splitPrestoStatements(";"), character(0))
  expect_equal(splitPrestoStatements(";;;"), character(0))
})

test_that("splitPrestoStatements: single statement with and without trailing semicolon", {
  expect_equal(splitPrestoStatements("SELECT 1"), c("SELECT 1"))
  expect_equal(splitPrestoStatements("SELECT 1;"), c("SELECT 1"))
  expect_equal(splitPrestoStatements("  SELECT 1  ;  "), c("SELECT 1"))
})

test_that("splitPrestoStatements: multiple statements", {
  expect_equal(
    splitPrestoStatements("DROP TABLE foo; CREATE TABLE foo AS SELECT 1; SELECT * FROM foo"),
    c("DROP TABLE foo", "CREATE TABLE foo AS SELECT 1", "SELECT * FROM foo")
  )
  expect_equal(
    splitPrestoStatements("SELECT 1;\nSELECT 2;\nSELECT 3;"),
    c("SELECT 1", "SELECT 2", "SELECT 3")
  )
})

test_that("splitPrestoStatements: ; inside single-quoted literal does not split", {
  expect_equal(splitPrestoStatements("SELECT 'a;b;c'"), c("SELECT 'a;b;c'"))
  expect_equal(
    splitPrestoStatements("SELECT 'a;b'; SELECT 2"),
    c("SELECT 'a;b'", "SELECT 2")
  )
  # escaped quote ('') inside literal
  expect_equal(
    splitPrestoStatements("SELECT 'it''s; ok'; SELECT 2"),
    c("SELECT 'it''s; ok'", "SELECT 2")
  )
})

test_that("splitPrestoStatements: ; inside double-quoted identifier does not split", {
  expect_equal(
    splitPrestoStatements("SELECT \"a;b\" FROM t; SELECT 2"),
    c("SELECT \"a;b\" FROM t", "SELECT 2")
  )
})

test_that("splitPrestoStatements: ; inside line comment does not split", {
  expect_equal(
    splitPrestoStatements("SELECT 1 -- comment ; not a split\n; SELECT 2"),
    c("SELECT 1 -- comment ; not a split", "SELECT 2")
  )
})

test_that("splitPrestoStatements: ; inside block comment does not split", {
  expect_equal(
    splitPrestoStatements("SELECT 1 /* a;b;c */; SELECT 2"),
    c("SELECT 1 /* a;b;c */", "SELECT 2")
  )
})

test_that("prestoStatementReturnsRows: positive cases", {
  expect_true(prestoStatementReturnsRows("SELECT 1"))
  expect_true(prestoStatementReturnsRows("  SELECT 1"))
  expect_true(prestoStatementReturnsRows("WITH t AS (SELECT 1) SELECT * FROM t"))
  expect_true(prestoStatementReturnsRows("show schemas"))
  expect_true(prestoStatementReturnsRows("DESCRIBE foo"))
  expect_true(prestoStatementReturnsRows("DESC foo"))
  expect_true(prestoStatementReturnsRows("EXPLAIN SELECT 1"))
  expect_true(prestoStatementReturnsRows("VALUES (1), (2)"))
  expect_true(prestoStatementReturnsRows("TABLE foo"))
  expect_true(prestoStatementReturnsRows("(SELECT 1)"))
  expect_true(prestoStatementReturnsRows("-- leading comment\nSELECT 1"))
  expect_true(prestoStatementReturnsRows("/* block */ SELECT 1"))
  expect_true(prestoStatementReturnsRows("/* multi\nline\nblock */ SELECT 1"))
  expect_true(prestoStatementReturnsRows("  /* a */ -- b\n /* c */ \n  SELECT 1"))
})

test_that("prestoStatementReturnsRows: negative cases", {
  expect_false(prestoStatementReturnsRows("DROP TABLE foo"))
  expect_false(prestoStatementReturnsRows("CREATE TABLE foo AS SELECT 1"))
  expect_false(prestoStatementReturnsRows("INSERT INTO foo VALUES (1)"))
  expect_false(prestoStatementReturnsRows("UPDATE foo SET x = 1"))
  expect_false(prestoStatementReturnsRows("DELETE FROM foo"))
  expect_false(prestoStatementReturnsRows("ALTER TABLE foo ADD COLUMN x INT"))
  expect_false(prestoStatementReturnsRows("USE catalog.schema"))
  expect_false(prestoStatementReturnsRows("CALL system.runtime.kill_query('q')"))
  expect_false(prestoStatementReturnsRows(""))
  expect_false(prestoStatementReturnsRows(NULL))
})
