context("test presto helpers")

# ---------------------------------------------------------------------------
# splitPrestoStatements
# ---------------------------------------------------------------------------

test_that("splitPrestoStatements: empty / null / whitespace", {
  expect_equal(splitPrestoStatements(NULL), character(0))
  expect_equal(splitPrestoStatements(character(0)), character(0))
  expect_equal(splitPrestoStatements(""), character(0))
  expect_equal(splitPrestoStatements("   "), character(0))
  expect_equal(splitPrestoStatements("   \n  "), character(0))
  expect_equal(splitPrestoStatements("\t\r\n"), character(0))
  expect_equal(splitPrestoStatements(";"), character(0))
  expect_equal(splitPrestoStatements(";;;"), character(0))
  expect_equal(splitPrestoStatements("  ;  ;  "), character(0))
})

test_that("splitPrestoStatements: single statement with and without trailing semicolon", {
  expect_equal(splitPrestoStatements("SELECT 1"), c("SELECT 1"))
  expect_equal(splitPrestoStatements("SELECT 1;"), c("SELECT 1"))
  expect_equal(splitPrestoStatements("  SELECT 1  ;  "), c("SELECT 1"))
  expect_equal(splitPrestoStatements(";SELECT 1"), c("SELECT 1"))
  expect_equal(splitPrestoStatements(";;SELECT 1;;"), c("SELECT 1"))
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
  # empty statements between separators are dropped
  expect_equal(
    splitPrestoStatements("SELECT 1;;SELECT 2"),
    c("SELECT 1", "SELECT 2")
  )
  expect_equal(
    splitPrestoStatements("SELECT 1; ; \n ; SELECT 2"),
    c("SELECT 1", "SELECT 2")
  )
})

test_that("splitPrestoStatements: multi-line statements keep their internal newlines", {
  sql <- "SELECT a,\n       b\nFROM t;\nSELECT c\nFROM u"
  expect_equal(
    splitPrestoStatements(sql),
    c("SELECT a,\n       b\nFROM t", "SELECT c\nFROM u")
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
  # consecutive escaped quotes
  expect_equal(
    splitPrestoStatements("SELECT '''';SELECT 2"),
    c("SELECT ''''", "SELECT 2")
  )
  # comment markers inside a literal are inert
  expect_equal(
    splitPrestoStatements("SELECT '-- not a comment ; still literal'; SELECT 2"),
    c("SELECT '-- not a comment ; still literal'", "SELECT 2")
  )
  expect_equal(
    splitPrestoStatements("SELECT '/* not a comment ; */'; SELECT 2"),
    c("SELECT '/* not a comment ; */'", "SELECT 2")
  )
  # double-quote char inside a single-quoted literal is just data
  expect_equal(
    splitPrestoStatements("SELECT 'a\";b'; SELECT 2"),
    c("SELECT 'a\";b'", "SELECT 2")
  )
})

test_that("splitPrestoStatements: ; inside double-quoted identifier does not split", {
  expect_equal(
    splitPrestoStatements("SELECT \"a;b\" FROM t; SELECT 2"),
    c("SELECT \"a;b\" FROM t", "SELECT 2")
  )
  # escaped double-quote ("") inside identifier
  expect_equal(
    splitPrestoStatements("SELECT \"weird\"\";name\" FROM t; SELECT 2"),
    c("SELECT \"weird\"\";name\" FROM t", "SELECT 2")
  )
  # single-quote char inside a double-quoted identifier is just data
  expect_equal(
    splitPrestoStatements("SELECT \"a';b\" FROM t; SELECT 2"),
    c("SELECT \"a';b\" FROM t", "SELECT 2")
  )
})

test_that("splitPrestoStatements: ; inside line comment does not split", {
  expect_equal(
    splitPrestoStatements("SELECT 1 -- comment ; not a split\n; SELECT 2"),
    c("SELECT 1 -- comment ; not a split", "SELECT 2")
  )
  # line comment runs to end of input when no trailing newline
  expect_equal(
    splitPrestoStatements("SELECT 1 -- trailing ; comment"),
    c("SELECT 1 -- trailing ; comment")
  )
  # a statement consisting solely of a comment is preserved (non-empty after trim)
  expect_equal(
    splitPrestoStatements("SELECT 1;-- only a comment"),
    c("SELECT 1", "-- only a comment")
  )
})

test_that("splitPrestoStatements: ; inside block comment does not split", {
  expect_equal(
    splitPrestoStatements("SELECT 1 /* a;b;c */; SELECT 2"),
    c("SELECT 1 /* a;b;c */", "SELECT 2")
  )
  # multi-line block comment
  expect_equal(
    splitPrestoStatements("SELECT 1 /* line1 ;\n line2 ; */; SELECT 2"),
    c("SELECT 1 /* line1 ;\n line2 ; */", "SELECT 2")
  )
  # quote chars inside a block comment are inert
  expect_equal(
    splitPrestoStatements("SELECT 1 /* it's a \"trap\" ; */; SELECT 2"),
    c("SELECT 1 /* it's a \"trap\" ; */", "SELECT 2")
  )
  # block comments do NOT nest: the first */ closes the comment, so a ; after it splits
  expect_equal(
    splitPrestoStatements("SELECT 1 /* outer /* inner */ ; SELECT 2"),
    c("SELECT 1 /* outer /* inner */", "SELECT 2")
  )
})

test_that("splitPrestoStatements: leading comments before a statement are retained", {
  expect_equal(
    splitPrestoStatements("-- header\nSELECT 1"),
    c("-- header\nSELECT 1")
  )
  expect_equal(
    splitPrestoStatements("/* header */ SELECT 1; SELECT 2"),
    c("/* header */ SELECT 1", "SELECT 2")
  )
})

test_that("splitPrestoStatements: realistic multi-statement DDL + CTE script", {
  sql <- paste(
    "DROP TABLE IF EXISTS sandbox.tmp;",
    "CREATE TABLE sandbox.tmp AS",
    "WITH base AS (SELECT id, name FROM src WHERE name <> 'a;b')",
    "SELECT * FROM base;",
    "SELECT * FROM sandbox.tmp",
    sep = "\n"
  )
  expect_equal(
    splitPrestoStatements(sql),
    c(
      "DROP TABLE IF EXISTS sandbox.tmp",
      "CREATE TABLE sandbox.tmp AS\nWITH base AS (SELECT id, name FROM src WHERE name <> 'a;b')\nSELECT * FROM base",
      "SELECT * FROM sandbox.tmp"
    )
  )
})

test_that("splitPrestoStatements: INSERT with a string literal containing a semicolon", {
  expect_equal(
    splitPrestoStatements("INSERT INTO t VALUES ('x;y'); SELECT * FROM t"),
    c("INSERT INTO t VALUES ('x;y')", "SELECT * FROM t")
  )
})

# ---------------------------------------------------------------------------
# prestoStatementReturnsRows
# ---------------------------------------------------------------------------

test_that("prestoStatementReturnsRows: SELECT / WITH / VALUES / TABLE", {
  expect_true(prestoStatementReturnsRows("SELECT 1"))
  expect_true(prestoStatementReturnsRows("  SELECT 1"))
  expect_true(prestoStatementReturnsRows("\t\nSELECT 1"))
  expect_true(prestoStatementReturnsRows("select 1"))
  expect_true(prestoStatementReturnsRows("SeLeCt 1"))
  expect_true(prestoStatementReturnsRows("SELECT * FROM t WHERE x = 1 GROUP BY y ORDER BY z LIMIT 10"))
  expect_true(prestoStatementReturnsRows("SELECT a FROM t UNION ALL SELECT b FROM u"))
  expect_true(prestoStatementReturnsRows("WITH t AS (SELECT 1) SELECT * FROM t"))
  expect_true(prestoStatementReturnsRows("with a as (select 1), b as (select 2) select * from a join b on true"))
  expect_true(prestoStatementReturnsRows("VALUES (1), (2)"))
  expect_true(prestoStatementReturnsRows("values (1, 'a')"))
  expect_true(prestoStatementReturnsRows("TABLE foo"))
  expect_true(prestoStatementReturnsRows("TABLE catalog.schema.foo"))
})

test_that("prestoStatementReturnsRows: SHOW family", {
  expect_true(prestoStatementReturnsRows("SHOW SCHEMAS"))
  expect_true(prestoStatementReturnsRows("show schemas from catalog"))
  expect_true(prestoStatementReturnsRows("SHOW TABLES"))
  expect_true(prestoStatementReturnsRows("SHOW TABLES FROM sandbox"))
  expect_true(prestoStatementReturnsRows("SHOW COLUMNS FROM t"))
  expect_true(prestoStatementReturnsRows("SHOW CATALOGS"))
  expect_true(prestoStatementReturnsRows("SHOW FUNCTIONS"))
  expect_true(prestoStatementReturnsRows("SHOW SESSION"))
  expect_true(prestoStatementReturnsRows("SHOW STATS FOR t"))
  expect_true(prestoStatementReturnsRows("SHOW STATS FOR TABLE t"))
  expect_true(prestoStatementReturnsRows("SHOW CREATE TABLE t"))
  expect_true(prestoStatementReturnsRows("SHOW CREATE VIEW v"))
  expect_true(prestoStatementReturnsRows("SHOW CREATE SCHEMA s"))
  expect_true(prestoStatementReturnsRows("SHOW CREATE FUNCTION f"))
  expect_true(prestoStatementReturnsRows("SHOW CREATE MATERIALIZED VIEW mv"))
  expect_true(prestoStatementReturnsRows("SHOW GRANTS ON TABLE t"))
  expect_true(prestoStatementReturnsRows("SHOW ROLES"))
  expect_true(prestoStatementReturnsRows("SHOW ROLE GRANTS"))
})

test_that("prestoStatementReturnsRows: DESCRIBE / DESC / EXPLAIN", {
  expect_true(prestoStatementReturnsRows("DESCRIBE foo"))
  expect_true(prestoStatementReturnsRows("describe foo"))
  expect_true(prestoStatementReturnsRows("DESC foo"))
  expect_true(prestoStatementReturnsRows("DESCRIBE INPUT my_query"))
  expect_true(prestoStatementReturnsRows("DESCRIBE OUTPUT my_query"))
  expect_true(prestoStatementReturnsRows("EXPLAIN SELECT 1"))
  expect_true(prestoStatementReturnsRows("EXPLAIN ANALYZE SELECT 1"))
  expect_true(prestoStatementReturnsRows("EXPLAIN (TYPE LOGICAL) SELECT 1"))
})

test_that("prestoStatementReturnsRows: leading parentheses", {
  expect_true(prestoStatementReturnsRows("(SELECT 1)"))
  expect_true(prestoStatementReturnsRows("((SELECT 1))"))
  expect_true(prestoStatementReturnsRows("  ( ( SELECT 1 ) )"))
  expect_true(prestoStatementReturnsRows("(SELECT 1) UNION (SELECT 2)"))
})

test_that("prestoStatementReturnsRows: leading comments are stripped before classifying", {
  expect_true(prestoStatementReturnsRows("-- leading comment\nSELECT 1"))
  expect_true(prestoStatementReturnsRows("/* block */ SELECT 1"))
  expect_true(prestoStatementReturnsRows("/* block */SELECT 1"))
  expect_true(prestoStatementReturnsRows("/* multi\nline\nblock */ SELECT 1"))
  expect_true(prestoStatementReturnsRows("  /* a */ -- b\n /* c */ \n  SELECT 1"))
  expect_true(prestoStatementReturnsRows("-- c1\n-- c2\nSELECT 1"))
  expect_true(prestoStatementReturnsRows("/* c */ ( SELECT 1 )"))
})

test_that("prestoStatementReturnsRows: DDL is not row-returning", {
  expect_false(prestoStatementReturnsRows("CREATE TABLE foo (x INT)"))
  expect_false(prestoStatementReturnsRows("CREATE TABLE IF NOT EXISTS foo (x INT)"))
  expect_false(prestoStatementReturnsRows("CREATE TABLE foo AS SELECT 1"))
  expect_false(prestoStatementReturnsRows("CREATE VIEW v AS SELECT 1"))
  expect_false(prestoStatementReturnsRows("CREATE OR REPLACE VIEW v AS SELECT 1"))
  expect_false(prestoStatementReturnsRows("CREATE SCHEMA s"))
  expect_false(prestoStatementReturnsRows("CREATE MATERIALIZED VIEW mv AS SELECT 1"))
  expect_false(prestoStatementReturnsRows("CREATE FUNCTION f(x INT) RETURNS INT RETURN x + 1"))
  expect_false(prestoStatementReturnsRows("CREATE VECTOR INDEX ON t (c)"))
  expect_false(prestoStatementReturnsRows("CREATE ROLE admin"))
  expect_false(prestoStatementReturnsRows("DROP TABLE foo"))
  expect_false(prestoStatementReturnsRows("DROP TABLE IF EXISTS foo"))
  expect_false(prestoStatementReturnsRows("DROP VIEW v"))
  expect_false(prestoStatementReturnsRows("DROP SCHEMA s"))
  expect_false(prestoStatementReturnsRows("DROP MATERIALIZED VIEW mv"))
  expect_false(prestoStatementReturnsRows("DROP FUNCTION f"))
  expect_false(prestoStatementReturnsRows("DROP ROLE admin"))
  expect_false(prestoStatementReturnsRows("ALTER TABLE foo ADD COLUMN x INT"))
  expect_false(prestoStatementReturnsRows("ALTER TABLE foo RENAME TO bar"))
  expect_false(prestoStatementReturnsRows("ALTER SCHEMA s RENAME TO s2"))
  expect_false(prestoStatementReturnsRows("ALTER VIEW v RENAME TO v2"))
  expect_false(prestoStatementReturnsRows("ALTER FUNCTION f RETURNS NULL ON NULL INPUT"))
  expect_false(prestoStatementReturnsRows("REFRESH MATERIALIZED VIEW mv"))
  expect_false(prestoStatementReturnsRows("COMMENT ON TABLE foo IS 'note'"))
})

test_that("prestoStatementReturnsRows: DML is not row-returning", {
  expect_false(prestoStatementReturnsRows("INSERT INTO foo VALUES (1)"))
  expect_false(prestoStatementReturnsRows("INSERT INTO foo SELECT * FROM bar"))
  expect_false(prestoStatementReturnsRows("UPDATE foo SET x = 1"))
  expect_false(prestoStatementReturnsRows("DELETE FROM foo"))
  expect_false(prestoStatementReturnsRows("MERGE INTO foo USING bar ON foo.id = bar.id"))
  expect_false(prestoStatementReturnsRows("TRUNCATE TABLE foo"))
})

test_that("prestoStatementReturnsRows: session / transaction / admin are not row-returning", {
  expect_false(prestoStatementReturnsRows("USE catalog.schema"))
  expect_false(prestoStatementReturnsRows("SET SESSION foo = 1"))
  expect_false(prestoStatementReturnsRows("RESET SESSION foo"))
  expect_false(prestoStatementReturnsRows("SET ROLE admin"))
  expect_false(prestoStatementReturnsRows("SET TIME ZONE 'UTC'"))
  expect_false(prestoStatementReturnsRows("START TRANSACTION"))
  expect_false(prestoStatementReturnsRows("COMMIT"))
  expect_false(prestoStatementReturnsRows("ROLLBACK"))
  expect_false(prestoStatementReturnsRows("GRANT SELECT ON t TO u"))
  expect_false(prestoStatementReturnsRows("GRANT ROLES admin TO u"))
  expect_false(prestoStatementReturnsRows("REVOKE SELECT ON t FROM u"))
  expect_false(prestoStatementReturnsRows("REVOKE ROLES admin FROM u"))
  expect_false(prestoStatementReturnsRows("CALL system.runtime.kill_query('q')"))
  expect_false(prestoStatementReturnsRows("ANALYZE t"))
  expect_false(prestoStatementReturnsRows("PREPARE my_query FROM SELECT 1"))
  expect_false(prestoStatementReturnsRows("DEALLOCATE PREPARE my_query"))
  # EXECUTE returns rows when the prepared statement is SELECT, but the keyword
  # whitelist does not include EXECUTE -- classified conservatively as non-row-returning.
  expect_false(prestoStatementReturnsRows("EXECUTE my_query"))
  expect_false(prestoStatementReturnsRows("EXECUTE my_query USING 1, 'foo'"))
})

test_that("prestoStatementReturnsRows: keyword-prefixed words must not false-positive", {
  expect_false(prestoStatementReturnsRows("SELECTED"))
  expect_false(prestoStatementReturnsRows("SELECTION FROM t"))
  expect_false(prestoStatementReturnsRows("WITHOUT ROWID"))
  expect_false(prestoStatementReturnsRows("TABLES"))
  expect_false(prestoStatementReturnsRows("DESCRIPTION"))
  expect_false(prestoStatementReturnsRows("VALUED"))
  expect_false(prestoStatementReturnsRows("SHOWCASE"))
  expect_false(prestoStatementReturnsRows("EXPLAINER"))
})

test_that("prestoStatementReturnsRows: empty / comment-only / non-alpha / unterminated", {
  expect_false(prestoStatementReturnsRows(""))
  expect_false(prestoStatementReturnsRows(NULL))
  expect_false(prestoStatementReturnsRows("   "))
  expect_false(prestoStatementReturnsRows("-- only a comment"))
  expect_false(prestoStatementReturnsRows("-- only a comment\n"))
  expect_false(prestoStatementReturnsRows("/* only a block comment */"))
  expect_false(prestoStatementReturnsRows("123"))
  expect_false(prestoStatementReturnsRows("* FROM t"))
  # an unterminated block comment cannot be stripped -> not row-returning
  expect_false(prestoStatementReturnsRows("/* never closed SELECT 1"))
})
