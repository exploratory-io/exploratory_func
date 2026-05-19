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

# ---------------------------------------------------------------------------
# Complex real-world Presto / Treasure Data patterns
# (fictional schema: sandbox_retail, l1_web, l1_crm, l1_store, l0_segment)
# ---------------------------------------------------------------------------

test_that("splitPrestoStatements: inline -- comment with non-ASCII text sits before semicolon", {
  # Pattern: WHERE ... -- 期間変更箇所\n; next-statement
  sql <- paste0(
    "SELECT user_id, COUNT(1) AS sessions\n",
    "FROM l1_web.session_log\n",
    "WHERE TD_TIME_RANGE(time, '2025-12-01', '2026-02-01', 'JST') -- 期間変更箇所\n",
    "  AND hostname = 'shop.example.com'\n",
    "GROUP BY 1;\n",
    "SELECT COUNT(DISTINCT user_id) AS dau FROM l1_web.session_log"
  )
  stmts <- splitPrestoStatements(sql)
  expect_equal(length(stmts), 2)
  expect_true(grepl("-- 期間変更箇所", stmts[[1]]))
  expect_true(grepl("TD_TIME_RANGE", stmts[[1]]))
  expect_equal(stmts[[2]], "SELECT COUNT(DISTINCT user_id) AS dau FROM l1_web.session_log")
})

test_that("splitPrestoStatements: REGEXP_LIKE patterns with | inside string literals don't split", {
  sql <- paste(
    "SELECT user_id,",
    "  CASE",
    "    WHEN REGEXP_LIKE(category, 'shoes|bags|apparel') THEN 'fashion'",
    "    WHEN REGEXP_LIKE(category, 'tv|laptop|phone') THEN 'electronics'",
    "    ELSE 'other'",
    "  END AS segment",
    "FROM l1_crm.purchase_log",
    "WHERE TD_TIME_RANGE(time, '2026-01-01', NULL, 'JST');",
    "SELECT segment, COUNT(DISTINCT user_id) AS users FROM l1_crm.purchase_log GROUP BY 1",
    sep = "\n"
  )
  stmts <- splitPrestoStatements(sql)
  expect_equal(length(stmts), 2)
  expect_true(grepl("REGEXP_LIKE", stmts[[1]]))
  expect_true(grepl("'shoes\\|bags\\|apparel'", stmts[[1]]))
})

test_that("splitPrestoStatements: VALUES inline table with many literals inside a CTE", {
  # Pattern: excluded IDs defined as inline VALUES, semicolon must not fire inside literals
  sql <- paste(
    "WITH excluded_ids AS (",
    "  SELECT user_id FROM (",
    "    VALUES",
    "    ('uid-aaa-001'), ('uid-bbb-002'), ('uid-ccc-003'),",
    "    ('uid-ddd-004'), ('uid-eee-005'), ('uid-fff-006'),",
    "    ('uid-ggg-007'), ('uid-hhh-008')",
    "  ) AS t (user_id)",
    ")",
    "SELECT m.user_id, m.email",
    "FROM l1_crm.members AS m",
    "WHERE m.user_id NOT IN (SELECT user_id FROM excluded_ids)",
    "  AND m.status = 'active';",
    "SELECT COUNT(*) FROM l1_crm.members WHERE status = 'active'",
    sep = "\n"
  )
  stmts <- splitPrestoStatements(sql)
  expect_equal(length(stmts), 2)
  expect_true(grepl("VALUES", stmts[[1]]))
  expect_true(grepl("'uid-hhh-008'", stmts[[1]]))
  expect_equal(stmts[[2]], "SELECT COUNT(*) FROM l1_crm.members WHERE status = 'active'")
})

test_that("splitPrestoStatements: CROSS JOIN UNNEST + UNION ALL is a single statement", {
  # Pattern: REGEXP_EXTRACT_ALL produces array, UNNEST expands it,
  # UNION ALL re-attaches rows where array was empty
  sql <- paste(
    "WITH raw_events AS (",
    "  SELECT user_id,",
    "    REGEXP_EXTRACT_ALL(tag_string, '[a-z_]+') AS tag_array",
    "  FROM l1_web.event_log",
    "  WHERE TD_TIME_RANGE(time, '2026-01-01', NULL, 'JST') -- 期間変更箇所",
    "),",
    "expanded AS (",
    "  SELECT user_id, tag",
    "  FROM raw_events CROSS JOIN UNNEST(tag_array) AS t(tag)",
    "  UNION ALL",
    "  SELECT user_id, '' AS tag",
    "  FROM raw_events",
    "  WHERE ARRAY_JOIN(tag_array, ',') = ''",
    ")",
    "SELECT user_id, tag, COUNT(1) AS cnt",
    "FROM expanded",
    "GROUP BY 1, 2",
    sep = "\n"
  )
  stmts <- splitPrestoStatements(sql)
  expect_equal(length(stmts), 1)
  expect_true(grepl("CROSS JOIN UNNEST", stmts[[1]]))
  expect_true(grepl("UNION ALL", stmts[[1]]))
  expect_true(grepl("ARRAY_JOIN", stmts[[1]]))
})

test_that("splitPrestoStatements: nested WITH (CTE inside outer CTE subquery)", {
  # Presto allows WITH inside a subquery — the inner WITH is not a statement separator
  sql <- paste(
    "WITH outer_cte AS (",
    "  WITH inner_agg AS (",
    "    SELECT user_id, MAX(score) AS top_score",
    "    FROM l1_crm.user_scores",
    "    GROUP BY 1",
    "  ),",
    "  inner_filter AS (",
    "    SELECT user_id FROM inner_agg WHERE top_score > 80",
    "  )",
    "  SELECT user_id, top_score FROM inner_agg",
    "  WHERE user_id IN (SELECT user_id FROM inner_filter)",
    ")",
    "SELECT user_id, top_score FROM outer_cte ORDER BY top_score DESC",
    sep = "\n"
  )
  stmts <- splitPrestoStatements(sql)
  expect_equal(length(stmts), 1)
  expect_true(grepl("WITH inner_agg AS", stmts[[1]]))
  expect_true(grepl("ORDER BY top_score DESC", stmts[[1]]))
})

test_that("splitPrestoStatements: MAX_BY / MIN_BY / TD functions preserved across 3 statements", {
  # Pattern: aggregate using TD-specific MAX_BY / MIN_BY, split into 3 statements
  sql <- paste(
    "DROP TABLE IF EXISTS sandbox_retail.latest_purchase;",
    "CREATE TABLE sandbox_retail.latest_purchase AS",
    "WITH base AS (",
    "  SELECT",
    "    user_id,",
    "    MAX_BY(product_id, purchase_time) AS last_product,",
    "    MIN_BY(product_id, purchase_time) AS first_product,",
    "    COUNT(1) AS purchase_cnt",
    "  FROM l1_store.purchase_log",
    "  WHERE TD_TIME_RANGE(purchase_time, '2025-12-01', '2026-03-01', 'JST') -- 期間変更箇所",
    "  GROUP BY 1",
    ")",
    "SELECT user_id, last_product, first_product, purchase_cnt",
    "FROM base",
    "WHERE purchase_cnt >= 2;",
    "SELECT user_id, last_product FROM sandbox_retail.latest_purchase LIMIT 100",
    sep = "\n"
  )
  stmts <- splitPrestoStatements(sql)
  expect_equal(length(stmts), 3)
  expect_equal(stmts[[1]], "DROP TABLE IF EXISTS sandbox_retail.latest_purchase")
  expect_true(startsWith(stmts[[2]], "CREATE TABLE sandbox_retail.latest_purchase AS"))
  expect_true(grepl("MAX_BY", stmts[[2]]))
  expect_true(grepl("MIN_BY", stmts[[2]]))
  expect_true(grepl("TD_TIME_RANGE", stmts[[2]]))
  expect_equal(stmts[[3]], "SELECT user_id, last_product FROM sandbox_retail.latest_purchase LIMIT 100")
})

test_that("splitPrestoStatements: 5-stmt pipeline with step comments embedded in statements", {
  # Mirrors the real-world 3-step pattern: DROP+CREATE, DROP+CREATE, final SELECT
  # The step-header comments (--) are attached to the following DROP statement
  sql <- paste(
    "-- Step 1: build campaign user list",
    "DROP TABLE IF EXISTS sandbox_retail.kpi_user_list;",
    "CREATE TABLE sandbox_retail.kpi_user_list AS",
    "WITH uid_map AS (",
    "  SELECT user_hash AS uid, member_id",
    "  FROM l1_member.uid_member_mapping",
    "  WHERE is_latest = 1 AND member_id IS NOT NULL",
    "  GROUP BY 1, 2",
    "),",
    "web_access AS (",
    "  SELECT m.member_id",
    "  FROM l1_web.session_log AS s",
    "  LEFT JOIN l1_web.user_id_map AS m USING(session_uid)",
    "  WHERE TD_TIME_RANGE(s.access_time, '2025-12-01', '2026-02-01', 'JST') -- 期間変更箇所",
    "    AND s.hostname = 'shop.example.com'",
    "    AND m.member_id IS NOT NULL",
    "  GROUP BY 1",
    ")",
    "SELECT t1.member_id,",
    "  IF(t2.member_id IS NOT NULL, 1, 0) AS is_priority",
    "FROM l1_crm.registered_members AS t1",
    "LEFT JOIN web_access AS t2 ON t1.member_id = t2.member_id",
    "WHERE t1.status = 'active';",
    "",
    "-- Step 2: build action log",
    "DROP TABLE IF EXISTS sandbox_retail.kpi_action_list;",
    "CREATE TABLE sandbox_retail.kpi_action_list AS",
    "WITH actions AS (",
    "  SELECT user_id, event_type,",
    "    MIN(event_time) AS first_time, COUNT(1) AS cnt",
    "  FROM l1_web.event_log",
    "  WHERE TD_TIME_RANGE(event_time, '2026-01-01', NULL, 'JST') -- 期間変更箇所",
    "    AND event_type IS NOT NULL",
    "  GROUP BY 1, 2",
    ")",
    "SELECT * FROM actions;",
    "",
    "-- Step 3: final aggregation",
    "SELECT u.member_id,",
    "  COALESCE(a.cnt, 0) AS action_cnt,",
    "  u.is_priority",
    "FROM sandbox_retail.kpi_user_list AS u",
    "LEFT JOIN sandbox_retail.kpi_action_list AS a USING(member_id)",
    "ORDER BY 2 DESC",
    sep = "\n"
  )
  stmts <- splitPrestoStatements(sql)
  expect_equal(length(stmts), 5)
  # step comment is part of the DROP statement that follows it
  expect_true(grepl("DROP TABLE IF EXISTS sandbox_retail.kpi_user_list", stmts[[1]]))
  expect_true(startsWith(stmts[[2]], "CREATE TABLE sandbox_retail.kpi_user_list AS"))
  expect_true(grepl("MAX_BY\\|MIN_BY\\|IF\\|TD_TIME_RANGE\\|web_access", stmts[[2]]) ||
              grepl("IF\\(t2", stmts[[2]]))
  expect_true(grepl("DROP TABLE IF EXISTS sandbox_retail.kpi_action_list", stmts[[3]]))
  expect_true(startsWith(stmts[[4]], "CREATE TABLE sandbox_retail.kpi_action_list AS"))
  expect_true(grepl("SELECT u.member_id", stmts[[5]]))
  expect_true(grepl("ORDER BY 2 DESC", stmts[[5]]))
})

test_that("prestoStatementReturnsRows: complex WITH + nested WITH + UNION ALL + ORDER BY is row-returning", {
  # Mirrors Step 4 in the real-world script: final SELECT-only with
  # outer CTEs each containing inner WITH, joined by UNION ALL, ORDER BY at end
  sql <- paste(
    "WITH prev_stage AS (",
    "  WITH stage_scores AS (",
    "    SELECT user_id,",
    "      CASE",
    "        WHEN COUNT(DISTINCT stage || behavior) >= 3 THEN 2",
    "        WHEN COUNT(DISTINCT stage || behavior) >= 1 THEN 1",
    "        ELSE 0",
    "      END AS stage_lvl",
    "    FROM sandbox_retail.kpi_action_list",
    "    WHERE TD_TIME_RANGE(action_time, '2026-01-01', '2026-02-01', 'JST') -- 期間変更箇所",
    "      AND stage IN ('awareness', 'interest')",
    "    GROUP BY 1",
    "  )",
    "  SELECT user_id, MAX(stage_lvl) AS start_stage FROM stage_scores GROUP BY 1",
    "),",
    "curr_stage AS (",
    "  WITH stage_scores AS (",
    "    SELECT user_id,",
    "      CASE",
    "        WHEN COUNT(DISTINCT stage || behavior) >= 3 THEN 2",
    "        WHEN COUNT(DISTINCT stage || behavior) >= 1 THEN 1",
    "        ELSE 0",
    "      END AS stage_lvl",
    "    FROM sandbox_retail.kpi_action_list",
    "    WHERE TD_TIME_RANGE(action_time, '2026-02-01', NULL, 'JST') -- 期間変更箇所",
    "      AND stage IN ('awareness', 'interest')",
    "    GROUP BY 1",
    "  )",
    "  SELECT user_id, MAX(stage_lvl) AS latest_stage FROM stage_scores GROUP BY 1",
    "),",
    "ai_chat_users AS (",
    "  SELECT DISTINCT user_id FROM sandbox_retail.kpi_action_list",
    "  WHERE type = 'AI chat'",
    "    AND TD_TIME_RANGE(action_time, '2026-02-01', NULL, 'JST')",
    ")",
    "-- group A: AI chat users",
    "SELECT 'A: AI chat users' AS cohort,",
    "  CASE t2.start_stage WHEN 3 THEN '3:purchase' WHEN 2 THEN '2:visit'",
    "    WHEN 1 THEN '1:interest' ELSE '0:none' END AS start_stage,",
    "  CASE WHEN COALESCE(t3.latest_stage, 0) >= t2.start_stage",
    "    THEN CASE t3.latest_stage WHEN 3 THEN '3:purchase' WHEN 2 THEN '2:visit'",
    "           WHEN 1 THEN '1:interest' ELSE '0:none' END",
    "    ELSE CASE t2.start_stage WHEN 3 THEN '3:purchase' WHEN 2 THEN '2:visit'",
    "           WHEN 1 THEN '1:interest' ELSE '0:none' END",
    "  END AS latest_stage,",
    "  COUNT(DISTINCT t1.user_id) AS cnt",
    "FROM sandbox_retail.kpi_user_list AS t1",
    "LEFT JOIN prev_stage AS t2 ON t1.user_id = t2.user_id",
    "LEFT JOIN curr_stage AS t3 ON t1.user_id = t3.user_id",
    "INNER JOIN ai_chat_users AS t4 ON t1.user_id = t4.user_id",
    "WHERE t3.latest_stage IS NOT NULL",
    "GROUP BY 1, 2, 3",
    "UNION ALL",
    "-- group B: no AI chat",
    "SELECT 'B: no AI chat' AS cohort,",
    "  CASE t2.start_stage WHEN 3 THEN '3:purchase' WHEN 2 THEN '2:visit'",
    "    WHEN 1 THEN '1:interest' ELSE '0:none' END AS start_stage,",
    "  CASE WHEN COALESCE(t3.latest_stage, 0) >= t2.start_stage",
    "    THEN CASE t3.latest_stage WHEN 3 THEN '3:purchase' WHEN 2 THEN '2:visit'",
    "           WHEN 1 THEN '1:interest' ELSE '0:none' END",
    "    ELSE CASE t2.start_stage WHEN 3 THEN '3:purchase' WHEN 2 THEN '2:visit'",
    "           WHEN 1 THEN '1:interest' ELSE '0:none' END",
    "  END AS latest_stage,",
    "  COUNT(DISTINCT t1.user_id) AS cnt",
    "FROM sandbox_retail.kpi_user_list AS t1",
    "LEFT JOIN prev_stage AS t2 ON t1.user_id = t2.user_id",
    "LEFT JOIN curr_stage AS t3 ON t1.user_id = t3.user_id",
    "LEFT JOIN ai_chat_users AS t4 ON t1.user_id = t4.user_id",
    "WHERE t3.latest_stage IS NOT NULL",
    "  AND t4.user_id IS NULL",
    "GROUP BY 1, 2, 3",
    "ORDER BY 1, 2",
    sep = "\n"
  )
  # the statement starts with WITH → row-returning
  expect_true(prestoStatementReturnsRows(sql))
  # it is one unsplit statement
  stmts <- splitPrestoStatements(sql)
  expect_equal(length(stmts), 1)
})

test_that("prestoStatementReturnsRows: CREATE TABLE AS WITH multi-CTE is not row-returning", {
  sql <- paste(
    "CREATE TABLE sandbox_retail.kpi_action_list AS",
    "WITH web_log AS (",
    "  SELECT m.user_id, 'Web' AS channel,",
    "    CASE",
    "      WHEN REGEXP_LIKE(cat, 'pricing|specs|compare') THEN 'consideration'",
    "      WHEN REGEXP_LIKE(cat, 'brand|campaign') THEN 'awareness'",
    "    END AS stage,",
    "    TD_TIME_STRING(MIN(s.event_time), 'M!', 'JST') AS report_month,",
    "    MIN(s.event_time) AS action_time,",
    "    COUNT(1) AS cnt",
    "  FROM l1_web.event_log AS s",
    "  LEFT JOIN l1_web.user_id_map AS m USING(session_uid)",
    "  WHERE TD_TIME_RANGE(s.event_time, '2025-12-01', NULL, 'JST') -- 期間変更箇所",
    "    AND s.hostname = 'shop.example.com'",
    "    AND m.user_id IS NOT NULL",
    "    AND cat IS NOT NULL",
    "  GROUP BY 1, 2, 3, 4",
    "),",
    "chat_log AS (",
    "  SELECT user_id, 'AI chat' AS channel,",
    "    CASE",
    "      WHEN REGEXP_LIKE(intent_tags, 'price|discount|budget') THEN 'consideration'",
    "      WHEN REGEXP_LIKE(intent_tags, 'brand|model') THEN 'awareness'",
    "      WHEN conversation_completed IS NULL THEN 'no conversation'",
    "      ELSE 'other'",
    "    END AS stage,",
    "    TD_TIME_STRING(TD_TIME_PARSE(session_date, 'JST'), 'M!', 'JST') AS report_month,",
    "    MIN(TD_TIME_PARSE(session_date, 'JST')) AS action_time,",
    "    COUNT(1) AS cnt",
    "  FROM l1_store.ai_chat_sessions",
    "  WHERE TD_TIME_RANGE(TD_TIME_PARSE(session_date, 'JST'), '2026-01-01', NULL, 'JST')",
    "    AND user_id IS NOT NULL",
    "  GROUP BY 1, 2, 3, 4",
    ")",
    "SELECT * FROM web_log",
    "UNION ALL",
    "SELECT * FROM chat_log",
    sep = "\n"
  )
  expect_false(prestoStatementReturnsRows(sql))
})

test_that("prestoStatementReturnsRows: SELECT with CASE, REGEXP_LIKE, TD functions is row-returning", {
  sql <- paste(
    "SELECT",
    "  user_id,",
    "  CASE",
    "    WHEN REGEXP_LIKE(behavior_tags, 'purchase|checkout|payment') THEN 'converted'",
    "    WHEN REGEXP_LIKE(behavior_tags, 'wishlist|cart|compare') THEN 'considering'",
    "    WHEN REGEXP_LIKE(behavior_tags, 'view|search|browse') THEN 'browsing'",
    "    ELSE 'unknown'",
    "  END AS funnel_stage,",
    "  TD_TIME_STRING(MIN(event_time), 'yyyy-MM-dd', 'JST') AS first_event_date,",
    "  TD_TIME_STRING(MAX(event_time), 'yyyy-MM-dd', 'JST') AS last_event_date,",
    "  COUNT(DISTINCT session_id) AS session_cnt,",
    "  COUNT(1) AS event_cnt",
    "FROM l1_web.event_log",
    "WHERE TD_TIME_RANGE(event_time, '2026-01-01', NULL, 'JST') -- 期間変更箇所",
    "  AND hostname = 'shop.example.com'",
    "  AND user_id IS NOT NULL",
    "GROUP BY 1, 2",
    "ORDER BY session_cnt DESC",
    sep = "\n"
  )
  expect_true(prestoStatementReturnsRows(sql))
})
