context("test for SFTP data source (tam#39178)")

test_that("sftp_split_host_keys accepts newline, comma and semicolon separators", {
  keys <- "ssh-ed25519 AAAA1\nssh-rsa AAAA2, ecdsa-sha2-nistp256 AAAA3; \n"
  expect_equal(exploratory:::sftp_split_host_keys(keys),
               c("ssh-ed25519 AAAA1", "ssh-rsa AAAA2", "ecdsa-sha2-nistp256 AAAA3"))
  expect_equal(exploratory:::sftp_split_host_keys(""), character(0))
  expect_equal(exploratory:::sftp_split_host_keys(NULL), character(0))
  expect_equal(exploratory:::sftp_split_host_keys(NA), character(0))
})

test_that("sftp_known_hosts_lines uses [host]:port only for a non-default port", {
  keys <- "ssh-ed25519 AAAA1,ssh-rsa AAAA2"
  expect_equal(exploratory:::sftp_known_hosts_lines("example.com", 22, keys),
               c("example.com ssh-ed25519 AAAA1", "example.com ssh-rsa AAAA2"))
  expect_equal(exploratory:::sftp_known_hosts_lines("10.0.0.1", 2222, keys),
               c("[10.0.0.1]:2222 ssh-ed25519 AAAA1", "[10.0.0.1]:2222 ssh-rsa AAAA2"))
  expect_equal(exploratory:::sftp_known_hosts_lines("example.com", 22, ""), character(0))
})

test_that("sftp_parse_keyscan_output drops comments and computes the ssh-keygen fingerprint", {
  # github.com ed25519 host key and the fingerprint printed by ssh-keygen -lf.
  lines <- c("# github.com:22 SSH-2.0-b2ec264",
             "github.com ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl",
             "")
  keys <- exploratory:::sftp_parse_keyscan_output(lines)
  expect_equal(nrow(keys), 1)
  expect_equal(keys$keyType, "ssh-ed25519")
  expect_equal(keys$fingerprint, "SHA256:+DiY3wvvV6TuJJhbpZisF/zLDA0zPMSvHdkr4UvCOqU")
})

test_that("sftp_url encodes each path segment and resolves relative paths from home", {
  expect_equal(exploratory:::sftp_url("example.com", 22, "data/a b.csv"), "sftp://example.com:22/~/data/a%20b.csv")
  expect_equal(exploratory:::sftp_url("example.com", 2222, "/var/data/"), "sftp://example.com:2222/var/data/")
  expect_equal(exploratory:::sftp_url("example.com", 22, ""), "sftp://example.com:22/~/")
  expect_equal(exploratory:::sftp_url("::1", 22, "/x.csv"), "sftp://[::1]:22/x.csv")
  name <- "航空 会社 !\"#$%&'()*+, -.;=@[]^_{|}~ 表.csv"
  url <- exploratory:::sftp_url("example.com", 22, paste0("/data/", name))
  expect_equal(url, paste0("sftp://example.com:22/data/", curl::curl_escape(name)))
})

test_that("sftp_join_path joins folder and name", {
  expect_equal(exploratory:::sftp_join_path("", "a.csv"), "a.csv")
  expect_equal(exploratory:::sftp_join_path("/data/", "a.csv"), "/data/a.csv")
  expect_equal(exploratory:::sftp_join_path("data", "a.csv"), "data/a.csv")
})

test_that("sftp_parse_listing parses a long listing", {
  lines <- c("drwxr-xr-x    6 hide wheel         192 Sep 28 23:28 .",
             "drwxr-xr-x   17 hide wheel         544 Sep 28 23:28 ..",
             "-rw-r--r--    1 hide wheel          12 Sep 28 23:28 sales 2025.csv",
             "lrwxr-xr-x    1 hide wheel          10 Jan  3  2024 latest.csv -> sales 2025.csv",
             "drwxr-xr-x    2 hide wheel          64 Sep 28 23:28 sub",
             "")
  items <- exploratory:::sftp_parse_listing(lines)
  expect_equal(items$name, c("sales 2025.csv", "latest.csv", "sub"))
  expect_equal(items$isdir, c(FALSE, FALSE, TRUE))
  expect_equal(items$size, c(12, 10, 64))
  expect_equal(items$lastModified[2], "Jan  3  2024")
})

test_that("sftp_stop_with_error maps curl errors to EXP-DATASRC codes", {
  expect_error(exploratory:::sftp_stop_with_error(simpleError("Login denied [h]:\nAuthentication failure"), "h"), "EXP-DATASRC-38")
  expect_error(exploratory:::sftp_stop_with_error(simpleError("SSL peer certificate or SSH remote key was not OK [h]"), "h"), "EXP-DATASRC-39")
  expect_error(exploratory:::sftp_stop_with_error(simpleError("Remote file not found [h]:"), "h", "/a.csv"), "EXP-DATASRC-40")
  expect_error(exploratory:::sftp_stop_with_error(simpleError("Failed to connect to h port 22"), "h"), "EXP-DATASRC-41")
  expect_error(exploratory:::sftp_stop_with_error(simpleError("something else"), "h"), "something else")
})

test_that("sftp_create_handle refuses a connection without host keys", {
  skip_if_not("sftp" %in% curl::curl_version()$protocols)
  expect_error(exploratory:::sftp_create_handle("example.com", 22, "u", "p", "", "", ""), "EXP-DATASRC-37")
})

# Live tests against a real SFTP server. Set these environment variables to run them:
#   EXP_TEST_SFTP_HOST, EXP_TEST_SFTP_PORT, EXP_TEST_SFTP_USER, EXP_TEST_SFTP_KEYFILE,
#   EXP_TEST_SFTP_DIR (a folder that has sales_2025.csv and sales_2026.csv with columns a,b).
live_sftp <- function() {
  skip_if_not(nzchar(Sys.getenv("EXP_TEST_SFTP_HOST")), "EXP_TEST_SFTP_HOST is not set")
  skip_if_not("sftp" %in% curl::curl_version()$protocols, "curl does not support sftp")
  host <- Sys.getenv("EXP_TEST_SFTP_HOST")
  port <- as.integer(Sys.getenv("EXP_TEST_SFTP_PORT", "22"))
  keys <- getSFTPHostKeys(host, port)
  list(host = host, port = port, user = Sys.getenv("EXP_TEST_SFTP_USER"),
       keyFile = Sys.getenv("EXP_TEST_SFTP_KEYFILE"), dir = Sys.getenv("EXP_TEST_SFTP_DIR"),
       hostKeys = paste(paste(keys$keyType, keys$key), collapse = ","))
}

test_that("live: list, download, merge and search files", {
  s <- live_sftp()
  items <- listItemsInSFTP(host = s$host, port = s$port, user = s$user, keyFile = s$keyFile, hostKeys = s$hostKeys, folder = s$dir)
  expect_true(all(c("sales_2025.csv", "sales_2026.csv") %in% items$name))

  df <- getCSVFileFromSFTP(fileName = exploratory:::sftp_join_path(s$dir, "sales_2025.csv"), host = s$host, port = s$port, user = s$user,
                           keyFile = s$keyFile, hostKeys = s$hostKeys, delim = ",")
  expect_equal(colnames(df), c("a", "b"))

  merged <- searchAndGetCSVFilesFromSFTP(searchKeyword = "^sales_", host = s$host, port = s$port, user = s$user,
                                         keyFile = s$keyFile, hostKeys = s$hostKeys, folder = s$dir, delim = ",")
  expect_equal(colnames(merged), c("id", "a", "b"))
  expect_setequal(unique(merged$id), c("sales_2025.csv", "sales_2026.csv"))
})

test_that("live: a wrong host key and a missing file are reported", {
  s <- live_sftp()
  wrongKey <- "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl"
  expect_error(listItemsInSFTP(host = s$host, port = s$port, user = s$user, keyFile = s$keyFile, hostKeys = wrongKey, folder = s$dir),
               "EXP-DATASRC-39")
  expect_error(downloadDataFileFromSFTP(host = s$host, port = s$port, user = s$user, keyFile = s$keyFile, hostKeys = s$hostKeys,
                                        filePath = exploratory:::sftp_join_path(s$dir, "no_such_file.csv")),
               "EXP-DATASRC-40")
})
