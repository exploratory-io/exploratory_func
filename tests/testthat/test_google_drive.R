context("test for google_drive filename fallback (issue #36171)")

# Pure selector: given a data frame with columns id, name, modifiedTime,
# return the id of the most-recently-modified row whose name == fileName, or NULL.

test_that("selectMostRecentGoogleDriveFileId returns id of single exact match", {
  items <- data.frame(
    id = c("idA", "idB"),
    name = c("other.csv", "sales.csv"),
    modifiedTime = c("2024-01-01T00:00:00Z", "2024-02-02T00:00:00Z"),
    stringsAsFactors = FALSE
  )
  result <- exploratory:::selectMostRecentGoogleDriveFileId(items, "sales.csv")
  expect_equal(result, "idB")
})

test_that("selectMostRecentGoogleDriveFileId picks most-recently-modified among duplicates", {
  items <- data.frame(
    id = c("old", "new", "middle"),
    name = c("sales.csv", "sales.csv", "sales.csv"),
    modifiedTime = c(
      "2024-01-01T00:00:00Z",
      "2024-03-03T10:30:00Z",
      "2024-02-02T00:00:00Z"
    ),
    stringsAsFactors = FALSE
  )
  result <- exploratory:::selectMostRecentGoogleDriveFileId(items, "sales.csv")
  expect_equal(result, "new")
})

test_that("selectMostRecentGoogleDriveFileId returns NULL when no name matches", {
  items <- data.frame(
    id = c("idA"),
    name = c("other.csv"),
    modifiedTime = c("2024-01-01T00:00:00Z"),
    stringsAsFactors = FALSE
  )
  result <- exploratory:::selectMostRecentGoogleDriveFileId(items, "sales.csv")
  expect_null(result)
})

test_that("selectMostRecentGoogleDriveFileId returns NULL for empty/NULL inputs", {
  expect_null(exploratory:::selectMostRecentGoogleDriveFileId(NULL, "sales.csv"))
  empty <- data.frame(id = character(0), name = character(0),
                      modifiedTime = character(0), stringsAsFactors = FALSE)
  expect_null(exploratory:::selectMostRecentGoogleDriveFileId(empty, "sales.csv"))
  items <- data.frame(id = "idA", name = "sales.csv",
                      modifiedTime = "2024-01-01T00:00:00Z", stringsAsFactors = FALSE)
  expect_null(exploratory:::selectMostRecentGoogleDriveFileId(items, NULL))
  expect_null(exploratory:::selectMostRecentGoogleDriveFileId(items, ""))
  # NA / non-scalar fileName must return NULL, not raise "missing value where TRUE/FALSE needed".
  expect_null(exploratory:::selectMostRecentGoogleDriveFileId(items, NA_character_))
  expect_null(exploratory:::selectMostRecentGoogleDriveFileId(items, c("sales.csv", "other.csv")))
})

test_that("selectMostRecentGoogleDriveFileId handles a complex multibyte file name", {
  complexName <- "航空 会社 !\"#$%&'()_'{|}~ 表.csv"
  items <- data.frame(
    id = c("idA", "idB"),
    name = c("plain.csv", complexName),
    modifiedTime = c("2024-01-01T00:00:00Z", "2024-05-05T00:00:00Z"),
    stringsAsFactors = FALSE
  )
  result <- exploratory:::selectMostRecentGoogleDriveFileId(items, complexName)
  expect_equal(result, "idB")
})

test_that("selectMostRecentGoogleDriveFileId returns first match when modifiedTime column is absent", {
  items <- data.frame(
    id = c("idA", "idB"),
    name = c("sales.csv", "sales.csv"),
    stringsAsFactors = FALSE
  )
  result <- exploratory:::selectMostRecentGoogleDriveFileId(items, "sales.csv")
  expect_equal(result, "idA")
})

test_that("selectMostRecentGoogleDriveFileId orders real timestamps ahead of NA modifiedTime", {
  items <- data.frame(
    id = c("hasNA", "hasTime"),
    name = c("sales.csv", "sales.csv"),
    modifiedTime = c(NA_character_, "2024-02-02T00:00:00Z"),
    stringsAsFactors = FALSE
  )
  result <- exploratory:::selectMostRecentGoogleDriveFileId(items, "sales.csv")
  expect_equal(result, "hasTime")
})
