context("test for Google Sheets writeback retry (#40864)")

# googlesheets4::sheet_append/sheet_write/gs4_create can fail with a transient
# Google API error -- most commonly a 409 "ABORTED" read-modify-write
# concurrency conflict, but also a 429 rate limit or a 503 server error.
# Before this fix, uploadDataToGoogleSheets had no retry at all, so a single
# transient conflict failed the whole scheduled writeback job and was
# reported to the customer as a hard failure. These tests exercise the retry
# helper directly (no network/OAuth needed) with waits = c(0, 0) so they run
# instantly while still proving the backoff loop's attempt-count behavior.

makeGoogleApiError <- function(statusCode) {
  simpleError(paste0("Client error: (", statusCode, ") placeholder"))
}

test_that(".isRetryableGoogleSheetsWriteError treats 409/429/503 as retryable", {
  expect_true(exploratory:::.isRetryableGoogleSheetsWriteError(makeGoogleApiError(409)))
  expect_true(exploratory:::.isRetryableGoogleSheetsWriteError(makeGoogleApiError(429)))
  expect_true(exploratory:::.isRetryableGoogleSheetsWriteError(makeGoogleApiError(503)))
})

test_that(".isRetryableGoogleSheetsWriteError does not treat other errors as retryable", {
  expect_false(exploratory:::.isRetryableGoogleSheetsWriteError(makeGoogleApiError(403)))
  expect_false(exploratory:::.isRetryableGoogleSheetsWriteError(makeGoogleApiError(404)))
  expect_false(exploratory:::.isRetryableGoogleSheetsWriteError(simpleError("some unrelated failure")))
})

test_that(".retryGoogleSheetsWrite retries a 409 conflict and returns the result once it succeeds", {
  callCount <- 0
  writeFn <- function() {
    callCount <<- callCount + 1
    if (callCount < 3) {
      stop(makeGoogleApiError(409))
    }
    "success-on-third-try"
  }

  result <- exploratory:::.retryGoogleSheetsWrite(writeFn, waits = c(0, 0))

  expect_equal(result, "success-on-third-try")
  expect_equal(callCount, 3)
})

test_that(".retryGoogleSheetsWrite gives up and re-throws after exhausting retries on a persistent 409", {
  callCount <- 0
  writeFn <- function() {
    callCount <<- callCount + 1
    stop(makeGoogleApiError(409))
  }

  expect_error(
    exploratory:::.retryGoogleSheetsWrite(writeFn, waits = c(0, 0)),
    "\\(409\\)"
  )
  # The initial attempt plus one retry per entry of `waits`.
  expect_equal(callCount, 3)
})

test_that(".retryGoogleSheetsWrite does not retry a persistent non-transient error", {
  callCount <- 0
  writeFn <- function() {
    callCount <<- callCount + 1
    stop(makeGoogleApiError(403))
  }

  expect_error(
    exploratory:::.retryGoogleSheetsWrite(writeFn, waits = c(0, 0)),
    "\\(403\\)"
  )
  # No retry at all: a permission error is not transient.
  expect_equal(callCount, 1)
})

test_that(".retryGoogleSheetsWrite succeeds immediately with no retry when the write does not fail", {
  callCount <- 0
  writeFn <- function() {
    callCount <<- callCount + 1
    "ok"
  }

  result <- exploratory:::.retryGoogleSheetsWrite(writeFn, waits = c(0, 0))

  expect_equal(result, "ok")
  expect_equal(callCount, 1)
})
