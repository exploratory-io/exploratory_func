context("test getGithubIssues")

# These tests require EXPL_GITHUB_USERNAME and EXPL_GITHUB_PASSWORD environment variables
# Skip if credentials are not available

test_that("getGithubIssues returns issues with default parameters", {
  skip_if(Sys.getenv("EXPL_GITHUB_USERNAME") == "", "EXPL_GITHUB_USERNAME not set")
  skip_if(Sys.getenv("EXPL_GITHUB_PASSWORD") == "", "EXPL_GITHUB_PASSWORD not set")

  username <- Sys.getenv("EXPL_GITHUB_USERNAME")
  token <- Sys.getenv("EXPL_GITHUB_PASSWORD")

  result <- getGithubIssues(
    username = username,
    password = token,
    owner = "exploratory-io",
    repository = "exploratory_func"
  )

  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
  expect_true("state" %in% names(result))
})

test_that("getGithubIssues filters by state = 'open'", {
  skip_if(Sys.getenv("EXPL_GITHUB_USERNAME") == "", "EXPL_GITHUB_USERNAME not set")
  skip_if(Sys.getenv("EXPL_GITHUB_PASSWORD") == "", "EXPL_GITHUB_PASSWORD not set")

  username <- Sys.getenv("EXPL_GITHUB_USERNAME")
  token <- Sys.getenv("EXPL_GITHUB_PASSWORD")

  result <- getGithubIssues(
    username = username,
    password = token,
    owner = "exploratory-io",
    repository = "exploratory_func",
    state = "open"
  )

  expect_true(is.data.frame(result))
  if (nrow(result) > 0) {
    expect_true(all(result$state == "open"))
  }
})

test_that("getGithubIssues filters by state = 'closed'", {
  skip_if(Sys.getenv("EXPL_GITHUB_USERNAME") == "", "EXPL_GITHUB_USERNAME not set")
  skip_if(Sys.getenv("EXPL_GITHUB_PASSWORD") == "", "EXPL_GITHUB_PASSWORD not set")

  username <- Sys.getenv("EXPL_GITHUB_USERNAME")
  token <- Sys.getenv("EXPL_GITHUB_PASSWORD")

  result <- getGithubIssues(
    username = username,
    password = token,
    owner = "exploratory-io",
    repository = "exploratory_func",
    state = "closed"
  )

  expect_true(is.data.frame(result))
  if (nrow(result) > 0) {
    expect_true(all(result$state == "closed"))
  }
})

test_that("getGithubIssues filters by labels", {
  skip_if(Sys.getenv("EXPL_GITHUB_USERNAME") == "", "EXPL_GITHUB_USERNAME not set")
  skip_if(Sys.getenv("EXPL_GITHUB_PASSWORD") == "", "EXPL_GITHUB_PASSWORD not set")

  username <- Sys.getenv("EXPL_GITHUB_USERNAME")
  token <- Sys.getenv("EXPL_GITHUB_PASSWORD")

  result <- getGithubIssues(
    username = username,
    password = token,
    owner = "exploratory-io",
    repository = "exploratory_func",
    labels = "bug"
  )

  expect_true(is.data.frame(result))
  # If results returned, verify they have the bug label
  if (nrow(result) > 0 && "labels" %in% names(result)) {
    # labels column contains nested data, check each issue has "bug" label
    has_bug_label <- sapply(result$labels, function(labels_list) {
      if (is.null(labels_list) || length(labels_list) == 0) return(FALSE)
      if (is.data.frame(labels_list)) {
        return("bug" %in% labels_list$name)
      }
      return(FALSE)
    })
    expect_true(all(has_bug_label))
  }
})

test_that("getGithubIssues filters by combined state and labels", {
  skip_if(Sys.getenv("EXPL_GITHUB_USERNAME") == "", "EXPL_GITHUB_USERNAME not set")
  skip_if(Sys.getenv("EXPL_GITHUB_PASSWORD") == "", "EXPL_GITHUB_PASSWORD not set")

  username <- Sys.getenv("EXPL_GITHUB_USERNAME")
  token <- Sys.getenv("EXPL_GITHUB_PASSWORD")

  result <- getGithubIssues(
    username = username,
    password = token,
    owner = "exploratory-io",
    repository = "exploratory_func",
    state = "open",
    labels = "bug"
  )

  expect_true(is.data.frame(result))
  if (nrow(result) > 0) {
    # Verify state filter
    expect_true(all(result$state == "open"))

    # Verify labels filter
    if ("labels" %in% names(result)) {
      has_bug_label <- sapply(result$labels, function(labels_list) {
        if (is.null(labels_list) || length(labels_list) == 0) return(FALSE)
        if (is.data.frame(labels_list)) {
          return("bug" %in% labels_list$name)
        }
        return(FALSE)
      })
      expect_true(all(has_bug_label))
    }
  }
})

test_that("getGithubIssues filters by milestone title (resolves to number)", {
  skip_if(Sys.getenv("EXPL_GITHUB_USERNAME") == "", "EXPL_GITHUB_USERNAME not set")
  skip_if(Sys.getenv("EXPL_GITHUB_PASSWORD") == "", "EXPL_GITHUB_PASSWORD not set")

  username <- Sys.getenv("EXPL_GITHUB_USERNAME")
  token <- Sys.getenv("EXPL_GITHUB_PASSWORD")

  # Test with milestone title (not number) - using tam repo which has milestones
  result <- getGithubIssues(
    username = username,
    password = token,
    owner = "exploratory-io",
    repository = "tam",
    milestone = "14.5"
  )

  expect_true(is.data.frame(result))
  # If results returned, verify they all have the correct milestone title
  if (nrow(result) > 0 && "milestone.title" %in% names(result)) {
    expect_true(all(result$milestone.title == "14.5"))
  }
})

test_that("getGithubIssues handles milestone special values", {
  skip_if(Sys.getenv("EXPL_GITHUB_USERNAME") == "", "EXPL_GITHUB_USERNAME not set")
  skip_if(Sys.getenv("EXPL_GITHUB_PASSWORD") == "", "EXPL_GITHUB_PASSWORD not set")

  username <- Sys.getenv("EXPL_GITHUB_USERNAME")
  token <- Sys.getenv("EXPL_GITHUB_PASSWORD")

  # Test with milestone = "*" (any milestone)
  result <- getGithubIssues(
    username = username,
    password = token,
    owner = "exploratory-io",
    repository = "tam",
    milestone = "*"
  )

  expect_true(is.data.frame(result))
  # Most returned issues should have a milestone (some may have null milestone if it was deleted)
  if (nrow(result) > 0 && "milestone.title" %in% names(result)) {
    expect_true(any(!is.na(result$milestone.title)))
  }
})
