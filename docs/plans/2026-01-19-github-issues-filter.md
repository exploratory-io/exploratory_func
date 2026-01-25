# GitHub Issues Filter Parameters

**Date:** 2026-01-19

## Problem

The `getGithubIssues()` function currently fetches all issues from a repository with no filtering options. Users need the ability to filter issues by state, labels, assignee, and other criteria supported by the GitHub API.

## Solution

Add explicit filter parameters to `getGithubIssues()` while maintaining full backward compatibility.

## Function Signature

```r
getGithubIssues <- function(username, password, owner, repository,
                            state = "all",
                            milestone = NULL,
                            assignee = NULL,
                            creator = NULL,
                            mentioned = NULL,
                            labels = NULL,
                            sort = NULL,
                            direction = NULL,
                            since = NULL,
                            type = NULL,
                            ...) {
```

## Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `username` | string | — | GitHub username for authentication |
| `password` | string | — | GitHub personal access token |
| `owner` | string | — | Repository owner (user or organization) |
| `repository` | string | — | Repository name |
| `state` | string | `"all"` | Issue state: `"open"`, `"closed"`, or `"all"` |
| `milestone` | string | `NULL` | Filter by milestone number, `"*"` (any), or `"none"` |
| `assignee` | string | `NULL` | Filter by assignee username, `"*"` (any), or `"none"` |
| `creator` | string | `NULL` | Filter by issue creator username |
| `mentioned` | string | `NULL` | Filter by mentioned username |
| `labels` | string | `NULL` | Comma-separated label names (e.g., `"bug,ui"`) |
| `sort` | string | `NULL` | Sort by: `"created"`, `"updated"`, or `"comments"` |
| `direction` | string | `NULL` | Sort direction: `"asc"` or `"desc"` |
| `since` | string | `NULL` | Only issues updated after this time (ISO 8601 format) |
| `type` | string | `NULL` | Filter by issue type, `"*"` (any), or `"none"` |

## Backward Compatibility

All new parameters default to `NULL`. When `NULL`, parameters are not sent to the GitHub API.

The only non-NULL default is `state = "all"`, which matches the current hardcoded behavior. A call with no new parameters produces the exact same API request as before.

| Parameter | Our Default | GitHub Default (when not sent) |
|-----------|-------------|-------------------------------|
| `state` | `"all"` | `"open"` |
| `sort` | `NULL` | `"created"` |
| `direction` | `NULL` | `"desc"` |
| All others | `NULL` | No filtering |

## Implementation

### Query Building Logic

Build the query list dynamically, only including non-NULL parameters:

```r
# Build query list - only include non-NULL parameters
query_params <- list(per_page = 100, page = i)

# Always include state (has default "all")
query_params$state <- state

# Conditionally add optional filters
if (!is.null(milestone)) query_params$milestone <- milestone
if (!is.null(assignee)) query_params$assignee <- assignee
if (!is.null(creator)) query_params$creator <- creator
if (!is.null(mentioned)) query_params$mentioned <- mentioned
if (!is.null(labels)) query_params$labels <- labels
if (!is.null(sort)) query_params$sort <- sort
if (!is.null(direction)) query_params$direction <- direction
if (!is.null(since)) query_params$since <- since
if (!is.null(type)) query_params$type <- type

res <- httr::GET(endpoint,
                 query = query_params,
                 httr::authenticate(username, password))
```

### Documentation

```r
#' Get GitHub Issues
#'
#' Fetches issues from a GitHub repository with optional filtering.
#'
#' @param username GitHub username for authentication
#' @param password GitHub personal access token
#' @param owner Repository owner (user or organization)
#' @param repository Repository name
#' @param state Issue state: "open", "closed", or "all" (default: "all")
#' @param milestone Filter by milestone number, "*" (any), or "none"
#' @param assignee Filter by assignee username, "*" (any), or "none"
#' @param creator Filter by issue creator username
#' @param mentioned Filter by mentioned username
#' @param labels Comma-separated label names (e.g., "bug,ui")
#' @param sort Sort by: "created", "updated", or "comments"
#' @param direction Sort direction: "asc" or "desc"
#' @param since Only issues updated after this time (ISO 8601 format)
#' @param type Filter by issue type, "*" (any), or "none"
#' @param ... Reserved for future use
#' @return A data frame of GitHub issues
#' @export
```

## Testing

### Test Scenarios

1. **Backward compatibility** - Call with no new parameters, verify same behavior
2. **Single filters** - Test each filter individually (`state = "open"`, `labels = "bug"`, etc.)
3. **Combined filters** - Test multiple filters together
4. **Edge cases** - Test special values like `"none"`, `"*"`, ISO 8601 dates

### Note

Tests require either a test repository with known issues or mocking the `httr::GET` response.

## Files to Modify

- `R/system.R` - Update `getGithubIssues` function (lines 154-184)

## Files to Add (Optional)

- `tests/testthat/test-github-issues.R` - New tests
