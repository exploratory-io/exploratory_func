# Split a Presto/Trino SQL string into individual statements on top-level `;`.
# Recognizes single-quoted literals ('') with '' escape, double-quoted identifiers ("") with "" escape,
# line comments (-- ... \n), and block comments (/* ... */, no nesting).
# Returns a character vector of trimmed, non-empty statements.
splitPrestoStatements <- function(sql) {
  if (is.null(sql) || length(sql) == 0 || !nzchar(sql)) {
    return(character(0))
  }
  chars <- strsplit(sql, "", fixed = TRUE)[[1]]
  n <- length(chars)
  state <- "normal" # normal | sq | dq | lc | bc
  out <- character(0)
  i <- 1L
  start <- 1L
  flush <- function(end) {
    if (end >= start) {
      piece <- paste0(chars[start:end], collapse = "")
      piece <- trimws(piece)
      if (nzchar(piece)) {
        out[[length(out) + 1L]] <<- piece
      }
    }
  }
  while (i <= n) {
    ch <- chars[i]
    nx <- if (i < n) chars[i + 1L] else ""
    if (state == "normal") {
      if (ch == "'") {
        state <- "sq"
      } else if (ch == "\"") {
        state <- "dq"
      } else if (ch == "-" && nx == "-") {
        state <- "lc"
        i <- i + 1L
      } else if (ch == "/" && nx == "*") {
        state <- "bc"
        i <- i + 1L
      } else if (ch == ";") {
        flush(i - 1L)
        start <- i + 1L
      }
    } else if (state == "sq") {
      if (ch == "'") {
        if (nx == "'") {
          i <- i + 1L # escaped quote
        } else {
          state <- "normal"
        }
      }
    } else if (state == "dq") {
      if (ch == "\"") {
        if (nx == "\"") {
          i <- i + 1L # escaped quote
        } else {
          state <- "normal"
        }
      }
    } else if (state == "lc") {
      if (ch == "\n") {
        state <- "normal"
      }
    } else if (state == "bc") {
      if (ch == "*" && nx == "/") {
        state <- "normal"
        i <- i + 1L
      }
    }
    i <- i + 1L
  }
  flush(n)
  out
}

# Returns TRUE when the statement starts with a row-returning Presto keyword.
# Strips leading whitespace, leading line/block comments, and a leading `(` before checking.
prestoStatementReturnsRows <- function(sql) {
  if (is.null(sql) || !nzchar(sql)) {
    return(FALSE)
  }
  s <- sql
  repeat {
    s <- sub("^[[:space:]]+", "", s)
    if (startsWith(s, "--")) {
      s <- sub("^--[^\n]*\n?", "", s)
      next
    }
    if (startsWith(s, "/*")) {
      s_before <- s
      s <- sub("^/\\*(?s).*?\\*/", "", s, perl = TRUE)
      if (identical(s, s_before)) {
        return(FALSE)
      }
      next
    }
    if (startsWith(s, "(")) {
      s <- sub("^\\(", "", s)
      next
    }
    break
  }
  m <- regmatches(s, regexpr("^[A-Za-z]+", s))
  if (length(m) == 0L || !nzchar(m)) {
    return(FALSE)
  }
  toupper(m) %in% c("SELECT", "WITH", "SHOW", "DESCRIBE", "DESC", "EXPLAIN", "VALUES", "TABLE")
}

# Internal: execute a single Presto statement and return its result data frame.
# Mirrors the original single-statement behavior of queryPresto.
.queryPrestoSingle <- function(conn, query, numOfRows) {
  resultSet <- NULL
  df <- tryCatch({
    resultSet <- RPresto::dbSendQuery(conn, query)
    DBI::dbFetch(resultSet, n = numOfRows)
  }, error = function(err) {
    stop(err)
  }, finally = {
    if (!is.null(resultSet)) {
      try(RPresto::dbClearResult(resultSet), silent = TRUE)
    }
  })
  df
}

#' @export
queryPresto <- function(host, port, username, password = "", schema, catalog, numOfRows = -1, query, ...){
  if(!requireNamespace("httr")){stop("package httr must be installed.")}
  if(!requireNamespace("stringr")){stop("package stringr must be installed.")}
  if(!requireNamespace("RPresto")){stop("package RPresto must be installed.")}

  conn <- getDBConnection("presto", host = host, port = port, databaseName = "", username = username, password = password, catalog = catalog, schema = schema, dsn="", additionalParams = "",
                          collection = "", isSSL = FALSE, authSource = NULL, cluster = NULL, timeout = NULL)
  query <- convertUserInputToUtf8(query)
  # set envir = parent.frame() to get variables from users environment, not package environment
  # glue_sql does not quote Date or POSIXct. Let's use our sql_glue_transformer here.
  query <- glue_exploratory(query, .transformer=sql_glue_transformer, .envir = parent.frame())

  statements <- splitPrestoStatements(query)
  total <- length(statements)
  if (total == 0L) {
    return(data.frame())
  }
  if (total == 1L) {
    # single-statement fast path: uses trimmed statement from splitPrestoStatements
    df <- tryCatch({
      .queryPrestoSingle(conn, statements[[1L]], numOfRows)
    }, error = function(err) {
      clearDBConnection("presto", host, port, catalog, schema, username)
      stop(err)
    })
    return(df)
  }

  # multi-statement: execute non-final via dbExecute, then classify final
  if (total > 1L) {
    for (i in seq_len(total - 1L)) {
      tryCatch({
        RPresto::dbExecute(conn, statements[[i]])
      }, error = function(err) {
        clearDBConnection("presto", host, port, catalog, schema, username)
        stop(sprintf("Statement %d of %d failed: %s", i, total, conditionMessage(err)))
      })
    }
  }

  last <- statements[[total]]
  if (prestoStatementReturnsRows(last)) {
    df <- tryCatch({
      .queryPrestoSingle(conn, last, numOfRows)
    }, error = function(err) {
      clearDBConnection("presto", host, port, catalog, schema, username)
      stop(sprintf("Statement %d of %d failed: %s", total, total, conditionMessage(err)))
    })
    return(df)
  } else {
    tryCatch({
      RPresto::dbExecute(conn, last)
    }, error = function(err) {
      clearDBConnection("presto", host, port, catalog, schema, username)
      stop(sprintf("Statement %d of %d failed: %s", total, total, conditionMessage(err)))
    })
    return(data.frame())
  }
}
