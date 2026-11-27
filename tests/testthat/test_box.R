context("test for Box data source functions (tam#25706)")

# These tests never talk to the real Box. A tiny HTTP server written with base R sockets
# (serverSocket) runs in a background R process via callr; exploratory.box.base_url points
# the package at it. Each handler is function(req, base) -> list(status, headers, body) where
# req has method, target, path (decoded UTF-8), query (named list), headers (lower-case
# names) and body (character).

COMPLEX_NAME <- "航空 会社 !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表.csv"

stubServeLoop <- function(handler, port_file, log_file) {
  srv <- NULL
  port <- NULL
  for (p in sample(20000:40000)) {
    srv <- tryCatch(suppressWarnings(serverSocket(p)), error = function(e) NULL)
    if (!is.null(srv)) { port <- p; break }
  }
  writeLines(as.character(port), port_file)
  base <- paste0("http://127.0.0.1:", port)
  decode <- function(x) {
    x <- utils::URLdecode(x)
    Encoding(x) <- "UTF-8"
    x
  }
  reason <- c("200" = "OK", "202" = "Accepted", "302" = "Found", "401" = "Unauthorized",
              "403" = "Forbidden", "404" = "Not Found", "429" = "Too Many Requests",
              "500" = "Internal Server Error")
  repeat {
    con <- socketAccept(srv, blocking = TRUE, open = "a+b")
    tryCatch({
      line <- readLines(con, n = 1, warn = FALSE)
      if (length(line) == 0 || !nzchar(line)) next
      parts <- strsplit(sub("\r$", "", line), " ")[[1]]
      headers <- list()
      repeat {
        h <- sub("\r$", "", readLines(con, n = 1, warn = FALSE))
        if (length(h) == 0 || !nzchar(h)) break
        kv <- regmatches(h, regexpr(":", h), invert = TRUE)[[1]]
        headers[[tolower(kv[1])]] <- trimws(kv[2])
      }
      body <- ""
      if (!is.null(headers[["content-length"]]) && as.integer(headers[["content-length"]]) > 0) {
        body <- rawToChar(readBin(con, "raw", as.integer(headers[["content-length"]])))
      }
      target <- parts[2]
      pq <- strsplit(target, "?", fixed = TRUE)[[1]]
      query <- list()
      if (length(pq) > 1 && nzchar(pq[2])) {
        for (kv in strsplit(pq[2], "&", fixed = TRUE)[[1]]) {
          k <- strsplit(kv, "=", fixed = TRUE)[[1]]
          query[[decode(k[1])]] <- if (length(k) > 1) decode(k[2]) else ""
        }
      }
      req <- list(method = parts[1], target = target, path = decode(pq[1]),
                  query = query, headers = headers, body = body)
      cat(parts[1], target, if (is.null(headers$authorization)) "" else headers$authorization, "\n",
          file = log_file, append = TRUE, sep = "\t")
      res <- handler(req, base)
      payload <- if (is.raw(res$body)) res$body else charToRaw(enc2utf8(if (is.null(res$body)) "" else res$body))
      head <- c(sprintf("HTTP/1.1 %d %s", res$status, reason[[as.character(res$status)]]),
                sprintf("%s: %s", names(res$headers), unlist(res$headers)),
                sprintf("Content-Length: %d", length(payload)), "Connection: close", "", "")
      writeBin(c(charToRaw(paste(head, collapse = "\r\n")), payload), con)
    }, error = function(e) NULL, finally = close(con))
  }
}

# Runs `code` with a stub server up and the package pointed at it. Returns whatever `code` returns.
withBoxStub <- function(handler, code) {
  port_file <- tempfile()
  log_file <- tempfile()
  file.create(log_file)
  bg <- callr::r_bg(stubServeLoop, args = list(handler = handler, port_file = port_file, log_file = log_file))
  on.exit(bg$kill(), add = TRUE)
  for (i in 1:100) {
    if (file.exists(port_file) && length(readLines(port_file, warn = FALSE)) > 0) break
    Sys.sleep(0.1)
  }
  port <- as.integer(readLines(port_file, warn = FALSE))
  oldOpts <- options(exploratory.box.base_url = paste0("http://127.0.0.1:", port),
                     tam.should.cache.datafile = NULL)
  on.exit(options(oldOpts), add = TRUE)
  exploratory::setTokenInfo("box", list(access_token = "AT123", token_type = "bearer", expires_in = 3600))
  on.exit(exploratory::setTokenInfo("box", NULL), add = TRUE)
  assign("stubLogFile", log_file, envir = parent.frame())
  force(code)
}

stubLog <- function(log_file) {
  lines <- readLines(log_file, warn = FALSE, encoding = "UTF-8")
  lines[nzchar(lines)]
}

jsonRes <- function(obj, status = 200L, headers = list()) {
  list(status = status,
       headers = c(list("Content-Type" = "application/json"), headers),
       body = as.character(jsonlite::toJSON(obj, auto_unbox = TRUE, null = "null")))
}

folderEntry <- function(id, name) list(type = "folder", id = id, name = name, size = 0L,
                                      modified_at = "2026-01-01T00:00:00-08:00",
                                      path_collection = list(total_count = 1L, entries = list(list(id = "0", name = "All Files"))))
fileEntry <- function(id, name, modified = "2026-01-02T00:00:00-08:00") {
  list(type = "file", id = id, name = name, size = 10L, modified_at = modified,
       path_collection = list(total_count = 1L, entries = list(list(id = "0", name = "All Files"))))
}

# ---------------------------------------------------------------- token

test_that("getBoxToken builds credentials without refresh_token", {
  exploratory::setTokenInfo("box", as.environment(list(access_token = "AT1", token_type = "bearer",
                                                       expires_in = 3600, refresh_token = "RT-SECRET")))
  on.exit(exploratory::setTokenInfo("box", NULL))
  tok <- exploratory::getBoxToken()
  expect_equal(tok$credentials$access_token, "AT1")
  expect_equal(tok$credentials$token_type, "bearer")
  expect_equal(tok$credentials$expires_in, 3600)
  expect_null(tok$credentials$refresh_token)
  expect_false("refresh_token" %in% names(tok$credentials))
  # refreshBoxToken just re-reads (R never refreshes)
  expect_equal(exploratory::refreshBoxToken("x")$credentials$access_token, "AT1")
})

test_that("getBoxToken errors when no token is set", {
  exploratory::setTokenInfo("box", NULL)
  expect_error(exploratory::getBoxToken(), "OAuth token is not set for Box")
})

# ---------------------------------------------------------------- listing

test_that("listItemsInBox follows next_marker and never uses offset", {
  handler <- function(req, base) {
    jsonRes2 <- function(obj) list(status = 200L, headers = list("Content-Type" = "application/json"),
                                   body = as.character(jsonlite::toJSON(obj, auto_unbox = TRUE)))
    pc <- list(total_count = 1L, entries = list(list(id = "0", name = "All Files")))
    e <- function(type, id, name) list(type = type, id = id, name = name, size = 5L,
                                       modified_at = "2026-01-02T00:00:00-08:00", path_collection = pc)
    if (req$path == "/2.0/folders/0/items") {
      if (is.null(req$query$marker)) {
        jsonRes2(list(entries = list(e("folder", "10", "sub"), e("file", "11", "a.csv"), e("file", "12", "pic.png")),
                      limit = 1000L, next_marker = "m1"))
      } else {
        jsonRes2(list(entries = list(e("file", "13", "b.xlsx"), e("web_link", "14", "link.csv"), e("file", "15", "c.TXT")),
                      limit = 1000L, next_marker = NULL))
      }
    } else list(status = 404L, headers = list(), body = "{}")
  }
  withBoxStub(handler, {
    items <- exploratory::listItemsInBox("0")
    expect_equal(colnames(items), c("id", "name", "type", "size", "modified_at", "path"))
    # pic.png (unsupported ext) and the web_link are excluded; folder + csv/xlsx/txt kept
    expect_equal(items$id, c("10", "11", "13", "15"))
    expect_equal(items$type, c("folder", "file", "file", "file"))
    expect_equal(items$path[[2]], "All Files")
    log <- stubLog(stubLogFile)
    expect_equal(length(log), 2)
    expect_true(all(grepl("Bearer AT123", log, fixed = TRUE)))
    expect_true(all(grepl("usemarker=true", log, fixed = TRUE)))
    expect_true(all(grepl("limit=1000", log, fixed = TRUE)))
    expect_true(all(grepl("fields=id%2Cname%2Ctype%2Csize%2Cmodified_at%2Cpath_collection", log, fixed = TRUE)))
    expect_false(any(grepl("offset", log, fixed = TRUE)))
    expect_true(grepl("marker=m1", log[2], fixed = TRUE))
  })
})

test_that("listItemsInBox filters by type and pattern, and stops at n_max", {
  handler <- function(req, base) {
    pc <- list(total_count = 0L, entries = list())
    e <- function(type, id, name) list(type = type, id = id, name = name, size = 5L,
                                       modified_at = "2026-01-02T00:00:00-08:00", path_collection = pc)
    page <- if (is.null(req$query$marker)) {
      list(entries = list(e("file", "1", "Sales_2025.csv"), e("file", "2", "sales_2026.csv"), e("file", "3", "x.xlsx")),
           next_marker = "m1")
    } else {
      list(entries = list(e("file", "4", "other.csv")), next_marker = "")
    }
    list(status = 200L, headers = list("Content-Type" = "application/json"),
         body = as.character(jsonlite::toJSON(page, auto_unbox = TRUE)))
  }
  withBoxStub(handler, {
    csv <- exploratory::listItemsInBox("0", type = c("csv", "tsv", "txt"))
    expect_equal(csv$id, c("1", "2", "4"))
    xl <- exploratory::listItemsInBox("0", type = c("xls", "xlsx"))
    expect_equal(xl$id, "3")
    pat <- exploratory::listItemsInBox("0", type = c("csv"), pattern = "SALES")
    expect_equal(pat$id, c("1", "2"))
    expect_equal(nrow(exploratory::listItemsInBox("0", type = "csv", pattern = "zzz")), 0)
    # n_max reached on page 1 => page 2 must not be requested
    n0 <- length(stubLog(stubLogFile))
    two <- exploratory::listItemsInBox("0", type = c("csv"), n_max = 2)
    expect_equal(two$id, c("1", "2"))
    expect_equal(length(stubLog(stubLogFile)) - n0, 1)
  })
})

test_that("listItemsInBox keeps Japanese and complex names intact", {
  handler <- function(req, base) {
    pc <- list(total_count = 0L, entries = list())
    page <- list(entries = list(list(type = "file", id = "7", name = "航空 会社 !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表.csv",
                                     size = 1L, modified_at = "2026-01-02T00:00:00-08:00", path_collection = pc),
                                list(type = "file", id = "8", name = "売上データ.csv", size = 1L,
                                     modified_at = "2026-01-02T00:00:00-08:00", path_collection = pc)),
                 next_marker = "")
    list(status = 200L, headers = list("Content-Type" = "application/json; charset=utf-8"),
         body = as.character(jsonlite::toJSON(page, auto_unbox = TRUE)))
  }
  withBoxStub(handler, {
    items <- exploratory::listItemsInBox("0", type = "csv")
    expect_equal(items$name, c(COMPLEX_NAME, "売上データ.csv"))
    expect_equal(exploratory::listItemsInBox("0", type = "csv", pattern = "売上")$id, "8")
  })
})

test_that("getBoxFolderDetails returns the breadcrumb path", {
  handler <- function(req, base) {
    obj <- list(type = "folder", id = "55", name = "Reports",
                path_collection = list(total_count = 2L, entries = list(list(id = "0", name = "All Files"),
                                                                       list(id = "9", name = "2026"))))
    list(status = 200L, headers = list("Content-Type" = "application/json"),
         body = as.character(jsonlite::toJSON(obj, auto_unbox = TRUE)))
  }
  withBoxStub(handler, {
    d <- exploratory::getBoxFolderDetails("55")
    expect_equal(d$id, "55")
    expect_equal(d$name, "Reports")
    expect_equal(d$path_collection$id, c("0", "9"))
    expect_equal(d$path_collection$name, c("All Files", "2026"))
    expect_true(any(grepl("/2.0/folders/55", stubLog(stubLogFile), fixed = TRUE)))
  })
})

# ---------------------------------------------------------------- request classification

test_that("boxRequest retries 429 honoring Retry-After and then succeeds", {
  handler <- local({
    n <- 0L
    function(req, base) {
      n <<- n + 1L
      if (n <= 2L) list(status = 429L, headers = list("Retry-After" = "0"), body = "{}")
      else list(status = 200L, headers = list("Content-Type" = "application/json"), body = "{\"ok\":true}")
    }
  })
  withBoxStub(handler, {
    res <- exploratory:::boxRequest("/2.0/folders/1")
    expect_equal(httr::status_code(res), 200)
    expect_equal(length(stubLog(stubLogFile)), 3)
  })
})

test_that("boxRequest gives up after 3 retries on persistent 429", {
  handler <- function(req, base) list(status = 429L, headers = list("Retry-After" = "0"), body = "{}")
  withBoxStub(handler, {
    expect_error(exploratory:::boxRequest("/2.0/folders/1"), "Box")
    expect_equal(length(stubLog(stubLogFile)), 4) # 1 try + 3 retries
  })
})

test_that("boxRequest classifies 401 as box_auth_error, 403 and 404 with their own classes", {
  codes <- c("401", "403", "404")
  handler <- function(req, base) {
    list(status = as.integer(sub("/2.0/status/", "", req$path)), headers = list(), body = "{}")
  }
  withBoxStub(handler, {
    e401 <- tryCatch(exploratory:::boxRequest("/2.0/status/401"), error = function(e) e)
    expect_true(inherits(e401, "box_auth_error"))
    e403 <- tryCatch(exploratory:::boxRequest("/2.0/status/403"), error = function(e) e)
    expect_true(inherits(e403, "box_forbidden_error"))
    expect_false(inherits(e403, "box_auth_error"))
    e404 <- tryCatch(exploratory:::boxRequest("/2.0/status/404"), error = function(e) e)
    expect_true(inherits(e404, "box_not_found_error"))
    # 401 is not retried
    expect_equal(sum(grepl("status/401", stubLog(stubLogFile), fixed = TRUE)), 1)
  })
})

test_that("a missing folder in listItemsInBox raises the EXP-DATASRC-6 style error", {
  handler <- function(req, base) list(status = 404L, headers = list(), body = "{}")
  withBoxStub(handler, {
    expect_error(exploratory::listItemsInBox("999"), "EXP-DATASRC-6")
  })
})

# ---------------------------------------------------------------- download

csvHandler <- function(content = "a,b\n1,2\n", fileId = "100") {
  force(content); force(fileId)
  function(req, base) {
    if (req$path == paste0("/2.0/files/", fileId)) {
      list(status = 200L, headers = list("Content-Type" = "application/json"),
           body = as.character(jsonlite::toJSON(list(id = fileId, name = "f.csv", item_status = "active",
                                                     parent = list(id = "55")), auto_unbox = TRUE)))
    } else if (req$path == paste0("/2.0/files/", fileId, "/content")) {
      list(status = 302L, headers = list(Location = paste0(base, "/dl/", fileId)), body = "")
    } else if (req$path == paste0("/dl/", fileId)) {
      list(status = 200L, headers = list("Content-Type" = "text/csv"), body = charToRaw(enc2utf8(content)))
    } else list(status = 404L, headers = list(), body = "{}")
  }
}

test_that("downloadDataFileFromBox follows the 302 redirect and reads the CSV", {
  withBoxStub(csvHandler("x,y\n航空,2\n3,4\n"), {
    path <- exploratory:::downloadDataFileFromBox("100", type = "csv")
    expect_true(file.exists(path))
    expect_true(grepl("\\.csv$", path))
    expect_equal(readLines(path, encoding = "UTF-8")[2], "航空,2")
    df <- exploratory::getCSVFileFromBox("100", delim = ",")
    expect_equal(nrow(df), 2)
    expect_equal(colnames(df), c("x", "y"))
  })
})

test_that("downloadDataFileFromBox waits and retries on 202", {
  handler <- local({
    n <- 0L
    function(req, base) {
      if (req$path == "/2.0/files/100/content") {
        n <<- n + 1L
        if (n == 1L) return(list(status = 202L, headers = list("Retry-After" = "0"), body = ""))
        return(list(status = 302L, headers = list(Location = paste0(base, "/dl/100")), body = ""))
      }
      list(status = 200L, headers = list(), body = charToRaw("a\n1\n"))
    }
  })
  withBoxStub(handler, {
    path <- exploratory:::downloadDataFileFromBox("100", type = "csv")
    expect_equal(readLines(path), c("a", "1"))
    expect_equal(sum(grepl("/content", stubLog(stubLogFile), fixed = TRUE)), 2)
  })
})

test_that("downloadDataFileFromBox caches by digest(fileId + type) only when tam.should.cache.datafile is TRUE", {
  withBoxStub(csvHandler(), {
    p1 <- exploratory:::downloadDataFileFromBox("100", type = "csv")
    p2 <- exploratory:::downloadDataFileFromBox("100", type = "csv")
    expect_false(identical(p1, p2)) # option not set => no reuse
    options(tam.should.cache.datafile = TRUE)
    on.exit(options(tam.should.cache.datafile = NULL))
    c1 <- exploratory:::downloadDataFileFromBox("100", type = "csv")
    n <- length(stubLog(stubLogFile))
    c2 <- exploratory:::downloadDataFileFromBox("100", type = "csv")
    expect_equal(c1, c2)
    expect_equal(length(stubLog(stubLogFile)), n) # no second download
    hash <- digest::digest(stringr::str_c("100", "csv"), "md5", serialize = FALSE)
    expect_equal(exploratory:::getDownloadedFilePath(hash), c1)
    exploratory::clearBoxCacheFile("100", "csv")
    expect_false(file.exists(c1))
    expect_null(exploratory:::getDownloadedFilePath(hash))
  })
})

test_that("a trashed file falls back to the same-named file in the folder (detectStaleFile)", {
  handler <- function(req, base) {
    json <- function(obj) list(status = 200L, headers = list("Content-Type" = "application/json"),
                               body = as.character(jsonlite::toJSON(obj, auto_unbox = TRUE)))
    pc <- list(total_count = 0L, entries = list())
    if (req$path == "/2.0/files/100") {
      json(list(id = "100", name = "sales.csv", item_status = "trashed", parent = list(id = "55")))
    } else if (req$path == "/2.0/folders/55/items") {
      json(list(entries = list(list(type = "file", id = "200", name = "sales.csv", size = 1L,
                                    modified_at = "2026-02-01T00:00:00-08:00", path_collection = pc)), next_marker = ""))
    } else if (req$path == "/2.0/files/200/content") {
      list(status = 302L, headers = list(Location = paste0(base, "/dl/200")), body = "")
    } else if (req$path == "/dl/200") {
      list(status = 200L, headers = list(), body = charToRaw("v\nnew\n"))
    } else list(status = 404L, headers = list(), body = "{}")
  }
  withBoxStub(handler, {
    # no fileName/folderId passed: derived from the trashed file's own metadata
    df <- exploratory::getCSVFileFromBox("100", delim = ",")
    expect_equal(df$v, "new")
    expect_false(any(grepl("/files/100/content", stubLog(stubLogFile), fixed = TRUE)))
  })
})

test_that("a 404 file id falls back by folderId + complex fileName, else EXP-DATASRC-9", {
  handler <- function(req, base) {
    json <- function(obj) list(status = 200L, headers = list("Content-Type" = "application/json"),
                               body = as.character(jsonlite::toJSON(obj, auto_unbox = TRUE)))
    pc <- list(total_count = 0L, entries = list())
    if (req$path == "/2.0/files/100") {
      list(status = 404L, headers = list(), body = "{}")
    } else if (req$path == "/2.0/folders/55/items") {
      json(list(entries = list(
        list(type = "file", id = "199", name = "old.csv", size = 1L, modified_at = "2026-01-01T00:00:00-08:00", path_collection = pc),
        list(type = "file", id = "200", name = "航空 会社 !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表.csv", size = 1L,
             modified_at = "2026-02-01T00:00:00-08:00", path_collection = pc)), next_marker = ""))
    } else if (req$path == "/2.0/files/200/content") {
      list(status = 302L, headers = list(Location = paste0(base, "/dl/200")), body = "")
    } else if (req$path == "/dl/200") {
      list(status = 200L, headers = list(), body = charToRaw("v\n新しい\n"))
    } else list(status = 404L, headers = list(), body = "{}")
  }
  withBoxStub(handler, {
    df <- exploratory::getCSVFileFromBox("100", delim = ",", fileName = COMPLEX_NAME, folderId = "55")
    expect_equal(df$v, "新しい")
    # no replacement by that name => EXP-DATASRC-9
    expect_error(exploratory::getCSVFileFromBox("100", delim = ",", fileName = "nope.csv", folderId = "55"),
                 "EXP-DATASRC-9")
    # no folder context at all and nothing derivable => EXP-DATASRC-9
    expect_error(exploratory::getCSVFileFromBox("100", delim = ","), "EXP-DATASRC-9")
  })
})

test_that("a 401 during download surfaces as box_auth_error", {
  handler <- function(req, base) list(status = 401L, headers = list(), body = "{}")
  withBoxStub(handler, {
    e <- tryCatch(exploratory::getCSVFileFromBox("100", delim = ","), error = function(e) e)
    expect_true(inherits(e, "box_auth_error"))
  })
})

# ---------------------------------------------------------------- multi file + search

multiHandler <- function(req, base) {
  pc <- list(total_count = 0L, entries = list())
  json <- function(obj) list(status = 200L, headers = list("Content-Type" = "application/json"),
                             body = as.character(jsonlite::toJSON(obj, auto_unbox = TRUE)))
  if (req$path == "/2.0/folders/55/items") {
    json(list(entries = list(
      list(type = "file", id = "1", name = "売上_1月.csv", size = 1L, modified_at = "2026-01-01T00:00:00-08:00", path_collection = pc),
      list(type = "file", id = "2", name = "売上_2月.csv", size = 1L, modified_at = "2026-01-01T00:00:00-08:00", path_collection = pc),
      list(type = "file", id = "3", name = "memo.csv", size = 1L, modified_at = "2026-01-01T00:00:00-08:00", path_collection = pc)),
      next_marker = ""))
  } else if (grepl("^/2.0/files/[0-9]+/content$", req$path)) {
    id <- sub("^/2.0/files/([0-9]+)/content$", "\\1", req$path)
    list(status = 302L, headers = list(Location = paste0(base, "/dl/", id)), body = "")
  } else if (grepl("^/dl/", req$path)) {
    id <- sub("^/dl/", "", req$path)
    list(status = 200L, headers = list(), body = charToRaw(paste0("v\nrow", id, "\n")))
  } else list(status = 404L, headers = list(), body = "{}")
}

test_that("getCSVFilesFromBox and searchAndGetCSVFilesFromBox combine files with an id column", {
  withBoxStub(multiHandler, {
    df <- exploratory::getCSVFilesFromBox(c("1", "2"), c("売上_1月.csv", "売上_2月.csv"), delim = ",", detectStaleFile = FALSE)
    expect_equal(colnames(df), c("id", "v"))
    expect_equal(df$id, c("売上_1月.csv", "売上_2月.csv"))
    expect_equal(df$v, c("row1", "row2"))
    pv <- exploratory::getCSVFilesFromBox(c("1", "2"), c("売上_1月.csv", "売上_2月.csv"), forPreview = TRUE, delim = ",", detectStaleFile = FALSE)
    expect_equal(nrow(pv), 1)
    s <- exploratory::searchAndGetCSVFilesFromBox(folderId = "55", searchKeyword = "売上", delim = ",")
    expect_equal(s$v, c("row1", "row2"))
    expect_error(exploratory::searchAndGetCSVFilesFromBox(folderId = "55", searchKeyword = "zzz", delim = ","), "EXP-DATASRC-5")
  })
})

test_that("guessFileEncodingForBoxFile and getCSVFromBox work", {
  withBoxStub(csvHandler("a,b\n1,2\n"), {
    enc <- exploratory::guessFileEncodingForBoxFile("100")
    expect_true(is.data.frame(enc))
    df <- exploratory::getCSVFromBox("100", delim = ",")
    expect_equal(nrow(df), 1)
  })
})

# ---------------------------------------------------------------- excel

test_that("Excel sheets / file / multi-file / search from Box", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")
  skip_if_not_installed("openxlsx")
  xlsx <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(Sheet_A = data.frame(k = 1:2), `売上` = data.frame(k = 3:5)), xlsx)
  bytes <- readBin(xlsx, "raw", file.size(xlsx))
  handler <- function(req, base) {
    pc <- list(total_count = 0L, entries = list())
    if (req$path == "/2.0/folders/55/items") {
      list(status = 200L, headers = list("Content-Type" = "application/json"),
           body = as.character(jsonlite::toJSON(list(entries = list(
             list(type = "file", id = "300", name = "Book.xlsx", size = 1L, modified_at = "2026-01-01T00:00:00-08:00", path_collection = pc)),
             next_marker = ""), auto_unbox = TRUE)))
    } else if (req$path == "/2.0/files/300") {
      list(status = 200L, headers = list("Content-Type" = "application/json"),
           body = "{\"id\":\"300\",\"name\":\"Book.xlsx\",\"item_status\":\"active\",\"parent\":{\"id\":\"55\"}}")
    } else if (req$path == "/2.0/files/300/content") {
      list(status = 302L, headers = list(Location = paste0(base, "/dl/300")), body = "")
    } else if (req$path == "/dl/300") {
      list(status = 200L, headers = list(), body = bytes)
    } else list(status = 404L, headers = list(), body = "{}")
  }
  withBoxStub(handler, {
    expect_equal(exploratory::getExcelSheetsFromBoxExcelFile("300"), c("Sheet_A", "売上"))
    one <- exploratory::getExcelFileFromBox("300", sheet = "売上", detectStaleFile = FALSE)
    expect_equal(nrow(one), 3)
    many <- exploratory::getExcelFilesFromBox("300", "Book.xlsx", sheet = 1, detectStaleFile = FALSE)
    expect_equal(nrow(many), 2)
    expect_equal(many$id, c("Book.xlsx", "Book.xlsx"))
    srch <- exploratory::searchAndGetExcelFilesFromBox(folderId = "55", searchKeyword = "book", sheet = 1)
    expect_equal(nrow(srch), 2)
    expect_error(exploratory::searchAndGetExcelFilesFromBox(folderId = "55", searchKeyword = "zzz"), "EXP-DATASRC-5")
  })
})

# ---------------------------------------------------------------- no refresh from R

test_that("R never POSTs to /oauth2/token, even with a refresh_token in the token info", {
  handler <- function(req, base) {
    if (req$path == "/2.0/files/100/content") list(status = 401L, headers = list(), body = "{}")
    else list(status = 401L, headers = list(), body = "{}")
  }
  withBoxStub(handler, {
    exploratory::setTokenInfo("box", as.environment(list(access_token = "AT123", token_type = "bearer",
                                                         expires_in = 3600, refresh_token = "RT-SECRET")))
    options(exploratory.box.base_url = sub("/$", "", getOption("exploratory.box.base_url")))
    tok <- exploratory::getBoxToken()
    expect_null(tok$credentials$refresh_token)
    e <- tryCatch(exploratory::listItemsInBox("0"), error = function(e) e)
    expect_true(inherits(e, "box_auth_error"))
    e2 <- tryCatch(exploratory::getCSVFileFromBox("100", delim = ","), error = function(e) e)
    expect_true(inherits(e2, "box_auth_error"))
    log <- stubLog(stubLogFile)
    expect_false(any(grepl("oauth2/token", log, fixed = TRUE)))
    expect_false(any(grepl("^POST", log)))
    expect_false(any(grepl("RT-SECRET", log, fixed = TRUE)))
    # still no refresh after 401 retries: only GETs against /2.0 paths
    expect_true(all(grepl("^GET\t/2.0/", log)))
  })
})
