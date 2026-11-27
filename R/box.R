# Box (https://www.box.com) data source support (tam#25706). Mirrors R/google_drive.R.
#
# Box refresh tokens are single use, so R never refreshes the token: it only sends the access token
# that Exploratory set via setTokenInfo("box", ...) (see getBoxToken in R/oauth.R).
# The API base URL can be overridden with options(exploratory.box.base_url = ...) so that tests can
# point it at a local stub server. Paths below include the "/2.0" API version prefix.

BOX_DEFAULT_BASE_URL <- "https://api.box.com"
BOX_MAX_RETRIES <- 3
BOX_MAX_RETRY_WAIT_SECONDS <- 120
BOX_MAX_ACCEPTED_WAITS <- 30
BOX_LIST_FIELDS <- "id,name,type,size,modified_at,path_collection"

# Base URL of the Box API without a trailing slash.
boxBaseUrl <- function() {
  sub("/+$", "", getOption("exploratory.box.base_url", BOX_DEFAULT_BASE_URL))
}

# Build a classed error. class is the specific Box error class, e.g. "box_auth_error".
boxError <- function(message, class) {
  structure(class = c(class, "box_error", "error", "condition"),
            list(message = message, call = NULL))
}

# Sleep for the number of seconds the server asked for (Retry-After), capped to a sane maximum.
boxSleep <- function(seconds) {
  if (is.null(seconds) || length(seconds) != 1 || is.na(seconds) || seconds < 0) {
    seconds <- 1
  }
  Sys.sleep(min(seconds, BOX_MAX_RETRY_WAIT_SECONDS))
}

# Seconds from the Retry-After header of a response (NA when absent / not numeric).
boxRetryAfter <- function(res) {
  value <- httr::headers(res)[["retry-after"]]
  if (is.null(value)) return(NA_real_)
  suppressWarnings(as.numeric(value))
}

# Parse the JSON body of a response (empty list when there is no usable body).
boxParseJson <- function(res) {
  text <- tryCatch(httr::content(res, as = "text", encoding = "UTF-8"), error = function(e) "")
  if (is.null(text) || is.na(text) || !nzchar(text)) return(list())
  tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(e) list())
}

# Single place where the Box API is called.
# path: e.g. "/2.0/folders/0/items". query: named list. extra: httr config (e.g. write_disk) passed through.
# Sends "Authorization: Bearer <access_token>" itself and never tries to refresh the token.
# 429 -> waits Retry-After and retries (max BOX_MAX_RETRIES times). 401 -> error of class box_auth_error.
# 403 -> box_forbidden_error, 404 -> box_not_found_error. Other 4xx/5xx -> box_error.
# 2xx (including 202) responses are returned as they are; callers decide what 202 means.
boxRequest <- function(path, query = list(), token = NULL, ...) {
  if (is.null(token)) {
    token <- getBoxToken()
  }
  accessToken <- token$credentials$access_token
  url <- paste0(boxBaseUrl(), path)
  attempt <- 0
  repeat {
    res <- httr::GET(url, query = query,
                     httr::add_headers(Authorization = paste("Bearer", accessToken)),
                     httr::config(http_version = 0), ...)
    status <- httr::status_code(res)
    if (status == 429) {
      if (attempt >= BOX_MAX_RETRIES) {
        stop(boxError("Box request limit was reached (429). Please wait a while and run again.", "box_rate_limit_error"))
      }
      attempt <- attempt + 1
      boxSleep(boxRetryAfter(res))
      next
    }
    if (status == 401) {
      stop(boxError("Box login has expired or was revoked (401). Please log in to Box again.", "box_auth_error"))
    }
    if (status == 403) {
      stop(boxError("You do not have permission to open this Box file or folder (403).", "box_forbidden_error"))
    }
    if (status == 404) {
      stop(boxError("The Box file or folder was not found (404).", "box_not_found_error"))
    }
    if (status >= 400) {
      detail <- boxParseJson(res)$message
      stop(boxError(paste0("Box request failed (", status, ")", if (!is.null(detail)) paste0(": ", detail) else "", "."), "box_http_error"))
    }
    return(res)
  }
}

# Entries of a path_collection as a data frame with id and name.
boxPathCollectionToDf <- function(pathCollection) {
  entries <- pathCollection$entries
  if (is.null(entries) || length(entries) == 0) {
    return(data.frame(id = character(0), name = character(0), stringsAsFactors = FALSE))
  }
  data.frame(id = vapply(entries, function(x) as.character(x$id), character(1)),
             name = vapply(entries, function(x) as.character(x$name), character(1)),
             stringsAsFactors = FALSE)
}

#' API to get a list of files and folders in a Box folder
#' @export
#' @param folderId - ID of the folder. "0" is the root ("All Files").
#' @param type - what to include. "folder" and file extensions (csv, tsv, txt, xls, xlsx).
#' @param n_max - number of max items to return
#' @param pattern - if specified, only items whose name contains it (case insensitive, not a regular expression) are returned.
#' @return data frame with id, name, type ("file" or "folder"), size, modified_at (as returned by Box) and path.
listItemsInBox <- function(folderId = "0", type = c("csv", "tsv", "txt", "folder", "xls", "xlsx"), n_max = 5000, pattern = "") {
  if (is.null(folderId) || length(folderId) != 1 || is.na(folderId) || !nzchar(folderId)) {
    folderId <- "0"
  }
  token <- getBoxToken()
  rows <- list()
  count <- 0
  marker <- NULL
  repeat {
    query <- list(usemarker = "true", limit = 1000, fields = BOX_LIST_FIELDS)
    if (!is.null(marker)) query$marker <- marker
    res <- tryCatch({
      boxRequest(paste0("/2.0/folders/", folderId, "/items"), query = query, token = token)
    }, box_not_found_error = function(e) {
      stop('EXP-DATASRC-6 :: [] :: The specified Box folder does not exist.')
    })
    body <- boxParseJson(res)
    for (entry in body$entries) {
      itemName <- as.character(entry$name)
      isFolder <- identical(entry$type, "folder")
      isFile <- identical(entry$type, "file")
      ext <- tolower(tools::file_ext(itemName))
      if (!((isFolder && "folder" %in% type) || (isFile && ext %in% type))) next
      if (!is.null(pattern) && nzchar(pattern) &&
          !stringr::str_detect(itemName, stringr::fixed(pattern, ignore_case = TRUE))) next
      pathNames <- boxPathCollectionToDf(entry$path_collection)$name
      rows[[length(rows) + 1]] <- data.frame(
        id = as.character(entry$id),
        name = itemName,
        type = as.character(entry$type),
        size = if (is.null(entry$size)) NA_real_ else as.numeric(entry$size),
        modified_at = if (is.null(entry$modified_at)) NA_character_ else as.character(entry$modified_at),
        path = paste(pathNames, collapse = "/"),
        stringsAsFactors = FALSE)
      count <- count + 1
    }
    marker <- body$next_marker
    if (count >= n_max || !is.character(marker) || length(marker) != 1 || !nzchar(marker)) break
  }
  if (length(rows) == 0) {
    return(tibble::tibble(id = character(0), name = character(0), type = character(0),
                          size = numeric(0), modified_at = character(0), path = character(0)))
  }
  items <- tibble::as_tibble(dplyr::bind_rows(rows))
  if (nrow(items) > n_max) items <- items[seq_len(n_max), ]
  items
}

#' API to get folder details in Box (used for the breadcrumb)
#' @export
#' @param folderId - ID of the folder. "0" is the root.
#' @return list with id, name and path_collection (data frame of id and name of the ancestors).
getBoxFolderDetails <- function(folderId = "0") {
  if (is.null(folderId) || length(folderId) != 1 || is.na(folderId) || !nzchar(folderId)) {
    folderId <- "0"
  }
  res <- tryCatch({
    boxRequest(paste0("/2.0/folders/", folderId), query = list(fields = "id,name,path_collection"))
  }, box_not_found_error = function(e) {
    stop('EXP-DATASRC-6 :: [] :: The specified Box folder does not exist.')
  })
  body <- boxParseJson(res)
  list(id = as.character(body$id), name = as.character(body$name),
       path_collection = boxPathCollectionToDf(body$path_collection))
}

# Pure: exact-name match among listed items; the most recently modified wins. Returns id or NULL.
selectMostRecentBoxFileId <- function(items, fileName) {
  if (is.null(items) || is.null(fileName) || length(fileName) != 1 || is.na(fileName) || !nzchar(fileName)) {
    return(NULL)
  }
  if (!("name" %in% colnames(items)) || !("id" %in% colnames(items)) || nrow(items) == 0) {
    return(NULL)
  }
  matched <- items[items$name == fileName & !is.na(items$name), , drop = FALSE]
  if (nrow(matched) == 0) return(NULL)
  if ("modified_at" %in% colnames(matched)) {
    mtime <- suppressWarnings(lubridate::ymd_hms(as.character(matched$modified_at), quiet = TRUE))
    matched <- matched[order(mtime, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  }
  as.character(matched$id[[1]])
}

# Re-resolve a file by name in a folder (used when the stored file id is gone / trashed). Returns id or NULL.
resolveBoxFileIdByName <- function(fileName, folderId, type = "csv") {
  hasValue <- function(x) !is.null(x) && length(x) == 1 && !is.na(x) && nzchar(x)
  if (!hasValue(fileName) || !hasValue(folderId)) return(NULL)
  listType <- if (any(type %in% c("xls", "xlsx"))) c("xls", "xlsx") else c("csv", "tsv", "txt")
  items <- tryCatch(listItemsInBox(folderId, type = listType), box_auth_error = function(e) stop(e), error = function(e) NULL)
  selectMostRecentBoxFileId(items, fileName)
}

# Pure: status from the parsed body of GET /files/{id}?fields=item_status,name,parent.
# Anything but item_status == "active" (trashed, deleted) is stale.
extractBoxFileStatus <- function(meta) {
  if (is.null(meta) || length(meta) == 0) {
    return(list(trashedOrMissing = TRUE, name = NA_character_, parentFolderId = NA_character_))
  }
  status <- meta$item_status
  name <- if (is.null(meta$name)) NA_character_ else as.character(meta$name)
  parent <- if (is.null(meta$parent) || is.null(meta$parent$id)) NA_character_ else as.character(meta$parent$id)
  list(trashedOrMissing = !is.null(status) && !identical(status, "active"), name = name, parentFolderId = parent)
}

# I/O wrapper: status of the stored file id. A 404 is stale. A 401 is raised (login expired). Any other
# error is treated as not stale so a transient failure does not turn a working download into an error.
getBoxFileStatus <- function(fileId) {
  notStale <- list(trashedOrMissing = FALSE, name = NA_character_, parentFolderId = NA_character_)
  if (is.null(fileId) || length(fileId) != 1 || is.na(fileId) || !nzchar(fileId)) return(notStale)
  tryCatch({
    res <- boxRequest(paste0("/2.0/files/", fileId), query = list(fields = "item_status,name,parent"))
    extractBoxFileStatus(boxParseJson(res))
  }, box_not_found_error = function(e) {
    list(trashedOrMissing = TRUE, name = NA_character_, parentFolderId = NA_character_)
  }, box_auth_error = function(e) {
    stop(e)
  }, error = function(e) {
    notStale
  })
}

#' API to download remote data file (excel, csv) from Box and cache it if necessary.
#' Same cache scheme as downloadDataFileFromGoogleDrive: md5(fileId + type) is the key and the cache is
#' used only when option tam.should.cache.datafile is TRUE.
downloadDataFileFromBox <- function(fileId, type = "csv", fileName = NULL, folderId = NULL,
                                    fallbackAttempted = FALSE, detectStaleFile = FALSE) {
  hasValue <- function(x) !is.null(x) && length(x) == 1 && !is.na(x) && nzchar(x)
  notFoundError <- function() stop('EXP-DATASRC-9 :: [] :: The specified Box file does not exist.')
  # Re-resolve by name inside the folder; returns the downloaded path, or raises EXP-DATASRC-9.
  retryByName <- function(name, folder) {
    newFileId <- if (hasValue(name) && hasValue(folder)) resolveBoxFileIdByName(name, folder, type = type) else NULL
    if (!is.null(newFileId) && newFileId != fileId) {
      return(downloadDataFileFromBox(fileId = newFileId, type = type, fileName = name, folderId = folder,
                                     fallbackAttempted = TRUE, detectStaleFile = FALSE))
    }
    notFoundError()
  }
  # A trashed file may still be fetched by id, so check the status first (item_status != active => stale).
  if (isTRUE(detectStaleFile) && !isTRUE(fallbackAttempted) && hasValue(fileId)) {
    status <- getBoxFileStatus(fileId)
    if (isTRUE(status$trashedOrMissing)) {
      resolvedName <- if (hasValue(fileName)) fileName else status$name
      resolvedFolder <- if (hasValue(folderId)) folderId else status$parentFolderId
      return(retryByName(resolvedName, resolvedFolder))
    }
  }
  shouldCacheFile <- getOption("tam.should.cache.datafile")
  hash <- digest::digest(stringr::str_c(fileId, type), "md5", serialize = FALSE)
  filepath <- getDownloadedFilePath(hash)
  if (isTRUE(shouldCacheFile) && !is.null(filepath) && file.exists(filepath)) {
    return(filepath)
  }
  tmp <- tempfile(fileext = stringr::str_c(".", type))
  dir.create(tempdir(), showWarnings = FALSE)
  token <- getBoxToken()
  waits <- 0
  repeat {
    res <- tryCatch({
      boxRequest(paste0("/2.0/files/", fileId, "/content"), query = list(), token = token, httr::write_disk(tmp, overwrite = TRUE))
    }, box_not_found_error = function(e) {
      if (!isTRUE(fallbackAttempted)) {
        return(structure(list(), class = "box_stale_marker"))
      }
      notFoundError()
    })
    if (inherits(res, "box_stale_marker")) {
      return(retryByName(fileName, folderId))
    }
    # 202: the file is not ready yet. Wait for Retry-After and ask again.
    if (httr::status_code(res) == 202) {
      waits <- waits + 1
      if (waits > BOX_MAX_ACCEPTED_WAITS) {
        stop("The Box file is still being prepared for download. Please try again later.")
      }
      boxSleep(boxRetryAfter(res))
      next
    }
    break
  }
  if (isTRUE(shouldCacheFile)) {
    setDownloadedFilePath(hash, tmp)
  }
  tmp
}

#' API to clear Box cache file
#' @param fileId
#' @param type
#' @export
clearBoxCacheFile <- function(fileId, type = "csv") {
  options(tam.should.cache.datafile = FALSE)
  hash <- digest::digest(stringr::str_c(fileId, type), "md5", serialize = FALSE)
  filepath <- getDownloadedFilePath(hash)
  if (!is.null(filepath)) {
    unlink(filepath)
    rm(list = hash, envir = user_env$downloads)
  }
  invisible(NULL)
}

#' Plugin entry point for the Box data source
#' @export
getCSVFromBox <- function(fileId, delim = ",", ...) {
  exploratory::getCSVFileFromBox(fileId, delim = delim, ...)
}

#' API that imports a CSV file from Box.
#' @export
getCSVFileFromBox <- function(fileId, delim, quote = '"',
                              escape_backslash = FALSE, escape_double = TRUE,
                              col_names = TRUE, col_types = NULL,
                              locale = readr::default_locale(),
                              na = c("", "NA"), quoted_na = TRUE,
                              comment = "", trim_ws = FALSE,
                              skip = 0, n_max = Inf, guess_max = min(1000, n_max),
                              progress = interactive(),
                              fileName = NULL, folderId = NULL,
                              detectStaleFile = TRUE, forPreview = FALSE) {
  # forPreview is accepted for caller compatibility; the preview limit is applied through n_max.
  filePath <- downloadDataFileFromBox(fileId = fileId, type = "csv",
                                      fileName = fileName, folderId = folderId,
                                      detectStaleFile = detectStaleFile)
  exploratory::read_delim_file(filePath, delim = delim, quote = quote,
                               escape_backslash = escape_backslash, escape_double = escape_double,
                               col_names = col_names, col_types = col_types,
                               locale = locale,
                               na = na, quoted_na = quoted_na,
                               comment = comment, trim_ws = trim_ws,
                               skip = skip, n_max = n_max, guess_max = guess_max,
                               progress = progress)
}

# Bind per-file data frames; the file name goes into an id column placed first (as the Google Drive functions do).
boxBindFileFrames <- function(frames, fileNames) {
  df <- dplyr::bind_rows(frames, .id = "exp.file.id")
  # .id holds the list names, i.e. the file names (they are not paths, so no basename()).
  id_col <- avoid_conflict(colnames(df), "id")
  df[[id_col]] <- df[["exp.file.id"]]
  df %>% dplyr::select(!!rlang::sym(id_col), dplyr::everything(), -exp.file.id)
}

#' API that imports multiple same structure CSV files from Box and merges them into a single data frame
#' @export
getCSVFilesFromBox <- function(fileIds, fileNames, forPreview = FALSE, delim, quote = '"',
                               escape_backslash = FALSE, escape_double = TRUE,
                               col_names = TRUE, col_types = readr::cols(.default = readr::col_character()),
                               locale = readr::default_locale(),
                               na = c("", "NA"), quoted_na = TRUE,
                               comment = "", trim_ws = FALSE,
                               skip = 0, n_max = Inf, guess_max = min(1000, n_max),
                               progress = interactive(), detectStaleFile = TRUE) {
  if (forPreview & length(fileNames) > 0 & length(fileIds) > 0) {
    fileNames <- fileNames[1]
    fileIds <- fileIds[1]
  }
  frames <- purrr::map2(fileIds, fileNames, function(fileId, fileName) {
    exploratory::getCSVFileFromBox(fileId, delim = delim, quote = quote,
                                   escape_backslash = escape_backslash, escape_double = escape_double,
                                   col_names = col_names, col_types = col_types,
                                   locale = locale, na = na, quoted_na = quoted_na,
                                   comment = comment, trim_ws = trim_ws,
                                   skip = skip, n_max = n_max, guess_max = guess_max,
                                   progress = progress, fileName = fileName,
                                   detectStaleFile = detectStaleFile)
  })
  boxBindFileFrames(setNames(frames, fileNames), fileNames)
}

# List a folder for the search functions and apply the keyword filter; raises EXP-DATASRC-5 when empty.
boxSearchItems <- function(folderId, searchKeyword, type) {
  if (is.null(folderId) || length(folderId) != 1 || is.na(folderId) || !nzchar(folderId)) folderId <- "0"
  items <- exploratory::listItemsInBox(folderId, type = type)
  if (!is.null(searchKeyword) && nzchar(searchKeyword)) {
    # Same as the Google Drive search: case insensitive regular expression on the file name.
    items <- items[stringr::str_detect(items$name, stringr::str_c("(?i)", searchKeyword)), ]
  }
  if (nrow(items) == 0) {
    stop('EXP-DATASRC-5 :: [] :: There is no file in the Box folder that matches with the specified condition.')
  }
  items
}

#' API that imports CSV files whose name matches with the keyword in a Box folder
#' @export
searchAndGetCSVFilesFromBox <- function(folderId = NULL, searchKeyword = "", forPreview = FALSE, delim, quote = '"',
                                        escape_backslash = FALSE, escape_double = TRUE,
                                        col_names = TRUE, col_types = readr::cols(.default = readr::col_character()),
                                        locale = readr::default_locale(),
                                        na = c("", "NA"), quoted_na = TRUE,
                                        comment = "", trim_ws = FALSE,
                                        skip = 0, n_max = Inf, guess_max = min(1000, n_max),
                                        progress = interactive()) {
  items <- boxSearchItems(folderId, searchKeyword, c("csv", "tsv", "txt"))
  # ids were just listed live, so skip the per-file stale check.
  exploratory::getCSVFilesFromBox(items$id, items$name, forPreview = forPreview, delim = delim, quote = quote,
                                  escape_backslash = escape_backslash, escape_double = escape_double,
                                  col_names = col_names, col_types = col_types, locale = locale, na = na,
                                  quoted_na = quoted_na, comment = comment, trim_ws = trim_ws, skip = skip,
                                  n_max = n_max, guess_max = guess_max, progress = progress, detectStaleFile = FALSE)
}

#' API that imports an Excel file from Box.
#' @export
getExcelFileFromBox <- function(fileId, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0, trim_ws = TRUE, n_max = Inf, use_readxl = NULL, detectDates = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE, check.names = FALSE, tzone = NULL, convertDataTypeToChar = FALSE, ..., fileName = NULL, folderId = NULL, detectStaleFile = TRUE) {
  filePath <- downloadDataFileFromBox(fileId = fileId, type = "xlsx",
                                      fileName = fileName, folderId = folderId,
                                      detectStaleFile = detectStaleFile)
  exploratory::read_excel_file(path = filePath, sheet = sheet, col_names = col_names, col_types = col_types,
                               na = na, skip = skip, trim_ws = trim_ws, n_max = n_max, use_readxl = use_readxl,
                               detectDates = detectDates, skipEmptyRows = skipEmptyRows, skipEmptyCols = skipEmptyCols,
                               check.names = FALSE, tzone = tzone, convertDataTypeToChar = convertDataTypeToChar, ...)
}

#' API that imports multiple Excel files from Box
#' @export
getExcelFilesFromBox <- function(fileIds, fileNames, forPreview = FALSE, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0, trim_ws = TRUE, n_max = Inf, use_readxl = NULL, detectDates = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE, check.names = FALSE, convertDataTypeToChar = TRUE, tzone = NULL, ..., detectStaleFile = TRUE) {
  if (forPreview & length(fileNames) > 0 & length(fileIds) > 0) {
    fileNames <- fileNames[1]
    fileIds <- fileIds[1]
  }
  frames <- purrr::map2(fileIds, fileNames, function(fileId, fileName) {
    exploratory::getExcelFileFromBox(fileId, sheet = sheet, col_names = col_names, col_types = col_types, na = na,
                                     skip = skip, trim_ws = trim_ws, n_max = n_max, use_readxl = use_readxl,
                                     detectDates = detectDates, skipEmptyRows = skipEmptyRows, skipEmptyCols = skipEmptyCols,
                                     check.names = FALSE, tzone = tzone, convertDataTypeToChar = convertDataTypeToChar,
                                     detectStaleFile = detectStaleFile, fileName = fileName, ...)
  })
  boxBindFileFrames(setNames(frames, fileNames), fileNames)
}

#' API that imports Excel files whose name matches with the keyword in a Box folder
#' @export
searchAndGetExcelFilesFromBox <- function(folderId = NULL, searchKeyword = "", forPreview = FALSE, sheet = 1, col_names = TRUE,
                                          col_types = NULL, na = "", skip = 0, trim_ws = TRUE, n_max = Inf, use_readxl = NULL,
                                          detectDates = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE, check.names = FALSE,
                                          convertDataTypeToChar = TRUE, tzone = NULL, ...) {
  items <- boxSearchItems(folderId, searchKeyword, c("xls", "xlsx"))
  exploratory::getExcelFilesFromBox(items$id, items$name, forPreview = forPreview, sheet = sheet, col_names = col_names,
                                    col_types = col_types, na = na, skip = skip, trim_ws = trim_ws, n_max = n_max,
                                    use_readxl = use_readxl, detectDates = detectDates, skipEmptyRows = skipEmptyRows,
                                    skipEmptyCols = skipEmptyCols, check.names = check.names,
                                    convertDataTypeToChar = convertDataTypeToChar, tzone = tzone, detectStaleFile = FALSE, ...)
}

#' Wrapper for readxl::excel_sheets to support Box Excel file
#' @export
getExcelSheetsFromBoxExcelFile <- function(fileId) {
  filePath <- downloadDataFileFromBox(fileId = fileId, type = "xlsx", detectStaleFile = TRUE)
  readxl::excel_sheets(filePath)
}

#' Wrapper for readr::guess_encoding to support Box csv file
#' @export
guessFileEncodingForBoxFile <- function(fileId, n_max = 1e4, threshold = 0.20) {
  filePath <- downloadDataFileFromBox(fileId = fileId, type = "csv", detectStaleFile = TRUE)
  readr::guess_encoding(filePath, n_max, threshold)
}
