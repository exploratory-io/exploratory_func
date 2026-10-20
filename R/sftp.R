# SFTP data source (tam#39178).
#
# Files are transferred with libcurl's sftp:// support through the curl package.
# Host keys are always verified strictly against the host keys saved in the
# connection (`hostKeys`), because the scheduler cannot ask a user to accept an
# unknown host key.
#
# Common connection arguments:
#   host       - SFTP server host name or IP address.
#   port       - SFTP server port. Default 22.
#   user       - User name.
#   password   - Password. Used only when keyFile is empty.
#   keyFile    - Path to the SSH private key file (OpenSSH or PEM format).
#   passphrase - Passphrase of the private key. "" if the key is not encrypted.
#   hostKeys   - Host public keys of the server. One "<key type> <base64 key>" per
#                entry, separated by newline, "," or ";". getSFTPHostKeys() returns them.

# libcurl CURLSSH_AUTH_* bit masks.
SFTP_AUTH_PUBLICKEY <- 1L
SFTP_AUTH_PASSWORD <- 2L
SFTP_AUTH_KEYBOARD <- 8L
SFTP_CONNECT_TIMEOUT_SECONDS <- 30
SFTP_DEFAULT_PORT <- 22

#' Split the saved host keys into "<key type> <base64 key>" entries.
#' @param hostKeys Host keys separated by newline, "," or ";".
#' @return Character vector of trimmed, non-empty entries.
sftp_split_host_keys <- function(hostKeys) {
  if (is.null(hostKeys) || length(hostKeys) == 0 || is.na(hostKeys[1])) {
    return(character(0))
  }
  entries <- stringr::str_trim(unlist(stringr::str_split(paste(hostKeys, collapse = "\n"), "[\r\n,;]+")))
  entries[nzchar(entries)]
}

#' Build known_hosts lines for the host from the saved host keys.
#' @param host SFTP server host name or IP address.
#' @param port SFTP server port.
#' @param hostKeys Saved host keys. See sftp_split_host_keys.
#' @return Character vector of known_hosts lines.
sftp_known_hosts_lines <- function(host, port = SFTP_DEFAULT_PORT, hostKeys) {
  entries <- sftp_split_host_keys(hostKeys)
  if (length(entries) == 0) {
    return(character(0)) # paste() would otherwise return the host field alone.
  }
  # known_hosts writes a non-default port as "[host]:port".
  hostField <- if (as.integer(port) == SFTP_DEFAULT_PORT) host else paste0("[", host, "]:", port)
  paste(hostField, entries)
}

#' Compute the SHA256 fingerprint of a host public key, in the same format as ssh-keygen -l.
#' @param key Base64 encoded public key blob.
#' @return Fingerprint like "SHA256:+DiY3wvvV6TuJJhbpZisF/zLDA0zPMSvHdkr4UvCOqU".
sftp_fingerprint <- function(key) {
  digest <- openssl::sha256(openssl::base64_decode(key))
  paste0("SHA256:", sub("=+$", "", openssl::base64_encode(digest)))
}

#' Parse the output of ssh-keyscan.
#' @param lines Lines written to stdout by ssh-keyscan.
#' @return Data frame with keyType, key, fingerprint columns.
sftp_parse_keyscan_output <- function(lines) {
  lines <- stringr::str_trim(lines)
  lines <- lines[nzchar(lines) & !stringr::str_detect(lines, "^#")]
  parts <- stringr::str_split_fixed(lines, "\\s+", 3)
  df <- data.frame(keyType = parts[, 2], key = parts[, 3], stringsAsFactors = FALSE)
  df <- df[nzchar(df$keyType) & nzchar(df$key), , drop = FALSE]
  df$fingerprint <- vapply(df$key, sftp_fingerprint, character(1), USE.NAMES = FALSE)
  df
}

#' Build an sftp:// URL. A relative path is resolved from the user's home directory.
#' @param host SFTP server host name or IP address.
#' @param port SFTP server port.
#' @param path File or folder path. A folder path should end with "/".
#' @return URL string with each path segment percent-encoded.
sftp_url <- function(host, port = SFTP_DEFAULT_PORT, path = "") {
  if (is.null(path) || is.na(path)) {
    path <- ""
  }
  isAbsolute <- stringr::str_detect(path, "^/")
  segments <- stringr::str_split(stringr::str_remove(path, "^/+"), "/")[[1]]
  encoded <- paste(vapply(segments, curl::curl_escape, character(1), USE.NAMES = FALSE), collapse = "/")
  # libcurl resolves "/~/" to the home directory of the user.
  encodedPath <- if (isAbsolute) paste0("/", encoded) else paste0("/~/", encoded)
  hostPart <- if (stringr::str_detect(host, ":")) paste0("[", host, "]") else host # IPv6
  paste0("sftp://", hostPart, ":", as.integer(port), encodedPath)
}

#' Join a folder path and a file name.
#' @param folder Folder path. "" means the home directory.
#' @param name File name.
sftp_join_path <- function(folder, name) {
  if (is.null(folder) || is.na(folder) || !nzchar(folder)) {
    return(name)
  }
  paste0(stringr::str_remove(folder, "/+$"), "/", name)
}

#' Parse a long (ls -l style) directory listing returned by libcurl.
#' @param lines Lines of the listing.
#' @return Data frame with name, isdir, size, lastModified columns. "." and ".." are excluded.
sftp_parse_listing <- function(lines) {
  pattern <- "^([-dlbcps])\\S*\\s+\\d+\\s+\\S+\\s+\\S+\\s+(\\d+)\\s+(\\w{3}\\s+\\d{1,2}\\s+[0-9:]+)\\s(.+)$"
  m <- stringr::str_match(lines, pattern)
  m <- m[!is.na(m[, 1]), , drop = FALSE]
  name <- m[, 5]
  isLink <- m[, 2] == "l"
  # A symbolic link is listed as "name -> target".
  name[isLink] <- stringr::str_remove(name[isLink], " -> .*$")
  df <- data.frame(name = name, isdir = m[, 2] == "d", size = as.numeric(m[, 3]),
                   lastModified = m[, 4], stringsAsFactors = FALSE)
  df[!(df$name %in% c(".", "..")), , drop = FALSE]
}

#' Stop with an EXP-DATASRC error for a failed SFTP operation.
#' @param e Error raised by curl.
#' @param host SFTP server host name or IP address.
#' @param path File or folder path being accessed.
sftp_stop_with_error <- function(e, host, path = "") {
  message <- conditionMessage(e)
  if (stringr::str_detect(message, "(?i)Login denied|Authentication failure|Unable to extract public key|Unable to open private key")) {
    stop(paste0('EXP-DATASRC-38 :: ', jsonlite::toJSON(host), ' :: SFTP authentication failed.'))
  } else if (stringr::str_detect(message, "(?i)SSH remote key was not OK|host key")) {
    stop(paste0('EXP-DATASRC-39 :: ', jsonlite::toJSON(host), ' :: The host key of the SFTP server does not match the saved host key.'))
  } else if (stringr::str_detect(message, "(?i)Remote file not found|No such file")) {
    stop(paste0('EXP-DATASRC-40 :: ', jsonlite::toJSON(c(host, path)), ' :: The file or folder does not exist on the SFTP server.'))
  } else if (stringr::str_detect(message, "(?i)Could not resolve host|Failed to connect|Connection refused|Timeout was reached|timed out")) {
    stop(paste0('EXP-DATASRC-41 :: ', jsonlite::toJSON(host), ' :: Could not connect to the SFTP server.'))
  }
  stop(e)
}

#' Create a curl handle for SFTP and the temporary known_hosts file it uses.
#' The caller must unlink the returned knownHostsFile.
#' @return list(handle, knownHostsFile)
sftp_create_handle <- function(host, port = SFTP_DEFAULT_PORT, user = "", password = "", keyFile = "", passphrase = "", hostKeys = "") {
  if (!("sftp" %in% curl::curl_version()$protocols)) {
    stop('EXP-DATASRC-36 :: [] :: The installed curl package does not support SFTP.')
  }
  knownHostsLines <- sftp_known_hosts_lines(host, port, hostKeys)
  if (length(knownHostsLines) == 0) {
    stop(paste0('EXP-DATASRC-37 :: ', jsonlite::toJSON(host), ' :: The host key of the SFTP server is not set.'))
  }
  dir.create(tempdir(), showWarnings = FALSE) # Rserve on Linux may not create tempdir().
  knownHostsFile <- tempfile(fileext = ".known_hosts")
  writeLines(knownHostsLines, knownHostsFile)

  # fresh_connect/forbid_reuse: never reuse a connection authenticated with other credentials,
  # otherwise a wrong passphrase or a changed host key is not detected.
  handle <- curl::new_handle(username = user, ssh_knownhosts = knownHostsFile,
                             connecttimeout = SFTP_CONNECT_TIMEOUT_SECONDS,
                             fresh_connect = TRUE, forbid_reuse = TRUE)
  if (!is.null(keyFile) && !is.na(keyFile) && nzchar(keyFile)) {
    if (!file.exists(keyFile)) {
      unlink(knownHostsFile)
      stop(paste0('EXP-DATASRC-38 :: ', jsonlite::toJSON(host), ' :: SFTP authentication failed.'))
    }
    # An empty public key file path lets libssh2 derive the public key from the private key.
    curl::handle_setopt(handle, ssh_private_keyfile = path.expand(keyFile), ssh_public_keyfile = "",
                        keypasswd = ifelse(is.null(passphrase) || is.na(passphrase), "", passphrase),
                        ssh_auth_types = SFTP_AUTH_PUBLICKEY)
  } else {
    curl::handle_setopt(handle, password = ifelse(is.null(password) || is.na(password), "", password),
                        ssh_auth_types = SFTP_AUTH_PASSWORD + SFTP_AUTH_KEYBOARD)
  }
  list(handle = handle, knownHostsFile = knownHostsFile)
}

#' Get the host public keys of an SFTP server with ssh-keyscan.
#' @param host SFTP server host name or IP address.
#' @param port SFTP server port.
#' @param timeout Timeout in seconds.
#' @return Data frame with keyType, key, fingerprint columns.
#' @export
getSFTPHostKeys <- function(host, port = SFTP_DEFAULT_PORT, timeout = 10) {
  keyscan <- Sys.which("ssh-keyscan")
  if (!nzchar(keyscan) && .Platform$OS.type == "windows") {
    windowsKeyscan <- file.path(Sys.getenv("SystemRoot", "C:/Windows"), "System32", "OpenSSH", "ssh-keyscan.exe")
    if (file.exists(windowsKeyscan)) {
      keyscan <- windowsKeyscan
    }
  }
  if (!nzchar(keyscan)) {
    stop('EXP-DATASRC-37 :: [] :: ssh-keyscan is not available. Please enter the host key manually.')
  }
  lines <- suppressWarnings(system2(keyscan, c("-T", as.integer(timeout), "-p", as.integer(port), "-t", "ed25519,ecdsa,rsa", shQuote(host)),
                                    stdout = TRUE, stderr = FALSE))
  keys <- sftp_parse_keyscan_output(lines)
  if (nrow(keys) == 0) {
    stop(paste0('EXP-DATASRC-41 :: ', jsonlite::toJSON(host), ' :: Could not connect to the SFTP server.'))
  }
  keys
}

#' List files and folders in a folder of an SFTP server.
#' @param folder Folder path. A relative path is resolved from the home directory. "" means the home directory.
#' @return Data frame with name, isdir, size, lastModified columns.
#' @export
listItemsInSFTP <- function(host, port = SFTP_DEFAULT_PORT, user = "", password = "", keyFile = "", passphrase = "", hostKeys = "", folder = "") {
  conn <- sftp_create_handle(host, port, user, password, keyFile, passphrase, hostKeys)
  on.exit(unlink(conn$knownHostsFile), add = TRUE)
  if (is.null(folder) || is.na(folder)) {
    folder <- ""
  }
  path <- if (nzchar(folder)) paste0(stringr::str_remove(folder, "/+$"), "/") else ""
  res <- tryCatch({
    curl::curl_fetch_memory(sftp_url(host, port, path), handle = conn$handle)
  }, error = function(e) {
    sftp_stop_with_error(e, host, folder)
  })
  sftp_parse_listing(stringr::str_split(rawToChar(res$content), "\r?\n")[[1]])
}

#' Download a file from an SFTP server to a temporary file.
#' @param filePath File path. A relative path is resolved from the home directory.
#' @return Path of the downloaded temporary file.
#' @export
downloadDataFileFromSFTP <- function(host, port = SFTP_DEFAULT_PORT, user = "", password = "", keyFile = "", passphrase = "", hostKeys = "", filePath = "") {
  shouldCacheFile <- getOption("tam.should.cache.datafile")
  # Credentials are not part of the cache key.
  hash <- digest::digest(stringr::str_c(host, port, user, filePath, sep = ":"), "md5", serialize = FALSE)
  cachedPath <- tryCatch(getDownloadedFilePath(hash), error = function(e) NULL)
  if (isTRUE(shouldCacheFile) && !is.null(cachedPath) && file.exists(cachedPath)) {
    return(cachedPath)
  }
  conn <- sftp_create_handle(host, port, user, password, keyFile, passphrase, hostKeys)
  on.exit(unlink(conn$knownHostsFile), add = TRUE)
  ext <- stringr::str_to_lower(tools::file_ext(filePath))
  tmp <- tempfile(fileext = stringr::str_c(".", ext))
  tryCatch({
    curl::curl_download(sftp_url(host, port, filePath), tmp, handle = conn$handle, quiet = TRUE)
  }, error = function(e) {
    sftp_stop_with_error(e, host, filePath)
  })
  if (isTRUE(shouldCacheFile)) {
    setDownloadedFilePath(hash, tmp)
  }
  tmp
}

#' Clear the cached downloaded file of an SFTP file.
#' @export
clearSFTPCacheFile <- function(host, port = SFTP_DEFAULT_PORT, user = "", filePath = "") {
  hash <- digest::digest(stringr::str_c(host, port, user, filePath, sep = ":"), "md5", serialize = FALSE)
  cachedPath <- tryCatch(getDownloadedFilePath(hash), error = function(e) NULL)
  if (!is.null(cachedPath)) {
    unlink(cachedPath)
    rm(list = hash, envir = user_env$downloads)
  }
  invisible(NULL)
}

#' Search files in a folder of an SFTP server. The search keyword is a case insensitive regular expression.
#' @return Character vector of the matched file paths.
sftp_search_files <- function(searchKeyword, host, port, user, password, keyFile, passphrase, hostKeys, folder) {
  items <- listItemsInSFTP(host = host, port = port, user = user, password = password, keyFile = keyFile,
                           passphrase = passphrase, hostKeys = hostKeys, folder = folder)
  items <- items[!items$isdir & stringr::str_detect(items$name, stringr::str_c("(?i)", searchKeyword)), , drop = FALSE]
  if (nrow(items) == 0) {
    stop(paste0('EXP-DATASRC-40 :: ', jsonlite::toJSON(c(host, folder)), ' :: There is no file in the SFTP folder that matches with the specified condition.'))
  }
  # Sort by name so that the merged row order does not depend on the server listing order.
  vapply(sort(items$name), function(name) sftp_join_path(folder, name), character(1), USE.NAMES = FALSE)
}

#' Read multiple files with the reader and merge them. The file name is added as the id column.
sftp_read_and_merge_files <- function(files, forPreview, reader, ...) {
  # for preview mode, just use the first file.
  if (forPreview && length(files) > 0) {
    files <- files[1]
  }
  # set name to the files so that it can be used for the "id" column created by purrr:map_dfr.
  files <- setNames(as.list(files), files)
  df <- purrr::map_dfr(files, reader, ..., .id = "exp.file.id") %>% dplyr::mutate(exp.file.id = basename(exp.file.id))
  id_col <- avoid_conflict(colnames(df), "id")
  df[[id_col]] <- df[["exp.file.id"]]
  df %>% dplyr::select(!!rlang::sym(id_col), dplyr::everything(), -exp.file.id)
}

#'API that imports a CSV file from SFTP.
#'@export
getCSVFileFromSFTP <- function(fileName, host, port = SFTP_DEFAULT_PORT, user = "", password = "", keyFile = "", passphrase = "", hostKeys = "",
                               delim, quote = '"',
                               escape_backslash = FALSE, escape_double = TRUE,
                               col_names = TRUE, col_types = NULL,
                               locale = readr::default_locale(),
                               na = c("", "NA"), quoted_na = TRUE,
                               comment = "", trim_ws = FALSE,
                               skip = 0, n_max = Inf, guess_max = min(1000, n_max),
                               progress = interactive()) {
  filePath <- downloadDataFileFromSFTP(host = host, port = port, user = user, password = password, keyFile = keyFile,
                                       passphrase = passphrase, hostKeys = hostKeys, filePath = fileName)
  exploratory::read_delim_file(filePath, delim = delim, quote = quote,
                               escape_backslash = escape_backslash, escape_double = escape_double,
                               col_names = col_names, col_types = col_types,
                               locale = locale,
                               na = na, quoted_na = quoted_na,
                               comment = comment, trim_ws = trim_ws,
                               skip = skip, n_max = n_max, guess_max = guess_max,
                               progress = progress)
}

#'API that imports multiple same structure CSV files from SFTP and merge it to a single data frame.
#'
#'For col_types parameter, by default it forces character to make sure that merging the CSV based data frames doesn't error out due to column data types mismatch.
#'Once the data frames merging is done, readr::type_convert is called from Exploratory Desktop to restore the column data types.
#'@export
getCSVFilesFromSFTP <- function(files, host, port = SFTP_DEFAULT_PORT, user = "", password = "", keyFile = "", passphrase = "", hostKeys = "",
                                forPreview = FALSE, delim, quote = '"',
                                escape_backslash = FALSE, escape_double = TRUE,
                                col_names = TRUE, col_types = readr::cols(.default = readr::col_character()),
                                locale = readr::default_locale(),
                                na = c("", "NA"), quoted_na = TRUE,
                                comment = "", trim_ws = FALSE,
                                skip = 0, n_max = Inf, guess_max = min(1000, n_max),
                                progress = interactive()) {
  sftp_read_and_merge_files(files, forPreview, exploratory::getCSVFileFromSFTP,
                            host = host, port = port, user = user, password = password, keyFile = keyFile,
                            passphrase = passphrase, hostKeys = hostKeys, delim = delim, quote = quote,
                            escape_backslash = escape_backslash, escape_double = escape_double,
                            col_names = col_names, col_types = col_types, locale = locale,
                            na = na, quoted_na = quoted_na, comment = comment, trim_ws = trim_ws,
                            skip = skip, n_max = n_max, guess_max = guess_max, progress = progress)
}

#'API that searches CSV files in an SFTP folder, then imports and merges them.
#'@export
searchAndGetCSVFilesFromSFTP <- function(searchKeyword, host, port = SFTP_DEFAULT_PORT, user = "", password = "", keyFile = "", passphrase = "", hostKeys = "",
                                         folder = "", forPreview = FALSE, delim, quote = '"',
                                         escape_backslash = FALSE, escape_double = TRUE,
                                         col_names = TRUE, col_types = readr::cols(.default = readr::col_character()),
                                         locale = readr::default_locale(),
                                         na = c("", "NA"), quoted_na = TRUE,
                                         comment = "", trim_ws = FALSE,
                                         skip = 0, n_max = Inf, guess_max = min(1000, n_max),
                                         progress = interactive()) {
  files <- sftp_search_files(searchKeyword, host, port, user, password, keyFile, passphrase, hostKeys, folder)
  getCSVFilesFromSFTP(files = files, host = host, port = port, user = user, password = password, keyFile = keyFile,
                      passphrase = passphrase, hostKeys = hostKeys, forPreview = forPreview, delim = delim, quote = quote,
                      escape_backslash = escape_backslash, escape_double = escape_double,
                      col_names = col_names, col_types = col_types, locale = locale, na = na, quoted_na = quoted_na,
                      comment = comment, trim_ws = trim_ws, skip = skip, n_max = n_max, guess_max = guess_max, progress = progress)
}

#'API that imports a Parquet file from SFTP.
#'@export
getParquetFileFromSFTP <- function(fileName, host, port = SFTP_DEFAULT_PORT, user = "", password = "", keyFile = "", passphrase = "", hostKeys = "",
                                   col_select = NULL, skip_nul = FALSE) {
  filePath <- downloadDataFileFromSFTP(host = host, port = port, user = user, password = password, keyFile = keyFile,
                                       passphrase = passphrase, hostKeys = hostKeys, filePath = fileName)
  exploratory::read_parquet_file(filePath, col_select = col_select, skip_nul = skip_nul)
}

#'API that imports multiple Parquet files from SFTP and merge them.
#'@export
getParquetFilesFromSFTP <- function(files, host, port = SFTP_DEFAULT_PORT, user = "", password = "", keyFile = "", passphrase = "", hostKeys = "",
                                    forPreview = FALSE, col_select = NULL, skip_nul = FALSE) {
  sftp_read_and_merge_files(files, forPreview, exploratory::getParquetFileFromSFTP,
                            host = host, port = port, user = user, password = password, keyFile = keyFile,
                            passphrase = passphrase, hostKeys = hostKeys, col_select = col_select, skip_nul = skip_nul)
}

#'API that searches Parquet files in an SFTP folder, then imports and merges them.
#'@export
searchAndGetParquetFilesFromSFTP <- function(searchKeyword, host, port = SFTP_DEFAULT_PORT, user = "", password = "", keyFile = "", passphrase = "", hostKeys = "",
                                             folder = "", forPreview = FALSE, col_select = NULL, skip_nul = FALSE) {
  files <- sftp_search_files(searchKeyword, host, port, user, password, keyFile, passphrase, hostKeys, folder)
  getParquetFilesFromSFTP(files = files, host = host, port = port, user = user, password = password, keyFile = keyFile,
                          passphrase = passphrase, hostKeys = hostKeys, forPreview = forPreview, col_select = col_select, skip_nul = skip_nul)
}

#'API that imports an Excel file from SFTP.
#'@export
getExcelFileFromSFTP <- function(fileName, host, port = SFTP_DEFAULT_PORT, user = "", password = "", keyFile = "", passphrase = "", hostKeys = "",
                                 sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0, trim_ws = TRUE, n_max = Inf, use_readxl = NULL,
                                 detectDates = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE, check.names = FALSE, tzone = NULL,
                                 convertDataTypeToChar = FALSE, ...) {
  filePath <- downloadDataFileFromSFTP(host = host, port = port, user = user, password = password, keyFile = keyFile,
                                       passphrase = passphrase, hostKeys = hostKeys, filePath = fileName)
  exploratory::read_excel_file(path = filePath, sheet = sheet, col_names = col_names, col_types = col_types, na = na, skip = skip,
                               trim_ws = trim_ws, n_max = n_max, use_readxl = use_readxl, detectDates = detectDates,
                               skipEmptyRows = skipEmptyRows, skipEmptyCols = skipEmptyCols, check.names = check.names,
                               tzone = tzone, convertDataTypeToChar = convertDataTypeToChar, ...)
}

#'API that imports multiple same structure Excel files from SFTP and merge them.
#'@export
getExcelFilesFromSFTP <- function(files, host, port = SFTP_DEFAULT_PORT, user = "", password = "", keyFile = "", passphrase = "", hostKeys = "",
                                  forPreview = FALSE, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0, trim_ws = TRUE, n_max = Inf,
                                  use_readxl = NULL, detectDates = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE, check.names = FALSE,
                                  tzone = NULL, convertDataTypeToChar = TRUE, ...) {
  sftp_read_and_merge_files(files, forPreview, exploratory::getExcelFileFromSFTP,
                            host = host, port = port, user = user, password = password, keyFile = keyFile,
                            passphrase = passphrase, hostKeys = hostKeys, sheet = sheet,
                            col_names = col_names, col_types = col_types, na = na, skip = skip, trim_ws = trim_ws, n_max = n_max,
                            use_readxl = use_readxl, detectDates = detectDates, skipEmptyRows = skipEmptyRows,
                            skipEmptyCols = skipEmptyCols, check.names = check.names, tzone = tzone,
                            convertDataTypeToChar = convertDataTypeToChar, ...)
}

#'API that searches Excel files in an SFTP folder, then imports and merges them.
#'@export
searchAndGetExcelFilesFromSFTP <- function(searchKeyword, host, port = SFTP_DEFAULT_PORT, user = "", password = "", keyFile = "", passphrase = "", hostKeys = "",
                                           folder = "", forPreview = FALSE, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0,
                                           trim_ws = TRUE, n_max = Inf, use_readxl = NULL, detectDates = FALSE, skipEmptyRows = FALSE,
                                           skipEmptyCols = FALSE, check.names = FALSE, tzone = NULL, convertDataTypeToChar = TRUE, ...) {
  files <- sftp_search_files(searchKeyword, host, port, user, password, keyFile, passphrase, hostKeys, folder)
  getExcelFilesFromSFTP(files = files, host = host, port = port, user = user, password = password, keyFile = keyFile,
                        passphrase = passphrase, hostKeys = hostKeys, forPreview = forPreview, sheet = sheet,
                        col_names = col_names, col_types = col_types, na = na, skip = skip, trim_ws = trim_ws, n_max = n_max,
                        use_readxl = use_readxl, detectDates = detectDates, skipEmptyRows = skipEmptyRows,
                        skipEmptyCols = skipEmptyCols, check.names = check.names, tzone = tzone,
                        convertDataTypeToChar = convertDataTypeToChar, ...)
}

#'Wrapper for readxl::excel_sheets to support an Excel file on SFTP.
#'@export
getExcelSheetsFromSFTPExcelFile <- function(fileName, host, port = SFTP_DEFAULT_PORT, user = "", password = "", keyFile = "", passphrase = "", hostKeys = "") {
  filePath <- downloadDataFileFromSFTP(host = host, port = port, user = user, password = password, keyFile = keyFile,
                                       passphrase = passphrase, hostKeys = hostKeys, filePath = fileName)
  readxl::excel_sheets(filePath)
}
