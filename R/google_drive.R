#' API to get a list of files in Google Drive
#' @export
#' @param teamDriveId - in case you want to search for team drive
#' @param path - This should be ID of the folder since searching with folder name doesn't always work as expected.
#' @param type - object type that you want to include in your query result.
listItemsInGoogleDrive <- function(teamDriveId = NULL, path = NULL, type =  c("csv", "tsv", "txt", "folder", "xls", "xlsx")){
  if (!requireNamespace("googledrive")) {
    stop("package googledrive must be installed.")
  }
  # Remember the current config
  currentConfig <- getOption("httr_config")
  # To workaround Error in the HTTP2 framing layer
  # set below config (see https://github.com/jeroen/curl/issues/156)
  httr::set_config(httr::config(http_version = 0))

  token = getGoogleTokenForDrive()
  googledrive::drive_set_token(token)
  # "~/" is special case for listing under My Drive so do not call googledriev::as_id for "~/".
  if (!is.null(path) && path != "~/") {
    path = googledrive::as_id(path)
  }
  # If team id is provided search documents within the team.
  if (teamDriveId != "" && !is.null(teamDriveId)) {
    teamDriveId = googledrive::as_id(teamDriveId)
  }
  result <- tryCatch({
    # To improve performance, only get id, name, mimeType, modifiedTime, size, parents for each file.
    googledrive::drive_ls(path = path, type = type, team_drive = teamDriveId, pageSize = 1000, fields = "files/id, files/name, files/mimeType, files/modifiedTime, files/size, files/parents, nextPageToken")
  }, error = function(e) {
    stop(e)
  }, finally = {
    if (is.null(currentConfig)) {
      httr::reset_config()
    } else {
      httr::set_config(currentConfig)
    }
  })
  result
}

#' API to get a folder details in Google Drive
#' @export
getGoogleDriveFolderDetails <- function(teamDriveId = NULL , path = NULL) {
  if(!requireNamespace("googledrive")) {
    stop("package googledrive must be installed.")
  }
  # Remember the current config
  currentConfig <- getOption("httr_config")
  # To workaround Error in the HTTP2 framing layer
  # set below config (see https://github.com/jeroen/curl/issues/156)
  httr::set_config(httr::config(http_version = 0))

  token = getGoogleTokenForDrive()
  googledrive::drive_set_token(token)
  if (!is.null(path)) {
    path = googledrive::as_id(path)
  }
  df <- NULL
  #if team id is provided search documents within the team.
  # If team id is provided search documents within the team.
  if (teamDriveId != "" && !is.null(teamDriveId)) {
    teamDriveId = googledrive::as_id(teamDriveId)
  }
  df <- tryCatch({
    googledrive::drive_get(team_drive = teamDriveId, id = path)
  }, error = function(e) {
    stop(e)
  }, finally = {
    if (is.null(currentConfig)) {
      httr::reset_config()
    } else {
      httr::set_config(currentConfig)
    }
  })
  dfdetails <- NULL
  if (nrow(df) == 1) {
    dfdetails <- df %>% googledrive::drive_reveal("path")
  }
  dfdetails

}

#'API that imports a CSV file from Google Drive.
#'@export
getCSVFileFromGoogleDrive <- function(fileId, delim, quote = '"',
                             escape_backslash = FALSE, escape_double = TRUE,
                             col_names = TRUE, col_types = NULL,
                             locale = readr::default_locale(),
                             na = c("", "NA"), quoted_na = TRUE,
                             comment = "", trim_ws = FALSE,
                             skip = 0, n_max = Inf, guess_max = min(1000, n_max),
                             progress = interactive()) {
  filePath <- downloadDataFileFromGoogleDrive(fileId = fileId, type = "csv")
  exploratory::read_delim_file(filePath, delim = delim, quote = quote,
                               escape_backslash = escape_backslash, escape_double = escape_double,
                               col_names = col_names, col_types = col_types,
                               locale = locale,
                               na = na, quoted_na = quoted_na,
                               comment = comment, trim_ws = trim_ws,
                               skip = skip, n_max = n_max, guess_max = guess_max,
                               progress = progress)
}

#'API that imports multiple same structure CSV files and merge it to a single data frame
#'
#'For col_types parameter, by default it forces character to make sure that merging the CSV based data frames doesn't error out due to column data types mismatch.
# Once the data frames merging is done, readr::type_convert is called from Exploratory Desktop to restore the column data types.

#'@export
getCSVFilesFromGoogleDrive <- function(fileIds, fileNames, delim, quote = '"',
                              escape_backslash = FALSE, escape_double = TRUE,
                              col_names = TRUE, col_types = readr::cols(.default = readr::col_character()),
                              locale = readr::default_locale(),
                              na = c("", "NA"), quoted_na = TRUE,
                              comment = "", trim_ws = FALSE,
                              skip = 0, n_max = Inf, guess_max = min(1000, n_max),
                              progress = interactive()) {
  # set name to the files so that it can be used for the "id" column created by purrr:map_dfr.
  files <- setNames(as.list(fileIds), fileNames)
  df <- purrr::map_dfr(files, exploratory::getCSVFileFromGoogleDrive, delim = delim, quote = quote,
                       escape_backslash = escape_backslash, escape_double = escape_double,
                       col_names = col_names, col_types = col_types,
                       locale = locale,
                       na = na, quoted_na = quoted_na,
                       comment = comment, trim_ws = trim_ws,
                       skip = skip, n_max = n_max, guess_max = guess_max,
                       progress = progress, .id = "exp.file.id") %>% mutate(exp.file.id = basename(exp.file.id))  # extract file name from full path with basename and create file.id column.
  id_col <- avoid_conflict(colnames(df), "id")
  # copy internal exp.file.id to the id column.
  df[[id_col]] <- df[["exp.file.id"]]
  # drop internal column and move the id column to the very beginning.
  df %>% dplyr::select(!!rlang::sym(id_col), dplyr::everything(), -exp.file.id)
}

#'@export
searchAndGetCSVFilesFromGoogleDrive <- function(folderId = NULL, searchKeyword = "", delim, quote = '"',
                                       escape_backslash = FALSE, escape_double = TRUE,
                                       col_names = TRUE, col_types = readr::cols(.default = readr::col_character()),
                                       locale = readr::default_locale(),
                                       na = c("", "NA"), quoted_na = TRUE,
                                       comment = "", trim_ws = FALSE,
                                       skip = 0, n_max = Inf, guess_max = min(1000, n_max),
                                       progress = interactive()) {
  items <- exploratory::listItemsInGoogleDrive(path = folderId, type =  c("csv", "tsv", "txt"))
  if (searchKeyword != "") {
    # search condition is case insensitive. (ref: https://www.regular-expressions.info/modifiers.html, https://stackoverflow.com/questions/5671719/case-insensitive-search-of-a-list-in-r)
    items <- items %>% filter(stringr::str_detect(name, stringr::str_c("(?i)", searchKeyword)))
  }
  exploratory::getCSVFilesFromGoogleDrive(items$id, items$name, delim = delim, quote = quote, escape_backslash = escape_backslash, escape_double = escape_double, col_names = col_names,
                                          col_types = col_types, locale = locale, na = na, quoted_na = quoted_na, comment = comment, trim_ws = trim_ws, skip = skip, n_max = n_max,
                                          guess_max = guess_max, progress = progress)

}


#'API that imports a Excel file from Google Drive.
#'@export
getExcelFileFromGoogleDrive <- function(fileId, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0, trim_ws = TRUE, n_max = Inf, use_readxl = NULL, detectDates = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE, check.names = FALSE, tzone = NULL, convertDataTypeToChar = FALSE, ...) {
  filePath <- downloadDataFileFromGoogleDrive(fileId = fileId, type = "xlsx")
  exploratory::read_excel_file(path = filePath, sheet = sheet, col_names = col_names, col_types = col_types,
                               na = na, skip = skip, trim_ws = trim_ws, n_max = n_max, use_readxl = use_readxl,
                               detectDates = detectDates, skipEmptyRows =  skipEmptyRows, skipEmptyCols = skipEmptyCols,
                               check.names = FALSE, tzone = tzone, convertDataTypeToChar = convertDataTypeToChar, ...)
}


#'API that imports multiple Excel files from Google Drive
#'@export
getExcelFilesFromGoogleDrive <- function(fileIds, fileNames, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0, trim_ws = TRUE, n_max = Inf, use_readxl = NULL, detectDates = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE, check.names = FALSE, convertDataTypeToChar = TRUE, tzone = NULL, ...) {
  # set name to the files so that it can be used for the "id" column created by purrr:map_dfr.
  files <- setNames(as.list(fileIds), fileNames)
  df <- purrr::map_dfr(files, exploratory::getExcelFileFromGoogleDrive, sheet = sheet,
                       col_names = col_names, col_types = col_types, na = na, skip = skip, trim_ws = trim_ws, n_max = n_max, use_readxl = use_readxl,
                       detectDates = detectDates, skipEmptyRows =  skipEmptyRows, skipEmptyCols = skipEmptyCols, check.names = FALSE,
                       tzone = tzone, convertDataTypeToChar = convertDataTypeToChar, .id = "exp.file.id") %>% mutate(exp.file.id = basename(exp.file.id))  # extract file name from full path with basename and create file.id column.
  id_col <- avoid_conflict(colnames(df), "id")
  # copy internal exp.file.id to the id column.
  df[[id_col]] <- df[["exp.file.id"]]
  # drop internal column and move the id column to the very beginning.
  df %>% dplyr::select(!!rlang::sym(id_col), dplyr::everything(), -exp.file.id)
}

#'API that imports multiple Excel files from Google Drive
#'@export
searchAndGetExcelFilesFromGoogleDrive <- function(folderId = NULL, searchKeyword = "", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0, trim_ws = TRUE, n_max = Inf, use_readxl = NULL, detectDates = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE, check.names = FALSE, convertDataTypeToChar = TRUE, tzone = NULL, ...) {
  # set name to the files so that it can be used for the "id" column created by purrr:map_dfr.
  items <- exploratory::listItemsInGoogleDrive(path = folderId, type = c("xls", "xlsx"))
  if (searchKeyword != "") {
    # search condition is case insensitive. (ref: https://www.regular-expressions.info/modifiers.html, https://stackoverflow.com/questions/5671719/case-insensitive-search-of-a-list-in-r)
    items <- items %>% filter(stringr::str_detect(name, stringr::str_c("(?i)", searchKeyword)))
  }
  exploratory::getExcelFilesFromGoogleDrive(items$id, items$name, sheet = sheet, col_names = col_names, col_types = col_types, na = na, skip = skip,
                                            trim_ws = trim_ws, n_max = n_max, use_readxl = use_readxl, detectDates = detectDates, skipEmptyRows = skipEmptyRows,
                                            skipEmptyCols = skipEmptyCols, check.names = check.names, convertDataTypeToChar = convertDataTypeToChar, tzone = tzone, ...)
}

#'Wrapper for readxl::excel_sheets to support Google Drive Excel file
#'@export
getExcelSheetsFromGoogleDriveExcelFile <- function(fileId){
  filePath <- downloadDataFileFromGoogleDrive(fileId = fileId, type = "xlsx")
  readxl::excel_sheets(filePath)
}

#'Wrapper for readr::guess_encoding to support Google Drive csv file
#'@export
guessFileEncodingForGoogleDriveFile <- function(fileId, n_max = 1e4, threshold = 0.20){
  filePath <- downloadDataFileFromGoogleDrive(fileId = fileId, type = "csv")
  readr::guess_encoding(filePath, n_max, threshold)
}

#' API to download remote data file (excel, csv) from Google Drive and cache it if necessary
#' it uses tempfile https://stat.ethz.ch/R-manual/R-devel/library/base/html/tempfile.html
#' and a R variable with name of hashed region, bucket, key, secret, fileName are  assigned to the path given by tempfile.
downloadDataFileFromGoogleDrive <- function(fileId, type = "csv"){
  # Remember the current config
  currentConfig <- getOption("httr_config")
  # To workaround Error in the HTTP2 framing layer
  # set below config (see https://github.com/jeroen/curl/issues/156)
  httr::set_config(httr::config(http_version = 0))
  token <- exploratory::getGoogleTokenForDrive()

  googledrive::drive_set_token(token)
  shouldCacheFile <- getOption("tam.should.cache.datafile")
  filepath <- NULL
  hash <- digest::digest(stringr::str_c(fileId, type), "md5", serialize = FALSE)
  tryCatch({
    filepath <- eval(as.name(hash))
  }, error = function(e){
    # if filePath hash is not set as global variable yet, it raises error that says object not found
    # which can be ignored
    filepath <- NULL
  })
  # Check if cached excel/csv exists for the filepath
  if (!is.null(shouldCacheFile) && isTRUE(shouldCacheFile) && !is.null(filepath)) {
    filepath
  } else {
    tmp <- tempfile(fileext = stringr::str_c(".", type))

    # In case of using Rserve on linux, somehow it doesn't create a temporary
    # directory specified by tempdir() which is used as a part of temp file
    # path generated by tempfile(). So if you try to use that temp file path,
    # dump some data into it for example, it will fail because no such path
    # found. This function fails with the same reason at download.file below.
    #
    # It works fine from the R command line on linux, and it works
    # fine all the time on Mac and Windows regardless Rserv or not.
    #
    # The following command is harmless even if you have the directory already.
    # http://stackoverflow.com/questions/4216753/check-existence-of-directory-and-create-if-doesnt-exist
    dir.create(tempdir(), showWarnings = FALSE)

    # download file to temporary location
    tryCatch ({
      googledrive::drive_download(googledrive::as_id(fileId), overwrite = TRUE, path = tmp)
    }, error = function(e) {
      stop(e)
    }, finally = {
      if (is.null(currentConfig)) {
        httr::reset_config()
      } else {
        httr::set_config(currentConfig)
      }
    })
    # cache file
    if (!is.null(shouldCacheFile) && isTRUE(shouldCacheFile)) {
      assign(hash, tmp, envir = .GlobalEnv)
    }
    tmp
  }
}

#' API to clear Google Drive cache file
#' @param fileId
#' @param type
#' @export
clearGoogleDriveCacheFile <- function(fileId, type = "csv"){
  options(tam.should.cache.datafile = FALSE)
  hash <- digest::digest(stringr::str_c(fileId, type), "md5", serialize = FALSE)
  tryCatch({
    filepath <- eval(as.name(hash))
    do.call(rm, c(as.name(hash)),envir = .GlobalEnv)
    unlink(filepath)
  }, error = function(e){
  })
}


