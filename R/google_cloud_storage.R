#' API to download remote data file (excel, csv) from Google Cloud Storage and cache it if necessary
#' it uses tempfile https://stat.ethz.ch/R-manual/R-devel/library/base/html/tempfile.html
#' and a R variable with name of hashed bucket, file are  assigned to the path given by tempfile.
downloadDataFileFromGoogleCloudStorage <- function(bucket, file){
  token <- exploratory:::getGoogleTokenForCloudStorage()
  googleAuthR::gar_auth(token = token, skip_fetch = TRUE)

  shouldCacheFile <- getOption("tam.should.cache.datafile")
  filepath <- NULL
  hash <- digest::digest(stringr::str_c(bucket, file, sep = ":"), "md5", serialize = FALSE)
  tryCatch({
    filepath <- getDownloadedFilePath(hash)
  }, error = function(e){
    # if filePath hash is not set as global variable yet, it raises error that says object not found
    # which can be ignored
    filepath <- NULL
  })
  # Check if cached excel/csv exists for the file path
  if (!is.null(shouldCacheFile) && isTRUE(shouldCacheFile) && !is.null(filepath)) {
    filepath
  } else {
    ext <- stringr::str_to_lower(tools::file_ext(file))
    tmp <- tempfile(fileext = stringr::str_c(".", ext))

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
    googleCloudStorageR::gcs_get_object(file, bucket = bucket, saveToDisk = tmp)
    # cache file
    if(!is.null(shouldCacheFile) && isTRUE(shouldCacheFile)){
      setDownloadedFilePath(hash, tmp)
    }
    tmp
  }
}

#' API to list items inside a Google Cloud Storage Bucket.
#' @param bucket
#' @param prefix
#' @param delimiter
#' @export
listItemsInGoogleCloudStorageBucket <- function(bucket, prefix, delimiter){
  # set token before calling googleCloudStorageR API.
  token <- exploratory:::getGoogleTokenForCloudStorage()
  googleAuthR::gar_auth(token = token, skip_fetch = TRUE)

  googleCloudStorageR::gcs_list_objects(bucket = bucket, detail = "more", prefix = prefix, delimiter = delimiter)
}


#' API to clear Google Cloud Storage cache file
#' @param bucket
#' @param file
#' @export
clearGoogleCloudStorageCacheFile <- function(bucket, file){
  options(tam.should.cache.datafile = FALSE)
  hash <- digest::digest(stringr::str_c(bucket, file, sep = ":"), "md5", serialize = FALSE)
  tryCatch({
    filepath <- eval(as.name(hash))
    do.call(rm, c(as.name(hash)),envir = .GlobalEnv)
    unlink(filepath)
  }, error = function(e){
  })
}

#'Wrapper for readr::guess_encoding to support Google Cloud Storage csv file
#'@param bucket
#'@param file
#'@param n_max
#'@param threshold
#'@export
guessFileEncodingForGoogleCloudStorageFile <- function(bucket, file, n_max = 1e4, threshold = 0.20){
  loadNamespace("readr")
  filePath <- downloadDataFileFromGoogleCloudStorage(bucket = bucket, file = file)
  readr::guess_encoding(filePath, n_max, threshold)

}


#'API that imports a CSV file from Google Cloud Storage.
#'@export
getCSVFileFromGoogleCloudStorage <- function(file, bucket, delim, quote = '"',
                             escape_backslash = FALSE, escape_double = TRUE,
                             col_names = TRUE, col_types = NULL,
                             locale = readr::default_locale(),
                             na = c("", "NA"), quoted_na = TRUE,
                             comment = "", trim_ws = FALSE,
                             skip = 0, n_max = Inf, guess_max = min(1000, n_max),
                             progress = interactive()) {
  tryCatch({
    filePath <- downloadDataFileFromGoogleCloudStorage(bucket = bucket, file = file)
  }, error = function(e) {
    if (stringr::str_detect(e$message, "http_404 The specified bucket does not exist")) {
      # Looking for error that looks like "http_404 The specified bucket does not exist.".
      # This seems to be returned when the bucket itself does not exist.
      stop(paste0('EXP-DATASRC-18 :: ', jsonlite::toJSON(c(bucket)), ' :: The Google Cloud Storage bucket does not exist.'))
    } else if (stringr::str_detect(e$message, "http_404 Unspecified error")) {
      # Looking for error that looks like "http_404 Unspecified error".
      # This seems to be returned when the file does not exist.
      stop(paste0('EXP-DATASRC-19 :: ', jsonlite::toJSON(c(bucket, file)), ' :: There is no file in the Google Cloud Storage bucket that matches with the name.'))
    }
    else {
      stop(e)
    }
  })
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
getCSVFilesFromGoogleCloudStorage <- function(files, bucket, for_preview = FALSE, delim, quote = '"',
                              escape_backslash = FALSE, escape_double = TRUE,
                              col_names = TRUE, col_types = readr::cols(.default = readr::col_character()),
                              locale = readr::default_locale(),
                              na = c("", "NA"), quoted_na = TRUE,
                              comment = "", trim_ws = FALSE,
                              skip = 0, n_max = Inf, guess_max = min(1000, n_max),
                              progress = interactive()) {
  # for preview mode, just use the first file.
  if (for_preview & length(files) > 0) {
    files <- files[1]
  }
  # set name to the files so that it can be used for the "id" column created by purrr:map_dfr.
  files <- setNames(as.list(files), files)
  df <- purrr::map_dfr(files, exploratory::getCSVFileFromGoogleCloudStorage, bucket = bucket, delim = delim, quote = quote,
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

#'API that search files by search keyword then imports multiple same structure CSV files and merge it to a single data frame
#'
#'For col_types parameter, by default it forces character to make sure that merging the CSV based data frames doesn't error out due to column data types mismatch.
# Once the data frames merging is done, readr::type_convert is called from Exploratory Desktop to restore the column data types.

#'@export
searchAndGetCSVFilesFromGoogleCloudStorage <- function(bucket = "", folder = "", search_keyword = "", for_preview = FALSE, delim, quote = '"',
                                       escape_backslash = FALSE, escape_double = TRUE,
                                       col_names = TRUE, col_types = readr::cols(.default = readr::col_character()),
                                       locale = readr::default_locale(),
                                       na = c("", "NA"), quoted_na = TRUE,
                                       comment = "", trim_ws = FALSE,
                                       skip = 0, n_max = Inf, guess_max = min(1000, n_max),
                                       progress = interactive()) {
  # set token before calling googleCloudStorageR API.
  token <- exploratory:::getGoogleTokenForCloudStorage()
  googleAuthR::gar_auth(token = token, skip_fetch = TRUE)

  # search condition is case insensitive. (ref: https://www.regular-expressions.info/modifiers.html, https://stackoverflow.com/questions/5671719/case-insensitive-search-of-a-list-in-r)
  tryCatch({
    files <- googleCloudStorageR::gcs_list_objects(bucket = bucket, detail= "more", prefix = folder, delimiter = "/") %>%
      filter(str_detect(name, stringr::str_c("(?i)", search_keyword)))
  }, error = function(e) {
    if (stringr::str_detect(e$message, "http_404 The specified bucket does not exist")) {
      # Looking for error that looks like "http_404 The specified bucket does not exist.".
      # This seems to be returned when the bucket itself does not exist.
      stop(paste0('EXP-DATASRC-18 :: ', jsonlite::toJSON(c(bucket)), ' :: The Google Cloud Storage bucket does not exist.'))
    } else if (stringr::str_detect(e$message, "http_404 Unspecified error")) {
      # Looking for error that looks like "http_404 Unspecified error".
      # This seems to be returned when the file does not exist.
      stop(paste0('EXP-DATASRC-19 :: ', jsonlite::toJSON(c(bucket)), ' :: There is no file in the Google Cloud Storage bucket that matches with the file name.'))
    }
    else {
      stop(e)
    }
  })
  if (nrow(files) == 0) {
    stop(paste0('EXP-DATASRC-19 :: ', jsonlite::toJSON(bucket), ' :: There is no file in the Google Cloud Storage bucket that matches with the file name.')) # TODO: escape bucket name.
  }
  getCSVFilesFromGoogleCloudStorage(files = files$name, bucket = bucket, for_preview = for_preview, delim = delim, quote = quote,
                    col_names = col_names, col_types = col_types, locale = locale, na = na, quoted_na = quoted_na, comment = comment, trim_ws = trim_ws,
                    skip = skip, n_max = n_max, guess_max = guess_max, progress = progress)

}


#'API that imports a Excel file from Google Cloud Storage.
#'@export
getExcelFileFromGoogleCloudStorage <- function(file, bucket, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0, trim_ws = TRUE, n_max = Inf, use_readxl = NULL, detectDates = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE, check.names = FALSE, tzone = NULL, convertDataTypeToChar = FALSE, ...) {
  tryCatch({
    filePath <- downloadDataFileFromGoogleCloudStorage(bucket = bucket, file = file)
  }, error = function(e) {
    if (stringr::str_detect(e$message, "http_404 The specified bucket does not exist")) {
      # Looking for error that looks like "http_404 The specified bucket does not exist.".
      # This seems to be returned when the bucket itself does not exist.
      stop(paste0('EXP-DATASRC-18 :: ', jsonlite::toJSON(c(bucket)), ' :: The Google Cloud Storage bucket does not exist.'))
    } else if (stringr::str_detect(e$message, "http_404 Unspecified error")) {
      # Looking for error that looks like "http_404 Unspecified error".
      # This seems to be returned when the file does not exist.
      stop(paste0('EXP-DATASRC-19 :: ', jsonlite::toJSON(c(bucket)), ' :: There is no file in the Google Cloud Storage bucket that matches with the file name.'))
    }
    else {
      stop(e)
    }
  })
  exploratory::read_excel_file(path = filePath, sheet = sheet, col_names = col_names, col_types = col_types, na = na, skip = skip, trim_ws = trim_ws, n_max = n_max, use_readxl = use_readxl, detectDates = detectDates, skipEmptyRows =  skipEmptyRows, skipEmptyCols = skipEmptyCols, check.names = check.names, tzone = tzone, convertDataTypeToChar = convertDataTypeToChar, ...)
}

#'API that search files by search keyword then imports multiple same structure Excel files and merge it to a single data frame
#'
#'For col_types parameter, by default it forces character to make sure that merging the Excel based data frames doesn't error out due to column data types mismatch.
# Once the data frames merging is done, readr::type_convert is called from Exploratory Desktop to restore the column data types.

#'@export
searchAndGetExcelFilesFromGoogleCloudStorage <- function(bucket = '', folder = '', search_keyword, for_preview = FALSE, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0, trim_ws = TRUE, n_max = Inf, use_readxl = NULL, detectDates = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE, check.names = FALSE, tzone = NULL, convertDataTypeToChar = TRUE, ...){
  # set token before calling googleCloudStorageR API.
  token <- exploratory:::getGoogleTokenForCloudStorage()
  googleAuthR::gar_auth(token = token, skip_fetch = TRUE)

  # search condition is case insensitive. (ref: https://www.regular-expressions.info/modifiers.html, https://stackoverflow.com/questions/5671719/case-insensitive-search-of-a-list-in-r)
  tryCatch({
    files <- googleCloudStorageR::gcs_list_objects(bucket = bucket, detail= "more", prefix = folder, delimiter = "/") %>%
      filter(str_detect(name, stringr::str_c("(?i)", search_keyword)))
  }, error = function(e) {
    if (stringr::str_detect(e$message, "http_404 The specified bucket does not exist")) {
      # Looking for error that looks like "http_404 The specified bucket does not exist.".
      # This seems to be returned when the bucket itself does not exist.
      stop(paste0('EXP-DATASRC-18 :: ', jsonlite::toJSON(c(bucket)), ' :: The Google Cloud Storage bucket does not exist.'))
    } else if (stringr::str_detect(e$message, "http_404 Unspecified error")) {
      # Looking for error that looks like "http_404 Unspecified error".
      # This seems to be returned when the file does not exist.
      stop(paste0('EXP-DATASRC-19 :: ', jsonlite::toJSON(c(bucket)), ' :: There is no file in the Google Cloud Storage bucket that matches with the file name.'))
    }
    else {
      stop(e)
    }
  })
  if (nrow(files) == 0) {
    stop(paste0('EXP-DATASRC-19 :: ', jsonlite::toJSON(c(bucket)), ' :: There is no file in the Google Cloud Storage bucket that matches with the file name.'))
  }
  exploratory::getExcelFilesFromGoogleCloudStorage(files = files$name, bucket = bucket, for_preview = for_preview, sheet = sheet,
                                   col_names = col_names, col_types = col_types, na = na, skip = skip, trim_ws = trim_ws, n_max = n_max,
                                   use_readxl = use_readxl, detectDates = detectDates, skipEmptyRows = skipEmptyRows, skipEmptyCols = skipEmptyCols,
                                   check.names = check.names, tzone = tzone, convertDataTypeToChar = convertDataTypeToChar, ...)
}

#'API that imports multiple Excel files from AWS S3.
#'@export
getExcelFilesFromGoogleCloudStorage <- function(files, bucket, for_preview = FALSE, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0, trim_ws = TRUE, n_max = Inf, use_readxl = NULL, detectDates = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE, check.names = FALSE, tzone = NULL, convertDataTypeToChar = TRUE, ...) {
  # for preview mode, just use the first file.
  if (for_preview & length(files) > 0) {
    files <- files[1]
  }
  # set name to the files so that it can be used for the "id" column created by purrr:map_dfr.
  files <- setNames(as.list(files), files)
  df <- purrr::map_dfr(files, exploratory::getExcelFileFromGoogleCloudStorage,  bucket = bucket, sheet = sheet,
                       col_names = col_names, col_types = col_types, na = na, skip = skip, trim_ws = trim_ws, n_max = n_max, use_readxl = use_readxl,
                       detectDates = detectDates, skipEmptyRows =  skipEmptyRows, skipEmptyCols = skipEmptyCols, check.names = check.names,
                       tzone = tzone, convertDataTypeToChar = convertDataTypeToChar, .id = "exp.file.id") %>% mutate(exp.file.id = basename(exp.file.id))  # extract file name from full path with basename and create file.id column.
  id_col <- avoid_conflict(colnames(df), "id")
  # copy internal exp.file.id to the id column.
  df[[id_col]] <- df[["exp.file.id"]]
  # drop internal column and move the id column to the very beginning.
  df %>% dplyr::select(!!rlang::sym(id_col), dplyr::everything(), -exp.file.id)
}

#'Wrapper for readxl::excel_sheets to support AWS S3 Excel file
#'@export
getExcelSheetsFromGoogleCloudStorageExcelFile <- function(file, bucket){
  filePath <- downloadDataFileFromGoogleCloudStorage(bucket = bucket, file = file)
  readxl::excel_sheets(filePath)
}

#'API that imports a Parquet file from Google Cloud Storage.
#'@export
getParquetFileFromGoogleCloudStorage <- function(file, bucket, col_select = NULL) {
  tryCatch({
    filePath <- downloadDataFileFromGoogleCloudStorage(bucket = bucket, file = file)
  },  error = function(e) {
    if (stringr::str_detect(e$message, "http_404 The specified bucket does not exist")) {
      # Looking for error that looks like "http_404 The specified bucket does not exist.".
      # This seems to be returned when the bucket itself does not exist.
      stop(paste0('EXP-DATASRC-18 :: ', jsonlite::toJSON(c(bucket)), ' :: The Google Cloud Storage bucket does not exist.'))
    } else if (stringr::str_detect(e$message, "http_404 Unspecified error")) {
      # Looking for error that looks like "http_404 Unspecified error".
      # This seems to be returned when the file does not exist.
      stop(paste0('EXP-DATASRC-19 :: ', jsonlite::toJSON(c(bucket, file)), ' :: There is no file in the Google Cloud Storage bucket that matches with the name.'))
    }
    else {
      stop(e)
    }
  })
  exploratory::read_parquet_file(filePath, col_select = col_select)
}

#'API that imports multiple same structure Parquet files and merge it to a single data frame
#'
#'@export
getParquetFilesFromGoogleCloudStorage <- function(files, bucket, for_preview = FALSE, col_select = NULL) {
  # for preview mode, just use the first file.
  if (for_preview & length(files) > 0) {
    files <- files[1]
  }
  # set name to the files so that it can be used for the "id" column created by purrr:map_dfr.
  files <- setNames(as.list(files), files)
  df <- purrr::map_dfr(files, exploratory::getParquetFileFromGoogleCloudStorage, bucket = bucket, col_select = col_select, .id = "exp.file.id") %>% mutate(exp.file.id = basename(exp.file.id))  # extract file name from full path with basename and create file.id column.
  id_col <- avoid_conflict(colnames(df), "id")
  # copy internal exp.file.id to the id column.
  df[[id_col]] <- df[["exp.file.id"]]
  # drop internal column and move the id column to the very beginning.
  df %>% dplyr::select(!!rlang::sym(id_col), dplyr::everything(), -exp.file.id)
}

#'API that search files by search keyword then imports multiple same structure Parquet files and merge it to a single data frame
#'
#'@export
searchAndGetParquetFilesFromGoogleCloudStorage <- function(bucket = '', folder = '', search_keyword, for_preview = FALSE, col_select = NULL) {
  # set token before calling googleCloudStorageR API.
  token <- exploratory:::getGoogleTokenForCloudStorage()
  googleAuthR::gar_auth(token = token, skip_fetch = TRUE)

  # search condition is case insensitive. (ref: https://www.regular-expressions.info/modifiers.html, https://stackoverflow.com/questions/5671719/case-insensitive-search-of-a-list-in-r)
  tryCatch({
    files <- googleCloudStorageR::gcs_list_objects(bucket = bucket, detail= "more", prefix = folder, delimiter = "/") %>%
      filter(str_detect(name, stringr::str_c("(?i)", search_keyword)))
  }, error = function(e) {
    if (stringr::str_detect(e$message, "http_404 The specified bucket does not exist")) {
      # Looking for error that looks like "http_404 The specified bucket does not exist.".
      # This seems to be returned when the bucket itself does not exist.
      stop(paste0('EXP-DATASRC-18 :: ', jsonlite::toJSON(c(bucket)), ' :: The Google Cloud Storage bucket does not exist.'))
    } else if (stringr::str_detect(e$message, "http_404 Unspecified error")) {
      # Looking for error that looks like "http_404 Unspecified error".
      # This seems to be returned when the file does not exist.
      stop(paste0('EXP-DATASRC-19 :: ', jsonlite::toJSON(c(bucket)), ' :: There is no file in the Google Cloud Storage bucket that matches with the file name.'))
    }
    else {
      stop(e)
    }
  })
  if (nrow(files) == 0) {
    stop(paste0('EXP-DATASRC-4 :: ', jsonlite::toJSON(bucket), ' :: There is no file in the AWS S3 bucket that matches with the specified condition.')) # TODO: escape bucket name.
  }
  getParquetFilesFromGoogleCloudStorage(files = files$name, bucket = bucket, for_preview = for_preview, col_select = col_select)

}

