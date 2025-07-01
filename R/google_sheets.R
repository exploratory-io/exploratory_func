#' API to upload local CSV file to Google Sheets
#'
#' exploratory::setTokenInfo needs to be called to set OAuth token before using this API.
#'
#' @export
#' @param filepath path of source CSV file that you want to upload to Google Sheet
#' @param title name of the new sheet on Google Sheets.
#' @param overwrite flag to control if you want to overwrite existing sheet
uploadGoogleSheet <- function(filepath, title, overwrite = FALSE, sheetName = NULL) {
  if (!requireNamespace("googlesheets4", quietly = TRUE)) {
    stop("The 'googlesheets4' package must be installed.")
  }
  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop("The 'googledrive' package must be installed.")
  }

  currentConfig <- getOption("httr_config")

  # Ensure that the HTTP configuration is reset upon function exit
  on.exit({
    if (is.null(currentConfig)) {
      httr::reset_config()
    } else {
      httr::set_config(currentConfig)
    }
  }, add = TRUE)

  # Set HTTP configuration to workaround the HTTP2 framing layer error
  httr::set_config(httr::config(http_version = 0))

  # Obtain the Google token
  token <- getGoogleTokenForSheet("")
  googlesheets4::sheets_set_token(token)
  googledrive::drive_set_token(token)

  # Upload the file to Google Drive as a spreadsheet
  sheet <- googledrive::drive_upload(filepath, title, type = "spreadsheet", overwrite = overwrite)

  # Rename the sheet tab if 'sheetName' is provided
  if (!is.null(sheetName)) {
    googlesheets4::sheet_rename(sheet, sheet = NULL, sheetName)
  }

  return(sheet)
}


#' API to update existing Google Sheet with the local CSV file.
#' @export
#' @param filepath path of source CSV file that you want to update with
#' @param id id of the existing sheet on Google Sheets.
updateGoogleSheet <- function(filepath, id, overwrite = FALSE) {
  if (!requireNamespace("googlesheets4", quietly = TRUE)) {
    stop("The 'googlesheets4' package must be installed.")
  }
  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop("The 'googledrive' package must be installed.")
  }

  currentConfig <- getOption("httr_config")

  # Ensure that the HTTP configuration is reset upon function exit
  on.exit({
    if (is.null(currentConfig)) {
      httr::reset_config()
    } else {
      httr::set_config(currentConfig)
    }
  }, add = TRUE)

  # Set HTTP configuration to workaround the HTTP2 framing layer error
  httr::set_config(httr::config(http_version = 0))

  # Obtain the Google token
  token <- getGoogleTokenForSheet("")
  googlesheets4::sheets_set_token(token)
  googledrive::drive_set_token(token)

  # Update the Google Sheet
  sheet <- googledrive::drive_update(file = googledrive::as_id(id), media = filepath)

  return(sheet)
}

#' API to upload data frame to Google Sheets.
#'
#' @export
#' @param df - data frame
#' @param type - either "newSpreadSheet", "overrideSpreadSheet", "newWorkSheet", and "appendToWorkSheet"
#' @param spreadSheetName - name of the spread sheet (when creating a new spread sheet)
#' @param spreadSheetId - sheet id (when updating an existing spread sheet)
#' @param workSheet - name of the worksheet
#'
uploadDataToGoogleSheets <- function(df, type = "newSpreadSheet", spreadSheetName = "", workSheet = "") {
  if (!requireNamespace("googlesheets4", quietly = TRUE)) {
    stop("The 'googlesheets4' package must be installed.")
  }

  currentConfig <- getOption("httr_config")

  # Ensure that the HTTP configuration is reset upon function exit
  on.exit({
    if (is.null(currentConfig)) {
      httr::reset_config()
    } else {
      httr::set_config(currentConfig)
    }
  }, add = TRUE)

  # Set HTTP configuration to workaround the HTTP2 framing layer error
  httr::set_config(httr::config(http_version = 0))

  # Obtain the Google token
  token <- getGoogleTokenForSheet("")
  googlesheets4::sheets_set_token(token)

  # Perform actions based on the 'type' parameter
  if (type == "newSpreadSheet") {
    sheetsList <- list(df)
    names(sheetsList) <- c(workSheet)
    googlesheets4::gs4_create(spreadSheetName, sheets = sheetsList)
  } else if (type %in% c("overrideSpreadSheet", "newWorkSheet")) {
    googlesheets4::sheet_write(df, spreadSheetName, sheet = workSheet)
  } else if (type == "appendToWorkSheet") {
    googlesheets4::sheet_append(spreadSheetName, df, sheet = workSheet)
  } else {
    stop("Invalid 'type' parameter provided.")
  }
}
#' API to normalize data for Google Sheets Export
#' @param df - data frame
#
normalizeDataForGoogleSheetsExport <- function (df) {
  requireNamespace("dplyr")
  requireNamespace("lubridate")
  requireNamespace("bit64")
  df <- df %>%
   mutate(
     across(where(is.numeric), ~ifelse(is.infinite(.), as.numeric(NA), .)),
     across(where(lubridate::is.difftime), ~ as.numeric(.)),
     across(where(lubridate::is.period), ~ as.numeric(.)),
     across(where(bit64::is.integer64), ~ as.numeric(.))
   )
  df
}


#' API to get google sheet data
#' @export
#' @param title name of a sheet on Google Sheets.
#' @param sheetName name of a sheet of the Google Sheets
#' @param skipNRows - rows to skip loading
#' @param treatTheseAsNA - character vector that each item represents NA
#' @param firstRowAsHeader - argument to control if you want to treat first row as header
#' @param commentChar - treat the character as comment.
#' @param tokenFileId - No longer used. It was kept for backward compatibility for the old Desktop Versions that don't handle OAuth token in server side.
#' @param guessDataType - flag to tell if you want googlesheets::gs_read to guess column data type
#' @param tzone - timezone
#' @param id - ID of the sheet
#' @param col_types - column data type
#' @param guess_max - maximum number of rows to guess column data type (default is Inf)
#' @export
getGoogleSheet <- function(title, sheetName, skipNRows = 0, treatTheseAsNA = NULL, firstRowAsHeader = TRUE, commentChar, tokenFileId = NULL, guessDataType = TRUE, tzone = NULL, id = NULL, col_types = NULL, guess_max = Inf, ...){
  if(!requireNamespace("googlesheets4")){stop("package googlesheets4 must be installed.")}
  if(!requireNamespace("googledrive")){stop("package googledrive must be installed.")}
  if(!requireNamespace("stringr")){stop("package stringr must be installed.")}
  # Remember the current config
  currentConfig <- getOption("httr_config")

  tryCatch({
    # To workaround Error in the HTTP2 framing layer
    # set below config (see https://github.com/jeroen/curl/issues/156)
    httr::set_config(httr::config(http_version = 0))

    token <- getGoogleTokenForSheet(tokenFileId)
    googlesheets4::sheets_set_token(token)
    googledrive::drive_set_token(token)
    # For some of the sheets, below API does not return result with title so try it with the id if id parameter is passed.
    # If id is not provided, try it with title.
    # Exploratory Desktop might send an empty string for id for the existing data source, so check it too.
    if(!is.null(id) && id != "") {
      gsheet <- googledrive::drive_get(id = id)
    } else {
      gsheet <- googledrive::drive_get(title)
    }
    if(!is.null(col_types)) {
      # if col_types is provided, use it.
    } else if(!guessDataType) {
      # if guessDataType is FALSE, use character as the default column data type.
      col_types <- c(.default="c")
    } else {
      # if guessDataType is TRUE, use the default column data type.
      col_types <- NULL
    }
    # The "na" argument of googlesheets4::read_sheet does not accept null,
    # so if the treatTheseAsNA is null, do not pass it to googlesheets4::read_sheet
    if(!is.null(treatTheseAsNA)) {
      df <- gsheet %>% googlesheets4::read_sheet(range = sheetName, skip = skipNRows, na = treatTheseAsNA, col_names = firstRowAsHeader, col_types = col_types, guess_max = guess_max)
    } else {
      df <- gsheet %>% googlesheets4::read_sheet(range = sheetName, skip = skipNRows, col_names = firstRowAsHeader, col_types = col_types, guess_max = guess_max)
    }
    if(!is.null(tzone)) { # if timezone is specified, apply the timezone to POSIXct columns
      df <- df %>% dplyr::mutate(across(where(lubridate::is.POSIXct), ~ lubridate::force_tz(.x, tzone=tzone)))
    }
    # For list columns, change the data type to characters
    df <- df %>% dplyr::mutate(across(where(is.list), as.character))

    df
  }, error = function(e) {
    # When a worksheet is missing, Google returns "`range` doesn't appear to be a range in A1 notation" error so detect it.
    if (stringr::str_detect(e$message, "`range` doesn't appear to be a range in A1 notation, a named range")) {
      stop(paste0('EXP-DATASRC-16 :: ', jsonlite::toJSON(c(title, sheetName)), ' :: There is no such work sheet in the Google Sheets.'))
    } else if (stringr::str_detect(e$message, "Client error: \\(404\\) (Not Found|NOT_FOUND)")) { # When a sheet does not exist, Google Returns (404) Not Found (or NOT_FOUND) so detect it.
      stop(paste0('EXP-DATASRC-17 :: ', jsonlite::toJSON(c(title, sheetName)), ' :: There is no such sheet in the Google Sheets.'))
    } else {
      stop(e)
    }
  }, finally = function(e){
    if (is.null(currentConfig)) {
      httr::reset_config()
    } else {
      httr::set_config(currentConfig)
    }
  })
}

#' API to get a list of available google sheets
#' @export
getGoogleSheetList <- function(tokenFileId="", teamDriveId="", n_max=5000, pattern = ""){
  if(!requireNamespace("googlesheets4")){stop("package googlesheets4 must be installed.")}
  if(!requireNamespace("googledrive")){stop("package googledrive must be installed.")}

  currentConfig <- getOption("httr_config")
  tryCatch({
    # To workaround Error in the HTTP2 framing layer
    # set below config (see https://github.com/jeroen/curl/issues/156)
    httr::set_config(httr::config(http_version = 0))

    token = getGoogleTokenForSheet(tokenFileId)
    googlesheets4::sheets_set_token(token)
    googledrive::drive_set_token(token)
    sheetlist <- NULL
    if(teamDriveId != "" && !is.null(teamDriveId)) {
      # To improve performance, only get id, name and canEdit for each spreadsheet.
      # NOTE: googledrive changed team_drive argument to shared_drive
      sheetlist <- googledrive::drive_find(type = "spreadsheet", shared_drive=googledrive::as_id(teamDriveId) ,pageSize=1000, fields="files/id, files/name, files/capabilities/canEdit, nextPageToken", n_max = n_max, q = pattern)
    } else { #if team id is provided search documents within the team.
      # To improve performance, only get id, name and canEdit for each spreadsheet.
      sheetlist <- googledrive::drive_find(type = "spreadsheet", pageSize=1000, fields="files/id, files/name, files/capabilities/canEdit, nextPageToken", n_max = n_max, q = pattern )
    }
    sheetlist
  }, finally = function(){
    if (is.null(currentConfig)) {
      httr::reset_config()
    } else {
      httr::set_config(currentConfig)
    }
  })
}

#' API to get a list of available google sheets
#' @export
#' @param tokenFileId - No longer used. It was kept for backward compatibility for the old Desktop Versions that don't handle OAuth token in server side.
#' @param title - title of the sheet
#' @param id - ID of the sheet
#' @export
getGoogleSheetWorkSheetList <- function(tokenFileId = "", title, id = NULL){
  if(!requireNamespace("googlesheets4")){stop("package googlesheets4 must be installed.")}
  if(!requireNamespace("googledrive")){stop("package googledrive must be installed.")}

  currentConfig <- getOption("httr_config")
  tryCatch({
    # To workaround Error in the HTTP2 framing layer
    # set below config (see https://github.com/jeroen/curl/issues/156)
    httr::set_config(httr::config(http_version = 0))

    token = getGoogleTokenForSheet(tokenFileId)
    googlesheets4::sheets_set_token(token)
    googledrive::drive_set_token(token)
    # For some of the sheets, below API does not return result with title so try it with the id if id parameter is passed.
    # If id is not provided, try it with title.
    # Exploratory Desktop might send an empty string for id for the existing data source, so check it too.
    if(!is.null(id) && id != "") {
      sheet <- googledrive::drive_get(id = id)
    } else {
      sheet <- googledrive::drive_get(title)
    }
    googlesheets4::sheet_names(sheet)
  },finally = function(){
    if (is.null(currentConfig)) {
      httr::reset_config()
    } else {
      httr::set_config(currentConfig)
    }
  })
}

#' API to get Team Drives from Google Drive.
#' @export
#' @param tokenFileId - No longer used. It was kept for backward compatibility for the old Desktop Versions that don't handle OAuth token in server side.
#' @param useGoogleSheetsToken - Since this API is used for both Google Sheets Data Source and Google Drive Data Source from Exploratory Desktop,
#' set this parameter as TRUE to make it work with Google Sheets Data Source case.
#' If this parameter is set as TRUE, it uses OAuth token set for Google Sheets Data Source.
#' @export
getTeamDrives <- function(tokenFileId = "", useGoogleSheetsToken = TRUE){
  if(!requireNamespace("googledrive")){stop("package googledrive must be installed.")}
  currentConfig <- getOption("httr_config")
  tryCatch({
    # To workaround Error in the HTTP2 framing layer
    # set below config (see https://github.com/jeroen/curl/issues/156)
    httr::set_config(httr::config(http_version = 0))

    token <- NULL
    if (useGoogleSheetsToken) {
      token <- getGoogleTokenForSheet(tokenFileId)
    } else {
      token <- getGoogleTokenForDrive(tokenFileId)
    }
    googledrive::drive_set_token(token)
    # NOTE: googledrive changed API name to shared_drive_find
    googledrive::shared_drive_find()
  }, finally = function(){
    if (is.null(currentConfig)) {
      httr::reset_config()
    } else {
      httr::set_config(currentConfig)
    }
  })
}
