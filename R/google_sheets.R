#' API to upload local CSV file to Google Sheets
#'
#' exploratory::setTokenInfo needs to be called to set OAuth token before using this API.
#'
#' @export
#' @param filepath path of source CSV file that you want to upload to Google Sheet
#' @param title name of the new sheet on Google Sheets.
#' @param overwrite flag to control if you want to overwrite existing sheet
uploadGoogleSheet <- function(filepath, title, overwrite = FALSE){
  if(!requireNamespace("googlesheets4")){stop("package googlesheets4 must be installed.")}
  if(!requireNamespace("googledrive")){stop("package googledrive must be installed.")}
  # the first argument of getGoogleTokenForSheet is no longer used but pass empty string to make it work.
  token <- getGoogleTokenForSheet("")
  googlesheets4::sheets_set_token(token)
  googledrive::drive_set_token(token)
  sheet <- googledrive::drive_upload(filepath, title, type = "spreadsheet", overwrite = overwrite)
}

#' API to update existing Google Sheet with the local CSV file.
#' @export
#' @param filepath path of source CSV file that you want to update with
#' @param id id of the existing sheet on Google Sheets.
updateGoogleSheet <- function(filepath, id, overwrite = FALSE){
  if(!requireNamespace("googlesheets4")){stop("package googlesheets4 must be installed.")}
  if(!requireNamespace("googledrive")){stop("package googledrive must be installed.")}

  token <- getGoogleTokenForSheet("")
  googlesheets4::sheets_set_token(token)
  googledrive::drive_set_token(token)
  sheet <- googledrive::drive_update(file = googledrive::as_id(id), media = filepath)
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
  if(!requireNamespace("googlesheets4")){stop("package googlesheets4 must be installed.")}
  token <- getGoogleTokenForSheet("")
  googlesheets4::sheets_set_token(token)
  if (type == "newSpreadSheet") {
    sheetsList <- list(df)
    names(sheetsList) <- c(workSheet)
    googlesheets4::gs4_create(spreadSheetName, sheets = sheetsList)
  } else if (type == "overrideSpreadSheet" || type == "newWorkSheet") {
    googlesheets4::sheet_write(df, spreadSheetName, sheet = workSheet)
  } else if (type == "appendToWorkSheet") {
    googlesheets4::sheet_append(spreadSheetName, df, sheet = workSheet)
  }
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
#' @export
getGoogleSheet <- function(title, sheetName, skipNRows = 0, treatTheseAsNA = NULL, firstRowAsHeader = TRUE, commentChar, tokenFileId = NULL, guessDataType = TRUE, tzone = NULL, id = NULL, ...){
  if(!requireNamespace("googlesheets4")){stop("package googlesheets4 must be installed.")}
  if(!requireNamespace("googledrive")){stop("package googledrive must be installed.")}
  if(!requireNamespace("stringr")){stop("package stringr must be installed.")}

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
  col_types <- NULL
  if(!guessDataType) {
    # if guessDataType is FALSE, use character as the default column data type.
    col_types <- c(.default="c")
  }
  # The "na" argument of googlesheets4::read_sheet does not accept null,
  # so if the treatTheseAsNA is null, do not pass it to googlesheets4::read_sheet
  if(!is.null(treatTheseAsNA)) {
    df <- gsheet %>% googlesheets4::read_sheet(range = sheetName, skip = skipNRows, na = treatTheseAsNA, col_names = firstRowAsHeader, col_types = col_types)
  } else {
    df <- gsheet %>% googlesheets4::read_sheet(range = sheetName, skip = skipNRows, col_names = firstRowAsHeader, col_types = col_types)
  }
  if(!is.null(tzone)) { # if timezone is specified, apply the timezeon to POSIXct columns
    df <- df %>% dplyr::mutate_if(lubridate::is.POSIXct, funs(lubridate::force_tz(., tzone=tzone)))
  }
  # For list columns, change the data type to characters
  df <- df %>% dplyr::mutate_if(is.list, funs(as.character))

  df
}

#' API to get a list of available google sheets
#' @export
getGoogleSheetList <- function(tokenFileId="", teamDriveId="", n_max=5000, pattern = ""){
  if(!requireNamespace("googlesheets4")){stop("package googlesheets4 must be installed.")}
  if(!requireNamespace("googledrive")){stop("package googledrive must be installed.")}

  token = getGoogleTokenForSheet(tokenFileId)
  googlesheets4::sheets_set_token(token)
  googledrive::drive_set_token(token)
  if(teamDriveId != "" && !is.null(teamDriveId)) {
    # To improve performance, only get id, name and canEdit for each spreadsheet.
    # NOTE: googledrive changed team_drive argument to shared_drive
    googledrive::drive_find(type = "spreadsheet", shared_drive=googledrive::as_id(teamDriveId) ,pageSize=1000, fields="files/id, files/name, files/capabilities/canEdit, nextPageToken", n_max = n_max, q = pattern)
  } else { #if team id is provided search documents within the team.
    # To improve performance, only get id, name and canEdit for each spreadsheet.
    googledrive::drive_find(type = "spreadsheet", pageSize=1000, fields="files/id, files/name, files/capabilities/canEdit, nextPageToken", n_max = n_max, q = pattern )
  }
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
  token <- NULL
  if (useGoogleSheetsToken) {
    token <- getGoogleTokenForSheet(tokenFileId)
  } else {
    token <- getGoogleTokenForDrive(tokenFileId)
  }
  googledrive::drive_set_token(token)
  # NOTE: googledrive changed API name to shared_drive_find
  googledrive::shared_drive_find()
}
