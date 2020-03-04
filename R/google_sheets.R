#' API to upload local CSV file to Google Sheets
#' @export
#' @param filepath path of source CSV file that you want to upload to Google Sheet
#' @param title name of the new sheet on Google Sheets.
#' @param overwrite flag to control if you want to overwrite existing sheet
uploadGoogleSheet <- function(filepath, title, overwrite = FALSE){
  if(!requireNamespace("googlesheets4")){stop("package googlesheets4 must be installed.")}
  if(!requireNamespace("googledrive")){stop("package googledrive must be installed.")}

  token <- getGoogleTokenForSheet("")
  googlesheets4::sheets_set_token(token)
  googledrive::drive_set_token(token)
  sheet <- googledrive::drive_upload(filepath, title, type = "spreadsheet", overwrite = overwrite)
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
  df
}

#' API to get a list of available google sheets
#' @export
getGoogleSheetList <- function(tokenFileId=""){
  if(!requireNamespace("googlesheets4")){stop("package googlesheets4 must be installed.")}
  if(!requireNamespace("googledrive")){stop("package googledrive must be installed.")}

  token = getGoogleTokenForSheet(tokenFileId)
  googlesheets4::sheets_set_token(token)
  googledrive::drive_set_token(token)
  # To improve peformance, only get id, name and canEdit for each spreadsheet.
  googledrive::drive_find(type = "spreadsheet", pageSize=1000, fields="files/id, files/name, files/capabilities/canEdit, nextPageToken")
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
  if(!is.null(id) && id != "") {
    sheet <- googledrive::drive_get(id = id)
  } else {
    sheet <- googledrive::drive_get(title)
  }
  googlesheets4::sheets_sheets(sheet)
}
