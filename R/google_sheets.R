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
#' @param guessDataType - flag to tell if you want googlesheets::gs_read to guess column data type
#' @export
getGoogleSheet <- function(title, sheetName, skipNRows = 0, treatTheseAsNA = NULL, firstRowAsHeader = TRUE, commentChar, tokenFileId=NULL, guessDataType=TRUE, ...){
  if(!requireNamespace("googlesheets4")){stop("package googlesheets4 must be installed.")}
  if(!requireNamespace("googledrive")){stop("package googledrive must be installed.")}
  if(!requireNamespace("stringr")){stop("package stringr must be installed.")}

  token <- getGoogleTokenForSheet(tokenFileId)
  googlesheets4::sheets_set_token(token)
  googledrive::drive_set_token(token)
  gsheet <- googledrive::drive_get(title)
  col_types <- NULL
  if(!guessDataType) {
    # if guessDataType is FALSE, use character as the default column data type.
    col_types <- c(.default="c")
  }
  if(!is.null(treatTheseAsNA)) {
    df <- gsheet %>% googlesheets4::read_sheet(range = sheetName, skip = skipNRows, na = treatTheseAsNA, col_names = firstRowAsHeader, col_types = col_types)
  } else {
    df <- gsheet %>% googlesheets4::read_sheet(range = sheetName, skip = skipNRows, col_names = firstRowAsHeader, col_types = col_types)
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
  googlesheets4::sheets_find()
}

#' API to get a list of available google sheets
#' @export
getGoogleSheetWorkSheetList <- function(tokenFileId="", title){
  if(!requireNamespace("googlesheets4")){stop("package googlesheets must be installed.")}
  if(!requireNamespace("googledrive")){stop("package googledrive must be installed.")}

  token = getGoogleTokenForSheet(tokenFileId)
  googlesheets4::sheets_set_token(token)
  googledrive::drive_set_token(token)
  sheet <- googledrive::drive_get(title)
  googlesheets4::sheets_sheets(sheet)
}
