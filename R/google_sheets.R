#' API to upload local CSV file to Google Sheets
#' @export
#' @param filepath path of source CSV file that you want to upload to Google Sheet
#' @param title name of the new sheet on Google Sheets.
#' @param overwrite flag to control if you want to overwrite existing sheet
uploadGoogleSheet <- function(filepath, title, overwrite = FALSE){
  if(!requireNamespace("googlesheets")){stop("package googlesheets must be installed.")}
  token <- getGoogleTokenForSheet("")
  googlesheets::gs_auth(token)
  sheet <- googlesheets::gs_upload(filepath, title, overwrite = overwrite)
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
getGoogleSheet <- function(title, sheetName, skipNRows = 0, treatTheseAsNA, firstRowAsHeader = TRUE, commentChar, tokenFileId=NULL, guessDataType=TRUE, ...){
  if(!requireNamespace("googlesheets")){stop("package googlesheets must be installed.")}
  if(!requireNamespace("stringr")){stop("package stringr must be installed.")}

  token <- getGoogleTokenForSheet(tokenFileId)
  googlesheets::gs_auth(token)
  gsheet <- googlesheets::gs_title(title)
  col_types <- NULL
  if(!guessDataType) {
    # if guessDataType is FALSE, use character as the default column data type.
    col_types <- c(.default="c")
  }
  df <- gsheet %>% googlesheets::gs_read(ws = sheetName, skip = skipNRows, na = treatTheseAsNA, col_names = firstRowAsHeader, comment = commentChar, col_types = col_types)
  df
}

#' API to get a list of available google sheets
#' @export
getGoogleSheetList <- function(tokenFileId=""){
  if(!requireNamespace("googlesheets")){stop("package googlesheets must be installed.")}
  token = getGoogleTokenForSheet(tokenFileId)
  googlesheets::gs_auth(token)
  googlesheets::gs_ls()
}

#' API to get a list of available google sheets
#' @export
getGoogleSheetWorkSheetList <- function(tokenFileId="", title){
  if(!requireNamespace("googlesheets")){stop("package googlesheets must be installed.")}
  token = getGoogleTokenForSheet(tokenFileId)
  googlesheets::gs_auth(token)
  sheet <- googlesheets::gs_title(title)
  googlesheets::gs_ws_ls(sheet)
}
