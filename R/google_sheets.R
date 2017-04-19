#' API to upload local CSV file to Google Sheets
#' @export
#' @param filepath path of source CSV file that you want to upload to Google Sheet
#' @param title name of the new sheet on Google Sheets.
uploadGoogleSheet <- function(filepath, title){
  if(!requireNamespace("googlesheets")){stop("package googlesheets must be installed.")}
  token <- getGoogleTokenForSheet("")
  googlesheets::gs_auth(token)
  sheet <- googlesheets::gs_upload(filepath, title)
}

#' @export
getGoogleSheet <- function(title, sheetNumber, skipNRows, treatTheseAsNA, firstRowAsHeader, commentChar, tokenFileId=NULL){
  if(!requireNamespace("googlesheets")){stop("package googlesheets must be installed.")}
  token <- getGoogleTokenForSheet(tokenFileId)
  googlesheets::gs_auth(token)
  gsheet <- googlesheets::gs_title(title)
  df <- gsheet %>% googlesheets::gs_read(ws = sheetNumber, skip = skipNRows, na = treatTheseAsNA, col_names = firstRowAsHeader, comment = commentChar)
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
