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
#' @param colTypes - column types you want to assign.
#' @export
getGoogleSheet <- function(title, sheetName, skipNRows = 0, treatTheseAsNA, firstRowAsHeader = TRUE, commentChar, tokenFileId=NULL, colTypes=NULL, ...){
  if(!requireNamespace("googlesheets")){stop("package googlesheets must be installed.")}
  if(!requireNamespace("stringr")){stop("package stringr must be installed.")}

  token <- getGoogleTokenForSheet(tokenFileId)
  googlesheets::gs_auth(token)
  gsheet <- googlesheets::gs_title(title)
  col_type <- colTypes
  if(!is.null(colTypes)) {
    # From Exploratory Desktop, colTypes parameter value is passed as a string like "colA='c', colB='c'"
    # so need to convert it to an expression like c(colA='c', colB='c')
    col_types <- rlang::parse_expr(stringr::str_c("c(", colTypes, ")"))
  }
  df <- gsheet %>% googlesheets::gs_read(ws = sheetName, skip = skipNRows, na = treatTheseAsNA, col_names = firstRowAsHeader, comment = commentChar, col_types = col_types )
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
