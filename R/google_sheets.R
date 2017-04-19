#' @export
uploadGoogleSheet <- function(filepath, title){
  if(!requireNamespace("googlesheets")){stop("package googlesheets must be installed.")}
  token <- getGoogleTokenForSheet("")
  googlesheets::gs_auth(token)
  sheet <- googlesheets::gs_upload(filepath, title)
}
