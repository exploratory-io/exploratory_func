#' Create text from pdf file
#' @param path Path(URL) of pdf files.
#' @export
parse_pdf <- function(path){
  loadNamespace("purrr")
  # factar to character
  path <- as.character(path)
  pdf_parse_item <- function(path){
    loadNamespace("httr")
    loadNamespace("pdftools")
    loadNamespace("stringr")
    url_regex <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
    result <- sapply(path, function(path){
      if(grepl(url_regex, path)){
        # for url
        tryCatch({
          response <- httr::GET(path)
          if(httr::status_code(response) == 200){
            text <- pdftools::pdf_text(httr::content(response))
            if(length(text) > 1){
              # collapse pages
              text <- stringr::str_c(text, collapse = "\n")
            }
            as.character(text)
          } else {
            character(0)
          }
        }, warning = function(w) {
          character(0)
        }, error = function(e) {
          character(0)
        })
      } else {
        text <- pdftools::pdf_text(path)
        if(length(text) > 1){
          # collapse pages
          stringr::str_c(text, collapse = "\n")
        } else {
          text
        }
      }
    })
    result
  }
  parsed_text_list <- purrr::map(path, pdf_parse_item)
  parsed_text <- as.character(parsed_text_list)
  parsed_text
}
