#' Convert fullwidth (zenkaku) and halfwidth (hankaku) Japanese characters
#' @param text Character vector to convert characters
#' @param to "han" or "zen". "han" is for converting to halfwidth and "zen" is to halfwidth.
#' @export
convert_zen_han <- function(text, to = "han") {
  loadNamespace("stringr")
  # list got from http://so-zou.jp/web-app/text/fullwidth-halfwidth/
  df <- zen_han_mapping

  replace <- if (to == "han"){
    ret <- df$han
    names(ret) <- df$zen
    ret
  } else if (to == "zen"){
    ret <- df$zen
    names(ret) <- df$escaped_han
    ret
  } else {
    stop("to argument must be \"han\" or \"zen\"")
  }

  stringr::str_replace_all(text, replace)

}
