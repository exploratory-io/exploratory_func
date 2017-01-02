#' Convert fullwidth (zenkaku) and halfwidth (hankaku) Japanese characters
#' @param text Character vector to convert characters
#' @param to "han" or "zen". "han" is for converting to halfwidth and "zen" is to halfwidth.
#' @export
convert_han_zen <- function(text, to = "han") {
  loadNamespace("stringr")
  # list got from http://so-zou.jp/web-app/text/fullwidth-halfwidth/
  df <- data.frame(
    zen = c("０", "１", "２", "３", "４", "５", "６", "７", "８",
            "９", "！", "＂", "＃", "＄", "％", "＆", "＇", "（", "）",
            "＊", "＋", "，", "－", "．", "／", "：", "；", "＜",
            "＝", "＞", "？", "＠", "［", "＼", "］", "＾", "＿",
            "｀", "｛", "｜", "｝", "～", "　"),
    han = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "!", "\"", "#", "$", "%", "&", "'", "(", ")", "*", "+", ",",
            "-", ".", "/", ":", ";", "<", "=", ">", "?", "@", "[", "\\", "]",
            "^", "_", "`", "{", "|", "}", "~", " "),
    escaped_han = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "\\!", "\\\"",
                    "\\#", "\\$", "\\%", "\\&", "\\'", "\\(", "\\)", "\\*", "\\+",
                    "\\,", "\\-", "\\.", "\\/", "\\:", "\\;", "\\<", "\\=", "\\>",
                    "\\?", "\\@", "\\[", "\\\\", "\\]", "\\^", "_", "\\`", "\\{",
                    "\\|", "\\}", "\\~", "\\ "),
    stringsAsFactors = FALSE
  )

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
