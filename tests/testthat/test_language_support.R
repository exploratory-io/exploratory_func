context("language support")

test_that("text replacement", {
  text <- c("半角スペ2  全角スペ2　　", "半角2!!全角2！！")

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
    } else {
      ret <- df$zen
      names(ret) <- df$escaped_han
      ret
    }

    stringr::str_replace_all(text, replace)

  }

  ret1 <- convert_han_zen(text, to="han")

  expect_equal(ret1, c("半角スペ2  全角スペ2  ", "半角2!!全角2!!"))

  quotemeta <- function(string) {
    stringr::str_replace_all(string, "(\\W)", "\\\\\\1")
  }

  ret2 <- convert_han_zen(text, to="zen")

})
