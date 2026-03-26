

#' Check if the word is digits.
#' @param word Character to be checked if it's digits.
#' @return Logical vector if the word is digits or not.
is_digit <- function(word){
  loadNamespace("stringr")
  stringr::str_detect(word, "^[[:digit:]]+$")
}


#' Get vector of stopwords
#' @param lang Type of stopwords.
#' One of
#' "danish",
#' "dutch",
#' "english",
#' "english_snowball",
#' "english_smart",
#' "english_onix",
#' "finnish",
#' "french",
#' "german",
#' "hungarian",
#' "italian",
#' "japanese",
#' "norwegian",
#' "portuguese",
#' "russian",
#' "spanish",
#' "swedish"
#' @param include Values that should be included as stop words
#' @param exclude Values that should be excluded from stop words
#' @param is_twitter flag that tells if you want to get twitter related stopwords such as http and https.
#' @return vector of stop words.
#' @export
get_stopwords <- function(lang = "english", include = c(), exclude = c(), is_twitter = TRUE){
  if(!requireNamespace("tidystopwords")){stop("package tidystopwords must be installed.")}
  lang <- tolower(lang)
  stopwords <- if (lang %in% c(
    "english_snowball",
    "english_onix",
    "english_smart")){
    # these data are created from data-raw/create_internal_data.R
    get(paste0("stopwords_", lang))
  } else if(lang == "japanese") {
    stopwords_japanese_minimum
  } else if(lang %in% c( # tm only supports these language for stopwords
    "danish",
    "dutch",
    "english",
    "finnish",
    "french",
    "german",
    "hungarian",
    "italian",
    "norwegian",
    "portuguese",
    "russian",
    "spanish",
    "swedish")) {
    loadNamespace("tm")
    tm::stopwords(kind = lang)
  } else { # set empty vector as a fallback.
    c()
  }
  # if is_twitter argument is true, append exploratory_stopwords which contains stopwords for twitter
  if(is_twitter) {
    stopwords <- append(stopwords, exploratory_stopwords)
  }
  # if lang is not in below special cases, append stopwords from tidystopwords package because tidystopwords provides
  # wide range of stopwords such as http and https which are not inclued in tm package.
  if (lang %nin% c(
    "japanese", # Avoid this for Japanese too. We are going for minimum set of stopwords for Japanese for now.
    "english_snowball",
    "english_onix",
    "english_smart")){
    # tidystopwords required language name with Title Case so make sure it's title case.
    stopwords <- append(stopwords, tidystopwords::generate_stoplist(stringr::str_to_title(lang)))
  }
  # include first and then exclude, so that we can exclude words in the include, which we use for custom dictionary too.
  stopwords <- c(stopwords, include)
  ret <- stopwords[!stopwords %in% exclude]
  ret
}








#' Calculate sentiment
#' @export
get_sentiment <- function(text){
  loadNamespace("sentimentr")
  if(is.character(text)) {
    sentimentr::sentiment_by(text)$ave_sentiment
  } else { # sentiment_by fails if the text is not character so convert it to character first
    sentimentr::sentiment_by(as.character(text))$ave_sentiment
  }
}

#' Wrapper function for readr::parse_character
#' @param text to parse
#' @export
parse_character <- function(text, ...){
  # After updating readr version from 1.1.1 to to 1.3.1,
  # readr::parse_character now fails for non-characters.
  # So if the input is not character (e.g. numeric, Date, etc),
  # use base as.character to avoid the error raised from readr::parse_character.
  if(!is.character(text)) {
    as.character(text)
  } else {
    readr::parse_character(text, ...)
  }
}

#' Wrapper function for readr::parse_number
#' @param text to parse
#' @export
parse_number <- function(text, ...){
  # readr::parse_number used to allow already numeric input, by doing nothing,
  # but after updating readr version from 1.1.1 to to 1.3.1, it only allows character input.
  # if numeric, return as is for backward compatibility.
  if(is.numeric(text)) {
    text
  } else if (!is.character(text)) {
    # non character data raises Error in parse_vector(x, col_number(), na = na, locale = locale, trim_ws = trim_ws) : is.character(x) is not TRUE
    # so explicitly convert it to character before calling readr::parse_number
    # Passing non-ascii character to readr::parse_number causes an error so remove non-ascii character before calling readr::parse_number.
    # ref: https://github.com/tidyverse/readr/issues/1111
    as.numeric(readr::parse_number(gsub('[^\x20-\x7E]', '', as.character(text)), ...))
  } else {
    # For some reason, output from parse_number returns FALSE for
    # is.vector(), which becomes a problem when it is fed to ranger
    # as the target variable. To work it around, we apply as.numeric().
    # Passing non-ascii character to readr::parse_number causes an error so remove non-ascii character before calling readr::parse_number.
    # ref: https://github.com/tidyverse/readr/issues/1111
    as.numeric(readr::parse_number(gsub('[^\x20-\x7E]', '', text), ...))
  }
}


#' Wrapper function for readr::parse_logical
#' @param text to parse
#' @export
parse_logical <- function(text, ...){
  # After updating readr version from 1.1.1 to to 1.3.1, it only allows character input.
  # So if logical, return as is for backward compatibility.
  if(is.logical(text)) {
    text
  } else if (!is.character(text)){
    # non character data raises Error in parse_vector(x, col_number(), na = na, locale = locale, trim_ws = trim_ws) : is.character(x) is not TRUE
    # so explicitly convert it to character before calling readr::parse_number
    readr::parse_logical(as.character(text), ...)
  } else {
    readr::parse_logical(text, ...)
  }
}


#'Function to remove text inside the characters like bracket.
#'@export
str_remove_inside <- function(column, begin = "(", end = ")", all = FALSE, include_special_chars = TRUE){
  if(stringr::str_length(begin) > 1) {
    stop("The begin argument must be one character.")
  }
  if(stringr::str_length(end) > 1) {
    stop("The end argument must be one character.")
  }
  if(grepl("[A-Za-z]", begin)) {
    stop("The begin argument must be symbol such as (, {, [.")
  }
  if(grepl("[A-Za-z]", end)) {
    stop("The end argument must be symbol such as ), }, ].")
  }
  exp <- stringr::str_c("\\", begin, "[^\\" , begin, "\\", end, "]*\\", end)
  if(!include_special_chars) {
    exp <- stringr::str_c("(?<=\\", begin,  ").*?(?=\\", end,  ")");
  }
  if(all) {
    stringr::str_remove_all(column, exp)
  } else {
    stringr::str_remove(column, exp)
  }
}

#'Function to replace text inside the characters like bracket.
#'@export
str_replace_inside <- function(column, begin = "(", end = ")", rep = "", all = FALSE, include_special_chars = TRUE){
  if(stringr::str_length(begin) > 1) {
    stop("The begin argument must be one character.")
  }
  if(stringr::str_length(end) > 1) {
    stop("The end argument must be one character.")
  }
  if(grepl("[A-Za-z]", begin)) {
    stop("The begin argument must be symbol such as (, {, [.")
  }
  if(grepl("[A-Za-z]", end)) {
    stop("The end argument must be symbol such as ), }, ].")
  }
  exp <- stringr::str_c("\\", begin, "[^\\" , begin, "\\", end, "]*\\", end)
  if(!include_special_chars) {
    exp <- stringr::str_c("(?<=\\", begin,  ").*?(?=\\", end,  ")");
  }
  if(all) {
    stringr::str_replace_all(column, exp, rep)
  } else {
    stringr::str_replace(column, exp, rep)
  }
}

#' Wrapper function for stringr::str_remove.
#' When remove_extra_space argument is TRUE, it applies str_squish on top of the stringr::str_remove result.
#' @export
str_remove <- function(string, pattern, remove_extra_space = FALSE) {
  res <- stringr::str_remove(string, pattern)
  if (remove_extra_space) {
    res <- stringr::str_squish(res)
  }
  res
}

#' Wrapper function for stringr::str_remove_all.
#' When remove_extra_space argument is TRUE, it applies str_squish on top of the stringr::str_remove_all result.
#' @export
str_remove_all <- function(string, pattern, remove_extra_space = FALSE) {
  res <- stringr::str_remove_all(string, pattern)
  if (remove_extra_space) {
    res <- stringr::str_squish(res)
  }
  res
}

#'Function to remove URL from the text
#'@export
str_remove_url <- function(text, position = "any"){
  reg <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  if(position == "start") {
    reg <- stringr::str_c("^", reg, sep = "")
  } else if (position == "end") {
    reg <- stringr::str_c(reg, "$", sep = "")
  }
  stringr::str_replace_all(text, stringr::regex(reg, ignore_case = TRUE), "")
}
#'Function to replace URL from the text
#'@export
str_replace_url <- function(text, rep = ""){
  stringr::str_replace_all(text, stringr::regex("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", ignore_case = TRUE), rep)
}
#'Function to extract URL from the text
#'@export
str_extract_url <- function(text, position = "any"){
  reg <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  if(position == "start") {
    reg <- stringr::str_c("^", reg, sep = "")
  } else if (position == "end") {
    reg <- stringr::str_c(reg, "$", sep = "")
  }
  stringr::str_extract_all(text, stringr::regex(reg, ignore_case = TRUE))
}


#'Function to remove word from text.
#'@export
str_replace_word <- function(string, start = 1L, end = start, sep = fixed(" "), rep = "") {
  sep_ <- sep
  # Below is the list of predefined separators passed from Exploratory Desktop in a regular expression format.
  # Changed it back to the original separator.
  if(sep == "\\s*\\,\\s*") {
    sep_ <- ","
  } else if (sep == "\\s+") {
    sep_ <- " "
  } else if (sep == "\\s*\\;\\s*") {
    sep_ <- ";"
  } else if (sep == "\\s*\\:\\s*") {
    sep_ <- ":"
  } else if (sep == "\\s*\\/\\s*") {
    sep_ <- "/"
  } else if (sep ==  "\\s*\\-\\s*") {
    sep_ <- "-"
  } else if (sep == "\\s*\\_\\s*") {
    sep_ <- "_"
  } else if (sep == "\\s*\\.\\s*") {
    sep_ <- "."
  } else if (sep == "\\s*\\@\\s*") {
    sep_ <- "@"
  }
  ret <- stringr::str_split(string, pattern = sep_)
  sapply(ret, function(x){
    len <- length(x) # get number of words
    if (end == 1) { # It means the first word.
      if (rep == "") { # for remove case
        if (len > 1) { # trim the white space of the second word's left side
          x[2] = stringr::str_trim(x[2], side = "left")
        }
        # remove the first word and join back the words using the normalized separator.
        stringr::str_c(x[-1], collapse = sep_)
      } else {
        x[1] = rep # replace the first word with the replace text.
        # join back the words using the normalized separator.
        stringr::str_c(x, collapse = sep_)
      }
    } else if (end == -1) { # It means the last word.
      if (rep == "") { # for Remove Case
        if (len > 1) {# Trim the white space of the second word from the end.
          x[len - 1] = stringr::str_trim(x[len - 1], side = "right")
        }
        # remove the last word from list then collapse with the separator.
        stringr::str_c(x[-1 * len], collapse = sep_)
      } else {
        # replace the last word with the replace string.
        if (sep_ == ",") { # if the separator is "," add " " as a prefix it to make it look better.
          x[len] = stringr::str_c(" ", rep)
        } else {
          x[len] = rep
        }
        # join back the words using the normalized separator.
        stringr::str_c(x, collapse = sep_)
      }
    } else {
      ## TODO: Implement other cases
      x
    }
  })
}

get_emoji_regex <- function() {
  regexp = "\\p{EMOJI}|\\p{EMOJI_PRESENTATION}|\\p{EMOJI_MODIFIER_BASE}|\\p{EMOJI_MODIFIER}\\p{EMOJI_COMPONENT}";
  # it seems above condition does not cover all emojis.
  # So we will manually add below emojis. (ref: https://github.com/gagolews/stringi/issues/279)
  regexp = stringr::str_c(regexp, "|\U0001f970|\U0001f975|\U0001f976|\U0001f973|\U0001f974|\U0001f97a|\U0001f9b8|\U0001f9b9|\U0001f9b5|\U0001f9b6|\U0001f9b0|\U0001f9b1|\U0001f9b2", sep = "")
  regexp = stringr::str_c(regexp, "|\U0001f9b3|\U0001f9b4|\U0001f9b7|\U0001f97d|\U0001f97c|\U0001f97e|\U0001f97f|\U0001f99d|\U0001f999|\U0001f99b|\U0001f998|\U0001f9a1|\U0001f9a2", sep = "")
  regexp = stringr::str_c(regexp, "|\U0001f99a|\U0001f99c|\U0001f99e|\U0001f99f|\U0001f9a0|\U0001f96d|\U0001f96c|\U0001f96f|\U0001f9c2|\U0001f96e|\U0001f9c1|\U0001f9ed|\U0001f9f1", sep = "")
  regexp = stringr::str_c(regexp, "|\U0001f6f9|\U0001f9f3|\U0001f9e8|\U0001f9e7|\U0001f94e|\U0001f94f|\U0001f94d|\U0001f9ff|\U0001f9e9|\U0001f9f8|\U0001f9f5|\U0001f9f6|\U0001f9ee", sep = "")
  regexp = stringr::str_c(regexp, "|\U0001f9fe|\U0001f9f0|\U0001f9f2|\U0001f9ea|\U0001f9eb|\U0001f9ec|\U0001f9f4|\U0001f9f7|\U0001f9f9|\U0001f9fa|\U0001f9fb|\U0001f9fc|\U0001f9fd", sep = "")
  regexp = stringr::str_c(regexp, "|\U0001f9ef|\u267e",  sep = "")
  # Handle zero-width joiner (\u200d) prefixing another emoji. https://en.wikipedia.org/wiki/Zero-width_joiner
  # Also handle variation selector (\ufe0e, \ufe0f) suffixing emoji. https://en.wikipedia.org/wiki/Variation_Selectors_(Unicode_block)
  regexp = stringr::str_c("\u200d?(", regexp, ")(\ufe0e|\ufe0f)?",  sep = "")
  regexp
}

#'Function to remove emoji from a list of characters.
#' @export
str_remove_emoji <- function(column, position = "any"){
  regexp <- get_emoji_regex()
  if (position == "start") {
    regexp = stringr::str_c("^(", regexp, ")+", sep = "")
  } else if (position == "end") {
    regexp = stringr::str_c("(", regexp, ")+$", sep = "")
  }
  stringi::stri_replace_all(column, regex = regexp, "")
}


#'Function to replace text before the separator.
#'
#'export
#' @export
str_replace_before <- function(column, sep = "\\,", rep = "", include_sep = TRUE) {
  if (include_sep) {
    stringr::str_replace(column, stringr::str_c(".*", sep), rep)
  } else {
    stringr::str_replace(column, stringr::str_c("(.*)(?=", sep, ")"), rep)
  }
}

#'Function to replace text after the separator.
#'
#'export
#' @export
str_replace_after <- function(column, sep = "\\,", rep = "", include_sep = TRUE){
  if (include_sep) {
    stringr::str_replace(column, stringr::str_c(sep, ".*"), rep)
  } else {
    stringr::str_replace(column, stringr::str_c("(?<=", sep, ")(.*)"), rep)
  }
}



#'Function to replace range of text.
#'
#'It uses stringi::stri_sub_replace under the hood, but it does additional parameter validation.
#'For validation details, see comment below.
#'
#'export
#' @export
str_replace_range <- function(column, start, end = -1, replacement = ""){
  # To make the behavior consistent with extract case (i.e. str_sub),
  # if both start and end are negative, end should be greater than or equal to start.
  # For example, stringi::stri_sub("Aaron Bergman", -3, -4) returns "" since it doesn't match.
  # However, stringi::stri_sub_replace("Aaron Bergman", -3, -4, replacement = "A") returns "Aaron BergAman" where it should return "Aaron Bergman"
  # So to make the behavior of the stringi::stri_sub_replace as same as the stringi::stri_sub, if the below condition met, just return the value as is.
  if (!is.null(start) && !is.null(end) && start < 0 && end < 0 && start > end) {
    column
  } else {
    stringi::stri_sub_replace(column, from = start, to = end, replacement = replacement)
  }
}




#' Function to detect pattern from a string.
#' It's a wrapper function for stringr::str_detect and the wrapper function has ignore_case handling.
#' @param string Input vector. Either a character vector, or something coercible to one.
#' @param pattern Pattern to look for.
#' @param negate If TRUE, return non-matching elements.
#' @param ignore_case If TRUE, detect the pattern with case insensitive way.
#' @param allow_empty_pattern if TRUE, assumes it matches (i.e. return TRUE), if FALSE underlying stringr::str_detect raises an error.
#' @export
str_detect <- function(string, pattern, negate = FALSE, ignore_case = FALSE, allow_empty_pattern = TRUE) {
  # if pattern is not empty string and case insensitive is specified, use regex to handle the case insensitive match.
  # any() is necessary to avoid error when pattern is a vector.
  if (any(pattern != "") && ignore_case) {
    stringr::str_detect(string, stringr::regex(pattern, ignore_case = TRUE), negate = negate)
  } else if (all(pattern == "") && allow_empty_pattern) {
    rep(TRUE, length(string))
  } else { # if the pattern is empty string stringr::regexp throws warning, so simply use stringr::str_detect as is.
    stringr::str_detect(string, pattern, negate)
  }
}
