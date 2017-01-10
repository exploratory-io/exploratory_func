.onAttach <- function(...){
  # this loads dependency libraries for exploratory
  loading_libs <- c(
    "haven",
    "rvest",
    "readxl",
    "purrr",
    "janitor",
    "lubridate",
    "hms",
    "jsonlite",
    "tidyr",
    "stringr",
    "readr",
    "RcppRoll",
    "evaluate",
    "dplyr"
  )
  for (loading_lib in loading_libs) {
    library(loading_lib, character.only = TRUE)
  }
}
