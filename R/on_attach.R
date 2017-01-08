.onAttach <- function(...){
  # this loads dependency libraries for exploratory
  loading_libs <- c(
    "janitor",
    "lubridate",
    "hms",
    "tidyr",
    "urltools",
    "stringr",
    "broom",
    "RcppRoll",
    "tibble",
    "dplyr"
  )
  for (loading_lib in loading_libs) {
    library(loading_lib, character.only = TRUE)
  }
}
