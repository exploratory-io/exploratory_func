#' @importFrom dplyr %>%
dplyr::`%>%`

#' @importFrom tibble add_row
tibble::add_row

#' @importFrom urltools param_remove
urltools::param_remove

#' @importFrom urltools url_encode
urltools::url_encode

#' @importFrom urltools url_decode
urltools::url_decode

#' @importFrom psych logistic
#' @export
psych::logistic

#' @importFrom anonymizer anonymize
anonymize <- function(vec, algo = "sha256", seed = 1, chars = letters, n_chars = 5L, ...){
  anonymizer::anonymize(vec, .algo = algo, .seed = seed, .chars = chars, .n_chars = n_chars, ...)
}

# Importing tidy is needed to define tidy methods to other classes
#' @importFrom broom tidy
tidy

# Importing glance is needed to define glance methods to other classes
#' @importFrom broom glance
glance

# Importing augment is needed to define augment methods to other classes
#' @importFrom broom augment
augment

nest <- function(...) { # Temporary workaround to make nest return list as opposed to vctr_... classes.
  tidyr::nest_legacy(...)
}
