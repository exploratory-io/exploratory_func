#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

#' @importFrom tibble add_row
#' @export
tibble::add_row

#' @importFrom urltools param_remove
#' @export
urltools::param_remove

#' @importFrom urltools url_encode
#' @export
urltools::url_encode

#' @importFrom urltools url_decode
#' @export
urltools::url_decode

#' @importFrom psych logistic
#' @export
psych::logistic

#' @importFrom anonymizer anonymize
#' @export
anonymize <- function(vec, algo = "sha256", seed = 0, chars = letters, n_chars = 5L, ...){
  anonymizer::anonymize(vec, .algo = algo, .seed = seed, .chars = chars, .n_chars = n_chars, ...)
}

# Importing tidy is needed to define tidy methods to other classes
#' @importFrom broom tidy
#' @export
tidy

# Importing glance is needed to define glance methods to other classes
#' @importFrom broom glance
#' @export
glance

# Importing augment is needed to define augment methods to other classes
#' @importFrom broom augment
#' @export
augment

#' @export
nest <- function(...) { # Temporary workaround to make nest return list as opposed to vctr_... classes.
  tidyr::nest_legacy(...)
}
