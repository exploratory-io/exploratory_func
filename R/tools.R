generate_examples <- function(function_name, args){

  cat(paste(function_name, "\n\n"))

  cat("args\n")

  keys <- names(args)

  if(is.null(keys)){
    keys <- seq(length(args))
  }

  keys[keys==""] <- seq(length(keys[keys==""]))

  for(key in keys){
    arg <- args[[key]]
    cat("$")
    cat(key)
    cat("\n")
    if(is.data.frame(arg)){
      print(knitr::kable(arg, format="markdown"))
    }else {
      print(arg)
    }
    cat("\n")
  }
  cat("\n")
  cat("output")
  ret <- do.call(function_name, args)
  print(knitr::kable(ret, format="markdown"))
  ret
}

#' Returns TRUE if test_results has any error in it. For test automation.
#' @export
any_error <- function(test_results) {
  results <- purrr::map(test_results, function(x) {x$results})
  flattened <- purrr::flatten(results)
  # extract result class attr which has info on whether the result was success, warning, or error.
  result_classes <- purrr::map(flattened, function(x){attr(x,"class")})
  # return TRUE if there is any result with class "expectation_error".
  purrr::some(result_classes, function(x){"expectation_error" %in% x})
}
