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
