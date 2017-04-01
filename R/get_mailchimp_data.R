#' Get mailchimp data
#' @export
get_mailchimp_data <- function(api_key, obj, count = 10){
  area <- stringr::str_split(api_key, "-")[[1]][[2]]

  get_data <- function(obj){
    url <- paste0("https://", area,".api.mailchimp.com/3.0/", obj)
    res <- httr::GET(url, httr::authenticate("any", api_key), query = list(
      count = count
    ))

    from_json <- res %>% httr::content(as = "text") %>% jsonlite::fromJSON()

    split <- stringr::str_split(obj, "/")

    if(length(split[[1]]) > 1) {
      obj <- tail(split[[1]], 1)
    }

    ret <- from_json[[obj]]
  }

  ret <- if(obj == "lists/members"){
    lists <- get_data("lists")
    data <- purrr::map2(lists$id, lists$name, function(id, name){
      ret <- get_data(paste0(c("lists", id, "members"), collapse = "/"))
      if(length(ret) > 0){
        ret[["list_name"]] <- rep(name, nrow(ret))
        ret
      }else{
        NULL
      }
    })
    data <- data[vapply(data, function(elem){
      length(elem) > 0
    }, FUN.VALUE = TRUE)]
    do.call(rbind, data)
  } else {
    get_data(obj)
  }

  if(is.null(ret) || length(ret) == 0) {
    stop("No data found.")
  }

  ret

}
