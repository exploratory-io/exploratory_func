#'
#'
#'@return do wrapper function
do_data <- function(funcname) {
  ret <- function(df, ..., keep.source = TRUE){
    loadNamespace("dplyr")
    if (keep.source) {
      output <- df  %>%  dplyr::do(.model = do.call(funcname, list(data = ., ...)), .source.data = (.))
      # Add a class for Exploratyry to recognize the type of .source.data
      class(output$.source.data) <- c("list", ".source.data")
    } else {
      output <- df  %>%  dplyr::do(.model= do.call(funcname, list(data = ., ...)))
    }
    # Add a class for Exploratyry to recognize the type of .model
    class(output$.model) <- c("list", paste0(".model.", funcname))
    output
  }
  ret
}

#'@export
do_lm <- do_data("lm")

#'@export
do_glm <- do_data("glm")

#'@export
do_kmeans <- function(df, ..., centers=3, keep.source = TRUE, seed=0){
  loadNamespace("dplyr")
  set.seed(seed)
  labels <- attr(df, "labels")
  if(is.null(labels)){
    grouped_cname = NULL
  } else {
    grouped_cname = colnames(labels)
  }
  selected_df <- dplyr::select(df, ...)
  selected_cnames <- colnames(selected_df)
  selected_cnames <- selected_cnames[!selected_cnames %in% grouped_cname]
  if(keep.source){
    output <- (
      df
      %>%  dplyr::do(.model= kmeans(.[,selected_cnames], centers), .source.data=(.))
    )
    # Add a class for Exploratyry to recognize the type of .source.data
    class(output$.source.data) <- c("list", ".source.data")
  } else {
    output <- (
      df
      %>%  dplyr::do(.model= kmeans(.[,selected_cnames], centers))
    )
  }
  # Add a class for Exploratyry to recognize the type of .model
  class(output$.model) <- ".model.kmeans"
  output
}
