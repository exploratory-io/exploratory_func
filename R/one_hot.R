#' Create one hot encoding columns
#' @param ... Column selection
#' @param sep Separating character for new columns
#' @export
one_hot <- function(data, ..., sep = "_", aggregate = TRUE){
  selected <- data %>%
    dplyr::select(...)

  ret <- data

  for (col in colnames(selected)) {
    fct <- as.factor(selected[[col]])
    # create one hot encoding matrix based on factor number
    mat <- matrix(data = FALSE, ncol = length(levels(fct)), nrow = length(fct))
    # index goes down from left top
    # if the factor level is n, count should go through n-1 columns
    mat[(as.integer(fct) - 1) * nrow(mat) + seq(length(fct))] <- TRUE

    col_names <- paste(col, levels(fct), sep = sep)
    colnames(mat) <- col_names
    ret <- cbind(ret, as.data.frame(mat))
    # remove original column
    ret <- ret[,colnames(ret) != col]
  }

  if(aggregate){
    other_cols <- colnames(data)[!colnames(data) %in% colnames(selected)]
    if(length(other_cols) != 0){
      ret <- ret %>%
        dplyr::group_by_(.dots = other_cols) %>%
        dplyr::summarize_all(any)
    }
  }

  ret
}
