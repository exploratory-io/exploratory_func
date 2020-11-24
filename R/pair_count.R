#' Clone implementation of now deprecated tidytext::pair_count for backward compatibility in Exploratory.
#' @export
pair_count <- function (df,
                        group,
                        value,
                        distinct = TRUE,
                        diag = FALSE,
                        sort = TRUE,
                        unite = FALSE,
                        group_by = NULL){
  if(!is.null(group_by)) {
    df <- df %>% dplyr::group_by(!!!rlang::sym(group_by))
  }
  group_col <- col_name(substitute(group))
  value_col <- col_name(substitute(value))

  ret <- pair_count_(df,
              group_col,
              value_col,
              distinct = distinct,
              diag = diag,
              sort = sort,
              unite = unite)
 if(!is.null(group_by)) {
   ret %>% dplyr::ungroup();
 } else {
   ret
 }
}

pair_count_ <- function(df,
                         group_col,
                         value_col,
                         distinct = FALSE,
                         diag = FALSE,
                         sort = FALSE,
                         unite = FALSE) {
  validate_empty_data(df)
  grouped_col <- exploratory::grouped_by(df)
  if(!is.null(grouped_col) && length(grouped_col) > 0){
    if(group_col %in% grouped_col){
      stop(paste0(group_col, " is grouped. Please ungroup it."))
    }
    # If the data frame is grouped, perform the pair_count for each group.
    dplyr::group_modify(df, .f = function(.x, ...){
      pair_count__(.x,
                   group_col,
                   value_col,
                   distinct = distinct,
                   diag = diag,
                   sort = sort,
                   unite = unite)
    }, .keep = TRUE)
  } else {
    pair_count__(df,
                 group_col,
                 value_col,
                 distinct = distinct,
                 diag = diag,
                 sort = sort,
                 unite = unite)
  }
}

#' Clone implementation of now deprecated tidytext::pair_count_ for backward compatibility in Exploratory.
#' @export
pair_count__ <- function (df,
                         group_col,
                         value_col,
                         distinct = FALSE,
                         diag = FALSE,
                         sort = FALSE,
                         unite = FALSE){
  validate_empty_data(df)

  loadNamespace("Matrix")
  # sparse matrix by group rows and value columns
  sparse_tab <- xtabs(data = df[, c(group_col, value_col)], sparse = TRUE, exclude = NULL, na.action=na.pass)

  # this is cross product, so this returns cross table of values count
  count_mat <- Matrix::t(sparse_tab > 0) %*% (sparse_tab > 0)

  # gather the matrix
  cnames <- c(stringr::str_c(value_col, c(".x", ".y")), "value")
  cnames_wihtout_value <- c(stringr::str_c(value_col, c(".x", ".y")))
  ret <- upper_gather(count_mat, diag = diag, cnames = cnames)
  ret <- ret[ret[[3]] > 0, ]

  if(!distinct){
    # bind rows by swapping the first and second columns
    # remove self to avoid duplicate
    copy <- upper_gather(count_mat, diag = FALSE, cnames = cnames[c(2,1,3)])
    copy <- copy[copy[[3]] > 0 & !is.na(copy[[3]]), ]
    ret <- dplyr::bind_rows(ret, copy)
  }

  if (sort) {
    # sort based on the last column
    ret <- ret[sort.list(ret[, ncol(ret)], decreasing=TRUE),]
  }
  if (unite) {
    ret <- ret %>% tidyr::unite(col = !!rlang::sym(value_col), cnames_wihtout_value)
  }
  # reset row names because binding and sorting mess up them
  rownames(ret) <- NULL
  ret
}
