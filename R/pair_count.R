
#' @export
pair_count <- function (df,
                        group,
                        value,
                        unique_pair = TRUE,
                        self = FALSE,
                        sort = FALSE){
  group_col <- col_name(substitute(group))
  value_col <- col_name(substitute(value))
  pair_count_(df,
              group_col,
              value_col,
              unique_pair = unique_pair,
              self = self,
              sort = sort)
}

#' @export
pair_count_ <- function (df,
                         group_col,
                         value_col,
                         unique_pair = TRUE,
                         self = FALSE,
                         sort = FALSE){
  loadNamespace("Matrix")
  # sparse matrix by group rows and value columns
  sparse_tab <- xtabs(data = df[, c(group_col, value_col)], sparse = TRUE, exclude = NULL, na.action=na.pass)

  # this is cross product, so this returns cross table of values count
  count_mat <- Matrix::t(sparse_tab > 0) %*% (sparse_tab > 0)

  # gather the matrix
  cnames <- c("value1", "value2", "n")
  ret <- upper_gather(count_mat, diag = self, cnames = cnames)
  ret <- ret[ret[[3]] > 0, ]

  if(!unique_pair){
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
  # reset row names because binding and sorting mess up them
  rownames(ret) <- NULL
  ret
}
