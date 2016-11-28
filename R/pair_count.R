
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
  sparse_tab <- xtabs(data = df[, c(group_col, value_col)], sparse = TRUE, exclude = NULL, na.action=na.pass)

  count_mat <- Matrix::t(sparse_tab > 0) %*% (sparse_tab > 0)
  cnames <- c("value1", "value2", "n")

  ret <- upper_gather(count_mat, diag = self, cnames = cnames)
  ret <- ret[ret[[3]] > 0 & !is.na(ret[[3]]), ]

  if(!unique_pair){
    # remove self to avoid duplicate
    copy <- upper_gather(count_mat, diag = FALSE, cnames = cnames[c(2,1,3)])
    copy <- copy[copy[[3]] > 0 & !is.na(copy[[3]]), ]
    ret <- dplyr::bind_rows(ret, copy)
  }

  if (sort) {
    ret <- ret[sort.list(ret[, ncol(ret)], decreasing=TRUE),]
  }
  rownames(ret) <- NULL
  ret
}
