
#' @export
pair_count <- function (data, group, value, unique_pair = TRUE, self = FALSE,
          sort = FALSE)
{
  group_col <- col_name(substitute(group))
  value_col <- col_name(substitute(value))
  pair_count_(data, group_col, value_col, unique_pair = unique_pair,
              self = self, sort = sort)
}

#' @export
pair_count_ <- function (data, group_col, value_col, unique_pair = TRUE, self = FALSE,
          sort = FALSE){
  requireNamespace("Matrix")
  value_vals <- unique(data[[value_col]])
  value_indices <- match(data[[value_col]], value_vals)
  data[[".value_indices"]] <- value_indices
  sparse_mat <- tidytext::cast_sparse_(data, group_col, ".value_indices",
                             value_col = 1)
  co <- Matrix::t(sparse_mat) %*% sparse_mat
  matrix_indices <- as.integer(rownames(co))
  triplets <- Matrix::summary(co)
  if (unique_pair) {
    triplets <- dplyr::filter(triplets, i <= j)
  }
  if (!self) {
    triplets <- dplyr::filter(triplets, i != j)
  }
  ret <- triplets %>% dplyr::transmute(value1 = value_vals[matrix_indices[i]],
                                value2 = value_vals[matrix_indices[j]], n = x) %>% dplyr::tbl_df()
  if (sort) {
    ret <- dplyr::arrange(ret, desc(n))
  }
  ret
}
