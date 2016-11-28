
#' @export
pair_count <- function (df,
                        group,
                        value,
                        unique_pair = TRUE,
                        self = FALSE,
                        sort = FALSE)
{
  group_col <- col_name(substitute(group))
  value_col <- col_name(substitute(value))
  pair_count_(df, group_col, value_col, unique_pair = unique_pair,
              self = self, sort = sort)
}

#' @export
pair_count_ <- function (df,
                         group_col,
                         value_col,
                         unique_pair = TRUE,
                         self = FALSE,
                         sort = FALSE){
  loadNamespace("Matrix")
  unique_values <- unique(df[[value_col]])
  value_indice <- match(df[[value_col]], unique_values)

  indice_col <- avoid_conflict(colnames(df), "indice")

  df[[indice_col]] <- value_indice
  sparse_mat <- tidytext::cast_sparse_(df, group_col, indice_col,
                             value_col = 1)
  co <- Matrix::t(sparse_mat) %*% sparse_mat

  matrix_indice <- as.integer(rownames(co))

  triplets <- Matrix::summary(co)
  if (unique_pair) {
    triplets <- dplyr::filter(triplets, i <= j)
  }
  if (!self) {
    triplets <- dplyr::filter(triplets, i != j)
  }
  ret <- triplets %>% dplyr::transmute(value1 = unique_values[matrix_indice[i]],
                                value2 = unique_values[matrix_indice[j]], n = x) %>% dplyr::tbl_df()
  if (sort) {
    ret <- dplyr::arrange(ret, dplyr::desc(n))
  }
  ret
}
