#' Calc similarity similarity
calc_sim <- function(tbl, group, dimention, value, upper=FALSE, diag=FALSE, method="correlation"){
  group_col <- col_name(substitute(group))
  dimention_col <- col_name(substitute(dimention))
  value_col <- col_name(substitute(value))
  if(method=="cosine"){
    loadNamespace("qlcMatrix")
    loadNamespace("tidytext")
    mat <- tbl %>%  tidytext::cast_sparse_(dimention_col, group_col, value_col)
    sim <- qlcMatrix::cosSparse(mat)
  } else {
    loadNamespace("proxy")
    mat <- tbl %>%  simple_cast(group_col, dimention_col, value_col)
    sim <- proxy::simil(mat, method=method, diag=FALSE)
  }
  if(upper){
    df <- upper_gather(sim, rownames(mat), diag=diag)
  }else{
    loadNamespace("dplyr")
    df <- sim %>%  as.matrix() %>%  reshape2::melt()
    if(!diag){
      df <- dplyr::filter(df, Var1!=Var2)
    }
  }
  colnames(df) <- c("group1", "group2", "sim")
  df[,1] <- as.character(df[,1])
  df[,2] <- as.character(df[,2])
  df
}

#' Calc cosine similarity
calc_dist <- function(tbl, group, dimention, value, upper=FALSE, diag=FALSE, method="Euclidean"){
  loadNamespace("proxy")
  group_col <- col_name(substitute(group))
  dimention_col <- col_name(substitute(dimention))
  value_col <- col_name(substitute(value))
  mat <- tbl %>%  simple_cast(group_col, dimention_col, value_col)
  dist <- proxy::dist(mat, method=method, diag=FALSE)
  if(upper){
    df <- upper_gather(dist, rownames(mat), diag=diag)
  }else{
    loadNamespace("dplyr")
    df <- dist %>%  as.matrix() %>%  reshape2::melt()
    if(!diag){
      df <- dplyr::filter(df, Var1!=Var2)
    }
  }
  colnames(df) <- c("group1", "group2", "sim")
  df[,1] <- as.character(df[,1])
  df[,2] <- as.character(df[,2])
  df
}
