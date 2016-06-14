#' Calculate similarity of each pair of groups.
#' @param df data frame in tidy format
#' @param group A column you want to calculate the correlations for.
#' @param dimension A column you want to use as a dimension to calculate the correlations.
#' @param value A column for the values you want to use to calculate the correlations.
#' @param distinct The returned pair should be duplicated in swapped order or not.
#' TRUE makes it easy to filter group names.
#' @param diag If similarity between itself should be returned or not.
#' @param method Type of calculation. https://cran.r-project.org/web/packages/proxy/vignettes/overview.pdf
#' @export
calc_doc_sim <- function(df, group, dimention, value, distinct=FALSE, diag=FALSE){
  loadNamespace("qlcMatrix")
  loadNamespace("tidytext")
  group_col <- col_name(substitute(group))
  dimention_col <- col_name(substitute(dimention))
  value_col <- col_name(substitute(value))

  grouped_column <- grouped_by(df)

  cnames <- avoid_conflict(grouped_column, c("pair.name.1", "pair.name.2", "value.dim"))

  # this is executed to each group
  calc_doc_sim_each <- function(df){
    mat <- df %>%  tidytext::cast_sparse_(dimention_col, group_col, value_col)
    sim <- qlcMatrix::cosSparse(mat)
    if(distinct){
      df <- upper_gather(sim, rownames(mat), diag=diag, cnames=cnames)
    }else{
      loadNamespace("reshape2")
      loadNamespace("dplyr")
      df <- sim %>%  as.matrix() %>%  mat_to_df(cnames, na.rm=FALSE)
      if(!diag){
        df <- df[df[,1] != df[,2],]
      }
    }
    df
  }

  (df %>% dplyr::do_(.dots=setNames(list(~calc_doc_sim_each(.)), cnames[[1]])) %>%  tidyr::unnest_(cnames[[1]]))

}

#' Calculate distance of each pair of groups.
#' @param df data frame in tidy format
#' @param group A column you want to calculate the correlations for.
#' @param dimension A column you want to use as a dimension to calculate the correlations.
#' @param value A column for the values you want to use to calculate the correlations.
#' @param distinct The returned pair should be duplicated in swapped order or not.
#' TRUE makes it easy to filter group names.
#' @param diag If similarity between itself should be returned or not.
#' @param method Type of calculation. https://cran.r-project.org/web/packages/proxy/vignettes/overview.pdf
#' @export
calc_dist <- function(df, group, dimention, value, fill=0, fun.aggregate=mean, distinct=FALSE, diag=FALSE, method="euclidean", p=2 ){
  loadNamespace("dplyr")
  loadNamespace("tidyr")
  loadNamespace("reshape2")
  loadNamespace("stats")

  group_col <- col_name(substitute(group))
  dimention_col <- col_name(substitute(dimention))
  value_col <- col_name(substitute(value))

  grouped_column <- grouped_by(df)

  cnames <- avoid_conflict(grouped_column, c("pair.name.1", "pair.name.2", "value.dist"))

  # this is executed to each group
  calc_dist_each <- function(df){
    mat <- df %>%  simple_cast(group_col, dimention_col, value_col, fill=fill, fun.aggregate=fun.aggregate)
    # Dist is actually an atomic vector of upper half so upper and diag arguments don't matter
    dist <- stats::dist(mat, method=method, diag=FALSE, p=p)
    if(distinct){
      if(diag){
        diag <- 0
      }else{
        diag <- NULL
      }
      df <- upper_gather(as.vector(dist), rownames(mat), diag=diag, cnames=cnames)
    }else{
      df <- dist %>%  as.matrix() %>%  mat_to_df(cnames)
      if(!diag){
        df <- df[df[,1] != df[,2],]
      }
    }
    rownames(df) <- NULL
    df
  }
  (df %>% dplyr::do_(.dots=setNames(list(~calc_dist_each(.)), cnames[[1]])) %>%  tidyr::unnest_(cnames[[1]]))
}
