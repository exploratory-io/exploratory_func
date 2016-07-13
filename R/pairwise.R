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
do_cosine_sim.kv <- function(df, subject, key, value, distinct=FALSE, diag=FALSE){
  loadNamespace("qlcMatrix")
  loadNamespace("tidytext")
  loadNamespace("Matrix")
  subject_col <- col_name(substitute(subject))
  key_col <- col_name(substitute(key))
  value_col <- col_name(substitute(value))

  grouped_column <- grouped_by(df)

  cnames <- avoid_conflict(grouped_column, c("pair.name.1", "pair.name.2", "sim.value"))

  # this is executed on each group
  calc_doc_sim_each <- function(df){
    key_fact <- as.factor(df[[key_col]])
    subject_fact <- as.factor(df[[subject_col]])
    mat <- Matrix::sparseMatrix(i=as.integer(key_fact), j=as.integer(subject_fact), x=df[[value_col]])
    colnames(mat) <- levels(subject_fact)
    sim <- qlcMatrix::cosSparse(mat)
    if(distinct){
      if(!diag){
        diag <- NULL
      }
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
do_dist.kv <- function(df, subject, key, value, fill=0, fun.aggregate=mean, distinct=FALSE, diag=FALSE, method="euclidean", p=2 ){
  loadNamespace("dplyr")
  loadNamespace("tidyr")
  loadNamespace("reshape2")
  loadNamespace("stats")

  subject_col <- col_name(substitute(subject))
  key_col <- col_name(substitute(key))
  value_col <- col_name(substitute(value))

  grouped_column <- grouped_by(df)

  cnames <- avoid_conflict(grouped_column, c("pair.name.1", "pair.name.2", "dist.value"))

  # this is executed on each group
  calc_dist_each <- function(df){
    mat <- df %>%  simple_cast(subject_col, key_col, value_col, fill=fill, fun.aggregate=fun.aggregate)
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
do_dist.cols <- function(df, ..., label=NULL, fill=0, fun.aggregate=mean, distinct=FALSE, diag=FALSE, method="euclidean", p=2 ){
  loadNamespace("dplyr")
  loadNamespace("tidyr")
  loadNamespace("reshape2")
  loadNamespace("stats")
  loadNamespace("lazyeval")

  grouped_column <- grouped_by(df)
  label_col <- col_name(substitute(label))

  select_dots <- lazyeval::lazy_dots(...)

  cnames <- avoid_conflict(grouped_column, c("pair.name.1", "pair.name.2", "dist.value"))

  # this is executed on each group
  calc_dist_each <- function(df){
    mat <- df %>%  dplyr::select_(.dots=select_dots) %>%  as.matrix()

    # sort the column name so that the output of pair.name.1 and pair.name.2 will be sorted
    # it's better to be sorted so that heatmap in exploratory can be triangle if distinct is TRUE
    sortedNames <- sort(colnames(mat))
    mat <- t(mat)
    mat <- mat[sortedNames, ]

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
