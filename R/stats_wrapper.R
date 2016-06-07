#'
#'

#'
#' Calculate correlation among groups and output the correlation of each pair
#' @param df data frame in tidy format
#' @param group A column you want to calculate the correlations for.
#' @param dimension A column you want to use as a dimension to calculate the correlations.
#' @param value A column for the values you want to use to calculate the correlations.
#' @param use Operation type for dealing with missing values. This can be one of "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs"
#' @param method Method of calculation. This can be one of "pearson", "kendall", or "spearman".
#' @param fun.aggregate  Set an aggregate function when there are multiple entries for the key column per each category.
#' @return correlations between pairs of groups
#' @export
calc_cor <- function(df,
                         group,
                         dimension,
                         value,
                         use="pairwise.complete.obs",
                         method="pearson",
                         fun.aggregate=mean){
  loadNamespace("reshape2")
  loadNamespace("dplyr")
  row <- col_name(substitute(dimension))
  col <- col_name(substitute(group))
  val <- col_name(substitute(value))
  result <- (
    df
    # this spreads the data frame into matrix
    %>%  simple_cast(row, col, val, fun.aggregate=fun.aggregate, fill=NA_real_)
    %>%  cor(use = use, method = method)
    # this gathers the matrix into data frame
    %>%  reshape2::melt())
  colnames(result) <- c("pair.name.1", "pair.name.2", "cor.value")
  if(is.factor(result[,1])){
    # if names are facters, it should be converted to character
    result[,1] <- as.character(result[,1])
    result[,2] <- as.character(result[,2])
  }
  dplyr::arrange(result,pair.name.1)
}

#'
#' Calculate correlation among columns and output the correlation of each pair
#' @param df data frame in tidy format
#' @param ... Arguments to select columns to calculate correlation.
#' @param use Operation type for dealing with missing values. This can be one of "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs"
#' @param method Method of calculation. This can be one of "pearson", "kendall", or "spearman".
#' @return correlations between pairs of columns
#' @export
calc_cor_var <- function(df, ..., use="pairwise.complete.obs", method="pearson"){
  loadNamespace("dplyr")
  loadNamespace("lazyeval")
  loadNamespace("tibble")
  # select columns using dplyr::select logic
  selected_df <- dplyr::select_(df, .dots = lazyeval::lazy_dots(...))
  # check if the df's grouped
  indices <- attr(selected_df, "indices")
  labels <- attr(selected_df, "labels")
  if(is.null(indices)){
    # not grouped case
    indices <- list(seq(nrow(df))-1)
    calc_df <- selected_df
    label_df <- NULL
  } else {
    # grouped case: split data column and label column
    calc_df <- selected_df[,!colnames(selected_df) %in% colnames(labels)]
    label_df <- selected_df[,colnames(selected_df) %in% colnames(labels)]
  }
  df_list <- lapply(indices, function(index){
    input_df <- calc_df[index+1, ]
    # "pairwise.complete.obs" is to ignore NA. Range is the area of df columns to calculate
    cor_result <- as.data.frame(cor(input_df, use = use, method=method))
    # put label to recognize row names
    rownames(cor_result) <- colnames(input_df)
    df_list <- lapply(colnames(input_df), function(name){
      # get values for each column
      val <- cor_result[,name]
      data.frame(pair.name.1 = rep(name, length(val)), pair.name.2 = colnames(input_df), cor.value=val, stringsAsFactors = FALSE)
    })
    output_df <- do.call(rbind, df_list)
    if(is.null(label_df)){
      # not grouped case
      output_df
    } else {
      # grouped case: create group label column
      cols <- lapply(colnames(label_df), function(colname){
        df <- data.frame(unlist(replicate(nrow(output_df), label_df[index[[1]]+1, colname])), stringsAsFactors = FALSE)
        colnames(df) <- colname
        df
      })
      label <- do.call(rbind, replicate(nrow(output_df), label_df[1,]))
      group_df <- do.call(cbind, cols)

      cbind(group_df, output_df)
    }
  })
  result <- do.call(rbind, df_list)
  tibble::repair_names(result, sep="_")
}

#' Compress dimension using svd algorithm
#' @param df
#' @param group Column to be regarded as groups
#' @param dimension Column to be regarded as original dimensions
#' @param value Column to be regarded as values
#' @param value Column to be regarded as values
#' Compress dimension
reduce_dimension <- function(df, group, dimension, value, type="group", fill=0, fun.aggregate=mean, dim=NULL){
  loadNamespace("reshape2")
  group_col <- col_name(substitute(group))
  dimension_col <- col_name(substitute(dimension))
  value_col <- col_name(substitute(value))
  fml <- as.formula(paste(group_col, dimension_col, sep="~"))
  matrix <- reshape2::acast(df, fml, value.var=value_col, fill=fill, fun.aggregate=fun.aggregate)
  dim <- min(dim, ncol(matrix))
  if(type=="group"){
    result <- svd(sweep(matrix, 2, colMeans(matrix), "-"), nu=dim, nv=0)
    stdev <- result$d[seq(min(dim, length(result$d)))]
    stdev[is.na(stdev)] <- 0
    mat <- result$u %*% diag(result$d[seq(dim)])
    rownames(mat) <- rownames(matrix)
    result <- reshape2::melt(mat)
    colnames(result) <- c("group", "component", "value")
  } else if (type=="dimension") {
    result <- svd(sweep(matrix, 2, colMeans(matrix), "-"), nv=dim, nu=0)
    mat <- result$v
    stdev <- result$d[seq(max(dim, length(result$d)))]
    stdev[is.na(stdev)] <- 0
    rownames(mat) <- colnames(matrix)
    result <- reshape2::melt(mat)
    colnames(result) <- c("dimension", "component", "value")
  }
  rep_sdev <- rep(stdev, each=nrow(result)/length(stdev))
  result$stdev <- rep_sdev
  result
}

# is_digit only numbers
# is_alphabet only numbers
# is_stopword
#
