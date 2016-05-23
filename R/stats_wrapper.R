#'
#'

#'
#' Calculate correlation among categories and output the result in tidy format
#' @param df data frame in tidy format
#' @param category_col A column you want to calculate the correlations for.
#' @param dimension_col A column you want to use as a dimension to calculate the correlations.
#' @param value_col A column for the values you want to use to calculate the correlations.
#' @param use Operation type for dealing with missing values. This can be one of "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs"
#' @param method Method of calculation. This can be one of "pearson", "kendall", or "spearman".
#' @param fun.aggregate  Set an aggregate function when there are multiple entries for the key column per each category.
#' @return correlations between pairs of categories
#' @export
calc_cor_cat <- function(df,
                         category_col,
                         dimension_col,
                         value_col,
                         use="pairwise.complete.obs",
                         method="pearson",
                         fun.aggregate=mean){
  loadNamespace("reshape2")
  loadNamespace("dplyr")
  row <- as.character(substitute(dimension_col))
  col <- as.character(substitute(category_col))
  val <- as.character(substitute(value_col))
  fml <- as.formula(paste(row, col, sep = "~"))
  result <- (
    df
    # this spreads the data frame into matrix
    %>%  reshape2::acast(fml, value.var=val, fun.aggregate=fun.aggregate)
    %>%  cor(use = use, method = method)
    # this gathers the matrix into data frame
    %>%  reshape2::melt())
  colnames(result) <- c("pair.name.1", "pair.name.2", "cor.value")
  if(is.factor(result$.name.1)){
    # if names are facters, it should be converted to character
    result$.name.1 <- as.character(result$pair.name.1)
    result$.name.2 <- as.character(result$pair.name.2)
  }
  dplyr::arrange(result,pair.name.1)
}

#'
#' Calculate correlation among columns and output the result in tidy format
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
