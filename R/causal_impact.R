# Wrapper functions around CausalImpact

#' NSE version of do_causal_impact_
#' @export
do_causal_impact <- function(df, time, formula, ...) {
  time_colname <- col_name(substitute(time))
  do_causal_impact_(df, time_colname, formula = formula, ...)
}

do_causal_impact_ <- function(df, time_colname, formula, ...) {
  y_colname <- as.character(lazyeval::f_lhs(formula))
  predictor_column_names <- all.vars(lazyeval::f_rhs(formula))
  all_column_names <- all.vars(formula)
  grouped_col <- grouped_by(df)

  # column name validation
  if(!time_colname %in% colnames(df)){
    stop(paste0(time_colname, " is not in column names"))
  }

  if(time_colname %in% grouped_col){
    stop(paste0(time_colname, " is grouped. Please ungroup it."))
  }

  # remove rows with NA in predictors. CausalImpact does not allow NA in predictors (covariates).
  for(var in predictor_column_names) {
    df <- df[!is.na(df[[var]]), ]
  }
  # remove NA data
  df <- df[!is.na(df[[time_colname]]), ]

  # select only columns that appear in the formula.
  input_df <- dplyr::select_(df, .dots = c(time_colname, all_column_names))
  # rename the y column to fixed name y so that it is easier to handle in the next step.
  input_df <- dplyr::rename_(input_df, y = y_colname)
  input_df <- dplyr::rename_(input_df, time_points = time_colname)
  # bring y column at the beginning of the input_df, so that CausalImpact understand this is the column to predict.
  input_df <- dplyr::select(input_df, y, dplyr::everything())

  do_causal_impact_each <- function(df) {
    time_points_vec <- df$time_points
    df <- dplyr::select_(df, quote(-time_points)) # drop time_points from main df
    df_zoo <- zoo::zoo(df, time_points_vec)
    
    impact <- CausalImpact::CausalImpact(df_zoo, ...)
    ret <- as.data.frame(impact$series)
    ret <- tibble::rownames_to_column(ret)
    ret <- mutate(ret, rowname = as.Date(rowname))
    colnames(ret)[colnames(ret) == "rowname"] <- avoid_conflict(colnames(ret), time_colname) # set back original time column name
    colnames(ret)[colnames(ret) == "y"] <- avoid_conflict(colnames(ret), y_colname) # set back original y column name
    ret
  }

  # Calculation is executed in each group.
  # Storing the result in this tmp_col and
  # unnesting the result.
  # If the original data frame is grouped by "tmp",
  # overwriting it should be avoided,
  # so avoid_conflict is used here.
  tmp_col <- avoid_conflict(grouped_col, "tmp")
  ret <- input_df %>%
    dplyr::do_(.dots=setNames(list(~do_causal_impact_each(.)), tmp_col)) %>%
    tidyr::unnest_(tmp_col)

  # grouping should be kept
  if(length(grouped_col) != 0){
    ret <- dplyr::group_by_(ret, grouped_col)
  }
  ret
}
