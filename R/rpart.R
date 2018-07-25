#' @export
exp_rpart <- function(df,
                      target,
                      ...) {
  # this seems to be the new way of NSE column selection evaluation
  # ref: https://github.com/tidyverse/tidyr/blob/3b0f946d507f53afb86ea625149bbee3a00c83f6/R/spread.R
  target_col <- dplyr::select_var(names(df), !! rlang::enquo(target))
  # this evaluates select arguments like starts_with
  selected_cols <- dplyr::select_vars(names(df), !!! rlang::quos(...))

  grouped_cols <- grouped_by(df)

  each_func <- function(df) {
    rhs <- paste0("`", selected_cols, "`", collapse = " + ")
    fml <- as.formula(paste0("`", target_col, "`", " ~ ", rhs))
    model_df <- rpart::rpart(fml, df)
    model_df
  }

  do_on_each_group(df, each_func, name = "model", with_unnest = FALSE)
}

#' @export
#' @param type "importance", "evaluation" or "conf_mat". Feature importance, evaluated scores or confusion matrix of training data.
tidy.rpart <- function(x, type = "importance", pretty.name = FALSE, ...) {
  switch(
    type,
    importance = {
      # return variable importance

      imp <- x$variable.importance

      ret <- data.frame(
        variable = names(imp),
        importance = imp,
        stringsAsFactors = FALSE
      )

      ret
    },
    {
      stop(paste0("type ", type, " is not defined"))
    }
  )
}
