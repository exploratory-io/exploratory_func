#' Function for SEM Analytics View
#' @export
exp_sem <- function(df, model_desc, seed = 1) {
  if(!is.null(seed)) { # Set seed before starting to call sample_n.
    set.seed(seed)
  }

  each_func <- function(df) {
    fit <- sem(model_desc, data = df)
    model<-list(model=fit)
    class(model) <- c("sem_exploratory", "list")
    model
  }

  do_on_each_group(df, each_func, name = "model", with_unnest = FALSE)
}

#' extracts results from lavaan object as a dataframe
#' @export
#' @param n_sample Sample number for biplot. Default 5000, which is the default of our scatter plot.
#'        we use it for gathered_data for parallel coordinates too. sampling is applied before gather.
tidy.sem_exploratory <- function(x, type="summary") {
  browser()
  if (type == "summary") {
    res <- broom::glance(x$model)
  }
  else if (type == "loadings") {
    res <- broom::tidy(x$model)
  }
  res
}
