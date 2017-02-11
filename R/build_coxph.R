#' coxph wrapper with do
#' @return deta frame which has coxph model
#' @param data Data frame to be used as data
#' @param formula Formula for coxph
#' @param ... Parameters to be passed to coxph function
#' @param keep.source Whether source should be kept in source.data column
#' @param augment Whether the result should be augmented immediately
#' @param group_cols A vector with columns names to be used as group columns
#' @param test_rate Ratio of test data
#' @param seed Random seed to control test data sampling
#' @export
build_coxph <- function(data, formula, ...){
  # using lazyeval is needed for non standard evaluation arguments like weights
  lz_dots <- lazyeval::lazy_dots(...)
  lz_dots[["data"]] <- lazyeval::as.lazy(quote(data))
  lz_dots[["formula"]] <- lazyeval::lazy(formula)
  lz_dots[["model_func"]] <- lazyeval::as.lazy(quote(survival::coxph))
  lz_dots[["reserved_colnames"]] <- lazyeval::as.lazy(quote(c(
    # model_coef can add following columns at the next step
    "y.level",
    "term",
    "estimate",
    "std_error",
    "t_ratio",
    "p_value",
    # model_stats can add following columns at the next step
    "edf",
    "deviance",
    "AIC",
    # prediction_survfit can add following columns at the next step
    "time",
    "n.risk",
    "n_risk",
    "n.event",
    "n_event",
    "n.censor",
    "n_censor",
    "estimate",
    "std.error",
    "std_error",
    "conf.high",
    "conf_high",
    "conf.low",
    "conf_low"
  )))
  .call <- lazyeval::make_call(quote(build_model), lz_dots)
  .call$env <- environment()
  lazyeval::lazy_eval(.call)
}
