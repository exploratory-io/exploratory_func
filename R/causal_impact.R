# Wrapper functions around CausalImpact

#' broom::glance() implementation for bsts (Bayesian Structural Time Series) model,
#' which is the model used internally in CausalImpact to predict synthetic control (counterfactual).
#' @export
glance.bsts <- function(x) {
  ret <- summary(x)
  data.frame(residual.sd = ret$residual.sd,
             prediction.sd = ret$prediction.sd,
             rsquare = ret$rsquare,
             relative.gof = ret$relative.gof,
             coef_min = ret$size[[1]],
             coef_1st_quartile = ret$size[[2]],
             coef_median = ret$size[[3]],
             coef_mean = ret$size[[4]],
             coef_3rd_quartile = ret$size[[5]],
             coef_max = ret$size[[6]])
}

#' broom::tidy() implementation for bsts model
#' @export
tidy.bsts <- function(x) {
  as.data.frame(summary(x)$coefficients)
}

#' NSE version of do_causal_impact_
#' @export
do_causal_impact <- function(df, time, value, segment, ...) {
  time_col <- col_name(substitute(time))
  value_col <- col_name(substitute(value))
  segment_col <- col_name(substitute(segment))
  do_causal_impact_(df, time_col, value_col, segment_col, ...)
}

#' @param df - Data frame
#' @param time_col - Column that has time data
#' @param formula - Formula with target value column on the left-hand side, and predictor columns on the right-hand side. e.g. y ~ predictor1 + predictor2
#' @param intervention_time - The point of time where intervention happened.
#' @param na_fill_type - Type of NA fill:
#'                       "spline" - Spline interpolation.
#'                       "interpolate" - Linear interpolation.
#'                       "previous" - Fill with last previous non-NA value.
#'                       "value" - Fill with the value of na_fill_value.
#'                       NULL - Skip NA fill. Use this only when you know there is no NA.
#' @param na_fill_value - Value to fill NA when na_fill_type is "value"
#' @param output_type - Type of output data frame:
#'                      "series" - time series (default)
#'                      "model_stats" - model fit summary from broom::glance() on the bsts model.
#'                      "model_coef" - model coefficients from broom::tidy() on the bsts model.
#'                      "model" - model data frame with the bsts model. (Not use in Exploratory UI for now.)
#' @param alpha - Tail-area probability of posterior interval (a concept that is similar to confidence interval.)
#' @param niter - Number of MCMC Samples.
#' @param standardize.data - Whether to standardize data.
#' @param prior.level.sd - Prior Standard Deviation of Random Walk
#' @param nseasons - Period of Seasonal Trend. e.g. 7, for weekly trend.
#' @param season.duration - Used with nseasons. How many unit time one season consists of. e.g. 24, when unit time is hour.
#' @param dynamic.regression - Whether to include time-varying regression coefficients.
#' @param ... - extra values to be passed to CausalImpact::CausalImpact.
do_causal_impact_ <- function(df, time_col, value_col, segment_col, formula = formula, intervention_time = NULL, output_type = "series",
                              na_fill_type = "spline", na_fill_value = 0,
                              niter = NULL, standardize.data = NULL, prior.level.sd = NULL, nseasons = NULL, season.duration = NULL, dynamic.regression = NULL, ...) {
  validate_empty_data(df)

  y_colname <- all.vars(lazyeval::f_lhs(formula))
  predictor_column_names <- all.vars(lazyeval::f_rhs(formula))
  all_column_names <- all.vars(formula)
  grouped_col <- grouped_by(df)

  # column name validation
  if(!time_col %in% colnames(df)){
    stop(paste0(time_col, " is not in column names"))
  }

  if(time_col %in% grouped_col){
    stop(paste0(time_col, " is grouped. Please ungroup it."))
  }

  if (!class(df[[time_col]]) %in% c("Date", "POSIXct")) {
    stop(paste0(time_col, " must be Date or POSIXct."))
  }

  if (!(is.null(intervention_time) || class(intervention_time) == "character" || class(df[[time_col]]) == class(intervention_time))) {
    stop(paste0("intervention_time must be character or the same class as ", time_col, "."))
  }

  do_causal_impact_each <- function(df) {
    # keep time_col column, since we will drop it in the next step,
    # but will need it to compose zoo object.
    time_points_vec <- df[[time_col]]
    # select only columns that appear in the formula.
    # following fails with column names with spaces most likely because of dplyr bug.
    # input_df <- dplyr::select_(df, .dots = all_column_names)
    # using base R style to avoid it.
    # since this base R style "select" seems to mess up grouping info if done outside of do_causal_impact_each,
    # it has to be done inside do_causal_impact_each.
    input_df <- df[, all_column_names]
    # bring y column at the beginning of the input_df, so that CausalImpact understand this is the column to predict.
    input_df <- move_col(input_df, y_colname, 1)

    df_zoo <- zoo::zoo(input_df, time_points_vec)
    # fill NAs in the input.
    # since CausalImpact does not allow irregular time series,
    # filtering rows would not work.
    if (na_fill_type == "spline") {
      df_zoo <- zoo::na.spline(df_zoo)
    }
    else if (na_fill_type == "interpolate") {
      df_zoo <- zoo::na.approx(df_zoo)
    }
    else if (na_fill_type == "previous") {
      df_zoo <- zoo::na.locf(df_zoo)
    }
    # TODO: Getting this error with some input with na.StructTS().
    #       Error in rowSums(tsSmooth(StructTS(y))[, -2]) : 'x' must be an array of at least two dimensions
    #
    # else if (na_fill_type == "StructTS") {
    #   df_zoo <- zoo::na.StructTS(df_zoo)
    # }
    else if (na_fill_type == "value") {
      df_zoo <- zoo::na.fill(df_zoo, na_fill_value)
    }
    else if (is.null(na_fill_type)) {
      # skip when it is NULL. this is for the case caller is confident that
      # there is no NA and want to skip overhead of checking for NA.
    }
    else {
      stop(paste0(na_fill_type, " is not a valid na_fill_type option."))
    }

    # compose list for model.args argument of CausalImpact.
    model_args <- list()
    if (!is.null(niter)) {
      model_args$niter = niter
    }
    if (!is.null(standardize.data)) {
      model_args$standardize.data = standardize.data
    }
    if (!is.null(prior.level.sd)) {
      model_args$prior.level.sd = prior.level.sd
    }
    if (!is.null(nseasons)) {
      model_args$nseasons = nseasons
    }
    if (!is.null(season.duration)) {
      model_args$season.duration = season.duration
    }
    if (!is.null(dynamic.regression)) {
      model_args$dynamic.regression = dynamic.regression
    }

    if (!is.null(intervention_time)) { # if intervention_time is specified, create pre.period/post.period automatically.
      if (class(intervention_time) == "character") { # translate character intervention_time into Date or POSIXct.
        if (class(df[[time_col]]) == "Date") {
          intervention_time <- as.Date(intervention_time)
        }
        else {
          intervention_time <- as.POSIXct(intervention_time)
        }
      }
      pre_period <- c(min(time_points_vec), intervention_time - 1) # -1 works as -1 day on Date and -1 sec on POSIXct.
      post_period <- c(intervention_time, max(time_points_vec))
    
      # call CausalImpact::CausalImpact, which is the heart of this analysis.
      impact <- CausalImpact::CausalImpact(df_zoo, pre.period = pre_period, post.period = post_period, model.args = model_args, ...)
    }
    else {
      # pre.period, post.period must be in the ... in this case.
      impact <- CausalImpact::CausalImpact(df_zoo, model.args = model_args, ...)
    }

    # $series has the result of prediction. for now ignore the rest such as $model.
    if (output_type == "series") {
      df <- df[ , !(colnames(df) %in% grouped_col)] # drop group columns since they will be added back outside of do_causal_impact_each.
      df <- df %>% dplyr::mutate(predicted_value = impact$series$point.pred,
                                 predicted_value_high = impact$series$point.pred.upper,
                                 predicted_value_low = impact$series$point.pred.lower,
                                 effect = impact$series$point.effect,
                                 effect_high = impact$series$point.effect.upper,
                                 effect_low = impact$series$point.effect.lower,
                                 cumulative_effect = impact$series$cum.effect,
                                 cumulative_effect_high = impact$series$cum.effect.upper,
                                 cumulative_effect_low = impact$series$cum.effect.lower)
      df
    }
    else if (output_type == "model_stats") {
      broom::glance(impact$model$bsts.model)
    }
    else if (output_type == "model_coef") {
      broom::tidy(impact$model$bsts.model)
    }
    else { # output_type should be "model"
      # following would cause error : cannot coerce class ""bsts"" to a data.frame 
      # ret <- data.frame(model = list(impact$model$bsts.model))
      # working it around like following.
      ret <- data.frame(model = 1)
      ret$model = list(impact$model$bsts.model)
      ret
    }
  }

  # Calculation is executed in each group.
  # Storing the result in this tmp_col and
  # unnesting the result.
  # If the original data frame is grouped by "tmp",
  # overwriting it should be avoided,
  # so avoid_conflict is used here.
  tmp_col <- avoid_conflict(grouped_col, "tmp")
  ret <- df %>%
    dplyr::do_(.dots=setNames(list(~do_causal_impact_each(.)), tmp_col)) %>%
    tidyr::unnest_(tmp_col)

  # grouping should be kept
  if(length(grouped_col) != 0){
    ret <- dplyr::group_by_(ret, grouped_col)
  }
  ret
}
