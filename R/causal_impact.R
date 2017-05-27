# Wrapper functions around CausalImpact

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

#' @export
tidy.bsts <- function(x) {
  as.data.frame(summary(x)$coefficients)
}

#' NSE version of do_causal_impact_
#' @export
do_causal_impact <- function(df, time, formula, ...) {
  time_colname <- col_name(substitute(time))
  do_causal_impact_(df, time_colname, formula = formula, ...)
}

#' @param df - Data frame
#' @param time_colname - Column that has time data
#' @param formula - Formula with target value column on the left-hand side, and predictor columns on the right-hand side. e.g. y ~ predictor1 + predictor2
#' @param intervention_time - The point of time where intervention happened.
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
do_causal_impact_ <- function(df, time_colname, formula, intervention_time = NULL, output_type = "series",
                              niter = NULL, standardize.data = NULL, prior.level.sd = NULL, nseasons = NULL, season.duration = NULL, dynamic.regression = NULL, ...) {
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

  if (!class(df[[time_colname]]) %in% c("Date", "POSIXct")) {
    stop(paste0(time_colname, " must be Date or POSIXct."))
  }

  if (!is.null(intervention_time) && class(intervention_time) != "character" && class(df[[time_colname]]) != class(intervention_time)) {
    stop(paste0("intervention_time must be character or the same class as ", time_colname, "."))
  }

  # remove rows with NA in predictors. CausalImpact does not allow NA in predictors (covariates).
  for(var in predictor_column_names) {
    df <- df[!is.na(df[[var]]), ]
  }
  # remove NA data
  df <- df[!is.na(df[[time_colname]]), ]

  do_causal_impact_each <- function(df) {
    # select only columns that appear in the formula.
    # following fails with column names with spaces most likely because of dplyr bug.
    # input_df <- dplyr::select_(df, .dots = c(time_colname, all_column_names))
    # using base R style to avoid it.
    # since this base R style "select" seems to mess up grouping info if done outside of do_causal_impact_each,
    # it has to be done inside do_causal_impact_each.
    input_df <- df[, c(time_colname, all_column_names)]
    # rename the y column to fixed name y so that it is easier to handle in the next step.
    input_df <- dplyr::rename_(input_df, y = y_colname)
    input_df <- dplyr::rename_(input_df, time_points = time_colname)
    # bring y column at the beginning of the input_df, so that CausalImpact understand this is the column to predict.
    input_df <- dplyr::select(input_df, y, dplyr::everything())

    time_points_vec <- input_df$time_points
    input_df <- dplyr::select_(input_df, quote(-time_points)) # drop time_points from main df
    df_zoo <- zoo::zoo(input_df, time_points_vec)

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
        if (class(df[[time_colname]]) == "Date") {
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
      df <- df %>% dplyr::mutate(point.pred = impact$series$point.pred,
                                 point.pred.lower = impact$series$point.pred.lower,
                                 point.pred.upper = impact$series$point.pred.upper,
                                 point.effect = impact$series$point.effect,
                                 point.effect.lower = impact$series$point.effect.lower,
                                 point.effect.upper = impact$series$point.effect.upper,
                                 cum.effect = impact$series$cum.effect,
                                 cum.effect.lower = impact$series$cum.effect.lower,
                                 cum.effect.upper = impact$series$cum.effect.upper)
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
