# Common utility function called for tidy with type "partial_dependence".
# Used for ranger, rpart, lm, and glm.
handle_partial_dependence <- function(x) {
  if (is.null(x$partial_dependence)) {
    return(data.frame()) # Skip by returning empty data.frame.
  }
  # return partial dependence
  ret <- x$partial_dependence
  var_cols <- attr(x$partial_dependence, "vars")
  target_col <- attr(x$partial_dependence, "target")
  # We used to do the following, probably for better formatting of numbers, but this had side-effect of
  # turning close numbers into a same number, when differences among numbers are small compared to their
  # absolute values. It happened with Date data turned into numeric.
  # Now we are turning the numbers into factors as is.
  # Number of unique values should be small (20) here since the grid we specify with edarf is c(20,20).
  #
  # for (var_col in var_cols) {
  #   if (is.numeric(ret[[var_col]])) {
  #     ret[[var_col]] <- signif(ret[[var_col]], digits=4) # limit digits before we turn it into a factor.
  #   }
  # }

  if (!is.null(x$data)) { # This is for glm case.
    for (var_col in var_cols) {
      if (is.factor(ret[[var_col]])) { # TODO: Support other types
        actual_ret <- x$data %>% dplyr::group_by(!!rlang::sym(var_col)) %>% dplyr::summarise(Actual=mean(!!rlang::sym(target_col), na.rm=TRUE))
        ret <- ret %>% dplyr::bind_rows(actual_ret)
      }
    }
  }

  ret <- ret %>% tidyr::gather_("x_name", "x_value", var_cols, na.rm = TRUE, convert = FALSE)
  # sometimes x_value comes as numeric and not character, and it was causing error from bind_rows internally done
  # in tidy().
  ret <- ret %>% dplyr::mutate(x_value = as.character(x_value))
  # convert must be FALSE for y to make sure y_name is always character. otherwise bind_rows internally done
  # in tidy() to bind outputs from different groups errors out because y_value can be, for example, mixture of logical and character.
  ret <- ret %>% tidyr::gather("y_name", "y_value", -x_name, -x_value, na.rm = TRUE, convert = FALSE)
  ret <- ret %>% dplyr::mutate(x_name = forcats::fct_relevel(x_name, x$imp_vars)) # set factor level order so that charts appear in order of importance.
  # set order to ret and turn it back to character, so that the order is kept when groups are bound.
  # if it were kept as factor, when groups are bound, only the factor order from the first group would be respected.
  ret <- ret %>% dplyr::arrange(x_name) %>% dplyr::mutate(x_name = as.character(x_name))

  # Set original factor level back so that legend order is correct on the chart.
  # In case of logical, c("TRUE","FALSE") is stored in orig_level, so that we can
  # use it here either way.
  # glm (binomial family) is exception here, since we only show probability of being TRUE,
  # and instead show mean of binned actual data.
  if (!is.null(x$orig_levels) && "glm" %nin% class(x)) {
    ret <- ret %>%  dplyr::mutate(y_name = factor(y_name, levels=x$orig_levels))
  }

  # create mapping from column name (x_name) to facet chart type based on whether the column is numeric.
  chart_type_map <-c()
  df <- NULL
  if ("ranger" %in% class(x)) {
    df <- x$df
  }
  else if ("rpart" %in% class(x)) {
    # use partial_dependence itself for determining chart_type. Maybe this works for other models too?
    df <- x$partial_dependence
  }
  else if (!is.null(x$data)) {  # For glm case.
    df <- x$data
  }
  else { # For lm case
    df <- x$model
  }
  for(col in colnames(df)) {
    chart_type_map <- c(chart_type_map, is.numeric(df[[col]]))
  }
  chart_type_map <- ifelse(chart_type_map, "line", "scatter")
  names(chart_type_map) <- colnames(df)

  ret <- ret %>%  dplyr::mutate(chart_type = chart_type_map[x_name])
  ret <- ret %>% dplyr::mutate(x_name = x$terms_mapping[x_name]) # map variable names to original.
  ret
}
