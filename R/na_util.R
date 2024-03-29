#' Guess missing values by lm
#' @param target Target vector whose NA is filled
#' @param type This can be "mean", "median", "previous", "predict", "value" or aggregate function
#' @param val This is effective when type is "value" or "predict".
#' When type is "value", NA is replaced by this value.
#' When type is "predict", this is used to predict NA.
#' @param ... Additional vectors to be used to pridict NA when type is lm_predict
#' @export
impute_na <- function(target, type = mean, val = 0, ...) {
  if (typeof(type) == "closure") {
    # type is function in this case
    val <- type(target[!is.na(target)])
    if (length(val) != 1){
      stop("type function must return one value")
    }
    target[is.na(target)] <- val
    target
  } else {
    switch(type, predict = {
      # this is for when no predictor column is chosen. val == 0 does not work for this purpose well since it returns vector.
      if (length(val) != length(target)) {
        stop("Please choose predictor columns")
      }
      # list(val, ...) is a list of vectors to predict NA values
      df <- as.data.frame(list(val, ...))
      df$target <- target
      lm_model <- lm(data = df, target ~ ., na.action = na.omit)
      # even if the original column is integer,
      # predicted values can be double and they are appropriate
      # so it's converted to double
      ret <- as.double(target)
      try({
        ret[is.na(ret)] <- suppressWarnings({
          # predict where target is NA
          predict(lm_model, df[is.na(target),])
        })
      })
      ret
    },
    mean = {
      val <- mean(target, na.rm = TRUE)
      target[is.na(target)] <- val
      target
    },
    median = {
      val <- median(target, na.rm = TRUE)
      target[is.na(target)] <- val
      target
    },
    mode = {
      val <- get_mode(target, na.rm = TRUE)
      target[is.na(target)] <- val
      target
    },
    value = {
      if (length(val) == 1){
        # if val is length 1, na is filled with the value
        if (is.factor(target)) {
          # if target is factor, val should be added as target level
          # otherwise, it doesn't replace NA.
          # val might be already in the level but
          # it doesn't cause error, so it's safe
          levels(target) <- c(levels(target), val)
        }
        target[is.na(target)] <- val
      } else {
        # if val is not length 1,
        # NA in target is replaced with the value in the same position
        if (is.factor(target)){
          # if target is factor, val should be added as target level
          # otherwise, it doesn't replace NA.
          # val might be already in the level but
          # it doesn't cause error, so it's safe

          # get val only where target is NA not to get unnecessary level
          needed_val <- dplyr::if_else(is.na(target), as.character(val), NA_character_)
          # update target level so that it can accept the value to be replaced
          levels(target) <- c(levels(target), needed_val)
          # update val level so that it will be the same level with target's
          val <- factor(needed_val, levels = levels(target))
        }
        target <- dplyr::coalesce(target, val)
      }
      target
    },
    previous = {
      z <- zoo::zoo(target) # create zoo object
      z <- zoo::na.locf(z) # fill NA with previous non-NA value
      z <- as.data.frame(z)
      target <- z[[1]] # extract the resulting column
      target
    }, {
      stop(paste0(type, " is not supported as type"))
    })
  }
}

# same as zoo::na.locf, but fills only between non-NA values.
# e.g. fill_between_v(c(NA,2,NA,3,NA)) returns c(NA, 2, 2, 3,NA).
#' @param .direction "down" or "up".
#' @param value - Specific value to fill NA. (Obviously, when specified, .direction has no effect.)
fill_between_v <- function(v, .direction="down", value=NULL) {
  filled_downward<-zoo::na.locf(v, na.rm = FALSE)
  filled_upward<-zoo::na.locf(v, fromLast = TRUE, na.rm = FALSE)
  if (!is.null(value)) {
    ret <- v
    ret[is.na(ret)] <- value
    ret <- ifelse(!is.na(filled_upward) & !is.na(filled_downward), ret, NA)
  }
  else if (.direction == "down") {
    ret <- ifelse(!is.na(filled_upward), filled_downward, NA)
  }
  else { # for "up"
    ret <- ifelse(!is.na(filled_downward), filled_upward, NA)
  }
  if (is.factor(v)) { # if it was factor, get it back to factor since ifelse converts it to integer.
    ret <- factor(ret)
    levels(ret)<-levels(v)
  }
  ret
}

# Same as tidyr::fill, but fills only between non-NA values.
#' @param .direction - "down" or "up". (Dot-prefixed name honoring dplyr::fill())
#' @param value - Specific value to fill NA. (Obviously, when specified, .direction has no effect.)
#'                Note that this is used for all the specified columns regardless of data type,
#'                which can result in conversion of column data type.
#' @export
fill_between <- function(df, ..., .direction="down", value=NULL) {
  # this evaluates select arguments like starts_with
  selected_cols <- tidyselect::vars_select(names(df), !!! rlang::quos(...))
  grouped_col <- grouped_by(df)

  each_func <- function(df) {
    if(!is.null(grouped_col)){
      # drop grouping columns
      df <- df[, !colnames(df) %in% grouped_col]
    }
    # fill each specified columns.
    for (col in selected_cols) {
      df[[col]] <- fill_between_v(df[[col]], .direction=.direction, value=value)
    }
    df
  }

  tmp_col <- avoid_conflict(colnames(df), "tmp_col")
  df <- df %>%
    dplyr::do_(.dots=setNames(list(~each_func(.)), tmp_col)) %>%
    dplyr::ungroup()
  df <- df %>% unnest_with_drop(!!rlang::sym(tmp_col))

  if (length(grouped_col) > 0) { # set group back
    # group_by_ fails with column name with space.
    # tried group_by(!!!grouped_col), but
    # this messes up the result of grouped_by().
    # TODO: come up with a solution.
    df <- df %>% group_by(!!!rlang::syms(grouped_col))
  }
  df 
}

#' Fill NAs in time series data. TODO: Merge into impute_na?
#' @param type - 3 element character vector that specifies types of NA fill for NAs at the begining, in the middle, at the ending of the time series.
#'               For the beginning and ending part:
#'                 "extend" - Extend the first non-NA value (for the beginning), or the last non-NA value (for the ending) to fill the NAs.
#'                 "value" - Fill the NAs with the value specified by val argument.
#'               For the middle part:
#'                 "previous" - Fill with previous non-NA value.
#'                 "value" - Fill with the value of na_fill_value.
#'                 "interpolate" - Linear interpolation.
#'                 "spline" - Spline interpolation.
#'                 NULL - Skip NA fill. Use this only when you know there is no NA.
#' @param val - 3 element numeric vector that specified values to fill NAs at the begining, in the middle, at the ending of the time series when the corresponding type is "value".
#' @export
fill_ts_na <- function(target, time, type = c("value", "previous", "extend"), val = c(0, 0, 0)) {
  if (length(type) == 1) { # If only a single type is given, use it for the mid-section, and fill the beginning and ending with the default types.
    type <- c("value", type, "extend")
  }
  if (length(val) == 1) { # If only a single val is given, use it for the mid-section, and fill the beginning and ending with the default value. 
    val <- c(0, val, 0)
  }

  df_zoo <- zoo::zoo(target, time)

  if (is.null(type)) {
    # skip when it is NULL. this is for the case caller is confident that
    # there is no NA and want to skip overhead of checking for NA.
  }
  else {
    # Fill right values and left values first.
    val_left <- if (type[1] == "value") val[1] else "extend"
    val_right <- if (type[3] == "value") val[3] else "extend"
    df_zoo <- zoo::na.fill(df_zoo, c(val_left, NA, val_right))

    # Then, fill intermediate values.
    if (type[2] == "spline") {
      # na.rm = FALSE to handle the case where we don't fill leading or trailing NAs.
      df_zoo <- zoo::na.spline(df_zoo, na.rm = FALSE)
    }
    else if (type[2] == "interpolate") {
      df_zoo <- zoo::na.approx(df_zoo, na.rm = FALSE)
    }
    else if (type[2] == "previous") {
      df_zoo <- zoo::na.locf(df_zoo, na.rm = FALSE)
    }
    else if (type[2] == "next") {
      df_zoo <- zoo::na.locf(df_zoo, na.rm = FALSE, fromLast = TRUE)
    }
    # TODO: Getting this error with some input with na.StructTS().
    #       Error in rowSums(tsSmooth(StructTS(y))[, -2]) : 'x' must be an array of at least two dimensions
    #
    # else if (type == "StructTS") {
    #   df_zoo <- zoo::na.StructTS(df_zoo)
    # }
    else if (type[2] == "value") {
      # TODO: This would fill the leading or trailing NAs even if we specify NA as the value. Fix it.
      df_zoo <- zoo::na.fill(df_zoo, val[2])
    }
    else {
      stop(paste0(type[2], " is not a valid option for NA fill type."))
    }
  }
  ret <- zoo::coredata(df_zoo)
  ret
}

