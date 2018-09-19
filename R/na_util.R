#' Guess missing values by lm
#' @param target Target vector whose NA is filled
#' @param type This can be "mean", "median", "previous", "predict", "value" or aggregate function
#' @param val This is effective when type is "value" or "predict".
#' When type is "value", NA is replaced by this value.
#' When type is "predict", this is used to predict NA.
#' @param ... Additional vectors to be used to pridict NA when type is lm_predict
#' @export
impute_na <- function(target, type = mean, val = 0, ...) {
  if(typeof(type) == "closure"){
    # type is function in this case
    val <- type(target[!is.na(target)])
    if(length(val) != 1){
      stop("type function must return one value")
    }
    target[is.na(target)] <- val
    target
  } else {
    switch(type, predict = {
      if(val == 0 || length(val) != length(target)){
        # this is when no predictor columns are chosen
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
    }, mean = {
      val <- mean(target, na.rm = TRUE)
      target[is.na(target)] <- val
      target
    }, median = {
      val <- median(target, na.rm = TRUE)
      target[is.na(target)] <- val
      target
    }, value = {
      if(length(val) == 1){
        # if val is length 1, na is filled with the value
        if(is.factor(target)) {
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
        if(is.factor(target)){
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
    }, previous = {
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
fill_between_v <- function(v, .direction="down") {
  filled_downward<-zoo::na.locf(v, na.rm = FALSE)
  filled_upward<-zoo::na.locf(v, fromLast = TRUE, na.rm = FALSE)
  if (.direction == "down") {
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

# same as tidyr::fill, but fills only between non-NA values.
#' @param .direction "down" or "up". (dot-prefixed name honoring dplyr::fill())
#' @export
fill_between <- function(df, ..., .direction="down") {
  # this evaluates select arguments like starts_with
  selected_cols <- dplyr::select_vars(names(df), !!! rlang::quos(...))
  grouped_col <- grouped_by(df)

  each_func <- function(df) {
    if(!is.null(grouped_col)){
      # drop grouping columns
      df <- df[, !colnames(df) %in% grouped_col]
    }
    # fill each specified columns.
    for (col in selected_cols) {
      df[[col]] <- fill_between_v(df[[col]], .direction=.direction)
    }
    df
  }

  tmp_col <- avoid_conflict(colnames(df), "tmp_col")
  df <- df %>%
    dplyr::do_(.dots=setNames(list(~each_func(.)), tmp_col)) %>%
    dplyr::ungroup()
  df <- df %>%  unnest_with_drop_(tmp_col)

  if (length(grouped_col) > 0) { # set group back
    # group_by_ fails with column name with space.
    # tried group_by(!!!grouped_col), but
    # this messes up the result of grouped_by().
    # TODO: come up with a solution.
    df <- df %>% group_by(!!!rlang::syms(grouped_col))
  }
  df 
}
