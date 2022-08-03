#' @ref{https://gist.github.com/jimhester/a060323a05b40c6ada34}
#' @export
insensitive_join <- function(fun = dplyr::left_join, type = "LEFT") {
  new_fun <- fun
  body(new_fun) <- substitute({
    by <- dplyr:::common_by(by, x, y)

    tmp_by_x <- paste0("_", by$x, "_")
    tmp_by_y <- paste0("_", by$y, "_")
    for (i in seq_along(by$x)) {
      x[[tmp_by_x[[i]]]] <- tolower(x[[by$x[[i]]]])
      y[[tmp_by_y[[i]]]] <- tolower(y[[by$y[[i]]]])
      y[[by$y[[i]]]] <- NULL
    }

    res <- fun(x, y, list(x = tmp_by_x, y = tmp_by_y), suffix = suffix)
    res[tmp_by_x] <- list(NULL)

    res
  })
  new_fun
}

#' Wrapper function for dplyr's inner_join to support case insensitive join.
#' @export
inner_join <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ignorecase = FALSE, source_columns = NULL, target_columns = NULL, ...){
  # Limit source columns to use for join when source_columns are set.
  if (!is.null(source_columns)) {
    x <- x %>% dplyr::select(source_columns)
  }
  # Limit target columns to use for join when target_columns are set.
  if (!is.null(target_columns)) {
    y <- y %>% dplyr::select(target_columns)
  }
  if(ignorecase) {
    insensitive_join(dplyr::inner_join)(x = x, y = y, by = by, copy = copy, suffix = suffix)
  } else {
    dplyr::inner_join(x = x, y = y, by = by, copy = copy, suffix = suffix);
  }
}

#' @export
#' Wrapper function for dplyr's left_join to support case insensitive join.
left_join <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ignorecase = FALSE, source_columns = NULL, target_columns = NULL, ...){
  # Limit source columns to use for join when source_columns are set.
  if (!is.null(source_columns)) {
    x <- x %>% dplyr::select(source_columns)
  }
  # Limit target columns to use for join when target_columns are set.
  if (!is.null(target_columns)) {
    y <- y %>% dplyr::select(target_columns)
  }
  if(ignorecase) {
    insensitive_join(dplyr::left_join)(x = x, y = y, by = by, copy = copy, suffix = suffix)
  } else {
    dplyr::left_join(x = x, y = y, by = by, copy = copy, suffix = suffix);
  }
}

#' @export
#' Wrapper function for dplyr's right_join to support case insensitive join.
right_join <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ignorecase = FALSE, source_columns = NULL, target_columns = NULL, ...){
  # Limit source columns to use for join when source_columns are set.
  if (!is.null(source_columns)) {
    x <- x %>% dplyr::select(source_columns)
  }
  # Limit target columns to use for join when target_columns are set.
  if (!is.null(target_columns)) {
    y <- y %>% dplyr::select(target_columns)
  }
  if(ignorecase) {
    insensitive_join(dplyr::right_join, "RIGHT")(x, y, by = by, copy = copy, suffix = suffix)
  } else {
    dplyr::right_join(x, y, by = by, copy = copy, suffix = suffix);
  }
}

#' @export
#' Wrapper function for dplyr's full_join to support case insensitive join.
full_join <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ignorecase = FALSE, source_columns = NULL, target_columns = NULL, ...) {
  # Limit source columns to use for join when source_columns are set.
  if (!is.null(source_columns)) {
    x <- x %>% dplyr::select(source_columns)
  }
  # Limit target columns to use for join when target_columns are set.
  if (!is.null(target_columns)) {
    y <- y %>% dplyr::select(target_columns)
  }
  if(ignorecase) {
    insensitive_join(dplyr::full_join)(x = x, y = y, by = by, copy = copy, suffix = suffix)
  } else {
    dplyr::full_join(x = x, y = y, by = by, copy = copy, suffix = suffix);
  }
}

#' @export
#' Wrapper function for dplyr's semi_join to support case insensitive join.
semi_join <- function(x, y, by = NULL, copy = FALSE, ignorecase = FALSE, source_columns = NULL, ...) {
  # Limit source columns to use for join when source_columns are set.
  # For semi_join, it does not bring target columns so it only supports source_columns argument.
  if (!is.null(source_columns)) {
    x <- x %>% dplyr::select(source_columns)
  }
  if(ignorecase) {
    insensitive_join(dplyr::semi_join)(x = x, y = y, by = by, copy = copy)
  } else {
    dplyr::semi_join(x = x, y = y, by = by, copy = copy);
  }
}

#' @export
#' Wrapper function for dplyr's anti_join to support case insensitive join.
anti_join <- function(x, y, by = NULL, copy = FALSE, ignorecase = FALSE, source_columns = NULL, ...) {
  # Limit source columns to use for join when source_columns are set.
  # For anit_join, it does not bring target columns so it only supports source_columns argument.
  if (!is.null(source_columns)) {
    x <- x %>% dplyr::select(source_columns)
  }
  if(ignorecase) {
    insensitive_join(dplyr::anti_join)(x = x, y = y, by = by, copy = copy)
  } else {
    dplyr::anti_join(x = x, y = y, by = by, copy = copy);
  }
}







