#' Combine multiple dummy/indicator columns into one delimited text column.
#'
#' @param data A data frame.
#' @param columns Character vector of source column names (dummy/indicator
#'   columns, one per answer option).
#' @param output_column Name of the new combined column. Must not be one of
#'   `columns`.
#' @param selected_value The value that marks a column as "selected" for a
#'   row. Default `1`.
#' @param option_name_type One of "remove_prefix" (strip `option_name_prefix`
#'   from each column name to derive the option label) or "full_column_name"
#'   (use each column name verbatim as the option label).
#' @param option_name_prefix Required when `option_name_type` is
#'   "remove_prefix"; must be a prefix of every name in `columns`.
#' @param separator Delimiter joining selected option names. Default `","`.
#' @param no_selection Value used for a row where no column matches
#'   `selected_value`. Default `NA_character_`.
#' @param remove_original Drop the original `columns` from the result.
#'   Default `FALSE`.
#' @return `data` with the new combined column added (and, optionally, the
#'   source columns removed).
#' @export
combine_multiple_response_column <- function(
  data,
  columns,
  output_column,
  selected_value = 1,
  option_name_type = c("remove_prefix", "full_column_name"),
  option_name_prefix = NULL,
  separator = ",",
  no_selection = NA_character_,
  remove_original = FALSE
) {

  option_name_type <- match.arg(option_name_type)

  if (!((is.character(no_selection) && length(no_selection) == 1) || (length(no_selection) == 1 && is.na(no_selection)))) {
    stop("no_selection must be a character string or NA.")
  }
  no_selection <- as.character(no_selection)

  if (length(columns) == 0) {
    stop("At least one column must be specified.")
  }

  if (anyDuplicated(columns)) {
    stop("columns must not contain duplicates.")
  }

  missing_columns <- setdiff(columns, names(data))
  if (length(missing_columns) > 0) {
    stop(
      "The following columns do not exist in the data: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  if (output_column %in% columns) {
    stop("output_column must be different from the source columns.")
  }

  if (option_name_type == "remove_prefix") {
    if (is.null(option_name_prefix)) {
      stop(
        "option_name_prefix must be specified when ",
        "option_name_type is 'remove_prefix'."
      )
    }
    if (!all(startsWith(columns, option_name_prefix))) {
      stop(
        "option_name_prefix does not match the beginning of all selected columns."
      )
    }
    option_names <- substring(columns, nchar(option_name_prefix) + 1)
  } else {
    option_names <- columns
  }

  # Position of the FIRST selected source column in the ORIGINAL column
  # order -- the new combined column lands there, not always at position 1.
  # e.g. selecting columns 2-5 puts the new column at position 2.
  orig_names <- names(data)
  first_selected_index <- min(match(columns, orig_names))

  values <- data[, columns, drop = FALSE]
  sel_mat <- do.call(cbind, lapply(values, function(value) {
    !is.na(value) & (value == selected_value)
  }))

  result <- vapply(seq_len(nrow(sel_mat)), function(i) {
    sel <- sel_mat[i, ]
    if (!any(sel, na.rm = TRUE)) {
      no_selection
    } else {
      paste(option_names[sel], collapse = separator)
    }
  }, character(1))

  data[[output_column]] <- result

  if (remove_original) {
    data <- data[, setdiff(names(data), columns), drop = FALSE]
  }

  # Reassemble: columns that originally sat before the first selected
  # column (and are still present -- some may have been dropped by
  # remove_original) keep their order and go before the new column;
  # everything else -- from the first selected column onward, still
  # present, in original order -- goes after. `setdiff` preserves the
  # order of its first argument, so this never needs an explicit sort.
  remaining_names <- setdiff(names(data), output_column)
  names_before <- intersect(orig_names[seq_len(first_selected_index - 1)], remaining_names)
  names_after <- setdiff(remaining_names, names_before)

  data <- data[, c(names_before, output_column, names_after), drop = FALSE]

  data
}
