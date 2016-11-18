wrap_tbl_df <- function(func, rowwise_func) {
  function(x, ...) {
    # use the rowwise function if it is a one-row tbl_df with all list
    # columns
    if (nrow(x) == 1 && all(sapply(x, function(col) inherits(col, "list")))) {
      return(rowwise_func(x, ...))
    } else {
      # otherwise, do traditional tidy/augment/glancing
      # (generally goes to the data.frame method)
      func(x, ...)
    }
  }
}


apply_rowwise_df <- function(x, object, func, data, ...) {
  # group by columns that are not lists
  groupers <- colnames(x)[sapply(x, function(e) class(e)[1]) != "list"]
  groupers <- setdiff(groupers, object)
  # suppress "group_by" warning
  x <- suppressWarnings(dplyr::group_by_(x, .dots = as.list(groupers)))
  # let the "data" argument specify column (for augment)
  if (!missing(data)) {
    if (as.character(substitute(data)) %in% colnames(x)) {
      data_column <- col_name(substitute(data))
      dplyr::do(x, func(.[[object]][[1]], data = .[[data_column]][[1]], groupers = groupers, ...))
    } else {
      dplyr::do(x, func(.[[object]][[1]], data = data, groupers = groupers,...))
    }
  } else {
    dplyr::do(x, func(.[[object]][[1]], groupers = groupers,...))
  }
}

wrap_rowwise_df_ <- function(func) {
  function(x, object, ...) apply_rowwise_df(x, object, func, ...)
}

wrap_rowwise_df <- function(func) {
  function(x, object, ...) {
    n <- col_name(substitute(object))
    func(x, n, ...)
  }
}

tidy_.rowwise_df <- wrap_rowwise_df_(tidy)
tidy.rowwise_df <- wrap_rowwise_df(tidy_.rowwise_df)
tidy.tbl_df <- wrap_tbl_df(tidy, tidy.rowwise_df)

glance_.rowwise_df <- wrap_rowwise_df_(glance)
glance.rowwise_df <- wrap_rowwise_df(glance_.rowwise_df)
glance.tbl_df <- wrap_tbl_df(glance, glance.rowwise_df)

augment_.rowwise_df <- wrap_rowwise_df_(augment)
augment.rowwise_df <- wrap_rowwise_df(augment_.rowwise_df)
augment.tbl_df <- wrap_tbl_df(augment, augment.rowwise_df)
