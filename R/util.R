#' Column name parser
#' This function is from https://github.com/tidyverse/broom/blob/master/R/utilities.R
#' @export
col_name <- function(x, default = stop("Please supply column name", call. = FALSE)) {
  if (is.character(x))
    return(x)
  if (identical(x, quote(expr = )))
    return(default)
  if (is.name(x))
    return(as.character(x))
  if (is.null(x))
    return(x)
  stop("Invalid column specification", call. = FALSE)
}

#' Simple cast wrapper that spreads columns which is choosed as row and col into matrix.
#' Note that working on data frame with group_by is not supported.
#' @param data Data frame to cast
#' @param row Column name to be used as row
#' @param col Column name to be used as column
#' @param val Column name to be used as value. Default is number of rows
#' @param fun.aggregate Aggregate function for duplicated row and col
#' @param fill Values to fill NA.
#' @param time_unit Unit of time to aggregate key_col if key_col is Date or POSIXct#' @param time_unit Unit of time to aggregate key_col if key_col is Date or POSIXct. NULL doesn't aggregate.
#' @param na.rm If NA in val should be removed
#' @export
simple_cast <- function(data, row, col, val=NULL, fun.aggregate=mean, fill=0, time_unit=NULL, na.rm = FALSE) {
  loadNamespace("reshape2")
  loadNamespace("tidyr")


  if (!row %in% colnames(data)) {
    stop(paste0(row, " is not in column names"))
  }

  if (!col %in% colnames(data)) {
    stop(paste0(col, " is not in column names"))
  }

  # noraml na causes error in reshape2::acast so it has to be NA_real_
  if (is.na(fill)) {
    fill <- NA_real_
  }

  if (!is.null(val) && na.rm) {
    data <- data %>%
      dplyr::filter(!is.na(!!as.symbol(val)))
  }

  # remove NA from row and column
  data <- tidyr::drop_na(data, !!rlang::sym(row), !!rlang::sym(col))

  if ((inherits(data[[row]], "Date") ||
      inherits(data[[row]], "POSIXct")) &&
      !is.null(time_unit)) {
    data[[row]] <- lubridate::floor_date(data[[row]], unit = time_unit)
  }

  # validation
  uniq_row <- unique(data[[row]], na.rm=TRUE)
  uniq_col <- unique(data[[col]], na.rm=TRUE)
  suppressWarnings({
    # length(uniq_row)*length(uniq_col) become NA if it exceeds 2^31
    if (is.na(length(uniq_row)*length(uniq_col))) {
      # The number of data is supported under 2^31 by reshape2::acast
      stop("Data is too large to make a matrix for calculation.")
    }
  })

  fml <- as.formula(paste0("`", row, "`~`", col, "`"))
  if (is.null(val)) {
    # use sparse = TRUE and as.matrix because xtabs returns table object with occurance and it causes error in kmeans
    mat <- xtabs(as.formula(paste0("~", "`", row , "`", "+", "`", col, "`")), data = data, sparse = TRUE) %>%  as.matrix()
    mat[mat == 0] <- fill
    mat
  } else {
    if(!val %in% colnames(data)){
      stop(paste0(val, " is not in column names"))
    }
    #data %>%  reshape2::acast(fml, value.var=val, fun.aggregate=fun.aggregate, fill=fill)
    df <- data %>% dplyr::group_by(!!rlang::sym(row), !!rlang::sym(col))
    # TODO: handle name conflict with .temp_value_col and group cols.
    # NAs in val column is already filtered out, and we don't need to add na.rm = TRUE to fun.aggregate.
    df <- df %>% dplyr::summarize(.temp_value_col=fun.aggregate(!!rlang::sym(val)))
    # NAs in col column is already filtered out and we don't need to handle it.
    df <- df %>% dplyr::ungroup() %>% tidyr::spread(key = !!rlang::sym(col), value = .temp_value_col, fill=fill)

    df <- df %>% tibble::column_to_rownames(var=row)
    x <- df %>% as.matrix()
  }
}

#' Cast data to sparse matrix by choosing row and column from a data frame
#' @param count If val is NULL and count is TRUE, the value becomes count of the row and col set. Otherwise, it's binary data of row and col set.
#' @export
sparse_cast <- function(data, row, col, val=NULL, fun.aggregate=sum, count = FALSE) {
  loadNamespace("dplyr")
  loadNamespace("tidyr")
  loadNamespace("Matrix")

  if(!row %in% colnames(data)){
    stop(paste0(row, " is not in column names"))
  }

  if(!col %in% colnames(data)){
    stop(paste0(col, " is not in column names"))
  }

  # remove NA from row and col
  data <- tidyr::drop_na(data, !!rlang::sym(row), !!rlang::sym(col))

  if(is.null(val)){
    # if there's no value column, it creates binary sparse matrix.
    row_fact <- as.factor(data[[row]])
    col_fact <- as.factor(data[[col]])
    if(count){
      sparseMat <- xtabs(as.formula(paste0("~", "`", row , "`", "+", "`", col, "`")), data = data, sparse = TRUE)
    } else {
      sparseMat <- Matrix::sparseMatrix(
        i = as.integer(row_fact),
        j = as.integer(col_fact),
        dims = c(length(levels(row_fact)), length(levels(col_fact))),
        dimnames = list(levels(row_fact), levels(col_fact))
        )
    }
  } else {
    if(!val %in% colnames(data)){
      stop(paste0(val, " is not in column names"))
    }
    # Basic behaviour of Matrix::sparseMatrix is sum.
    # If fun.aggregate is different, it should be aggregated by it.
    if(!identical(fun.aggregate, sum)){
      # create a formula to aggregate duplicated row and col pairs
      # ex: ~mean(val)
      fml <- as.formula(paste0("~", as.character(substitute(fun.aggregate)), "(", val, ")"))

      # execute the formula to each row and col pair
      data <- dplyr::group_by(data, !!!rlang::syms(c(row, col))) %>%
        dplyr::summarise_(.dots=setNames(list(fml), val)) %>%
        dplyr::ungroup()
    }

    row_fact <- as.factor(data[[row]])
    col_fact <- as.factor(data[[col]])

    na_index <- is.na(data[[val]])
    zero_index <- data[[val]] == 0

    valid_index <- na_index | !zero_index

    sparseMat <- Matrix::sparseMatrix(
      i = as.integer(row_fact[valid_index]),
      j = as.integer(col_fact[valid_index]),
      x = as.numeric(data[[val]][valid_index]),
      dims = c(length(levels(row_fact)), length(levels(col_fact))),
      dimnames = list(levels(row_fact), levels(col_fact))
      )
  }

  sparseMat
}

#' as.matrix from select argument or cast by three columns
#' @export
to_matrix <- function(df, select_dots, by_col=NULL, key_col=NULL, value_col=NULL, fill=0, fun.aggregate=mean) {
  should_cast <- !(is.null(by_col) & is.null(key_col) & is.null(value_col))
  if(should_cast) {
    if(is.null(by_col) | is.null(key_col) | is.null(value_col)){
      stop("all by, key and value should be defined")
    }
    simple_cast(
      df,
      by_col,
      key_col,
      value_col,
      fun.aggregate = fun.aggregate,
      fill=fill,
      na.rm = TRUE
    )
  } else {
    loadNamespace("dplyr")
    dplyr::select_(df, .dots=select_dots) %>%  as.matrix()
  }
}

#' Gather only right upper half of matrix - where row_num > col_num
#' @param mat Matrix to be converted to data frame
#' @param names Dimension names of input matrix
#' @param diag If diagonal values should be returned
#' @param cnames Column names of output
#' @param na.rm If NA should be removed from the result
#' @param zero.rm If 0 should be removed from the result
#' @export
upper_gather <- function(mat, names=NULL, diag=NULL, cnames = c("Var1", "Var2", "value"), na.rm = TRUE, zero.rm = TRUE) {
  loadNamespace("Matrix")
  if(is.vector(mat)){
    # This is basically for dist function
    # It provides numeric vector of upper half

    # Calculate the side of matrix
    dim_size <- sqrt(2*length(mat)+1/4)+1/2
    if(is.null(names)){
      names <- seq(dim_size)
    } else {
      if(length(names) != dim_size){
        stop("number of names doesn't match matrix dimension")
      }
    }

    # create a triangler matrix to melt
    # use NA_real_ for performance
    trimat <- matrix(data=NA_real_, nrow=length(names), ncol=length(names))
    # fill only lower half of the matrix (transpose later to keep the order)
    trimat[row(trimat)>col(trimat)] <- as.numeric(mat)
    colnames(trimat) <- names
    rownames(trimat) <- names
    if(!is.null(diag)){
      # fill diagonal elements
      trimat[row(trimat)==col(trimat)] = rep(diag, length(names))
    }
    mat_to_df(t(trimat), na.rm=na.rm, cnames=cnames, zero.rm = zero.rm)
  } else {
    # diag can be NULL or FALSE
    if(is.null(diag)){
      diag <- FALSE
    }
    # use transpose and lower tri to make output order clean
    tmat <- Matrix::t(mat)
    c_names <- colnames(tmat)
    r_names <- rownames(tmat)
    if(is.null(c_names)){
      c_names <- seq(ncol(tmat))
    }
    if(is.null(r_names)){
      r_names <- seq(nrow(tmat))
    }

    # remove 0 if zero.rm is TRUE
    ind_mat <- if(zero.rm){
      tmat != 0
    } else {
      is.na(tmat) | !is.na(tmat) # Just return matrix of same shape with TRUE for all the values.
    }
    # preserve NA if na.rm is FALSE
    if(!na.rm){
      ind_mat <- is.na(ind_mat) | ind_mat
    }
    # get indice of matrix
    ind <- Matrix::which(ind_mat, arr.ind = TRUE)

    # remove duplicated pairs
    # by comparing indice
    filtered <- if(diag) {
      ind[ind[,2] <= ind[,1], ]
    } else {
      ind[ind[,2] < ind[,1], ]
    }

    # when there is only one index pairs,
    # filtered becomes a vector, not matrix
    # but matrix is expected later
    # so should be converted to matrix with
    # one row
    if(is.vector(filtered)){
      filtered <- t(as.matrix(filtered))
    }

    # this creates pairs of row and column indices
    # make a vector of upper half of matrix
    row <- r_names[filtered[,1]]
    col <- c_names[filtered[,2]]
    val <- tmat[filtered]
    df <- data.frame(
      Var1=col,
      Var2=row,
      value=val, stringsAsFactors = F)
    colnames(df) <- cnames
    df
  }
}

#' prevent conflict of 2 character vectors and avoid it by adding .new to elements in the second
#' @export
avoid_conflict <- function(origin, new, suffix = ".new") {
  conflict <- new %in% origin
  while(any(conflict)){
    new[conflict] <- paste(new[conflict], suffix, sep="")
    conflict <- new %in% origin
  }
  new
}

#' check grouped column
#' @export
grouped_by <- function(df){
  dplyr::group_vars(df)
}

#' matrix to dataframe with gathered form
#' @param mat Matrix to be converted to data frame
#' @param cnames Column names of output
#' @param na.rm If NA should be removed from the result
#' @param zero.rm If 0 should be removed from the result
#' @param diag If diagonal values should be returned
#' @export
mat_to_df <- function(mat, cnames=NULL, na.rm=TRUE, zero.rm = TRUE, diag=TRUE) {
  loadNamespace("reshape2")
  df <- reshape2::melt(t(mat), na.rm=na.rm)

  if(zero.rm){
    df <- df[is.na(df[[3]]) | df[[3]] != 0, ]
  }

  if(!diag){
    df <- df[df[[1]]!=df[[2]],]
  }

  # make the first column to be sorted
  df <- df[,c(2,1,3)]
  if(!is.null(colnames)){
    colnames(df) <- cnames
  }

  if (!is.character(df[,1])) { # Can be a factor. Also can be integer if the origin column name was number.
    df[,1] <- as.character(df[,1])
  }

  if (!is.character(df[,2])) { # Can be a factor. Also can be integer if the origin column name was number.
    df[,2] <- as.character(df[,2])
  }

  df
}

#' Cast the vector to the same type as the original.
to_same_type <- function(vector, original) {
  if(is.null(original)){
    vector
  }
  else if(is.factor(original)) {
    if(all(vector[!is.na(vector)] %in% levels(original))){
      # if original is factor and vector has all values,
      # should return factor with same levels
      factor(vector, levels = levels(original))
    } else {
      as.factor(vector)
    }
  } else if(is.integer(original)) {
    as.integer(vector)
  } else if(inherits(original, "Date")) {
    # when original data is Date.
    # if the column is wrapped with lubridate function, it's possible that data is converted to numeric like year
    # and character like month name. For these cases, it fails with "character string is not in a standard unambiguous format" error
    # so fallback to original value.
    tryCatch({as.Date(vector, tz = lubridate::tz(original))},
              error = function(e){
                vector
              }
    )
  } else if(inherits(original, "POSIXct")) {
    # when original data is POSIXct
    # if the column is wrapped with lubridate function, it's possible that data is converted to numeric like year
    # and character like month name. For these cases, it fails with "character string is not in a standard unambiguous format" error
    # so fallback to original value.
    tryCatch({ as.POSIXct(vector, tz = lubridate::tz(original))},
             error = function(e) {
               vector
             }
    )
  } else if (is.numeric(original)) {
    as.numeric(vector)
  } else if(is.character(original)) {
    as.character(vector)
  } else if (is.logical(original)) {
    as.logical(vector)
  }
}


#' Not %in% function
#' @export
`%nin%` <- function (x, table) match(x, table, nomatch = 0L) == 0L

#' get number of elements in list data type column for each row
#' @export
list_n <- function(column) {
  sapply(column, length)
}

#' extract elements from each row of list type column or data frame type column
#' @export
list_extract <- function(column, position = 1, rownum = 1) {

  if(position==0){
    stop("position 0 is not supported")
  }

  if(is.data.frame(column[[1]])){
    if(position<0){
      sapply(column, function(column) {
        index <- ncol(column) + position + 1
        if(is.null(column[rownum, index]) | index <= 0) {
          # column[rownum, position] still returns data frame if it's minus, so position < 0 should be caught here
          NA
        } else {
          column[rownum, index][[1]]
        }
      })
    } else {
      sapply(column, function(column) {
        if(is.null(column[rownum, position])) {
          NA
        } else {
          column[rownum, position][[1]]
        }
      })
    }
  } else {
    if(position<0){
      sapply(column, function(column) {
        index <- length(column) + position + 1
        if(index <= 0){
          # column[rownum, position] still returns data frame if it's minus, so position < 0 should be caught here
          NA
        } else {
          column[index]
        }
      })
    } else {
      sapply(column, function(column) {
        column[position]
      })
    }
  }
}

#' convert list column into text column
#' @export
list_to_text <- function(column, sep = ", ") {
  loadNamespace("stringr")
  ret <- sapply(column, function(x) {
    ret <- stringr::str_c(stringr::str_replace_na(x), collapse = sep)
    if(identical(ret, character(0))){
      # if it's character(0). Not too sure if this would still happen now that we do str_replace_na first.
      NA
    } else {
      ret
    }
  })
  as.character(ret)
}

#' concatinate vectors in a list
#' @export
list_concat <- function(..., collapse = FALSE) {
  lists <- list(...)

  # size of each list
  lengths <- lapply(lists, function(arg){
    length(arg)
  })

  max_index <- which.max(lengths)

  ret <- lapply(seq(lengths[[max_index]]), function(index) {
    val <- unlist(lapply(lists, function(arg){
      arg[[index]]
    }))
  })

  if(collapse){
    ret <- list(unlist(ret))
  }

  ret
}

#' wrapper around sample_n to avoid error caused by fewer rows than size.
#' @export
sample_rows <- function(df, size, seed = NULL, ...) {
  if(!is.null(seed)) {
    set.seed(seed)
  }

  # To evaluate size for each group rather than for entire data frame, just like dplyr::sample_n does, loop through groups by nest/mutate/unnest.
  grouped_cols <- grouped_by(df)
  if (length(grouped_cols) > 0) {
    nested <- df %>% tidyr::nest(.temp.data=-(!!grouped_cols)) #TODO: avoid possibility of column name conflict between .temp.data and group_by columns.
  } else {
    nested <- df %>% tidyr::nest(.temp.data=everything()) # Without .temp.data=everything(), warning is displayed.
  }

  ret <- nested %>% dplyr::mutate(.temp.data = purrr::map(.temp.data, function(df) {
    if (!is.null(size) && nrow(df) > size) {
      slice_sample(df, n = size, ...)
    }
    else {
      df
    }
  }))

  ret <- ret %>% tidyr::unnest(cols=.temp.data)
  # For some reason, the output after unnest has group_by columns whose order is reverted.
  # ungroup, group_by is to set the order of group_by columns back to the original.
  if (length(grouped_cols) > 0) {
    ret <- ret %>% dplyr::ungroup() %>% dplyr::group_by(!!!rlang::syms(grouped_cols))
  }
  ret
}

#' replace sequence of spaces or periods with
#' single space or period, then trim spaces on both ends.
#' @export
str_clean <- function(words){
  # change \n, \t into space
  words <- stringr::str_replace_all(words, "\n|\t", " ")
  # change continuous spaces into one space
  words <- stringr::str_replace_all(words, " +", " ")
  # change continuous period into one period
  words <- stringr::str_replace_all(words, "\\.\\.+", ".")
  # remove spaces on the both side
  words <- stringr::str_trim(words)
}

#' count word patterns
#' @export
str_count_all <- function(text, patterns, remove.zero = TRUE) {
  # string count for each pattern list
  lapply(text, function(text_elem) {
    countList <- lapply(patterns, function(pattern) {
      stringr::str_count(text_elem, pattern)
    })
    count <- as.numeric(countList)
    # if remove.zero is FALSE, it returns all element
    return_elem <- (!remove.zero | count > 0)
    data.frame(.count=count[return_elem], .pattern=patterns[return_elem], stringsAsFactors = FALSE)
  })
}

#' Normalize characters in the text according to Unicode Normalization Forms.
#' This is a wrapper around stringi::stri_trans_nfkc to give it a user-friendly name.
#' @export
str_normalize <- function(text) {
  stringi::stri_trans_nfkc(text)
}

#' convert df to numeric matrix
#' @param colnames Vector of column names or lazy dot for select arg. ex:lazyeval::lazy_dots(...)
as_numeric_matrix_ <- function(df, columns) {
  loadNamespace("dplyr")

  orig_mat <- df[,columns] %>%
    as.matrix()

  ret <- orig_mat %>%
    as.numeric() %>%
    matrix(nrow = nrow(df))
  # set colnames because re-constructing matrix by as.numeric eraces column names
  colnames(ret) <- colnames(orig_mat)
  ret
}

#' evaluate select argument
#' @param dots Lazy dot for select arg. ex:lazyeval::lazy_dots(...)
#' @param excluded Excluded column names
#' @export
evaluate_select <- function(df, .dots, excluded = NULL) {
  loadNamespace("dplyr")
  tryCatch({
    ret <- setdiff(colnames(dplyr::select_(df, .dots=.dots)), excluded)
    if(length(ret) == 0){
      stop("no column selected")
    }
    ret
  }, error = function(e){
    loadNamespace("stringr")
    if(stringr::str_detect(e$message, "not found")) {
      stop("undefined columns selected")
    }
    stop(e$message)
  })
}

#' re-build arguments of a function as string
#' @param call This expects returned value from match.call()
#' @param exclude Argument names that should be excluded for expansion
#' @export
expand_args <- function(call, exclude = c()) {
  excluded <- call[!names(call) %in% exclude]
  args <- excluded[-1]
  if (is.null(args)) {
    ""
  } else {
    names(args) <- names(excluded[-1])
    arg_char <- paste(vapply(seq(length(args)) , function(index) {
      arg_name <- names(args)[[index]]
      arg_value <- if(is.character(args[[index]])) paste0('"', as.character(args[index]), '"') else as.character(args[index])
      if(is.null(arg_name)) {
        arg_value
      } else if(arg_name == ""){
        # this have to be separated from is.null because is.null(arg_name) | arg_name == "" returns logical(0)
        arg_value
      } else {
        paste0(arg_name, " = ", arg_value , "")
      }
    }, FUN.VALUE = ""), collapse = ", ")
  }
}

#' get sampled indice from data frame
#' @export
sample_df_index <- function(df, rate, seed = NULL, ordered = FALSE) {
  if (!ordered) {
    if(!is.null(seed)){
      set.seed(seed)
    }
    sample(seq(nrow(df)), nrow(df) * rate)
  }
  else {
    # Return indexes above threshold determined by rate.
    ceiling(nrow(df)*(1-rate)):nrow(df)
  }
}

#' slice of 2 dimensional data that can handle empty vector
#' @export
safe_slice <- function(data, index, remove = FALSE) {
  ret <- if(remove){
    if(is.null(index)){
      data
    } else if(length(index) == 0){
      data
    } else {
      data[-index, ]
    }
  } else {
    if(is.null(index)){
      data[c(), ]
    } else if(length(index) == 0){
      data[c(), ]
    } else {
      data[index, ]
    }
  }

  if(is.vector(ret)){
    mat <- matrix(ret, nrow = 1)
    colnames(mat) <- names(ret)
    mat
  } else {
    ret
  }
}

#' Add fitted response value to augment result in prediction functions
#' @param data Data frame to augment (expected to have ".fitted" column augmented by broom::augment)
#' @param model model that has $family$linkinv attribute (normally glm model)
#' @param response_label column to be augmented as fitted response values
add_response <- function(data, model, response_label = "predicted_response") {
  # fitted values are converted to response values through inverse link function
  # for example, inverse of logit function is used for logistic regression
  if (!is.null(data$.fitted)) {
    data[[response_label]] <- if (nrow(data) == 0) {
      numeric(0)
    } else {
      model$family$linkinv(data[[".fitted"]])
    }
  }
  data
}

#' move a column to a different index
#' @param df Data frame whose column will be moved
#' @param cname Column name to be moved
#' @param position Column index to move to
#' @export
move_col <- function(df, cname, position) {
  # get column index to move
  cname_posi = which(colnames(df) == cname)
  if(length(cname_posi) == 0){
    stop("no column matches cname")
  }else if (length(cname_posi) > 1){
    stop("duplicated cname is indicated")
  }

  if(cname_posi == position){
    # no change in this case
    ret <- df
  } else {
    # create a new index for columns
    # for example, suppose 8th column goes to 3rd column in 10 columns
    # 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
    # should be
    # 1, 2, 8, 3, 4, 5, 6, 7, 9, 10
    # in this case,
    # 1, 2 are regarded as start below
    # 8, 3, 4, 5, 6, 7 are regarded as inside below
    # 9, 10 are regarded as end below

    vec = seq(ncol(df))
    n <- cname_posi
    m <- position

    start <- if(n == 1 | m == 1){
      # start should be empty in this case
      c()
    } else {
      seq(min(c(n, m)) - 1)
    }

    inside <- if (n>m) {
      # n comes to left in this case
      c(n, m:(n-1))
    } else {
      # n comes to right in this case
      c((n+1):m, n)
    }

    end <- if( n == length(vec) | m == length(vec)) {
      # end should be empty in this case
      c()
    } else {
      (max(c(n, m))+1):length(vec)
    }

    order <- c(start, inside, end)

    ret <- df[, order]
  }

  ret
}

#' Unix time numeric values to POSIXct
#' @param data Numeric vector to convert to date
#' @export
unixtime_to_datetime <- function(data){
  # referred from http://stackoverflow.com/questions/27408131/convert-unix-timestamp-into-datetime-in-r
  as.POSIXct(as.numeric(data), origin="1970-01-01", tz='GMT')
}

# get binary prediction scores
get_score <- function(act_label, pred_label) {
  tp <- pred_label & act_label
  fp <- pred_label & !act_label
  tn <- !pred_label & !act_label
  fn <- !pred_label & act_label

  true_positive <- sum(tp, na.rm = TRUE)
  false_positive <- sum(fp, na.rm = TRUE)
  true_negative <- sum(tn, na.rm = TRUE)
  false_negative <- sum(fn, na.rm = TRUE)

  test_size <- true_positive + false_positive + true_negative + false_negative

  precision <- true_positive / sum(pred_label, na.rm = TRUE)
  recall <- true_positive / sum(act_label, na.rm = TRUE)
  specificity <- true_negative / sum(!act_label, na.rm = TRUE)
  accuracy <- (true_positive + true_negative) / test_size
  misclassification_rate <- 1 - accuracy
  f_score <- 2 * (precision * recall) / (precision + recall)

  # modify the name for column name
  accuracy_rate <- accuracy

  data.frame(
    f_score,
    accuracy_rate,
    misclassification_rate,
    precision,
    recall,
    specificity,
    true_positive,
    false_positive,
    true_negative,
    false_negative,
    test_size
  )
}

# get optimized binary prediction scores
get_optimized_score <- function(actual_val, pred_prob, threshold = "f_score") {
  # accracy was changed to accuracy_rate, so should work with both
  if (threshold == "accuracy") {
    threshold <- "accuracy_rate"
  }

  # threshold can be optimized to the result below
  accept_optimize <- c(
    "f_score",
    "accuracy_rate",
    "precision",
    "recall",
    "specificity"
  )

  # try 100 threshold to search max
  max_values <- NULL
  max_value <- -1
  for (thres in ((seq(101) - 1) / 100)){

    pred_label <- pred_prob >= thres

    score <- get_score(actual_val, pred_label)

    if (!threshold %in% accept_optimize) {
      stop(paste0("threshold must be chosen from ", paste(accept_optimize, collapse = ", ")))
    } else if (is.nan(score[[threshold]])) {
      # if nan, pass to avoid error
    } else if (max_value < score[[threshold]]){
      max_values <- score
      max_values[["threshold"]] <- thres
      max_value <- score[[threshold]]
    }
  }
  max_values
}

#' Put prefix and suffix to column names
append_colnames <- function(df, prefix = "", suffix = "") {
  colnames(df) <- avoid_conflict(colnames(df), stringr::str_c(prefix, colnames(df), suffix, sep = ""))
  df
}

#' Returns half-width of confidence interval of given vector. NAs are skipped and not counted.
#' This is useful when used in dplyr::summarize().
#' It seems there is no commonly accepted name for half-width of confidence interval.
#' Reference for naming: https://ncss-wpengine.netdna-ssl.com/wp-content/themes/ncss/pdf/Procedures/PASS/Confidence_Intervals_for_One_Mean.pdf
#' Here we name it confint_radius.
#' @export
confint_radius <- function(x, level=0.95) {
  n <- sum(!is.na(x))
  s <- sd(x, na.rm = TRUE)
  error <- qt((level+1)/2, df=n-1)*s/sqrt(n)
  error
}

#' Calculate the confidence interval range (half-width of confidence interval)
#' of a given vector.
#' A synonym to confint_radius.
confint_mean <- confint_radius

#' Calculate the confidence interval range (half-width of confidence interval)
#' from a sample size and an sd values of a group.
#' See the confint_radius for the implementation detail.
#'
#` @param sd - standard deviation of the group.
#` @param n - sample size of the group.
calc_confint_mean <- function (sd, n, level=0.95) {
  error <- qt((level+1)/2, df=n-1)*sd/sqrt(n)
  error
}

#' Returns half-width of confidence interval of population proportion of the given logical vector. NAs are skipped and not counted.
#' This is useful when used in dplyr::summarize().
#' Reference: http://www.r-tutor.com/elementary-statistics/interval-estimation/interval-estimate-population-proportion
#' @export
prop_confint_radius <- function(x, level=0.95) {
  n <- sum(!is.na(x))
  t <- sum(x, na.rm = TRUE)
  p <- t/n
  error <- qnorm((level+1)/2)*sqrt(p*(1-p)/n)
  error
}


#' Calculate the confidence interval range (half-width of confidence interval)
#' of a population proportion of a given vector.
#' A synonym to prop_confint_radius.
confint_ratio <- prop_confint_radius

#' Calculate the confidence interval range (half-width of confidence interval)
#' of a population proportion from a size and a target ratio of a group.
#' See the prop_confint_radius for the implementation detail.
#'
#` @param ratio - target ratio (0-1) of the group.
#` @param n - sample size of the group.
calc_confint_ratio <- function (ratio, n, level=0.95) {
  error <- qnorm((level+1)/2)*sqrt(ratio*(1-ratio)/n)
  error
}

#' get confidence interval value
#' @param val Predicted value
#' @param conf_int Confidence interval to get
#' @export
get_confint <- function(val, se, conf_int = 0.95) {
  critval=qnorm(conf_int,0,1)
  val + critval * se
}

#' SE version of pivot. For backward compatibility.
#' @export
pivot_ <- function(df, row_cols, col_cols, row_funs = NULL, col_funs = NULL, value_col = NULL, ...) {
  pivot(df, row_cols = row_cols, col_cols = col_cols, row_funs = row_funs, col_funs = col_funs, value = value_col, ...)
}

#' Calculate a pivot table.
#' @param df Data frame to pivot
#' @param row_cols - Columns to be the rows of the resulting pivot table.
#' @param col_cols - Columns to be the columns of the resulting pivot table.
#' @param row_funs - Functions to be applied on row_cols before grouping.
#' @param col_funs - Functions to be applied on col_cols before grouping.
#' @param value - Column name for value. If null, values are count
#' @param fun.aggregate - Function to aggregate duplicated columns
#' @param fill - Value to be filled for missing values
#' @param na.rm - If na should be removed from values
#' @param cols_sep - If na should be removed from values
#' @export
pivot <- function(df, row_cols = NULL, col_cols = NULL, row_funs = NULL, col_funs = NULL, value = NULL, fun.aggregate = mean, fill = NA, na.rm = TRUE, cols_sep = "_") {
  validate_empty_data(df)

  value_col <- if(!missing(value)){
    tidyselect::vars_select(names(df), !! rlang::enquo(value))
  }

  # Output row column names can be specified as names of row_cols. Extract them.
  if (!is.null(names(row_cols))) {
    new_row_cols <- names(row_cols)
  }
  else {
    new_row_cols <- row_cols
  }

  # Create new_col_cols, which is output column names of summarize_group.
  # Since new_col_cols are purely internal in this function, no need to look at names(col_cols) unlike row_cols.
  # Just make sure to make them unique.
  if (!is.null(col_funs)) {
    new_col_cols <- if_else(col_funs == "none", col_cols, paste0(col_cols, '_', col_funs))
  }
  else {
    new_col_cols <- col_cols
  }

  all_cols <- c(row_cols, col_cols)
  all_funs <- c(row_funs, col_funs)
  all_new_cols <- c(new_row_cols, new_col_cols)
  group_cols_arg <- all_cols
  names(group_cols_arg) <- all_new_cols

  # remove rows with NA categories. TODO: Why do we need this? Can it be an old reshape2::acast requirement?
  for(var in all_cols) {
    df <- df[!is.na(df[[var]]), ]
  }

  if(!is.null(value_col) && (is.null(fill) || is.na(fill))) {
    # in this case, values in data frame is aggregated values
    # , so default is NA and fill must be NA of the same type
    # with returned values from aggregate function

    # NA should be same type as returned type of fun.aggregate
    if (identical(fun.aggregate, all) || identical(fun.aggregate, any) ) {
      # NA is regarded as logical
      fill <- NA
    } else {
      # NA_real_ is regarded as numeric
      fill <- NA_real_
    }
  } else if (is.null(fill)) {
    # this case is counting row col pairs and default is 0
    fill <- 0
  } else if (is.na(fill)) {
    # this case is counting row col pairs and
    # fill must be a numeric type of NA for data type consistency
    fill <- NA_real_
  }

  pivot_each <- function(df) {
    res <- if(is.null(value_col)) {
      # make a count matrix if value_col is NULL
      df %>% summarize_group(group_cols = group_cols_arg, group_funs = all_funs, value=dplyr::n())
    } else {
      if(na.rm &&
         !identical(na_ratio, fun.aggregate) &&
         !identical(non_na_ratio, fun.aggregate) &&
         !identical(na_pct, fun.aggregate) &&
         !identical(non_na_pct, fun.aggregate) &&
         !identical(na_count, fun.aggregate) &&
         !identical(non_na_count, fun.aggregate)){
        # remove NA, unless fun.aggregate function is one of the above NA related ones.
        df <- df %>% dplyr::filter(!is.na(!!rlang::sym(value_col)))
      }
      df %>% summarize_group(group_cols = group_cols_arg, group_funs = all_funs, value=fun.aggregate(!!rlang::sym(value_col)))
    }
    res <- res %>% dplyr::arrange(!!!rlang::syms(new_col_cols)) # arrange before pivot_wider, so that the create columns are sorted.
    res <- res %>% tidyr::pivot_wider(names_from = !!new_col_cols, values_from=value, values_fill=list(value=!!fill), names_sep=cols_sep)
    res <- res %>% dplyr::arrange(!!!rlang::syms(new_row_cols)) # arrange grouping rows.
    res
  }

  grouped_col <- grouped_by(df)

  # Calculation is executed in each group.
  # Storing the result in this tmp_col and
  # unnesting the result.
  # If the original data frame is grouped by "tmp",
  # overwriting it should be avoided,
  # so avoid_conflict is used here.
  tmp_col <- avoid_conflict(grouped_col, "tmp")
  ret <- df %>%
    dplyr::do_(.dots=setNames(list(~pivot_each(.)), tmp_col)) %>%
    dplyr::ungroup() %>%
    unnest_with_drop(!!rlang::sym(tmp_col))

  # replace NA values in new columns with fill value
  if(!is.na(fill)) {
    # exclude grouping columns and row label column
    newcols <- setdiff(colnames(ret), c(grouped_col, new_row_cols))
    # create key value with list
    # whose keys are value columns
    # and values are fill
    replace <- as.list(rep(fill, length(newcols)))
    names(replace) <- newcols
    ret <- ret %>%
      tidyr::replace_na(replace = replace)
  }

  # grouping should be kept
  if(length(grouped_col) != 0) {
    ret <- dplyr::group_by(ret, !!!rlang::syms(grouped_col))
  }
  ret
}

#' convert values to binary label
#' this is basically as.logical but this handles factor
#' and numeric vector with 2 unique values
binary_label <- function(val) {
  if(is.factor(val)){
    # Need to subtract 1 because the first level in factor is regarded as 1
    # though it should be FALSE.
    val <- as.logical(as.integer(val) - 1)
  } else {
    logi_val <- as.logical(val)
    # if the values are non-zero values,
    # larger value should be regarded as TRUE
    # this is especially for survival analysis,
    # which can take 1(FALSE) and 2(TRUE) as binary labels
    if(all(logi_val[!is.na(logi_val)])){
      # here, all values are numbers that can be regarded as TRUE (non-zero)
      unique_val <- unique(val[!is.na(val)])
      if(length(unique_val) == 2) {
        # here, val has only 2 unique values
        # larger values are regarded as TRUE
        logi_val <- val == max(unique_val)
      } else if (length(unique_val) > 2) {
        stop("binary labels can't have more than 2 unique values")
      }
      # if it is one unique value, as.logical is respected
    }
    val <- logi_val
  }
}

#' function to find column names that can be numeric values
quantifiable_cols <- function(data) {
  ret <- c()
  for (colname in colnames(data)) {
    if(is.numeric(data[[colname]])){
      ret <- c(colname, ret)
    } else if (is.logical(data[[colname]])) {
      ret <- c(colname, ret)
    }
  }
  ret
}

#' get multinomial predicted value results
get_multi_predicted_values <- function(prob_mat, actual_vals = NULL) {
  prob_label <- colnames(prob_mat)[max.col(prob_mat)]
  if(!is.null(actual_vals)){
    prob_label <- to_same_type(prob_label, actual_vals)
  }

  # get max values from each row
  max_prob <- prob_mat[(max.col(prob_mat) - 1) * nrow(prob_mat) + seq(nrow(prob_mat))]
  ret <- as.data.frame(prob_mat) %>%
    append_colnames(prefix = "predicted_probability_")
  ret$predicted_probability <- max_prob
  ret$predicted_label <- prob_label
  ret
}

#' Fill missing values with NA
#' @param indice Indice where the values should be placed in the output vector.
#' @param values Vector to be filled with NA.
#' @param max_index The size of output vector
fill_vec_NA <- function(indice, values, max_index = max(indice, na.rm = TRUE)) {
  ret <- to_same_type(rep(NA, max_index), values)
  ret[indice] <- values
  ret
}

#' Fill missing rows by NA
#' @param indice Row indice where the values should be placed in the output vector.
#' @param mat Matrix to be filled with NA.
#' @param max_index The row size of output matrix
fill_mat_NA <- function(indice, mat, max_index = max(indice, na.rm = TRUE)) {
  if(nrow(mat) != length(indice)) {
    stop("matrix must have the same length of indice")
  }
  na_val <- to_same_type(NA, as.vector(mat))
  ret <- matrix(na_val, nrow = max_index, ncol = ncol(mat))
  colnames(ret) <- colnames(mat)
  ret[indice, ] <- mat
  ret
}

# get data type to distinguish more than typeof function
get_data_type <- function(data) {
  if (is.factor(data)){
    # factor is regarded as integer by typeof
    "factor"
  } else if (inherits(data, "Date")) {
    # Date is regarded as double by typeof
    "Date"
  } else if (inherits(data, "POSIXct")) {
    # POSIXct is regarded as double by typeof
    "POSIXct"
  } else if (inherits(data, "POSIXlt")) {
    # POSIXct is regarded as list by typeof
    "POSIXlt"
  } else {
    typeof(data)
  }
}

# Add confidence interval from .fitted column and .se.fit column.
# This is about t-test for slope of a regression line, but here we estimate
# confidence interval assuming normal distribution, so that we can calculate it
# without having to know sample size.
# https://en.wikipedia.org/wiki/Student%27s_t-test#Slope_of_a_regression_line
add_confint <- function(data, conf_int) {
  # add confidence interval if conf_int is not null and there are .fitted and .se.fit
  if (!is.null(conf_int) & ".se.fit" %in% colnames(data) & ".fitted" %in% colnames(data)) {
    if (conf_int < 0 | conf_int > 1) {
      stop("conf_int must be between 0 and 1")
    }
    conf_low_colname <- avoid_conflict(colnames(data), "conf_low")
    conf_high_colname <- avoid_conflict(colnames(data), "conf_high")
    lower <- (1-conf_int)/2
    higher <- 1-lower
    data[[conf_low_colname]] <- get_confint(data[[".fitted"]], data[[".se.fit"]], conf_int = lower)
    data[[conf_high_colname]] <- get_confint(data[[".fitted"]], data[[".se.fit"]], conf_int = higher)

    # move confidece interval columns next to standard error
    data <- move_col(data, conf_low_colname, which(colnames(data) == ".se.fit") + 1)
    data <- move_col(data, conf_high_colname, which(colnames(data) == conf_low_colname) + 1)
  }
  data
}

#' validate data type of newdata for prediction
#' @param types Named vector. Values are data types and names are column names
#' @param data new data to predict
validate_data <- function(types, data) {
  if(!is.null(types)){
    message <- vapply(names(types), function(name){
      original_type <- types[[name]]
      if(is.null(data[[name]])){
        # can't find a column
        paste0(" ", name, " is NULL in new data")
      } else {
        data_type <- get_data_type(data[[name]])
        if((data_type != original_type) &&
           # difference of factor and character is acceptable
           !(all(c(data_type, original_type) %in% c("character", "factor"))) &&
           # difference of integer and double is acceptable
           !(all(c(data_type, original_type) %in% c("double", "integer")))){
          # data type is different
          paste0(name, ": ", original_type, " - ", data_type)
        } else {
          NA_character_
        }
      }
    }, FUN.VALUE = "")

    if(any(!is.na(message))) {
      stop(paste0("Data type mismatch detected for ", paste0(message[!is.na(message)], collapse = ", ")))
    }
  }
  TRUE
}

#' turn character predictor columns into factors,
#' with the same levels as training data.
#' @param flevels list. Names are column names, and values are factor levels for the columns.
#' @param data new data to predict on.
factorize_data <- function(flevels, data) {
  fcol_names <- names(flevels)
  if (length(flevels) > 0) {
    for (i in 1:length(flevels)) {
      if (class(data[[fcol_names[[i]]]]) == "character") {
        data[[fcol_names[[i]]]] <- factor(data[[fcol_names[[i]]]], levels = flevels[[i]])
      }
    }
  }
  data
}

# This is used in model building functions
# to create meta data.
# It contains terms to create model and
# column types of the variables.
# Its's used for column type validation.
# return value looks like following example.
# list(
#   types = c(col1 = "numeric", col2 = "character"),
#   terms = res ~ col1 + col2
# )
create_model_meta <- function(df, formula) {
  ret <- list()
  tryCatch({
    md_frame <- model.frame(formula, data = df)
    ret$terms <- terms(md_frame, formula)
    # To avoid saving a huge environment when caching with RDS.
    attr(ret$terms, ".Environment") <- NULL

    pred_cnames <- all.vars(ret$terms)[-1]

    # capture column data types info
    types <- vapply(pred_cnames, function(cname) {
      get_data_type(df[[cname]])
    }, FUN.VALUE = "")
    names(types) <- pred_cnames
    ret$types <- types

    # capture factor levels info so that we can use same levels
    # when we preprocess newdata.
    flevels <- list()
    for (cname in pred_cnames) {
      if (is.factor(df[[cname]])) {
        flevels[[cname]] <- levels(df[[cname]])
      }
    }
    ret$flevels <- flevels
  }, error = function(e){
    NULL
  })
  ret
}

#' NSE version of unnest_without_empty_
#' @export
unnest_without_empty <- function(data, nested) {
  nested_col <- col_name(substitute(nested))
  unnest_without_empty_(data, nested_col)
}

#' unnest with removing NULL or empty list
#' @export
unnest_without_empty_ <- function(data, nested_col) {
  validate_empty_data(data)
  empty <- list_n(data[[nested_col]]) == 0
  without_empty <- data[!empty, ]
  if(nrow(without_empty) == 0){
    # returns 0 row data frame,
    # if all values in nested_col are empty
    without_empty
  } else {
    unnest_with_drop(without_empty, !!rlang::sym(nested_col))
  }
}

#' Count TRUE in a vector
#' @param x vector
#' @export
true_count <- function(x) {
  sum(x, na.rm = TRUE)
}

#' Count FALSE in a vector
#' @param x vector
#' @export
false_count <- function(x) {
  sum(!x, na.rm = TRUE)
}

#' Percentage of TRUE in a vector
#' @param x vector
#' @export
true_pct <- function(x) {
  sum(x, na.rm =!all(is.na(x))) / length(x) * 100
}

#' Percentage of FALSE in a vector
#' @param x vector
#' @export
false_pct <- function(x) {
  sum(!x, na.rm =!all(is.na(x))) / length(x) * 100
}

#' Count NA in a vector
#' @param x vector
#' @export
na_count <- function(x) {
  sum(is.na(x))
}

#' Count Non NA in a vector
#' @param x vector
#' @export
non_na_count <- function(x) {
  sum(!is.na(x))
}

#' Percentage of NA in a vector
#' @param x vector
#' @export
na_pct <- function(x) {
  sum(is.na(x)) / length(x) * 100
}

#' Percentage of Non NA in a vector
#' @param x vector
#' @export
non_na_pct <- function(x) {
  sum(!is.na(x)) / length(x) * 100
}

#' Ratio of NA in a vector
#' @param x vector
#' @export
na_ratio <- function(x) {
  sum(is.na(x)) / length(x)
}

#' Ratio of TRUE in a vector
#' @param x vector
#' @export
true_ratio <- function(x) {
  sum(x, na.rm =!all(is.na(x))) / length(x)
}

#' Ratio of FALSE in a vector
#' @param x vector
#' @export
false_ratio <- function(x) {
  sum(!x, na.rm =!all(is.na(x))) / length(x)
}

#' Ratio of Non NA in a vector
#' @param x vector
#' @export
non_na_ratio <- function(x) {
  sum(!is.na(x)) / length(x)
}

#' This is a wrapper of tidyr::unnest
#' to change the default of .drop,
#' so that it always drops other list
#' columns, which is an expected behaviour
#' for usage in this package in most cases.
#' By default, unnest will drop other list columns
#' if unnesting the specified columns requires the
#' rows to be duplicated because of more than
#' 2 rows data frames for example.
unnest_with_drop <- function(df, ...) {
  ret <- df %>% dplyr::select_if(function(x){!is.list(x)}) %>% dplyr::bind_cols(df %>% dplyr::ungroup() %>% dplyr::select(...)) %>% tidyr::unnest(...)
  ret
}

#' validate empty data frame
validate_empty_data <- function(df) {
  if(nrow(df) == 0) {stop("Input data frame is empty.")}
  df
}

#' Execute func to each group with params
#' @param df Data frame
#' @param func Function to execute
#' @param params Parameters for func
#' @export
do_on_each_group <- function(df, func, params = quote(list()), name = "tmp", with_unnest = TRUE) {
  name <- avoid_conflict(colnames(df), name)
  # This is a list of arguments in do clause
  args <- append(list(quote(.)), rlang::call_args(params))
  call <- rlang::new_call(func, as.pairlist(args))
  ret <- df %>%
    # UQ and UQ(get_expr()) evaluates those variables
    dplyr::do(UQ(name) := UQ(rlang::get_expr(call)))
  if (with_unnest) {
    ret %>%
      dplyr::ungroup() %>%
      unnest_with_drop(!!rlang::sym(name))
  } else {
    # Pass on original group_by columns via rowwise().
    # tidy_rowwise() etc. expect this info.
    grouped_cols <- grouped_by(df)
    if (length(grouped_cols) > 0) {
      ret <- ret %>% dplyr::rowwise(grouped_cols)
    } else {
      ret <- ret %>% dplyr::rowwise()
    }
    ret
  }
}

#' @export
do_on_each_group_2 <- function(df, func1, func2, params1 = quote(list()), params2 = quote(list()),
                               name1 = "model1", name2 = "model2") {
  name1 <- avoid_conflict(colnames(df), name1)
  name2 <- avoid_conflict(colnames(df), name2)
  # This is a list of arguments in do clause
  args1 <- append(list(quote(.)), rlang::call_args(params1))
  args2 <- append(list(quote(.)), rlang::call_args(params2))
  call1 <- rlang::new_call(func1, as.pairlist(args1))
  call2 <- rlang::new_call(func2, as.pairlist(args2))
  ret <- df %>%
    # UQ and UQ(get_expr()) evaluates those variables
    dplyr::do(UQ(name1) := UQ(rlang::get_expr(call1)), UQ(name2) := UQ(rlang::get_expr(call2)))
  # Pass on original group_by columns via rowwise().
  # tidy_rowwise() etc. expect this info.
  grouped_cols <- grouped_by(df)
  if (length(grouped_cols) > 0) {
    ret <- ret %>% dplyr::rowwise(grouped_cols)
  } else {
    ret <- ret %>% dplyr::rowwise()
  }
  ret
}

#' @export
#' Utility function that categorizes numeric column based on the type argument.
#' For example, if type argument is aschar, the column value is categorized as character.
categorize_numeric <- function(x, type = "asnum") {
  ret <- NULL
  switch(type,
     asnum = {
       ret <- as.numeric(x)
     },
     asint = {
       ret <- as.integer(x)
     },
     asintby10 = {
       ret <- floor(x/10)*10
     },
     aschar = {
       ret <- as.character(x)
     })
  ret
}

#' @export
extract_from_date <- function(x, type = "fltoyear") {
  ret <- NULL
  switch(type,
    fltoyear = {
      ret <- lubridate::floor_date(x, unit="year")
    },
    # This key is a synonym for fltoyear and is required by Exploratory Desktop for Chart and Summarize Group Dialog.
    # The reason for having the synonym is that Analytics and Chart/Summarize Group Dialog use two different keys for this function.
    rtoyear = {
      ret <- lubridate::floor_date(x, unit="year")
    },
    fltohalfyear = {
      ret <- lubridate::floor_date(x, unit="halfyear")
    },
    # This key is a synonym for fltohalfyear and is required by Exploratory Desktop for Chart and Summarize Group Dialog.
    # The reason for having the synonym is that Analytics and Chart/Summarize Group Dialog use two different keys for this function.
    rtohalfyear = {
      ret <- lubridate::floor_date(x, unit="halfyear")
    },
    fltoquarter = {
      ret <- lubridate::floor_date(x, unit="quarter")
    },
    # This key is a synonym for fltoquarter and is required by Exploratory Desktop for Chart and Summarize Group Dialog.
    # The reason for having the synonym is that Analytics and Chart/Summarize Group Dialog use two different keys for this function.
    rtoq = {
      ret <- lubridate::floor_date(x, unit="quarter")
    },
    fltobimonth = {
      ret <- lubridate::floor_date(x, unit="bimonth")
    },
    # This key is a synonym for fltobimonth and is required by Exploratory Desktop for Chart and Summarize Group Dialog.
    # The reason for having the synonym is that Analytics and Chart/Summarize Group Dialog use two different keys for this function.
    rtobimon = {
      ret <- lubridate::floor_date(x, unit="bimonth")
    },
    fltomonth = {
      ret <- lubridate::floor_date(x, unit="month")
    },
    # This key is a synonym for fltomonth and is required by Exploratory Desktop for Chart and Summarize Group Dialog.
    # The reason for having the synonym is that Analytics and Chart/Summarize Group Dialog use two different keys for this function.
    rtomon = {
      ret <- lubridate::floor_date(x, unit="month")
    },
    fltoweek = {
      ret <- lubridate::floor_date(x, unit="week")
    },
    # This key is a synonym for fltoweek and is required by Exploratory Desktop for Chart and Summarize Group Dialog.
    # The reason for having the synonym is that Analytics and Chart/Summarize Group Dialog use two different keys for this function.
    rtoweek = {
      ret <- lubridate::floor_date(x, unit="week")
    },
    fltoday = {
      ret <- lubridate::floor_date(x, unit="day")
    },
    # This key is a synonym for fltoday and is required by Exploratory Desktop for Chart and Summarize Group Dialog.
    # The reason for having the synonym is that Analytics and Chart/Summarize Group Dialog use two different keys for this function.
    rtoday = {
      ret <- lubridate::floor_date(x, unit="day")
    },
    # This key is required by Exploratory Desktop for Summarize Group Dialog
    rtohour = {
      ret <- lubridate::floor_date(x, unit="hour")
    },
    # This key is required by Exploratory Desktop for Summarize Group Dialog
    rtomin = {
      ret <- lubridate::floor_date(x, unit="minute")
    },
    # This key is required by Exploratory Desktop for Summarize Group Dialog
    rtosec = {
      ret <- lubridate::floor_date(x, unit="second")
    },
    year = {
      ret <- lubridate::year(x)
    },
    halfyear = {
      ret <- (lubridate::month(x)+5) %/% 6
    },
    quarter = {
      ret <- lubridate::quarter(x)
    },
    bimonth = {
      ret <- (lubridate::month(x)+1) %/% 2
    },
    # This key is a synonym for bimonth and is required by Exploratory Desktop for Chart and Summarize Group Dialog.
    # The reason for having the synonym is that Analytics and Chart/Summarize Group Dialog use two different keys for this function.
    bimon = {
      ret <- (lubridate::month(x)+1) %/% 2
    },
    month = {
      ret <- lubridate::month(x)
    },
    # This key is a synonym for month and is required by Exploratory Desktop for Chart and Summarize Group Dialog.
    # The reason for having the synonym is that Analytics and Chart/Summarize Group Dialog use two different keys for this function.
    mon = {
      ret <- lubridate::month(x)
    },
    monthname = {
      ret <- lubridate::month(x, label=TRUE)
    },
    # This key is a synonym for monthname and is required by Exploratory Desktop for Chart and Summarize Group Dialog.
    # The reason for having the synonym is that Analytics and Chart/Summarize Group Dialog use two different keys for this function.
    monname = {
      ret <- lubridate::month(x, label=TRUE)
    },
    monthnamelong = {
      ret <- lubridate::month(x, label=TRUE, abbr=FALSE)
    },
    # This key is a synonym for monthnamelong and is required by Exploratory Desktop for Chart and Summarize Group Dialog.
    # The reason for having the synonym is that Analytics and Chart/Summarize Group Dialog use two different keys for this function.
    monnamelong = {
      ret <- lubridate::month(x, label=TRUE, abbr=FALSE)
    },
    week = {
      ret <- lubridate::week(x)
    },
    week_of_month = {
      ret <- exploratory::week(x, unit="month")
    },
    week_of_quarter = {
      ret <- exploratory::week(x, unit="quarter")
    },
    day = {
      # Convert integer to numeric. mmpf::marginalPrediction we use for partial dependence throws assertion error, if the data is integer and specified grid points are not integer.
      ret <- as.numeric(lubridate::day(x))
    },
    # This key is required by Exploratory Desktop for Summarize Group Dialog
    dayofyear = {
      ret <- lubridate::yday(x)
    },
    # This key is required by Exploratory Desktop for Summarize Group Dialog.
    dayofquarter = {
      ret <- lubridate::qday(x)
    },
    # This key is required by Exploratory Desktop for Summarize Group Dialog.
    dayofweek = {
      ret <- lubridate::wday(x)
    },
    wday = {
      ret <- lubridate::wday(x, label=TRUE)
    },
    # This key is required by Exploratory Desktop for Summarize Group Dialog.
    wdaylong = {
      ret <- lubridate::wday(x, label=TRUE, abbr=FALSE)
    },
    # This key is required by Exploratory Desktop for Chart, Analytics, and Data Wrangling.
    weekend = {
      ret <- weekend(x)
    },
    hour = {
      ret <- lubridate::hour(x)
    },
    minute = {
      ret <- lubridate::minute(x)
    },
    second = {
      ret <- lubridate::second(x)
    })
  ret
}

#' @export
#' It returns Weekend if the provided date is weekend and Weekday if the provided date is weekday.
#' @param x - Date (or POSIXct)
weekend <- function(x){
  ret <- dplyr::if_else(is.na(x), NA_character_,
                        #if it's 1: Sun or 7: Sat, assume it's Weekend.
                        dplyr::if_else(lubridate::wday(x, label = F, week_start = "7") %in% c(1,7),  "Weekend", "Weekday"))
  factor(ret, levels = c("Weekday", "Weekend"))
}

#' @export
extract_from_numeric <- function(x, type = "asdisc") {
  switch(type,
    asnum = {
      ret <- x
    },
    asint = {
      ret <- as.integer(x)
    },
    asintby10 = {
      ret <- floor(x/10) * 10
    },
    aschar = {
      ret <- as.character(x)
    })
  ret
}

#' Calculate R-Squared
#' @param null_model_mean - Mean value the basis null model gives.
#'                          To calculate R-Squared for test data, one from training data should be specified here.
#' @export
r_squared <- function(actual, predicted, null_model_mean=NULL) {
  # https://stats.stackexchange.com/questions/230556/calculate-r-square-in-r-for-two-vectors
  # https://en.wikipedia.org/wiki/Coefficient_of_determination
  if (is.null(null_model_mean)) {
    # if null_model_mean is not specified, use mean of actual.
    null_model_mean <- mean(actual, na.rm=TRUE)
  }
  ret <- 1 - (sum((actual-predicted)^2, na.rm=TRUE)/sum((actual-null_model_mean)^2, na.rm=TRUE))
  ret
}

adjusted_r_squared <- function(rsq, n_observations, df_residual) {
  ret <- 1 - (1 - rsq) * (n_observations - 1) / df_residual
  ret
}

#' Calculate MAE.
#' @param actual - Vector that includes actual value. The part is_test_data is FALSE should be actual value.
#' @param predicted - Vector that includes predicted value. The part is_test_data is TRUE should be predicted value.
#' @param is_test_data - logical vector that indicates test data portion of actual and predicted.
#' @export
mae <- function(actual, predicted, is_test_data) {
  actual <- actual[is_test_data]
  predicted <- predicted[is_test_data]
  ret <- mean(abs(actual-predicted), na.rm=TRUE)
  ret
}

#' Calculate RMSE.
#' @param actual - Vector that includes actual value. The part is_test_data is FALSE should be actual value.
#' @param predicted - Vector that includes predicted value. The part is_test_data is TRUE should be predicted value.
#' @param is_test_data - logical vector that indicates test data portion of actual and predicted.
#' @export
rmse <- function(actual, predicted, is_test_data=NULL) {
  if (!is.null(is_test_data)) {
    actual <- actual[is_test_data]
    predicted <- predicted[is_test_data]
  }
  ret <- sqrt(mean((actual-predicted)^2, na.rm=TRUE))
  ret
}

#' Calculate MAPE.
#' @param actual - Vector that includes actual value. The part is_test_data is FALSE should be actual value.
#' @param predicted - Vector that includes predicted value. The part is_test_data is TRUE should be predicted value.
#' @param is_test_data - logical vector that indicates test data portion of actual and predicted.
#' @export
mape <- function(actual, predicted, is_test_data) {
  actual <- actual[is_test_data]
  predicted <- predicted[is_test_data]
  ret <- mean(abs((actual-predicted)/actual), na.rm=TRUE)
  ret
}

# https://stackoverflow.com/questions/11092536/forecast-accuracy-no-mase-with-two-vectors-as-arguments
computeMASE <- function(forecast, train, test, period){

  # forecast - forecasted values
  # train - data used for forecasting .. used to find scaling factor
  # test - actual data used for finding MASE.. same length as forecast
  # period - in case of seasonal data.. if not, use 1

  forecast <- as.vector(forecast)
  train <- as.vector(train)
  test <- as.vector(test)

  n <- length(train)
  scalingFactor <- mean(abs(train[(period+1):n] - train[1:(n-period)]), na.rm=TRUE)

  et <- abs(test-forecast)
  qt <- et/scalingFactor
  meanMASE <- mean(qt, na.rm=TRUE)
  return(meanMASE)
}

#' Calculate MASE.
#' @param actual - Vector that includes actual value. The part is_test_data is FALSE should be actual value.
#' @param predicted - Vector that includes predicted value. The part is_test_data is TRUE should be predicted value.
#' @param is_test_data - logical vector that indicates test data portion of actual and predicted.
#' @export
mase <- function(actual, predicted, is_test_data, period = 1) {
  train <- actual[!is_test_data]
  test <- actual[is_test_data]
  forecast <- predicted[is_test_data]
  ret <- computeMASE(forecast, train, test, period)
  ret
}


#' Return result of %in% if y is not empty or NULL. Otherwise return TRUE.
#' We use this for filter condition controlled by a variable so that filtering is effectively
#' skipped when the variable is empty or NULL.
#' @export
`%in_or_all%` <- function(x,y) {
  if (length(y) == 0) {
    return(!(x %in% y))
  }
  else {
    return(x %in% y)
  }
}

#' Column reorder function we use from Reorder steps of Exploratory.
#' @export
reorder_cols <- function(df, ...) {
  # use any_of to make it work even if the columns in the arguments do not exist.
  dplyr::select(df, dplyr::any_of(!!purrr::flatten_chr(purrr::map(rlang::quos(...), rlang::as_name))), dplyr::everything())
}

#' @export
excel_numeric_to_date <- function(date_num, date_system = "modern",
                                  include_time = FALSE, round_seconds = TRUE) {
  # working around https://github.com/sfirke/janitor/issues/241
  # by applying as.numeric on the input in case it is integer.
  janitor::excel_numeric_to_date(as.numeric(date_num), date_system = date_system,
                                 include_time = include_time, round_seconds = round_seconds)
}

#' @export
excel_numeric_to_datetime <- function(datetime_num, tz = "", ...) {
  res <- openxlsx::convertToDateTime(as.numeric(datetime_num), tz = tz, ...)
  # Convert output timezone to the specified tz, in addition to reading the number with the tz.
  res <- lubridate::with_tz(res, tz = tz)
  res
}

#' A utility function for One-hot encoding
#' @export
one_hot <- function(df, key) {
  # Avoid conflict with names for temporary columns.
  tmp_value_col <- avoid_conflict(colnames(df), ".tmp_value")
  tmp_id_col <- avoid_conflict(colnames(df), ".tmp_id")

  # Add unique .id column so that spread will not coalesce multiple rows into one row.
  df <- df %>% mutate(!!rlang::sym(tmp_value_col) := 1, !!rlang::sym(tmp_id_col) := seq(n()))
  # Spread the column into multiple columns with name <original column name>_<original value> and value of 1 or 0.
  df %>% tidyr::spread(!!rlang::enquo(key), !!rlang::sym(tmp_value_col), fill = 0, sep = "_") %>% select(-!!rlang::sym(tmp_id_col))
}

# API to get a list of argument names
extract_argument_names <- function(...) {
  q <- rlang::quos(...)
  purrr::map(q, function(x){rlang::quo_name(x)})
}

#'Wrapper function for dplyr::bind_rows to support named data frames when it's called inside dplyr chain.
#'@export
bind_rows <- function(..., id_column_name = NULL, current_df_name = '', force_data_type = FALSE, .id = NULL, encoding = NULL) {
  # for compatiblity with dply::bind_rows
  # if dplyr::bind_rows' .id argument is passed and id_column_name is NA
  # use dplyr::bind_rows' .id argumetn value as id_column_name
  if(!is.null(.id) && is.null(id_column_name)) {
    id_column_name = .id
  }
  # get a list of argument names to resolve data frame names passed to this bind_rows.
  # only exception is the current data frame which is passed via dplyr pipe operation (%>%).
  # it becomes period (.) instead of actual df name.
  args <- extract_argument_names(...)
  # If the dplyr::bind_rows is called within a dplyr chain like df1 %>% dplyr::bind_rows(list(df_2 = df2, df_3 = df3), .id="id"),
  # since df1 does not have a name, the "id" column of the resulting data frame does not have the data frame name for rows from df1.
  # To workaround this issue, set a name to the first data frame with the value specified by fistLabel argument as a pre-process
  # then pass the updated list to dplyr::bind_rows.
  dataframes_updated <- list()
  # Create a list of data frames from arguments passed to bind_rows.
  dataframes <- list()
  # In order to avoid unexpected data structure change by flattening,
  # we call dots_values here instead of dots_list.
  # Since return from dots_values can be a nested list, let's flatten it here.
  purrr::map(rlang::dots_values(...), function(x) {
    if ('data.frame' %in% class(x)) {
      # If x is a data frame, need to enclose it with list() to add to a list. https://stackoverflow.com/questions/33177118/append-a-data-frame-to-a-list
      dataframes <<- c(dataframes, list(x))
    }
    else {
      # Here we assume that x is a list of data frames.
      dataframes <<- c(dataframes, x)
    }
  })

  if(force_data_type || stringr::str_length(current_df_name) >0) {
    index <- 1;
    # for the case where a user passes a list that contains key (data frame name) and value (data frame) pair.
    if(!is.null(names(dataframes))) {
      # iterate data frames list by name as a key.
      for (name in names(dataframes)) {
        # for the first item, it's the data frame passed via %>% operator, so it does not have a key (data frame name) yet.
        # so populate the key with the value passed by first_id argument.
        if(stringr::str_length(current_df_name) > 0 && index == 1) {
          # if force_data_type is set, force character as column data types
          if(force_data_type) {
            dataframes_updated[[current_df_name]] <- dplyr::mutate_all(dataframes[[1]], list(as.character=as.character))
          } else {
            dataframes_updated[[current_df_name]] <- dataframes[[1]]
          }
        } else {
          # if the key (data frame name) is empty, use index instead.
          if(name == "") {
            name = index;
          }
          # force character as column data types
          if(force_data_type) {
            dataframes_updated[[name]] <- dplyr::mutate_all(dataframes[[name]], list(as.character=as.character))
          } else {
            dataframes_updated[[name]] <- dataframes[[name]]
          }
        }
        index <- index + 1
      }
    } else { # for the case that list does not have key (data frame name), use index number.
      for(i in 1:length(dataframes)) {
        # check if we can get each data frame name from the arguments
        df_name <- args[[i]]
        if(is.na(df_name) || df_name == "") {
          # if we cannot find data fram name, use index i instead.
          df_name = i
        }
        # if force_data_type is set, force character as column data types
        if(force_data_type) {
          if(stringr::str_length(current_df_name) > 0) {
            if(i == 1) {  # for the first item, use current_df_name
              dataframes_updated[[current_df_name]] <- dplyr::mutate_all(dataframes[[i]], funs(as.character))
            } else {
              dataframes_updated[[df_name]] <- dplyr::mutate_all(dataframes[[i]], funs(as.character))
            }
          } else {
            dataframes_updated[[i]] <- dplyr::mutate_all(dataframes[[i]], funs(as.character))
          }
        } else {
          # if we need to set data frame name to each row as a new column
          if(stringr::str_length(current_df_name) > 0) {
            if(i == 1) { # for the first item, use current_df_name
              dataframes_updated[[current_df_name]] <- dataframes[[i]]
            } else {
              dataframes_updated[[df_name]] <- dataframes[[i]]
            }
          } else { # otherwise, keep using index
            dataframes_updated[[i]] <- dataframes[[i]]
          }
        }
      }
    }
    # create a name for the column that holds data frame name.
    # and make sure to make the column name uniqe with avoid_conflict API.
    if(!is.null(id_column_name)) {
      new_id <- avoid_conflict(colnames(dataframes_updated[[1]]), id_column_name)
    } else {
      new_id  = id_column_name
    }
    #re-evaluate column data types
    if(force_data_type) {
      # If encoding is passed, use it to set locale argument of readr::type_convert to avoid unwanted garbled character on Windows for non-ascii data with 'unknown' encoding.
      # 'unknown' encoding can happen with dplyr::recode and dplyr::case_when.
      # We are already working them around by having wrappers for those functions in the first place, but we keep this encoding
      # argument so that we can work it around if we find new cases.
      if(!is.null(encoding)) {
        readr::type_convert(dplyr::bind_rows(dataframes_updated, .id = new_id), locale = readr::locale(encoding = encoding))
      } else {
        readr::type_convert(dplyr::bind_rows(dataframes_updated, .id = new_id))
      }
    } else {
      dplyr::bind_rows(dataframes_updated, .id = new_id)
    }
  } else {
    # if .id argument is passed, create a name for the column that holds data frame name.
    # and make sure to make the column name uniqe with avoid_conflict API.
    if(!is.null(id_column_name)) {
      new_id <- avoid_conflict(colnames(dataframes[[1]]), id_column_name)
    } else {
      new_id <- id_column_name
    }
    dplyr::bind_rows(..., .id = new_id)
  }
}

#'Wrapper function for dplyr's set operations to support ignoring data type difference.
set_operation_with_force_character <- function(func, x, y, ...) {
  x <- dplyr::mutate_all(x, funs(as.character))
  y <- dplyr::mutate_all(y, funs(as.character))
  readr::type_convert(func(x, y, ...))
}

#'Wrapper function for dplyr::union to support ignoring data type difference.
#'@export
union <- function(x, y, force_data_type = FALSE, ...) {
  if(!is.na(force_data_type) && class(force_data_type) ==  "logical" && force_data_type == FALSE)  {
    dplyr::union(x, y, ...)
  } else {
    set_operation_with_force_character(dplyr::union, x, y, ...)
  }
}

#'Wrapper function for dplyr::union_all to support ignoring data type difference.
#'@export
union_all <- function(x, y, force_data_type = FALSE, ...) {
  if(!is.na(force_data_type) && class(force_data_type) ==  "logical" && force_data_type == FALSE)  {
    dplyr::union_all(x, y, ...)
  } else {
    set_operation_with_force_character(dplyr::union_all, x, y, ...)
  }
}

#'Wrapper function for dplyr::intersect to support ignoring data type difference.
#'@export
intersect <- function(x, y, force_data_type = FALSE, ...) {
  if(!is.na(force_data_type) && class(force_data_type) ==  "logical" && force_data_type == FALSE)  {
    dplyr::intersect(x, y, ...)
  } else {
    set_operation_with_force_character(dplyr::intersect, x, y, ...)
  }
}

#'Wrapper function for dplyr::setdiff to support ignoring data type difference.
#'@export
setdiff <- function(x, y, force_data_type = FALSE, ...) {
  if(!is.na(force_data_type) && class(force_data_type) ==  "logical" && force_data_type == FALSE)  {
    dplyr::setdiff(x, y, ...)
  } else {
    set_operation_with_force_character(dplyr::setdiff, x, y, ...)
  }
}

#'Wrapper function for dplyr::recode to workaround encoding info getting lost.
#'@export
recode <- function(x, ...) {
  ret <- dplyr::recode(x, ...)
  # Workaround for the issue that Encoding of recoded values becomes 'unknown' on Windows.
  # Such values are displayed fine on the spot, but later if bind_row is applied,
  # they get garbled. Working it around by converting to UTF-8.
  if (Sys.info()['sysname'] == 'Windows' &&
      ((is.character(x) && is.character(ret) &&
        all(Encoding(x) == 'UTF-8') && # Do it only when all values were originally UTF-8, and some turned into 'unknown'.
        all(Encoding(ret) %in% c('UTF-8', 'unknown'))) ||
       (!is.character(x) || is.character(ret)))) { # If original is non-character column like numeric, the resulting column's encoding seems to become 'unknown' too.
    ret <- tryCatch({
      enc2utf8(ret)
    }, error = function(e) { # In case of error, just use the original.
      ret
    })
  }
  ret
}

#'Wrapper function for dplyr::case_when to workaround encoding info getting lost.
#'@export
case_when <- function(x, ...) {
  ret <- dplyr::case_when(x, ...)
  # Workaround for the issue that Encoding of recoded values becomes 'unknown' on Windows.
  # Such values are displayed fine on the spot, but later if bind_row is applied,
  # they get garbled. Working it around by converting to UTF-8.
  if (Sys.info()['sysname'] == 'Windows' &&
      (is.character(ret))) { # The resulting character column's encoding seems to become 'unknown'.
    ret <- tryCatch({
      enc2utf8(ret)
    }, error = function(e) { # In case of error, just use the original.
      ret
    })
  }
  ret
}

# This is written by removing unnecessary part from calculate_cohens_d.
#'Calculate common standard deviation.
#'@export
calculate_common_sd <- function(var1, var2) {
  df <- data.frame(var1=var1, var2=var2)
  summarized <- df %>% dplyr::group_by(var2) %>%
    dplyr::summarize(n=n(), v=var(var1, na.rm=TRUE))

  lx <- summarized$n[[1]] - 1
  ly <- summarized$n[[2]] - 1
  vx <- summarized$v[[1]]
  vy <- summarized$v[[2]]
  csd <- lx * vx + ly * vy
  csd <- csd/(lx + ly)
  csd <- sqrt(csd) # common sd computation
}

# Reference: https://stackoverflow.com/questions/15436702/estimate-cohens-d-for-effect-size
#'Calculate Cohen's d
#'@export
calculate_cohens_d <- function(var1, var2) {
  df <- data.frame(var1=var1, var2=var2)
  summarized <- df %>% dplyr::group_by(var2) %>%
    dplyr::summarize(n=n(), m=mean(var1, na.rm=TRUE), v=var(var1, na.rm=TRUE))

  lx <- summarized$n[[1]] - 1
  ly <- summarized$n[[2]] - 1
  mx <- summarized$m[[1]]
  my <- summarized$m[[2]]
  vx <- summarized$v[[1]]
  vy <- summarized$v[[2]]
  md  <- abs(mx - my) # mean difference (numerator)
  csd <- lx * vx + ly * vy
  csd <- csd/(lx + ly)
  csd <- sqrt(csd) # common sd computation
  cd  <- md/csd # cohen's d
}

# References:
# SSb, SSt, eta squared definition: https://learningstatisticswithr.com/lsr-0.6.pdf
# Cohen's f definition: https://en.wikipedia.org/wiki/Effect_size
# Compared results with sjstats::cohens_f(), and powerAnalysis::ES.anova.oneway()
# Did not use sjstats::cohens_f() to avoid requiring entire sjstats and its dependencies.
# Did not use powerAnalysis::ES.anova.oneway() because it only works for the case all categories
# have same number of observations.
#'Calculate Cohen's f
#'@export
calculate_cohens_f <- function(var1, var2) {
  m <- mean(var1, na.rm = TRUE)
  df <- data.frame(var1=var1, var2=var2)
  summarized <- df %>% dplyr::group_by(var2) %>%
    dplyr::mutate(diff_between = mean(var1, na.rm=TRUE) - m, diff_total = var1 - m) %>% dplyr::ungroup() %>%
    dplyr::summarize(ssb=sum(diff_between^2, na.rm=TRUE), sst=sum(diff_total^2, na.rm=TRUE))
  ssb <- summarized$ssb # Sum of squares between groups
  sst <- summarized$sst # Total sum of squares
  f <- sqrt(ssb/(sst - ssb))
  f
}

# Reference: https://rdrr.io/github/markushuff/PsychHelperFunctions/src/R/cohens_w.R
#'Calculate Cohen's w from Chi-Square value and total number of observations.
#'@export
calculate_cohens_w <- function(chi_sq, N) {
  sqrt(chi_sq/N)
}

# References:
# Cohen's f2 definition: https://en.wikipedia.org/wiki/Effect_size
#'Calculate Cohen's f squared, which is an effect size of F-test for multiple regression.
#'@export
calculate_cohens_f_squared <- function(r2) {
  f2 <- r2 / (1 - r2)
  f2
}

# References:
# https://stats.stackexchange.com/questions/415037/effect-size-calculation-for-kruskal-wallis-mean-rank-test
# https://rcompanion.org/handbook/F_08.html
#'Calculate epsilon squared, which is an effect size of Kruskal-Wallis test.
#'@export
calculate_epsilon_squared <- function(KW, N) {
  H = KW$statistic
  Epsilon2 = H / (N-1)
  Epsilon2
}

#'Calculates mode. Function name is capitalized to avoid conflict with base::mode(), which does something other than calculating mode.
# Reference: https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
#'@export
get_mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

# Returns logical vector that indicates the position of rows in df that has categorical values
# that does not appear in training_df. TRUE means such a row with unknown categorical value.
# Used to remove such rows from test/new data before predicting with the model, to avoid error.
get_unknown_category_rows_index_vector <- function(df, training_df) {
  # list of unique values of each column of training_df.
  uniq_index <- purrr::map(training_df, function(x){
    if(is.character(x) || is.factor(x) || is.logical(x)) {
      unique(x)
    }
    else {
      NULL
    }
  })

  # list of vectors each of which is logical vector indicating location of unknown values.
  # TRUE means unknown value at the row position.
  unknown_indexes <- purrr::map2(uniq_index, names(uniq_index), function(unique_values, col_name) {
    if (is.null(unique_values)) {
      FALSE
    }
    else {
      df[[col_name]] %nin% unique_values
    }
  })

  # Combine unknown_indexes into one logical vector that indicates location of rows with unknown values.
  ret <- purrr::reduce(unknown_indexes,function(x,y){x|y})
  ret
}

# Converts logical vector such as the output from get_unknown_category_rows_index_vector into
# vector of index integer of TRUE rows.
get_row_numbers_from_index_vector <- function(index_vector) {
  seq(length(index_vector))[index_vector]
}

#' Returns NA value included in prediction result excluding NA
#' @param value - prediction results without NA
#' @param n_data - original data length
#' @param na_row_numbers - row numbers containing the NA value of data
restore_na <- function(value, na_row_numbers){
  n_data <- length(value) + length(na_row_numbers)
  na_at <- if (!is.null(na_row_numbers)) {
    seq_len(n_data) %in% as.integer(na_row_numbers)
  } else {
    NULL
  }
  return_value <- rep(NA, time = n_data)

  if(length(na_at) > 0){
    return_value[!na_at] <- value
    return_value
    if (is.factor(value)) {
      return_value <- levels(value)[return_value]
      return_value <- factor(return_value, levels=levels(value))
      return_value
    } else {
      return_value
    }
  } else {
    value
  }
}

# Returns a quosure that can be used as right-hand-side of arguments of mutate. Used in mutate_predictors and group_by arguments for summarize_group.
column_mutate_quosure <- function(func, cname) {
  if("call" %in% class(func)) { # TODO: better way to detect expr. Note that rlang::is_expression() returns TRUE for any expression not limited to output from rlang::expr().
    # func is an expression with . representing the column to be mutated. e.g. rlang::expr(log(., base=2))
    rlang::quo(eval(func, envir=list(.=UQ(rlang::sym(cname)))))
  } else if(is.function(func)) { # func is already a function. Use it as is.
    rlang::quo(UQ(func)(UQ(rlang::sym(cname))))
  } else if(is.na(func) || length(func)==0 || func == "none"){
    rlang::quo((UQ(rlang::sym(cname))))
  } else if (func %in% c("fltoyear","rtoyear",
      "fltohalfyear",
      "rtohalfyear",
      "fltoquarter",
      "rtoq",
      "fltobimonth",
      "rtobimon",
      "fltomonth",
      "rtomon",
      "fltoweek",
      "rtoweek",
      "fltoday",
      "rtoday",
      "rtohour",
      "rtomin",
      "rtosec",
      "year",
      "halfyear",
      "quarter",
      "bimonth",
      "bimon",
      "month",
      "mon",
      "monthname",
      "monname",
      "monthnamelong",
      "monnamelong",
      "week",
      "week_of_month",
      "week_of_quarter",
      "dayofyear",
      "dayofquarter",
      "dayofweek",
      "day",
      "wday",
      "wdaylong",
      "weekend",
      "hour",
      "minute",
      "second")) {
    # For date column, call extract_from_date
    rlang::quo(extract_from_date(UQ(rlang::sym(cname)), type = UQ(func)))
  } else if (func %in% c("asnum","asint","asintby10","aschar")) {
    # For numeric column, call categorize_numeric
    rlang::quo(categorize_numeric(UQ(rlang::sym(cname)), type = UQ(func)))
  } else { # For non-numeric and non-date related function case.
    rlang::quo(UQ(func)(UQ(rlang::sym(cname))))
  }
}

# Wrapper function that takes care of dplyr::group_by and dplyr::summarize as a single step.
#' @param .data - data frame
#' @param group_cols - Columns to group_by
#' @param group_funs - Functions to apply to group_by columns
#' @param ... - Name-value pairs of summary functions. The name will be the name of the variable in the result. The value should be an expression that returns a single value like min(x), n(), or sum(is.na(y)).
#' @export
summarize_group <- function(.data, group_cols = NULL, group_funs = NULL, ...) {
  ret <- if(length(group_cols) == 0) {
    .data %>% dplyr::summarize(...)
  } else {
    # if group_cols argument is passed, make sure to ungroup first so that it won't throw an error
    # when group_cols conflict with group columns in previous steps.
    .data <- .data %>% dplyr::ungroup()
    groupby_args <- list() # default empty list
    name_list <- list()
    name_index = 1
    # If group_by columns and associated categorizing functionts are provided,
    # quote the columns/functions with rlang::quo so that dplyr can understand them.
    if (!is.null(group_cols) && !is.null(group_funs)) {
      groupby_args <- purrr::map2(group_funs, group_cols, column_mutate_quosure)
      # Set names of group_by columns in the output.
      name_list <- names(group_cols) # If names are specified in group_cols, use them for output.
      if (is.null(name_list)) { # If name is not specified, use original column names.
        name_list <- group_cols
      }
      names(groupby_args) <- name_list
      # make sure to ungroup result
      .data %>% dplyr::group_by(!!!groupby_args) %>% summarize(...) %>% dplyr::ungroup()
    } else {
      if(!is.null(group_cols)) { # In case only group_by columns are provied, group_by with the columns
        # make sure to ungroup result
        .data %>% dplyr::group_by(!!!rlang::syms(group_cols)) %>% summarize(...) %>% dplyr::ungroup()
      } else { # In case no group_by columns are provided,skip group_by
        .data %>% dplyr::summarize(...)
      }
    }
  }
  # For integer columns (like # of rows, unique), change them to numeric columns for better usability.
  # Without this, when the next transform step is a mutate using case_when that contains the # of rows as a condition,
  # case_when command fails due to data type mismatch (integer vs numeric).
  # For example, the below command fails with: Error : Problem with `mutate()` input `count`. must be a double vector, not an integer vector.
  #
  #   activities %>%
  #      summarize_group(group_cols = c(`userid` = "userid"), group_funs = c("none"), count = n()) %>%
  #         mutate(count = case_when(count > 10 ~ 10, TRUE ~ count))
  #
  ret %>% dplyr::mutate_if(is.integer, as.numeric)
}

# Wrapper function that takes care of dplyr::group_by and dplyr::mutate as a single step.
#' @param .data - data frame
#' @param group_cols - Columns to group_by
#' @param group_funs - Functions to apply to group_by columns
#' @param ... - Name-value pairs of mutate functions. The name will be the name of the variable in the result. The value should be an expression that returns a single value like min(x), n(), or sum(is.na(y)).
#' @export
window_calculation <- function(.data, keep_group = FALSE, group_cols = NULL, group_funs = NULL, ...) {
  ret <- if(length(group_cols) == 0) {
    .data %>% dplyr::mutate(.data = .data, ...)
  } else {
    # if group_cols argument is passed, make sure to ungroup first so that it won't throw an error
    # when group_cols conflict with group columns in previous steps.
    .data <- .data %>% dplyr::ungroup()
    groupby_args <- list() # default empty list
    name_list <- list()
    name_index = 1
    # If group_by columns and associated categorizing functionts are provided,
    # quote the columns/functions with rlang::quo so that dplyr can understand them.
    if (!is.null(group_cols) && !is.null(group_funs)) {
      groupby_args <- purrr::map2(group_funs, group_cols, column_mutate_quosure)
      # Set names of group_by columns in the output.
      name_list <- names(group_cols) # If names are specified in group_cols, use them for output.
      if (is.null(name_list)) { # If name is not specified, use original column names.
        name_list <- group_cols
      }
      names(groupby_args) <- name_list
      # make sure to ungroup result
      .data %>% dplyr::group_by(!!!groupby_args) %>% dplyr::mutate(...)
    } else {
      if(!is.null(group_cols)) { # In case only group_by columns are provied, group_by with the columns
        # make sure to ungroup result
        .data %>% dplyr::group_by(!!!rlang::syms(group_cols)) %>% dplyr::mutate(...)
      } else { # In case no group_by columns are provided,skip group_by
        .data %>% dplyr::mutate(...)
      }
    }
  }
  # For integer columns (like # of rows, unique), change them to numeric columns for better usability.
  # Without this, when the next transform step is a mutate using case_when that contains the # of rows as a condition,
  # case_when command fails due to data type mismatch (integer vs numeric).
  #
  ret <- ret %>% dplyr::mutate_if(is.integer, as.numeric)
  if (keep_group) {
    ret
  } else {
    ret %>% dplyr::ungroup()
  }
}

bind_expr <- function(expr1, expr2) {
  rlang::expr(!!expr1 & !!expr2)
}

aggregate_if <- function(x, aggregateFunc, ..., na.rm = T) {
  conditions <- dplyr:::dplyr_quosures(...)
  # iterate the if conditions and creates consolidated conditions connected with &
  flatten_conditions_exprs <- conditions %>% purrr::reduce(bind_expr)
  # create a dummy data frame that has 2 columns (one with original x and the other with TRUE/FALSE results for specified conditions)
  # then extract the only x column as a vector (same as doing df$x) with dplyr::pull()
  condition <- tibble::tibble(x = x) %>% dplyr::mutate(exp_internal_condition_col = !!flatten_conditions_exprs) %>% dplyr::pull(exp_internal_condition_col)

  if (aggregateFunc == "sum") {
    sum(x[condition], na.rm = na.rm)
  } else if (aggregateFunc == "mean" || aggregateFunc == "average") {
    mean(x[condition], na.rm = na.rm)
  } else if (aggregateFunc == "count") {
    sum(condition, na.rm = na.rm)
  } else if (aggregateFunc == "median") {
    median(x[condition], na.rm = na.rm)
  } else if (aggregateFunc == "min") {
    min(x[condition], na.rm = na.rm)
  } else if (aggregateFunc == "max") {
    max(x[condition], na.rm = na.rm)
  }
}

#' export
sum_if <- function(x, ..., na.rm = TRUE) {
  aggregate_if(x, "sum", ..., na.rm = na.rm)
}

#' export
count_if <- function(x, ..., na.rm = TRUE) {
  aggregate_if(x, "count", ..., na.rm = na.rm)
}

#' export
average_if <- function(x, ..., na.rm = TRUE) {
  aggregate_if(x, "average", ..., na.rm = na.rm)
}

#' export
mean_if <- function(x, ..., na.rm = TRUE) {
  aggregate_if(x, "mean", ..., na.rm = na.rm)
}

#' export
median_if <- function(x, ..., na.rm = TRUE) {
  aggregate_if(x, "median", ..., na.rm = na.rm)
}

#' export
min_if <- function(x, ..., na.rm = TRUE) {
  aggregate_if(x, "min", ..., na.rm = na.rm)
}

#' export
max_if <- function(x, ..., na.rm = TRUE) {
  aggregate_if(x, "max", ..., na.rm = na.rm)
}

# Wrapper function around apply to apply aggregation function across columns for each row.
# Example Usage:
# airquality %>% mutate(total = summarize_row(across(where(is.numeric)), median, na.rm=TRUE))
#' @param x - data frame
#' @param f - function
#' @export
summarize_row <- function(x, f = mean, ...) {
  apply(x, 1, f, ...)
}

# Maps locale across platforms. e.g. From Japanese_Japan.932 on Windows to ja_JP.UTF-8 on unix.
# We keep LC_TIME locale with models created by Analytics View, so that preprocessor function like wday
# should produce the same results as when the model was created, when the model is used for prediction with new data.
# This function is used for the cases when model creation and prediction are done on different platforms.
# from/to can take values of "windows" or "unix".
map_platform_locale <- function(locale, from, to) {
  locale_lang <- stringr::str_split(locale, "\\.")[[1]][[1]] # Extract lang part of the locale. e.g. en_US from en_US.UTF-8, English_United States from English_United States.1252
  if (from == "windows" && to == "unix") {
    # platform_locale_mapping data was generated by joining our Locale LOV JSON files we use for Exploratory Desktop UI. (Locales_lov.json and LocalesWin_lov.json)
    ret <- platform_locale_mapping$locale[platform_locale_mapping$locale_windows==locale_lang & !is.na(platform_locale_mapping$locale_windows)]
    # We add .UTF-8, since Sys.setlocale() does not accept locale without encoding, unlike Mac or Windows.
    ret <- paste0(ret, '.UTF-8')
  }
  else if (from == "unix" && to == "windows") {
    ret <- platform_locale_mapping$locale_windows[platform_locale_mapping$locale==locale_lang & !is.na(platform_locale_mapping$locale)]
    # On Windows, we return lang without encoding since Sys.setlocale() accepts lang only, e.g. English_United States, as opposed to English_United States.1252.
  }
  else if (to == "unix") { # No need for locale conversion, but make sure to add .UTF-8, since Sys.setlocale() does not accept locale without encoding, unlike Mac or Windows.
    if (length(stringr::str_split(locale, "\\.")[[1]]) == 1) { # Encoding is not included in the locale string. Add one so that it is accepted on linux.
      ret <- paste0(locale, '.UTF-8')
    }
    else {
      ret <- locale
    }
  }
  else { # Handle same platform. No conversion is necessary.
    ret <- locale
  }
  ret
}

# Mutate predictor columns for preprocessing before feeding to a model. Functions are expressed by tokens we use in our JSON metadata.
# e.g. mutate_predictors(df, cols = c("col1","col2"), funs=list("col1"="log", list("col2_day"="day", "col2_mon"="month")))
mutate_predictors <- function(df, cols, funs) {
  orig_LC_TIME <- Sys.getlocale("LC_TIME")
  orig_lubridate.week.start <- getOption("lubridate.week.start")
  model_LC_TIME <- attr(funs, "LC_TIME")
  model_sysname <- attr(funs, "sysname")
  model_lubridate.week.start <- attr(funs, "lubridate.week.start")
  tryCatch({
    if (!is.null(model_LC_TIME)) {
      if (model_sysname == "Windows") {
        model_platform <- "windows"
      }
      else {
        model_platform <- "unix"
      }
      if (Sys.info()[["sysname"]] == "Windows") {
        this_platform <- "windows"
      }
      else {
        this_platform <- "unix"
      }
      mapped_model_locale <- map_platform_locale(model_LC_TIME, from=model_platform, to=this_platform)
      Sys.setlocale("LC_TIME", mapped_model_locale)
    }
    if (!is.null(model_lubridate.week.start)) {
      options(lubridate.week.start = model_lubridate.week.start)
    }

    missing_cols <- cols[cols %nin% colnames(df)]
    if (length(missing_cols) > 0) {
      stop(paste0("EXP-ANA-1 :: ", jsonlite::toJSON(paste0(missing_cols, collapse=", ")), " :: Columns are required for the model, but do not exist."))
    }
    mutate_args <- purrr::map2(funs, cols, function(func, cname) {
      if (is.list(func)) {
        purrr::map(func, function(func) {
          column_mutate_quosure(func, cname)
        })
      }
      else {
        column_mutate_quosure(func, cname)
      }
    })
    mutate_args <- unlist(mutate_args)
    ret <- df %>% dplyr::mutate(!!!mutate_args)
    ret
  }, finally = {
    Sys.setlocale("LC_TIME", orig_LC_TIME)
    options(lubridate.week.start = orig_lubridate.week.start)
  })
}

#' calc_feature_imp (Random Forest) or exp_rpart (Decision Tree) converts logical columns into factor
#' with level of "TRUE" and "FALSE". This function reverts such columns back to logical.
#' @export
revert_factor_cols_to_logical <- function(df) {
  dplyr::mutate_if(df, function(col) {
    is.factor(col) && length(levels(col)) == 2 && (all(levels(col) == c("TRUE", "FALSE")) || all(levels(col) == c("FALSE", "TRUE")))
  }, as.logical)
}

# Checks if a vector has only integer values.
is_integer <- function(x) {
  # isTRUE is necessary since all.equal does not return FALSE for FALSE case. See ?all.equal.
  is.integer(x) || (is.numeric(x) && isTRUE(all.equal(x, as.integer(x))))
}

# Wrapper function for sample_n
# obsoleted. use slice_sample instead.
sample_n <- function(..., seed = NULL) {
  if(!is.null(seed)) {
    set.seed(seed)
  }
  dplyr::sample_n(...);
}

# Wrapper function for slice_sample
slice_sample <- function(..., seed = NULL) {
  if(!is.null(seed)) {
    set.seed(seed)
  }
  dplyr::slice_sample(...);
}

# Wrapper function for sample_frac
# obsoleted. use sample_slie instead.
sample_frac <- function(..., seed = NULL){
  if(!is.null(seed)) {
    set.seed(seed)
  }
  dplyr::sample_frac(...)
}

# Get the week number of month https://stackoverflow.com/a/58370031
# @deprecated Leave it for a while for Desktop 5.4.0.12 support.
get_week_of_month <- function(date) {
  (5 + lubridate::day(date) + lubridate::wday(lubridate::floor_date(date, "month"))) %/% 7;
}

#' Wrapper function for lubridate::week
#' @export
#' @param date - Date value
#' @param unit - Either "year", "querter" or "month". Default is "year".
week <- function(date, unit="year") {
  if (unit=="month") {
    ceiling(lubridate::day(date) / 7)
  } else if (unit=="quarter") {
    ceiling(lubridate::qday(date) / 7)
  } else {
    # Default: year
    lubridate::week(date)
  }
}


#' API to calculate duration between the start_date and the end_date in the provided time unit.
time_between <- function(start_date, end_date, unit = "years") {
  lubridate::time_length(lubridate::interval(start_date, end_date), unit = unit)
}

#' API to calculate duration between the start_date and the end_date in years.
years_between <- function(start_date, end_date=lubridate::today()) {
  time_between(start_date, end_date)
}

#' API to calculate duration between the start_date and the end_date in months.
months_between <- function(start_date, end_date=lubridate::today()) {
  time_between(start_date, end_date, unit = "months")
}

#' API to calculate duration between the start_date and the end_date in weeks.
weeks_between <- function(start_date, end_date=lubridate::today()) {
  time_between(start_date, end_date, unit = "weeks")
}

#' API to calculate duration between the start_date and the end_date in days.
days_between <- function(start_date, end_date=lubridate::today()) {
  time_between(start_date, end_date, unit = "days")
}

#' API to calculate duration between the start_date and the end_date in hours
hours_between <- function(start_date, end_date=lubridate::now()) {
  time_between(start_date, end_date, unit = "hours")
}

#' API to calculate duration between the start_date and the end_date in minutes
minutes_between <- function(start_date, end_date=lubridate::now()) {
  time_between(start_date, end_date, unit = "minutes")
}

#' API to calculate duration between the start_date and the end_date in seconds
seconds_between <- function(start_date, end_date=lubridate::now()) {
  time_between(start_date, end_date, unit = "seconds")
}

#' Calculates area under ROC. (AUC)
#' @export
#' Reference: https://blog.mbq.me/augh-roc/
auroc <- function(score, bool) {
  not_na <- !(is.na(score) | is.na(bool)) # Index to filter out score-bool pairs with NA in either of them.
  bool <- bool[not_na]
  score <- score[not_na]
  n1 <- sum(!bool)
  n2 <- sum(bool)
  U  <- sum(rank(score)[!bool]) - n1 * (n1 + 1) / 2
  return(1 - U / n1 / n2)
}

#' Calculates time-dependent AUC for survival prediction.
#' score - Vector of predicted risk of event
#' time - Vector of survival time
#' status - logical vector of event status. TRUE: event, FALSE: censor.
#' at - The time at which the time-dependent AUC is calculated.
survival_auroc <- function(score, time, status, at, revert=FALSE) {
  if (revert) {
    score <- -score
  }
  df <- tibble::tibble(score=score, time=time, status=status)
  df <- df %>% filter(!(time < !!at & !status)) %>% mutate(dead = time < !!at | (time == !!at & status))
  auroc(df$score, df$dead)
}

# Our time_unit argument is based on floor_date, but we also need to
# pass down the same info to seq.Date/seq.POSIXct, and to do so,
# some values needs to be converted.
to_time_unit_for_seq <- function(time_unit) {
  if (time_unit == "minute") {
    "min"
  }
  else if (time_unit == "second") {
    "sec"
  }
  else {
    time_unit
  }
}

# Completes a Date/POSIXct column by inserting rows with the skipped Date/POSIXct values with the specified time unit.
complete_date <- function(df, date_col, time_unit = "day") {
  if(inherits(df[[date_col]], "Date")){
    ret <- df %>%
      tidyr::complete(!!rlang::sym(date_col) := seq.Date(min(!!rlang::sym(date_col)), max(!!rlang::sym(date_col)), by = to_time_unit_for_seq(time_unit)))
  } else if(inherits(df[[date_col]], "POSIXct")) {
    ret <- df %>%
      tidyr::complete(!!rlang::sym(date_col) := seq.POSIXt(min(!!rlang::sym(date_col)), max(!!rlang::sym(date_col)), by = to_time_unit_for_seq(time_unit)))
  } else {
    stop("time must be Date or POSIXct.")
  }
  ret
}

# Caluculates cumulative sum of decaying effects.
# It is same as cumsum when r is 1.
#' @param r - After n periods, original effect a decays down to a*r^n.
#' @export
cumsum_decayed <- function(x, r) {
  purrr::accumulate(x, function(x, y){x*r + y})
}

# Caluculates intra group population variance. Used by merge_vars.
#' @param population_vars
#' @param sizes
#' @return variance
intra_group_population_var <- function(population_vars, sizes) {
  weighted.mean(population_vars, sizes)
}

# Caluculates inter group population variance. Used by merge_vars.
#' @param means
#' @param sizes
#' @return variance
inter_group_population_var <- function(means, sizes) {
  tot_mean <- weighted.mean(means, sizes)
  weighted.mean((means - tot_mean)^2, sizes)
}

# Caluculates a variance by merging variances from multiple groups.
#' @param vars - Variances of groups.
#' @param means - Means of groups.
#' @param sizes - Sizes of groups.
#' @return Variance
#' @export
merge_vars <- function(vars, means, sizes) {
  tot_size <- sum(sizes)
  population_vars <- vars * (sizes - 1) / sizes
  population_var <- intra_group_population_var(population_vars, sizes) + inter_group_population_var(means, sizes)
  res <- population_var * tot_size / (tot_size - 1)
  res
}

# Caluculates an SD by merging SDs from multiple groups.
#' @param sds - SDs of groups.
#' @param means - Means of groups.
#' @param sizes - Sizes of groups.
#' @return SD
#' @export
merge_sds <- function(sds, means, sizes) {
  vars <- sds^2
  var_merged <- merge_vars(vars, means, sizes)
  res <- sqrt(var_merged)
  res
}
