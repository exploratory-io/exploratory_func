#' Column name parser
#' This function is from https://github.com/tidyverse/broom/blob/master/R/utilities.R
#' @export
col_name <- function(x, default = stop("Please supply column name", call. = FALSE)){
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

#' Simple cast wrapper that spreads columns which is choosed as row and col into matrix
#' @export
simple_cast <- function(data, row, col, val = NULL, fun.aggregate=mean, fill=0){
  loadNamespace("reshape2")
  loadNamespace("tidyr")


  if(!row %in% colnames(data)){
    stop(paste0(row, " is not in column names"))
  }

  if(!col %in% colnames(data)){
    stop(paste0(col, " is not in column names"))
  }

  # noraml na causes error in reshape2::acast so it has to be NA_real_
  if(is.na(fill)){
    fill <- NA_real_
  }

  # remove NA from row and column
  data <- tidyr::drop_na_(data, c(row, col))

  # validation
  uniq_row <- unique(data[[row]], na.rm=TRUE)
  uniq_col <- unique(data[[col]], na.rm=TRUE)
  suppressWarnings({
    # length(uniq_row)*length(uniq_col) become NA if it exceeds 2^31
    if(is.na(length(uniq_row)*length(uniq_col))){
      # The number of data is supported under 2^31 by reshape2::acast
      stop("Data is too large to make a matrix for calculation.")
    }
  })

  fml <- as.formula(paste0("`", row, "`~`", col, "`"))
  if(is.null(val)){
    # use sparse = TRUE and as.matrix because xtabs returns table object with occurance and it causes error in kmeans
    mat <- xtabs(as.formula(paste0("~", "`", row , "`", "+", "`", col, "`")), data = data, sparse = TRUE) %>%  as.matrix()
    mat[mat == 0] <- fill
    mat
  }else{
    if(!val %in% colnames(data)){
      stop(paste0(val, " is not in column names"))
    }
    data %>%  reshape2::acast(fml, value.var=val, fun.aggregate=fun.aggregate, fill=fill)
  }
}

#' Cast data to sparse matrix by choosing row and column from a data frame
#' @param count If val is NULL and count is TRUE, the value becomes count of the row and col set. Otherwise, it's binary data of row and col set.
#' @export
sparse_cast <- function(data, row, col, val=NULL, fun.aggregate=sum, count = FALSE){
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
  data <- tidyr::drop_na_(data, c(row, col))

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
  }else{
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
      data <- dplyr::group_by_(data, .dots=list(as.symbol(row), as.symbol(col))) %>%
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
to_matrix <- function(df, select_dots, by_col=NULL, key_col=NULL, value_col=NULL, fill=0, fun.aggregate=mean){
  should_cast <- !(is.null(by_col) & is.null(key_col) & is.null(value_col))
  if(should_cast){
    if(is.null(by_col) | is.null(key_col) | is.null(value_col)){
      stop("all by, key and value should be defined")
    }
    simple_cast(df, by_col, key_col, value_col, fun.aggregate = fun.aggregate, fill=fill)
  } else {
    loadNamespace("dplyr")
    dplyr::select_(df, .dots=select_dots) %>%  as.matrix()
  }
}

#' Gather only right upper half of matrix - where row_num > col_num
#' @export
upper_gather <- function(mat, names=NULL, diag=NULL, cnames = c("Var1", "Var2", "value")){
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
    mat_to_df(t(trimat), na.rm=TRUE, cnames=cnames)
  }else{
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
    # get indice of non-zero values
    ind <- Matrix::which(tmat != 0, arr.ind = TRUE)

    # remove duplicated pairs
    # by comparing indice
    filtered <- if(diag) {
      ind[ind[,2] <= ind[,1], ]
    } else {
      ind[ind[,2] < ind[,1], ]
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

#' group by other columns so that next action can preserve as many columns as possible
group_exclude <- function(df, ...){
  loadNamespace("dplyr")
  cols <- as.character(substitute(list(...)))[-1]
  excluded <- setdiff(colnames(df), cols)
  # exclude list column to avoid error from dplyr::group_by
  target <- excluded[!sapply(df[,excluded], is.list)]
  dplyr::group_by_(df, .dots=target)
}

#' prevent conflict of 2 character vectors and avoid it by adding .new to elements in the second
#' @export
avoid_conflict <- function(origin, new, suffix = ".new"){
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
  # remove backtick for escaped column names
  stringr::str_replace_all(as.character(attr(df, "vars")), "`", "")
}

#' matrix to dataframe with gathered form
#' @export
mat_to_df <- function(mat, cnames=NULL, na.rm=TRUE, diag=TRUE){
  loadNamespace("reshape2")
  df <- reshape2::melt(t(mat), na.rm=na.rm)

  if(!diag){
    df <- df[df[[1]]!=df[[2]],]
  }

  # make the first column to be sorted
  df <- df[,c(2,1,3)]
  if(!is.null(colnames)){
    colnames(df) <- cnames
  }

  if(is.factor(df[,1])){
    df[,1] <- as.character(df[,1])
  }

  if(is.factor(df[,2])){
    df[,2] <- as.character(df[,2])
  }

  df
}

#' match the type of two vector
same_type <- function(vector, original){
  if(is.factor(original)){
    if(all(vector[!is.na(vector)] %in% levels(original))){
      # if original is factor and vector has all values,
      # should return factor with same levels
      factor(vector, levels = levels(original))
    } else {
      as.factor(vector)
    }
  } else if(is.integer(original)){
    as.integer(vector)
  } else if(is.numeric.Date(original)) {
    as.Date(vector)
  } else if(is.numeric.POSIXt(original)){
    as.POSIXct(vector)
  } else if (is.numeric(original)){
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
list_n <- function(column){
  sapply(column, length)
}

#' extract elements from each row of list type column or data frame type column
#' @export
list_extract <- function(column, position = 1, rownum = 1){

  if(position==0){
    stop("position 0 is not supported")
  }

  if(is.data.frame(column[[1]])){
    if(position<0){
      sapply(column, function(column){
        index <- ncol(column) + position + 1
        if(is.null(column[rownum, index]) | index <= 0){
          # column[rownum, position] still returns data frame if it's minus, so position < 0 should be caught here
          NA
        } else {
          column[rownum, index][[1]]
        }
      })
    } else {
      sapply(column, function(column){
        if(is.null(column[rownum, position])){
          NA
        } else {
          column[rownum, position][[1]]
        }
      })
    }
  } else {
    if(position<0){
      sapply(column, function(column){
        index <- length(column) + position + 1
        if(index <= 0){
          # column[rownum, position] still returns data frame if it's minus, so position < 0 should be caught here
          NA
        } else {
          column[index]
        }
      })
    } else {
      sapply(column, function(column){
        column[position]
      })
    }
  }
}

#' convert list column into text column
#' @export
list_to_text <- function(column, sep = ", "){
  loadNamespace("stringr")
  ret <- sapply(column, function(x) {
    ret <- stringr::str_c(x, collapse = sep)
    if(identical(ret, character(0))){
      # if it's character(0)
      NA
    } else {
      ret
    }
  })
  as.character(ret)
}

#' concatinate vectors in a list
#' @export
list_concat <- function(..., collapse = FALSE){
  lists <- list(...)

  # size of each list
  lengths <- lapply(lists, function(arg){
    length(arg)
  })

  max_index <- which.max(lengths)

  ret <- lapply(seq(lengths[[max_index]]), function(index){
    val <- unlist(lapply(lists, function(arg){
      arg[[index]]
    }))
  })

  if(collapse){
    ret <- list(unlist(ret))
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
str_count_all <- function(text, patterns, remove.zero = TRUE){
  # string count for each pattern list
  lapply(text, function(text_elem){
    countList <- lapply(patterns, function(pattern){
      stringr::str_count(text_elem, pattern)
    })
    count <- as.numeric(countList)
    # if remove.zero is FALSE, it returns all element
    return_elem <- (!remove.zero | count > 0)
    data.frame(.count=count[return_elem], .pattern=patterns[return_elem], stringsAsFactors = FALSE)
  })
}

#' convert df to numeric matrix
#' @param colnames Vector of column names or lazy dot for select arg. ex:lazyeval::lazy_dots(...)
as_numeric_matrix_ <- function(df, columns){
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
evaluate_select <- function(df, .dots, excluded = NULL){
  loadNamespace("dplyr")
  tryCatch({
    ret <- setdiff(colnames(dplyr::select_(df, .dots=.dots)), excluded)
    if(length(ret) == 0){
      stop("no column selected")
    }
    ret
  }, error = function(e){
    loadNamespace("stringr")
    if(stringr::str_detect(e$message, "not found")){
      stop("undefined columns selected")
    }
    stop(e$message)
  })
}

#' re-build arguments of a function as string
#' @param call This expects returned value from match.call()
#' @param exclude Argument names that should be excluded for expansion
#' @export
expand_args <- function(call, exclude = c()){
  excluded <- call[!names(call) %in% exclude]
  args <- excluded[-1]
  if (is.null(args)) {
    ""
  } else {
    names(args) <- names(excluded[-1])
    arg_char <- paste(vapply(seq(length(args)) , function(index){
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
sample_df_index <- function(df, rate, seed = NULL){
  if(!is.null(seed)){
    set.seed(seed)
  }
  sample(seq(nrow(df)), nrow(df) * rate)
}

#' slice of 2 dimensional data that can handle empty vector
#' @export
safe_slice <- function(data, index, remove = FALSE){
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
add_response <- function(data, model, response_label = "predicted_response"){
  # fitted values are converted to response values through inverse link function
  # for example, inverse of logit function is used for logistic regression
  data[[response_label]] <- if (nrow(data) == 0) {
    numeric(0)
  } else {
    model$family$linkinv(data[[".fitted"]])
  }
  data
}

#' move a column to a different index
#' @param df Data frame whose column will be moved
#' @param cname Column name to be moved
#' @param position Column index to move to
#' @export
move_col <- function(df, cname, position){
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

    end <- if( n == length(vec) | m == length(vec)){
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
  as.POSIXct(data, origin="1970-01-01", tz='GMT')
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
get_optimized_score <- function(actual_val, pred_prob, threshold = "f_score"){
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
append_colnames <- function(df, prefix = "", suffix = ""){
  colnames(df) <- avoid_conflict(colnames(df), stringr::str_c(prefix, colnames(df), suffix, sep = ""))
  df
}

#' get confidence interval value
#' @param val Predicted value
#' @param conf_int Confidence interval to get
#' @export
get_confint <- function(val, se, conf_int = 0.95) {
  critval=qnorm(conf_int,0,1)
  val + critval * se
}

#' NSE version of pivot_
#' @export
pivot <- function(df, formula, value = NULL, ...) {
  value_col <- col_name(substitute(value))
  pivot_(df, formula = formula, value_col = value_col, ...)
}

#' pivot columns based on formula
#' @param df Data frame to pivot
#' @param formula lhs is composed of columns for rows and rhs is for cols
#' For example, data1 + data2 ~ var1 + var2 makes a matrix of combinations of
#' values in data1, data2 pair and var, var2 pair
#' @param value_col Column name for value. If null, values are count
#' @param fun.aggregate Function to aggregate duplicated columns
#' @param fill Value to be filled for missing values
#' @param na.rm If na should be removed from values
#' @export
pivot_ <- function(df, formula, value_col = NULL, fun.aggregate = mean, fill = NULL, na.rm = TRUE) {
  # create a column name for row names
  # column names in lhs are collapsed by "_"
  cname <- paste0(all.vars(lazyeval::f_lhs(formula)), collapse = "_")

  vars <- all.vars(formula)

  # remove NA data
  for(var in vars) {
    df <- df[!is.na(df[[var]]), ]
  }

  if(!is.null(value_col) && (is.null(fill) || is.na(fill))){
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
  } else if (is.null(fill)){
    # this case is counting row col pairs and default is 0
    fill <- 0
  } else if (is.na(fill)) {
    # this case is counting row col pairs and
    # fill must be a numeric type of NA for data type consistency
    fill <- NA_real_
  }

  pivot_each <- function(df) {
    casted <- if(is.null(value_col)) {
      # make a count matrix if value_col is NULL
      reshape2::acast(df, formula = formula, fun.aggregate = length, fill = fill)
    } else {
      if(na.rm && !identical(na_pct, fun.aggregate) && !identical(na_count, fun.aggregate) ){
        # remove NA
        # if fun.aggregate function is na_pct or na_count,
        # NA should not be removed because the user wants that information
        df <- df[!is.na(df[[value_col]]),]
      }
      reshape2::acast(df, formula = formula, value.var = value_col, fun.aggregate = fun.aggregate, fill = fill)
    }
    casted %>%
      as.data.frame %>%
      tibble::rownames_to_column(var = cname)
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
    unnest_with_drop_(tmp_col)

  # replace NA values in missing columns in some groups with fill
  if(!is.na(fill)) {
    newcols <- colnames(ret)[!colnames(ret) %in% grouped_col]
    replace <- as.list(rep(fill, length(newcols)))
    names(replace) <- newcols
    ret <- ret %>%
      tidyr::replace_na(replace = replace)
  }

  # grouping should be kept
  if(length(grouped_col) != 0){
    ret <- dplyr::group_by_(ret, grouped_col)
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
quantifiable_cols <- function(data){
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
get_multi_predicted_values <- function(prob_mat, actual_vals = NULL){
  prob_label <- colnames(prob_mat)[max.col(prob_mat)]
  if(!is.null(actual_vals)){
    prob_label <- same_type(prob_label, actual_vals)
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
fill_vec_NA <- function(indice, values, max_index = max(indice, na.rm = TRUE)){
  ret <- same_type(rep(NA, max_index), values)
  ret[indice] <- values
  ret
}

#' Fill missing rows by NA
#' @param indice Row indice where the values should be placed in the output vector.
#' @param mat Matrix to be filled with NA.
#' @param max_index The row size of output matrix
fill_mat_NA <- function(indice, mat, max_index = max(indice, na.rm = TRUE)){
  if(nrow(mat) != length(indice)) {
    stop("matrix must have the same length of indice")
  }
  na_val <- same_type(NA, as.vector(mat))
  ret <- matrix(na_val, nrow = max_index, ncol = ncol(mat))
  colnames(ret) <- colnames(mat)
  ret[indice, ] <- mat
  ret
}

# get data type to distinguish more than typeof function
get_data_type <- function(data){
  if (is.factor(data)){
    # factor is regarded as integer by typeof
    "factor"
  } else if (inherits(data, "Date")){
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

# add confidence interval
add_confint <- function(data, conf_int){
  # add confidence interval if conf_int is not null and there are .fitted and .se.fit
  if (!is.null(conf_int) & ".se.fit" %in% colnames(data) & ".fitted" %in% colnames(data)) {
    if (conf_int < 0 | conf_int > 1){
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
validate_data <- function(types, data){
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

    if(any(!is.na(message))){
      stop(paste0("Data type mismatch detected for ", paste0(message[!is.na(message)], collapse = ", ")))
    }
  }
  TRUE
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
create_model_meta <- function(df, formula){
  ret <- list()
  tryCatch({
    md_frame <- model.frame(formula, data = df)
    ret$terms <- terms(md_frame, formula)
    pred_cnames <- all.vars(ret$terms)[-1]
    types <- vapply(pred_cnames, function(cname) {
      get_data_type(df[[cname]])
    }, FUN.VALUE = "")
    names(types) <- pred_cnames
    ret$types <- types
  }, error = function(e){
    NULL
  })
  ret
}

#' NSE version of unnest_without_empty_
#' @export
unnest_without_empty <- function(data, nested){
  nested_col <- col_name(substitute(nested))
  unnest_without_empty_(data, nested_col)
}

#' unnest with removing NULL or empty list
#' @export
unnest_without_empty_ <- function(data, nested_col){
  empty <- list_n(data[[nested_col]]) == 0
  without_empty <- data[!empty, ]
  if(nrow(without_empty) == 0){
    # returns 0 row data frame,
    # if all values in nested_col are empty
    without_empty
  } else {
    unnest_with_drop_(without_empty, nested_col)
  }
}

#' Count NA in a vector
#' @param x vector
#' @export
na_count <- function(x){
  sum(is.na(x))
}

#' Percentage of NA in a vector
#' @param x vector
#' @export
na_pct <- function(x){
  sum(is.na(x)) / length(x) * 100
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
unnest_with_drop_ <- function(..., .drop = TRUE){
  tidyr::unnest_(..., .drop = .drop)
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
unnest_with_drop <- function(..., .drop = TRUE){
  tidyr::unnest(..., .drop = .drop)
}
