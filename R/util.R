#' Column name parser
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
simple_cast <- function(data, row, col, val = NULL, fun.aggregate=mean, fill=0){
  loadNamespace("reshape2")
  loadNamespace("tidyr")

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
    data %>%  reshape2::acast(fml, value.var=val, fun.aggregate=fun.aggregate, fill=fill)
  }
}

#' Cast data to sparse matrix by choosing row and column from a data frame
#' @param count If val is NULL and count is TRUE, the value becomes count of the row and col set. Otherwise, it's binary data of row and col set.
sparse_cast <- function(data, row, col, val=NULL, fun.aggregate=sum, count = FALSE){
  loadNamespace("dplyr")
  loadNamespace("tidyr")
  loadNamespace("Matrix")

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
    lower_tri <- lower.tri(tmat, diag=diag)
    c_names <- colnames(tmat)
    r_names <- rownames(tmat)
    if(is.null(c_names)){
      c_names <- seq(ncol(tmat))
    }
    if(is.null(r_names)){
      r_names <- seq(nrow(tmat))
    }

    # this creates pairs of row and column indices
    ind <- which( lower_tri , arr.ind = TRUE )
    # make a vector of upper half of matrix
    row <- r_names[ind[,1]]
    col <- c_names[ind[,2]]
    val <- tmat[lower_tri]
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
avoid_conflict <- function(origin, new){
  conflict <- new %in% origin
  while(any(conflict)){
    new[conflict] <- paste(new[conflict], ".new", sep="")
    conflict <- new %in% origin
  }
  new
}

#' check grouped column
grouped_by <- function(df){
  as.character(attr(df, "vars"))
}

#' matrix to dataframe with gathered form
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
    as.factor(vector)
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
  text <- sapply(column, function(x) str_c(x, collapse = sep))
  as.character(text)
}

#' concat vectors in a list column
#' @export
list_concat <- function(list){
  list(unlist(list))
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
#' @param colnames Lazy dot for select arg
as_numeric_matrix_ <- function(df, colnames){
  loadNamespace("dplyr")
  selected <- dplyr::select_(df, .dots=colnames)
  selected %>%
    as.matrix() %>%
    as.numeric() %>%
    matrix(nrow = nrow(selected))
}
