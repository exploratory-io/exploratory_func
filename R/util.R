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

#' Simple cast wrapper
simple_cast <- function(data, row, col, val, fun.aggregate=mean, fill=0){
  loadNamespace("reshape2")
  fml <- as.formula(paste(row, col, sep = "~"))
  data %>%  reshape2::acast(fml, value.var=val, fun.aggregate=fun.aggregate, fill=fill)
}

#' Gather only upper half
upper_gather <- function(mat, names=NULL, diag=NULL, cnames = NULL){
  if(is.vector(mat)){
    # This is basically for dist function
    # It provides numeric vector of upper half
    dim_size <- sqrt(2*length(mat)+1/4)+1/2
    if(is.null(names)){
      # detect dimension
      names <- seq(dim_size)
    } else {
      if(length(names) != dim_size){
        stop("number of names doesn't match matrix dimension")
      }
    }
    loadNamespace("Matrix")
    # create a triangler matrix to melt
    trimat <- matrix(nrow=length(names), ncol=length(names))
    trimat[row(trimat)<col(trimat)] <- as.numeric(mat)
    colnames(trimat) <- names
    rownames(trimat) <- names
    if(!is.null(diag)){
      trimat[row(trimat)==col(trimat)] = rep(diag, length(names))
    }
    # transpose to show younger numbers in the first column
    if(is.null(cnames)){
      cnames <- c("Var1", "Var2", "value")
    }
    mat_to_df(t(trimat), na.rm=TRUE, cnames=cnames)
  }else{
    # diag can be NULL or FALSE
    if(is.null(diag)){
      diag <- FALSE
    }
    upper_tri <- upper.tri(mat, diag=diag)
    c_names <- colnames(mat)
    r_names <- rownames(mat)
    if(is.null(c_names)){
      c_names <- seq(ncol(mat))
    }
    if(is.null(r_names)){
      r_names <- seq(nrow(mat))
    }
    ind <- which( upper_tri , arr.ind = TRUE )
    val <- mat[upper_tri]
    df <- data.frame(
      Var1=r_names[ind[,1]],
      Var2=c_names[ind[,2]],
      value=val)
    if(!is.null(cnames)){
      colnames(df) <- cnames
    }
    df
  }
}

#' group by other columns
group_exclude <- function(df, ...){
  loadNamespace("dplyr")
  cols <- as.character(substitute(list(...)))[-1]
  excluded <- setdiff(colnames(df), cols)
  # exclude list column
  target <- excluded[!sapply(df[,excluded], is.list)]
  dplyr::group_by_(df, .dots=target)
}

#' prevent name conflict
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
mat_to_df <- function(mat, cnames=NULL, na.rm=TRUE){
  loadNamespace("reshape2")
  df <- reshape2::melt(mat, na.rm=na.rm)
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
