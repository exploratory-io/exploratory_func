#' Column name parser
col_name <- function(x, default = stop("Please supply column name", call. = FALSE))
{
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
upper_gather <- function(mat, names=NULL, diag=NULL){
  if(is.atomic(mat) & is.vector(mat)){
    # This is basically for proxy::dist, simil functions
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
    reshape2::melt(trimat, na.rm=TRUE)
  }else{
    # diag can be NULL or FALSE
    upper_tri <- upper.tri(mat, diag=!is.null(diag) && diag)
    cnames <- colnames(mat)
    rnames <- rownames(mat)
    if(is.null(cnames)){
      cnames <- seq(ncol(mat))
    }
    if(is.null(rnames)){
      rnames <- seq(nrow(mat))
    }
    ind <- which( upper_tri , arr.ind = TRUE )
    val <- mat[upper_tri]
    data.frame(
      Var1=rnames[ind[,1]],
      Var2=cnames[ind[,2]],
      value=val)
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
