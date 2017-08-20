#' @export
build_fa <- function(df, ..., params = list()) {
  parse_params <- substitute(params)
  select_dots <- rlang::quos(...)
  each_func <- function(df) {
    df <- df %>% dplyr::select(!!!select_dots)

    args <- append(list(r = quote(df)), rlang::lang_args(parse_params))
    call <- rlang::new_language(quote(psych::fa), rlang::as_pairlist(args))

    rlang::eval_bare(call)
  }

  colnames(df) <- avoid_conflict("model", colnames(df), suffix = "_original")

  do_on_each_group(df, each_func, name = "model", with_unnest = FALSE)
}

#' @export
tidy.fa <- function(x, type = c("loadings", "r_scores"), ...){
  type <- match.arg(type)
  if(type == "loadings"){
    cols <- lapply(seq(ncol(x$loadings)), function(col){
      d <- x$loadings[, col] %>% as.data.frame()
      colnames(d) <- stringr::str_replace(colnames(x$loadings)[col], "MR", "minimum_residual_")
      d
    })
    cols <- append(list(names = rownames(x$loadings)), cols)
    ret <- do.call(dplyr::bind_cols, cols)
  } else if (type == "r_scores") {
    d <- x$r.scores %>% as.data.frame()
    d$name <- rownames(d)
    d
  }
}
