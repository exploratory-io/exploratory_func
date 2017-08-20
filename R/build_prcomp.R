#' @export
build_prcomp <- function(df, ..., params = list()) {
  parse_params <- substitute(params)
  cols <- dplyr::select_vars(colnames(df), !!!rlang::quos(...))
  each_func <- function(df) {
    fml <- as.formula(paste0("~`", paste0(c(cols), collapse = "`+`"), "`"))

    if(is.null(rownames(df))){
      rownames(df) <- seq(nrow(df))
    }

    args <- append(list(formula = fml, data = quote(df)), rlang::lang_args(parse_params))
    call <- rlang::new_language(quote(stats::prcomp), rlang::as_pairlist(args))

    rlang::eval_bare(call)
  }

  colnames(df) <- avoid_conflict("model", colnames(df), suffix = "_original")

  do_on_each_group(df, each_func, name = "model", with_unnest = FALSE)
}
