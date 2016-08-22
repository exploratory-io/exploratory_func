do_t.test <- function(df, value, key=NULL, ...){
  value_col <- col_name(substitute(value))
  with_key <- !is.null(substitute(key))
  if(with_key){
    key_col <- col_name(substitute(key))
    fml <- as.formula(paste(paste("`", value_col, "`", sep=""), paste("`", key_col, "`", sep=""), sep="~"))
  }
  grouped_col <- grouped_by(df)

  model_col <- avoid_conflict(grouped_col, "model")

  do_t.test_each <- function(df, ...){
    if(with_key){
      model <- t.test(data=df, fml, ...)
    } else {
      model <- t.test(df[[value_col]], ...)
    }
    ret <- broom::tidy(model)

    ret[["method"]] <- as.character(ret[["method"]])
    ret[["alternative"]] <- as.character(ret[["alternative"]])

    col_names <- avoid_conflict(grouped_col, vapply(colnames(ret), function(name){
      switch (name,
              estimate = "effect_size",
              estimate1 = "mean1",
              estimate2 = "mean2",
              statistic = "t.value",
              p.value = "p.value",
              parameter = "digree_of_freedom",
              name
      )
    }, FUN.VALUE = ""))

    colnames(ret) <- col_names
    ret
  }

  df  %>%  dplyr::do_(.dots=setNames(list(~do_t.test_each(df = ., ...)), model_col)) %>%  tidyr::unnest_(model_col)
}
