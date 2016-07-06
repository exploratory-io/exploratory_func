#' Find association rules from itemsets.
#' It calculates support, confidence and lift values from combinations of items.
#' @export
do_apriori <- function(df, subject, key, minlen=1, maxlen=10, min_support=0.1, max_support=1, min_confidence=0.5, lhs=NULL, rhs=NULL){
  loadNamespace("dplyr")
  loadNamespace("tidyr")
  loadNamespace("arules")
  loadNamespace("stringr")

  subject_col <- col_name(substitute(subject))
  key_col <- col_name(substitute(key))

  if(!subject_col %in% colnames(df)){
    stop(paste(subject_col, "is not in colums", sep=" "))
  }

  if(!key_col %in% colnames(df)){
    stop(paste(key_col, "is not in colums", sep=" "))
  }

  grouped_col <- grouped_by(df)

  cnames <- avoid_conflict(grouped_col, c("lhs", "rhs", "support", "confidence", "lift"))

  # This is executed by each group
  do_apriori_each <- function(df){
    mat <- sparse_cast(df, subject_col, key_col)

    # create appearance list
    if(is.null(lhs)){
      if(is.null(rhs)){
        appearance <- NULL
      } else {
        rhs <- as.character(unique(df[[subject_col]][stringr::str_detect(df[[subject_col]], rhs)]))
        appearance <- list(rhs = rhs, default = "lhs")
      }
    } else {
      if(is.null(rhs)){
        lhs <- as.character(unique(df[[subject_col]][stringr::str_detect(df[[subject_col]], lhs)]))
        appearance <- list(lhs = lhs, default = "rhs")
      } else {
        lhs <- as.character(unique(df[[subject_col]][stringr::str_detect(df[[subject_col]], lhs)]))
        rhs <- as.character(unique(df[[subject_col]][stringr::str_detect(df[[subject_col]], rhs)]))
        appearance <- list(lhs = lhs, rhs = rhs, default = "none")
      }
    }

    rules <- arules::apriori(
      mat,
      parameter = list(
        minlen=minlen,
        maxlen=maxlen,
        support=min_support,
        confidence = min_confidence,
        target="rules",
        smax=max_support
      ),
      appearance = appearance)
    lhs <- vapply(arules::LIST(rules@lhs), function(items){
      paste(items, collapse=",")
    }, FUN.VALUE = "")
    rhs <- vapply(arules::LIST(rules@rhs), function(items){
      paste(items, collapse=",")
    }, FUN.VALUE = "")
    quality <- rules@quality
    ret <- data.frame(
      lhs,
      rhs,
      quality$support,
      quality$confidence,
      quality$lift)
    colnames(ret) <- cnames
    ret
  }

  (df %>%  dplyr::do_(.dots = setNames(~do_apriori_each(.), cnames[[5]])) %>%  tidyr::unnest_(cnames[[5]]))
}

