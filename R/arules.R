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
  if(subject_col %nin% colnames(df)){
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
        # lhs and rhs are both NULL
        appearance <- NULL
      } else {
        # lhs is NULL and rhs is not NULL
        # find matched values by stringr::str_detect in subject_col to limit rhs
        appearance <- list(rhs = rhs, default = "lhs")
      }
    } else {
      if(is.null(rhs)){
        # rhs is NULL and lhs is not NULL
        appearance <- list(lhs = lhs, default = "rhs")
      } else {
        # rhs are both not NULL
        appearance <- list(lhs = lhs, rhs = rhs, default = "none")
      }
    }
    rules <- NULL
    # capture.output suppress summary output from arules::apriori function
    capture.output({
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
    })
    lhs_val <- vapply(arules::LIST(rules@lhs), function(items){
      paste(items, collapse=", ")
    }, FUN.VALUE = "")
    rhs_val <- vapply(arules::LIST(rules@rhs), function(items){
      paste(items, collapse=", ")
    }, FUN.VALUE = "")

    # remove empty strings if lhs or rhs is indicated
    if(!is.null(lhs) & !is.null(rhs)){
      filtered <- lhs_val != "" & rhs_val != ""
    } else if(!is.null(lhs)){
      filtered <- lhs_val != ""
    } else if(!is.null(rhs)){
      filtered <- rhs_val != ""
    } else {
      filtered <- TRUE
    }

    quality <- rules@quality
    ret <- data.frame(
      lhs_val[filtered],
      rhs_val[filtered],
      quality$support[filtered],
      quality$confidence[filtered],
      quality$lift[filtered], stringsAsFactors = FALSE)
    colnames(ret) <- cnames
    ret
  }

  ret <- (df %>%  dplyr::do_(.dots = setNames(~do_apriori_each(.), cnames[[5]])))

  # this happens when lhs and rhs are indicated and no matching rule was found
  if(nrow(ret) == 0){
    stop("No matching rule was found.")
  }

  ret <- (ret %>%  tidyr::unnest_(cnames[[5]]))
  if(all(is.na(ret[[1]])) & nrow(ret)==1){
    stop("No rule was found. Adjusting arguments might work.")
  }
  ret
}
