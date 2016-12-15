#' glance for lm
#' @export
glance_lm <- broom::glance

#' glance for glm
#' @export
glance_glm <- broom::glance

#' glance for kmeans
#' @export
glance_kmeans <- broom::glance

#' tidy for lm
#' @export
tidy_lm <- broom::tidy
#' tidy for glm
#' @export
tidy_glm <- broom::tidy
#' tidy for kmeans
#' @export
tidy_kmeans <- broom::tidy

#' augment for lm
#' @export
augment_lm <- broom::augment
#' augment for glm
#' @export
augment_glm <- broom::augment
#' augment for kmeans
#' @export
augment_kmeans <- function(df, model, data){
  model_col <- col_name(substitute(model))
  data_col <- col_name(substitute(data))
  if(!model_col %in% colnames(df)){
    stop(paste(model_col, "is not in column names"), sep=" ")
  }
  if(!data_col %in% colnames(df)){
    stop(paste(data_col, "is not in column names"), sep=" ")
  }
  ret <- tryCatch({
    # use do.call to evaluate data_col from a variable
    augment_func <- get("augment", asNamespace("broom"))
    ret <- do.call(augment_func, list(df, model_col, data=data_col))
    # cluster column is factor labeled "1", "2"..., so convert it to integer to avoid confusion
    ret[[ncol(ret)]] <- as.integer(ret[[ncol(ret)]])
    ret
  },
  error = function(e){
    loadNamespace("dplyr")

    if(grepl("arguments imply differing number of rows",e$message)){
      # bind .cluster column refering subject names
      grouped_col <- grouped_by(df)
      cluster_col <- avoid_conflict(grouped_col, ".cluster")

      augment_each <- function(df_each){
        source_data <- df_each[[data_col]]
        kmeans_obj <- df_each[[model_col]]
        if(!is.data.frame(source_data)){
          source_data <- source_data[[1]]
          kmeans_obj <- kmeans_obj[[1]]
        }

        subject_colname <- attr(kmeans_obj, "subject_colname")
        index <- as.factor(source_data[[subject_colname]])
        source_data[[cluster_col]] <- kmeans_obj$cluster[index]
        source_data
      }

      (df %>%  dplyr::do_(.dots=setNames(list(~augment_each(.)), model_col)) %>%  tidyr::unnest_(model_col))
    } else {
      stop(e)
    }
  })
  # update .cluster to cluster or cluster.new if it exists
  colnames(ret)[[ncol(ret)]] <- avoid_conflict(colnames(ret), "cluster")
  ret
}

#' augment wrapper
#' @export
predict <- function(df, model, ...){
  model_col <- col_name(substitute(model))
  data_col <- col_name(substitute(data))
  if(!model_col %in% colnames(df)){
    stop(paste(model_col, "is not in column names"), sep=" ")
  }
  if(any(class(df[[model_col]]) %in% ".model.kmeans")){
    augment_kmeans(df, model, ...)
  } else {
    broom::augment(df, model, ...)
  }
}

#' apply data frame with model to a data frame
#' @export
add_prediction <- function(df, model_df){
  broom::augment(model_df, model, newdata = df)
}

#' assign cluster number to each rows
#' @export
assign_cluster <- function(df, source_data){
  df_cnames <- colnames(df)
  # columns that are not model related are regarded as grouping column
  grouping_cols <- df_cnames[!df_cnames %in% c("model", "source.data")]

  # remove na from selected column (column names from cluster centers)
  # without this, augment throws an error with different number of data
  center_cnames <- colnames(df[["model"]][[1]]$centers)
  for (center_cname in center_cnames) {
    source_data <- source_data[!is.na(source_data[[center_cname]]), ]
  }

  source <- if(any(colnames(source_data) %in% grouping_cols)){
    # nest the source data by each group
    source_data %>%
      dplyr::group_by_(.dots = grouping_cols) %>%
      tidyr::nest()
  } else {
    # put one value column so that all data can be nested
    source_data %>%
      dplyr::mutate(data = 1) %>%
      dplyr::group_by(data) %>%
      tidyr::nest()
  }

  # drop unnecessary columns
  df <- dplyr::select_(df, .dots = c("model", grouping_cols))
  # augment data by each row
  # ungroup is needed because grouping is reseted by bind_cols

  joined <- if(length(grouping_cols) > 0) {
    # need this because do and nest has different row order,
    # so grouping_col should be used for index
    dplyr::inner_join(df, source, by = grouping_cols)
  } else {
    dplyr::bind_cols(df, source)
  }

  ret <- joined %>%
    dplyr::ungroup() %>%
    dplyr::rowwise() %>%
    broom::augment(model, data = data)
  # change factor to numeric
  ret[[".cluster"]] <- as.numeric(ret[[".cluster"]])
  colnames(ret)[colnames(ret) == ".cluster"] <- avoid_conflict(colnames(ret), "cluster")
  ret
}

#' tidy wrapper for kmeans
#' @export
cluster_info <- function(df){
  # , col.names = names(model[["cluster"]])
  ret <- df %>%
    dplyr::ungroup() %>%
    dplyr::mutate(model = purrr::map(model, function(m){
      info <- data.frame(
        Size = m$size,
        Withinss = m$withinss
      )
      ret <- m$centers %>%
        as.data.frame()
      colnames(ret) <- paste("Center", colnames(ret))
      ret <- ret %>%
        dplyr::bind_cols(info)
      ret
    })) %>%
    tidyr::unnest(model)
}

#' glance wrapper for kmeans
#' @export
kmeans_info <- function(df){
  ret <- broom::glance(df, model)
  colnames(ret)[colnames(ret) == "totss"] <- "Total Sum of Squares"
  colnames(ret)[colnames(ret) == "tot.withinss"] <- "Total Sum of Squares within Clusters"
  colnames(ret)[colnames(ret) == "betweenss"] <- "Total Sum of Squares between Clusters"
  colnames(ret)[colnames(ret) == "iter"] <- "Number of Iterations"
  ret
}

#' augment using source data and test index
#' @param df Data frame that has model and .test_index
#' @param source_data Data frame that has model and .test_index
#' @param test Test data or training data should be used as data
#' @export
prediction <- function(df, source_data, test = TRUE){
  df_cnames <- colnames(df)
  # columns that are not model related are regarded as grouping column
  grouping_col <- df_cnames[!df_cnames %in% c("model", ".test_index", "source.data")]

  source <- if(any(colnames(source_data) %in% grouping_col)){
    # nest the source data by each group
    source_data %>%
      dplyr::group_by_(.dots = grouping_col) %>%
      tidyr::nest()
  } else {
    # put one value column so that all data can be nested
    source_data %>%
      dplyr::mutate(data = 1) %>%
      dplyr::group_by(data) %>%
      tidyr::nest()
  }

  # drop unnecessary columns
  df <- dplyr::select(df, .test_index, model)

  ret <- if(test){
    # augment by test data
    dplyr::bind_cols(df, source) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(data = purrr::map2(data, .test_index, function(df, index){
        # keep data only in test_index
        safe_slice(df, index)
      })) %>%
      dplyr::select(-.test_index) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(data = list(broom::augment(model, newdata = data))) %>%
      dplyr::select(-model) %>%
      tidyr::unnest(data)
  } else {
    # augment by trainig data
    dplyr::bind_cols(df, source) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(data = purrr::map2(data, .test_index, function(df, index){
        # remove data in test_index
        safe_slice(df, index, remove = TRUE)
      })) %>%
      dplyr::select(-.test_index) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(data = list(broom::augment(model, data = data))) %>%
      dplyr::select(-model) %>%
      tidyr::unnest(data)
  }
  colnames(ret)[colnames(ret) == ".fitted"] <- avoid_conflict(colnames(ret), "Fitted")
  colnames(ret)[colnames(ret) == ".se.fit"] <- avoid_conflict(colnames(ret), "Standard Error")
  colnames(ret)[colnames(ret) == ".resid"] <- avoid_conflict(colnames(ret), "Residuals")
  colnames(ret)[colnames(ret) == ".hat"] <- avoid_conflict(colnames(ret), "Hat")
  colnames(ret)[colnames(ret) == ".sigma"] <- avoid_conflict(colnames(ret), "Residual Standard Deviation")
  colnames(ret)[colnames(ret) == ".cooksd"] <- avoid_conflict(colnames(ret), "Cooks Distance")
  colnames(ret)[colnames(ret) == ".std.resid"] <- avoid_conflict(colnames(ret), "Standardised Residuals")
  ret
}

#' tidy wrapper for lm and glm
#' @export
model_coef <- function(df){
  ret <- broom::tidy(df, model)
  colnames(ret)[colnames(ret) == "term"] <- "Term"
  colnames(ret)[colnames(ret) == "statistic"] <- "t Ratio"
  colnames(ret)[colnames(ret) == "p.value"] <- "Prob > |t|"
  colnames(ret)[colnames(ret) == "std.error"] <- "Std Error"
  colnames(ret)[colnames(ret) == "estimate"] <- "Estimate"
  ret
}

#' glance wrapper
#' @export
model_stats <- function(df){
  ret <- broom::glance(df, model)
  colnames(ret)[colnames(ret) == "r.squared"] <- "RSquare"
  colnames(ret)[colnames(ret) == "adj.r.squared"] <- "RSquare Adj"
  colnames(ret)[colnames(ret) == "sigma"] <- "Root Mean Square Error"
  colnames(ret)[colnames(ret) == "statistic"] <- "F Ratio"
  colnames(ret)[colnames(ret) == "p.value"] <- "Prob > F"
  colnames(ret)[colnames(ret) == "df"] <- "Degree of Freedom"
  colnames(ret)[colnames(ret) == "logLik"] <- "Log Likelihood"
  colnames(ret)[colnames(ret) == "deviance"] <- "Deviance"
  colnames(ret)[colnames(ret) == "df.residual"] <- "Residual Degree of Freedom"
  # for glm
  colnames(ret)[colnames(ret) == "null.deviance"] <- "Null Deviance"
  colnames(ret)[colnames(ret) == "df.null"] <- "Degree of Freedom for Null Model"

  ret
}

#' tidy after converting model to anova
#' @export
model_anova <- function(df){
  ret <- suppressWarnings({
    # this causes warning for Deviance, Resid..Df, Resid..Dev in glm model
    df %>% dplyr::mutate(model = list(anova(model))) %>% broom::tidy(model)
  })
  colnames(ret)[colnames(ret) == "term"] <- "Term"
  colnames(ret)[colnames(ret) == "sumsq"] <- "Sum of Squares"
  colnames(ret)[colnames(ret) == "meansq"] <- "Mean Square"
  colnames(ret)[colnames(ret) == "statistic"] <- "F Ratio"
  colnames(ret)[colnames(ret) == "p.value"] <- "Prob > F"
  colnames(ret)[colnames(ret) == "df"] <- "Degree of Freedom"
  # for glm anova
  colnames(ret)[colnames(ret) == "Resid..Df"] <- "Residual Degree of Freedom"
  colnames(ret)[colnames(ret) == "Resid..Dev"] <- "Residual Deviance"
  ret
}

#' tidy after converting model to confint
#' @export
model_confint <- function(df, ...){
  caller <- match.call()
  # this expands dots arguemtns to character
  arg_char <- expand_args(caller, exclude = c("df"))
  if (arg_char != "") {
    fml <- as.formula(paste0("~list(stats::confint(model, ", arg_char, "))"))
  } else {
    fml <- as.formula(paste0("~list(stats::confint(model))"))
  }
  ret <- df %>% dplyr::mutate_(.dots = list(model = fml)) %>% broom::tidy(model)
  colnames(ret)[colnames(ret) == ".rownames"] <- "Term"
  # original columns are like X0.5..   X99.5.., so replace X to Prob and remove trailing dots
  new_p_cnames <- stringr::str_replace(colnames(ret)[(ncol(ret)-1):ncol(ret)], "X", "Prob ") %>%
    stringr::str_replace("\\.+$", "")
  colnames(ret)[(ncol(ret)-1):ncol(ret)] <- new_p_cnames
  ret
}
