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

#' @export
prediction <- function(df, source_data, test = TRUE){
  df_cnames <- colnames(df)
  grouping_col <- df_cnames[!df_cnames %in% c("model", ".test_index", "source.data")]

  source <- if(any(colnames(source_data) %in% grouping_col)){
     source_data %>%
      dplyr::group_by_(.dots = grouping_col) %>%
      tidyr::nest()
  } else {
    source_data %>%
      dplyr::mutate(data = 1) %>%
      dplyr::group_by(data) %>%
      tidyr::nest()
  }

  df <- dplyr::select(df, .test_index, model)

  if(test){
    dplyr::bind_cols(df, source) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(data = purrr::map2(data, .test_index, function(df, index){
        safe_slice(df, index)
      })) %>%
      dplyr::select(-.test_index) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(data = list(broom::augment(model, newdata = data))) %>%
      dplyr::select(-model) %>%
      tidyr::unnest(data)
  } else {
    dplyr::bind_cols(df, source) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(data = purrr::map2(data, .test_index, function(df, index){
        safe_slice(df, index, remove = TRUE)
      })) %>%
      dplyr::select(-.test_index) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(data = list(broom::augment(model, data = data))) %>%
      dplyr::select(-model) %>%
      tidyr::unnest(data)
  }
}

#' @export
model_coef <- function(df){
  broom::tidy(df, model)
}

#' @export
model_stats <- function(df){
  broom::glance(df, model)
}

#' @export
model_anova <- function(df){
  df %>% dplyr::mutate(model = list(anova(model))) %>% broom::tidy(model)
}
