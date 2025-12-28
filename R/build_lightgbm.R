# Helper function to check if names have special JSON characters
# Uses three patterns because escaped characters in character class can be problematic
has_special_json_chars <- function(names) {
  if (is.null(names) || length(names) == 0) {
    return(FALSE)
  }
  pattern1 <- "[{}\\[\\]\"'\\\\/`]"
  pattern2 <- "[,:]"
  pattern3 <- "[()@+[:space:]]"
  has_special1 <- grepl(pattern1, names)
  has_special2 <- grepl(pattern2, names)
  has_special3 <- grepl(pattern3, names)
  any(has_special1 | has_special2 | has_special3)
}

# Helper: expand metric names to include common LightGBM aliases across versions.
# This matters because `lgb.get.eval.result(..., eval_name=)` expects the internal
# evaluation name (e.g. MAE is often exposed as "l1").
expand_lightgbm_metric_aliases <- function(metric_names) {
  if (is.null(metric_names) || length(metric_names) == 0) {
    return(metric_names)
  }
  metric_names <- as.character(metric_names)
  metric_names <- metric_names[!is.na(metric_names)]
  metric_names <- trimws(metric_names)
  metric_names <- metric_names[metric_names != ""]
  if (length(metric_names) == 0) {
    return(metric_names)
  }

  alias_map <- list(
    mae = c("mae", "l1"),
    l1 = c("l1", "mae"),
    mse = c("mse", "l2"),
    l2 = c("l2", "mse"),
    rmse = c("rmse", "l2_root", "l2root"),
    l2_root = c("l2_root", "rmse", "l2root"),
    l2root = c("l2root", "rmse", "l2_root"),
    logloss = c("logloss", "binary_logloss"),
    binary_logloss = c("binary_logloss", "logloss"),
    mlogloss = c("mlogloss", "multi_logloss"),
    multi_logloss = c("multi_logloss", "mlogloss"),
    merror = c("merror", "multi_error"),
    multi_error = c("multi_error", "merror")
  )

  expanded <- unique(unlist(lapply(metric_names, function(m) {
    key <- tolower(m)
    if (!is.null(alias_map[[key]])) alias_map[[key]] else m
  }), use.names = FALSE))

  expanded
}

# Helper function to sanitize column names for LightGBM
# LightGBM doesn't support special JSON characters in feature names
# Special JSON characters: { } [ ] " ' \ / : , ` and control characters, spaces
sanitize_lightgbm_colnames <- function(names) {
  if (is.null(names)) {
    return(NULL)
  }
  # Replace special JSON characters and other problematic characters with underscores
  # Special JSON characters: { } [ ] " ' \ / : , ` ( ) + @ and control characters, spaces
  # Note: : and , are also JSON-special characters that can break JSON parsing
  # Backticks (`) are used in R for non-standard names but can cause issues in JSON
  # Parentheses (), plus (+), and at-sign (@) are also problematic for JSON
  # Split into multiple patterns because escaped characters in character class can be problematic
  # First pattern: basic special chars that need escaping (excluding comma and colon)
  sanitized <- gsub("[{}\\[\\]\"'\\\\/`]", "_", names)
  # Second pattern: comma and colon (handle separately as they can cause issues in character classes)
  sanitized <- gsub("[,:]", "_", sanitized)
  # Third pattern: additional problematic chars (no escaping needed in char class)
  sanitized <- gsub("[()@+[:space:]]", "_", sanitized)
  # Remove any remaining control characters
  sanitized <- gsub("[[:cntrl:]]", "_", sanitized)
  # Collapse multiple consecutive underscores into one
  sanitized <- gsub("_{2,}", "_", sanitized)
  # Remove leading/trailing underscores
  sanitized <- gsub("^_+|_+$", "", sanitized)
  # Handle empty strings (shouldn't happen, but just in case)
  sanitized[sanitized == ""] <- "feature"
  # Ensure names are unique (in case sanitization creates duplicates)
  sanitized <- make.unique(sanitized, sep = "_")
  
  sanitized
}

#' formula version of LightGBM
#' @param data Dataframe to create model
#' @param formula Formula for model
#' @param nrounds Maximum number of iteration of training
#' @param weights Weight of data for modeling
#' @param watchlist_rate Ratio of validation data to watch learning process
#' @param na.action How to handle data with na
#' Can be na.omit, na.pass, na.fail
#' @param sparse If matrix should be sparse.
#' As default, it becomes sparse if there is any categorical value.
#' @export
fml_lightgbm <- function(data, formula, nrounds = 10, weights = NULL, watchlist_rate = 0, na.action = na.pass, sparse = NULL, ...) {
  loadNamespace("lightgbm")

  lgb_dataset_compat <- function(data, label, weight = NULL, free_raw_data = FALSE) {
    # Ensure column names are sanitized right before passing to LightGBM
    # This is a final safety check in case column names were modified elsewhere
    if (inherits(data, "Matrix")) {
      current_colnames <- colnames(data)
      if (is.null(current_colnames)) {
        current_colnames <- dimnames(data)[[2]]
      }
      if (!is.null(current_colnames)) {
        # Always sanitize to be safe - LightGBM is very strict
        sanitized <- sanitize_lightgbm_colnames(current_colnames)
        dimnames(data) <- list(dimnames(data)[[1]], sanitized)
        tryCatch({ colnames(data) <- sanitized }, error = function(e) {})
        # Verify one more time
        final_names <- colnames(data)
        if (is.null(final_names)) final_names <- dimnames(data)[[2]]
        if (!is.null(final_names) && (has_special_json_chars(final_names) || any(grepl("[[:cntrl:]]", final_names)))) {
          # Force sanitization one more time
          final_sanitized <- sanitize_lightgbm_colnames(final_names)
          dimnames(data) <- list(dimnames(data)[[1]], final_sanitized)
        }
      }
    } else {
      current_colnames <- colnames(data)
      if (!is.null(current_colnames)) {
        # Always sanitize to be safe
        sanitized <- sanitize_lightgbm_colnames(current_colnames)
        colnames(data) <- sanitized
        # Verify one more time
        final_names <- colnames(data)
        if (!is.null(final_names) && (has_special_json_chars(final_names) || any(grepl("[[:cntrl:]]", final_names)))) {
          colnames(data) <- sanitize_lightgbm_colnames(final_names)
        }
      }
    }
    
    args <- list(data = data, label = label)
    if (!is.null(weight)) {
      args$weight <- weight
    }
    # Some older versions might not have free_raw_data.
    ds_formals <- names(formals(lightgbm::lgb.Dataset))
    if ("free_raw_data" %in% ds_formals) {
      args$free_raw_data <- free_raw_data
    }
    do.call(lightgbm::lgb.Dataset, args)
  }

  lgb_train_compat <- function(args) {
    train_formals <- names(formals(lightgbm::lgb.train))
    # Keep only supported named arguments unless lgb.train has ...
    if (!("..." %in% train_formals)) {
      args <- args[names(args) %in% train_formals]
    }
    do.call(lightgbm::lgb.train, args)
  }

  term <- terms(formula, data = data)

  # do.call is used to substitute weights
  df_for_model_matrix <- tryCatch({
    do.call(model.frame, list(term, data = data, weights = substitute(weights), na.action = na.action))
  }, error = function(e) {
    if (e$message == "missing values in object") {
      # this happens when na.action argument is na.fail
      stop("There are NAs in the training data. You might want to set 'na.action' parameter to 'na.pass' or impute NAs manually.")
    }
    stop(e)
  })

  if (nrow(df_for_model_matrix) == 0) {
    # this might happen when na.action is na.omit
    stop("No valid data to create lightgbm model after removing NA.")
  }

  y <- model.response(df_for_model_matrix)

  # NA in y causes an error in training
  df_for_model_matrix <- df_for_model_matrix[!is.na(y), ]
  y <- y[!is.na(y)]

  md_mat <- tryCatch({
    if (is.null(sparse)) {
      sparse <- FALSE
      # If any variable is factor, it uses sparse matrix
      for (var in all.vars(lazyeval::f_rhs(term))) {
        if (is.factor(data[[var]])) {
          sparse <- TRUE
        }
      }
    }

    if (sparse) {
      tryCatch({
        Matrix::sparse.model.matrix(term, data = df_for_model_matrix)
      }, error = function(e) {
        if (e$message == "fnames == names(mf) are not all TRUE") {
          # if there are not clean column names like including spaces or special characters,
          # Matrix::sparse.model.matrix causes this error
          stop("EXP-ANA-3 :: [] :: Invalid column names are found. Please run clean_names function beforehand.")
        }
        stop(e)
      })
    } else {
      model.matrix(term, data = df_for_model_matrix)
    }
  }, error = function(e) {
    if (e$message == "contrasts can be applied only to factors with 2 or more levels") {
      stop("more than 1 unique values are expected for categorical columns assigned as predictors")
    }
    stop(e)
  })

  weight <- model.weights(df_for_model_matrix)

  # Build dataset(s)
  if (!is.null(dim(md_mat)) && nrow(md_mat) == 0) {
    stop("No valid data to create lightgbm model after removing NA.")
  }

  # Store original column names before sanitization
  # Handle both regular matrices and sparse matrices (Matrix objects)
  if (inherits(md_mat, "Matrix")) {
    original_colnames <- colnames(md_mat)
    if (is.null(original_colnames)) {
      original_colnames <- dimnames(md_mat)[[2]]
    }
    if (is.null(original_colnames)) {
      # If still null, create default names
      original_colnames <- paste0("feature_", seq_len(ncol(md_mat)))
    }
  } else {
    original_colnames <- colnames(md_mat)
    if (is.null(original_colnames)) {
      original_colnames <- paste0("feature_", seq_len(ncol(md_mat)))
    }
  }
  
  # Sanitize column names for LightGBM (removes special JSON characters)
  sanitized_colnames <- sanitize_lightgbm_colnames(original_colnames)
  
  # Create mapping from sanitized to original names
  colname_mapping <- original_colnames
  names(colname_mapping) <- sanitized_colnames
  
  # Update column names of md_mat for LightGBM
  # For sparse matrices, we need to set dimnames properly
  if (inherits(md_mat, "Matrix")) {
    # For Matrix objects, set both dimnames and colnames to ensure LightGBM reads them correctly
    current_dimnames <- dimnames(md_mat)
    dimnames(md_mat) <- list(current_dimnames[[1]], sanitized_colnames)
    # Also try setting colnames directly (some Matrix types support this)
    tryCatch({
      colnames(md_mat) <- sanitized_colnames
    }, error = function(e) {
      # If colnames() doesn't work, dimnames() should be enough
    })
  } else {
    colnames(md_mat) <- sanitized_colnames
  }
  
  # Final verification: ensure no special JSON characters remain and names are set correctly
  final_colnames <- if (inherits(md_mat, "Matrix")) {
    cn <- colnames(md_mat)
    if (is.null(cn)) cn <- dimnames(md_mat)[[2]]
    cn
  } else {
    colnames(md_mat)
  }
  
  if (is.null(final_colnames) || has_special_json_chars(final_colnames) || any(grepl("[[:cntrl:]]", final_colnames))) {
    # If special characters still exist or names are null, sanitize again
    if (is.null(final_colnames)) {
      final_colnames <- sanitized_colnames
    } else {
      final_colnames <- sanitize_lightgbm_colnames(final_colnames)
    }
    if (inherits(md_mat, "Matrix")) {
      current_dimnames <- dimnames(md_mat)
      dimnames(md_mat) <- list(current_dimnames[[1]], final_colnames)
      tryCatch({ colnames(md_mat) <- final_colnames }, error = function(e) {})
    } else {
      colnames(md_mat) <- final_colnames
    }
    sanitized_colnames <- final_colnames
  }

  # Collect any extra args once so we can be version-tolerant.
  dots <- list(...)
  # Extract params (LightGBM uses params as a named list).
  params <- if (!is.null(dots$params)) dots$params else list()
  dots$params <- NULL
  # LightGBM defaults can be too strict for small datasets (can result in 0 usable features).
  # Provide permissive defaults unless explicitly overridden.
  if (is.null(params$min_data_in_leaf)) params$min_data_in_leaf <- 1
  if (is.null(params$min_data_in_bin)) params$min_data_in_bin <- 1
  if (is.null(params$feature_pre_filter)) params$feature_pre_filter <- FALSE

  # Merge callbacks (if present) with evaluation recording callback.
  callbacks <- list()
  if (!is.null(dots$callbacks)) {
    callbacks <- dots$callbacks
    dots$callbacks <- NULL
  }
  evals_result <- list()
  # Prefer callback-based recording (works across versions).
  if (exists("cb.record.evaluation", asNamespace("lightgbm"), inherits = FALSE)) {
    callbacks <- c(callbacks, list(lightgbm::cb.record.evaluation(evals_result)))
  }

  booster <- if (watchlist_rate != 0.0) {
    if (watchlist_rate < 0 || 1 <= watchlist_rate) {
      stop("watchlist_rate must be between 0 and 1")
    }
    index <- sample(seq(nrow(md_mat)), ceiling(nrow(md_mat) * watchlist_rate))

    watch_mat <- safe_slice(md_mat, index)
    train_mat <- safe_slice(md_mat, index, remove = TRUE)
    # Ensure sanitized column names are preserved in sliced matrices
    # Handle both regular and sparse matrices
    if (inherits(watch_mat, "Matrix")) {
      dimnames(watch_mat) <- list(dimnames(watch_mat)[[1]], sanitized_colnames)
      dimnames(train_mat) <- list(dimnames(train_mat)[[1]], sanitized_colnames)
    } else {
      colnames(watch_mat) <- sanitized_colnames
      colnames(train_mat) <- sanitized_colnames
    }

    watch_y <- y[index]
    train_y <- y[-index]

    watch_w <- if (!is.null(weight)) weight[index] else NULL
    train_w <- if (!is.null(weight)) weight[-index] else NULL

    dtrain <- lgb_dataset_compat(data = train_mat, label = train_y, weight = train_w, free_raw_data = FALSE)
    dvalid <- lgb_dataset_compat(data = watch_mat, label = watch_y, weight = watch_w, free_raw_data = FALSE)

    train_args <- c(
      list(
        params = params,
        data = dtrain,
        nrounds = nrounds,
        valids = list(train = dtrain, validation = dvalid)
      ),
      if (length(callbacks) > 0) list(callbacks = callbacks) else list(),
      dots
    )

    lgb_train_compat(train_args)
  } else {
    dtrain <- lgb_dataset_compat(data = md_mat, label = y, weight = weight, free_raw_data = FALSE)

    train_args <- c(
      list(
        params = params,
        data = dtrain,
        nrounds = nrounds,
        valids = list(train = dtrain)
      ),
      if (length(callbacks) > 0) list(callbacks = callbacks) else list(),
      dots
    )

    lgb_train_compat(train_args)
  }

  # Wrap booster because lgb.Booster can be a locked environment (cannot add new bindings).
  # Store original column names (before sanitization) in x_names for consistency with other code
  
  # If evals_result is empty and callback might not have worked, try to get evaluation results directly from booster
  if ((is.null(evals_result) || length(evals_result) == 0) && inherits(booster, "lgb.Booster")) {
    loadNamespace("lightgbm")
    # Try to get evaluation results using lgb.get.eval.result if available
    tryCatch({
      if (exists("lgb.get.eval.result", asNamespace("lightgbm"), inherits = FALSE)) {
        # Determine dataset names based on watchlist_rate
        dataset_names <- if (watchlist_rate != 0.0) {
          c("train", "validation")
        } else {
          c("train")
        }
        
        # Try to get metric from params
        metric_names <- NULL
        if (!is.null(params$metric)) {
          metric_names <- params$metric
        }
        metric_names <- expand_lightgbm_metric_aliases(metric_names)
        
        # If no metric specified, try common ones based on objective
        if (is.null(metric_names) && !is.null(params$objective)) {
          objective <- params$objective
          if (objective == "regression" || objective == "mean_squared_error" || objective == "mse") {
            metric_names <- c("rmse", "l2", "mse", "mae", "l1")
          } else if (objective == "binary") {
            metric_names <- c("auc", "binary_logloss", "logloss", "binary_error")
          } else if (objective == "multiclass") {
            metric_names <- c("multi_logloss", "mlogloss", "multi_error", "merror")
          }
        }
        
        # Fallback to common metrics
        if (is.null(metric_names)) {
          metric_names <- c("rmse", "mae", "auc", "logloss", "binary_logloss", "error", "merror", "mlogloss", "multi_logloss")
        }
        metric_names <- expand_lightgbm_metric_aliases(metric_names)
        
        # Ensure metric_names is a vector
        if (!is.character(metric_names)) {
          metric_names <- as.character(metric_names)
        }
        
        # Try to get evaluation results for each dataset and metric combination
        evals_list <- list()
        for (ds in dataset_names) {
          for (met in metric_names) {
            tryCatch({
              result <- lightgbm::lgb.get.eval.result(booster, data_name = ds, eval_name = met)
              if (!is.null(result) && length(result) > 0 && !all(is.na(result))) {
                if (is.null(evals_list[[ds]])) {
                  evals_list[[ds]] <- list()
                }
                evals_list[[ds]][[met]] <- result
              }
            }, error = function(e) {
              # This dataset/metric combination doesn't exist, continue
            })
          }
        }
        
        if (length(evals_list) > 0) {
          evals_result <- evals_list
        }
      }
    }, error = function(e) {
      # If API access fails, silently continue with empty evals_result
    })
  }
  
  ret <- list(
    booster = booster,
    evals_result = evals_result,
    terms = term,
    x_names = original_colnames,  # Store original names, not sanitized ones
    x_names_sanitized = sanitized_colnames,  # Store sanitized names for LightGBM operations
    colname_mapping = colname_mapping,  # Mapping from sanitized to original
    is_sparse = sparse,
    xlevels = .getXlevels(term, df_for_model_matrix)
  )
  # To avoid saving a huge environment when caching with RDS.
  attr(ret$terms, ".Environment") <- NULL
  class(ret) <- c("fml_lightgbm", "lightgbm_wrapper")
  ret
}

# Build evaluation log data.frame compatible with xgboost_exp.
lightgbm_build_evaluation_log <- function(model) {
  loadNamespace("lightgbm")
  
  # Prefer evals_result if present (passed by reference), fallback to record_evals.
  evals <- NULL
  
  # First, check if evals_result is populated (from callback or our fallback in fml_lightgbm)
  if (!is.null(model$evals_result) && length(model$evals_result) > 0) {
    evals <- model$evals_result
  } else {
    booster <- if (!is.null(model$booster)) model$booster else model
    
    # Check record_evals in the booster object (some LightGBM versions store it here)
    if (!is.null(booster$record_evals) && length(booster$record_evals) > 0) {
      evals <- booster$record_evals
    } else {
      # Try to access record_evals from the booster's environment if it's an R6 object
      # Some LightGBM versions store evaluation results in the booster's private environment
      if (inherits(booster, "lgb.Booster") && !is.null(booster$.__enclos_env__)) {
        tryCatch({
          private_env <- booster$.__enclos_env__$private
          if (!is.null(private_env)) {
            # Try different possible field names for evaluation results
            if (!is.null(private_env$record_eval) && length(private_env$record_eval) > 0) {
              evals <- private_env$record_eval
            } else if (!is.null(private_env$record_evals) && length(private_env$record_evals) > 0) {
              evals <- private_env$record_evals
            } else if (!is.null(private_env$evals_result) && length(private_env$evals_result) > 0) {
              evals <- private_env$evals_result
            }
          }
        }, error = function(e) {
          # If accessing private members fails, silently continue
        })
      }
      
      # If still no evals, try to get them using LightGBM API
      if (is.null(evals) || length(evals) == 0) {
        if (inherits(booster, "lgb.Booster")) {
          tryCatch({
            # Try to get evaluation results using lgb.get.eval.result if available
            # First, try to determine what datasets were used during training
            # Check if we can get dataset names from the booster's internal state
            dataset_names <- c("train", "validation", "valid", "test")
            
            # Try to get the metric name from params or infer from model
            metric_names <- NULL
            if (!is.null(model$params) && !is.null(model$params$metric)) {
              metric_names <- model$params$metric
            } else if (!is.null(booster$params) && !is.null(booster$params$metric)) {
              metric_names <- booster$params$metric
            }
            metric_names <- expand_lightgbm_metric_aliases(metric_names)
            
            # If we can't determine metrics, try common ones based on objective
            if (is.null(metric_names)) {
              objective <- NULL
              if (!is.null(model$params) && !is.null(model$params$objective)) {
                objective <- model$params$objective
              } else if (!is.null(booster$params) && !is.null(booster$params$objective)) {
                objective <- booster$params$objective
              }
              
              # Set default metrics based on objective
              if (!is.null(objective)) {
                if (objective == "regression" || objective == "mean_squared_error" || objective == "mse") {
                  metric_names <- c("rmse", "l2", "mse", "mae", "l1", "l2_root")
                } else if (objective == "binary") {
                  metric_names <- c("auc", "binary_logloss", "logloss", "binary_error", "binary")
                } else if (objective == "multiclass") {
                  metric_names <- c("multi_logloss", "mlogloss", "multi_error", "merror")
                }
              }
              
              # Fallback to common metrics if still unknown
              if (is.null(metric_names)) {
                metric_names <- c("rmse", "l2", "mse", "mae", "l1", "l2_root", "auc", "logloss", "binary_logloss", "error", "merror", "mlogloss", "multi_logloss")
              }
            }
            metric_names <- expand_lightgbm_metric_aliases(metric_names)
            
            # Ensure metric_names is a vector
            if (!is.character(metric_names)) {
              metric_names <- as.character(metric_names)
            }
            
            # Try to get evaluation results for each dataset and metric combination
            evals_list <- list()
            if (exists("lgb.get.eval.result", asNamespace("lightgbm"), inherits = FALSE)) {
              for (ds in dataset_names) {
                for (met in metric_names) {
                  tryCatch({
                    result <- lightgbm::lgb.get.eval.result(booster, data_name = ds, eval_name = met)
                    if (!is.null(result) && length(result) > 0 && !all(is.na(result))) {
                      if (is.null(evals_list[[ds]])) {
                        evals_list[[ds]] <- list()
                      }
                      evals_list[[ds]][[met]] <- result
                    }
                  }, error = function(e) {
                    # This dataset/metric combination doesn't exist, continue
                  })
                }
              }
            }
            
            if (length(evals_list) > 0) {
              evals <- evals_list
            }
          }, error = function(e) {
            # If API access fails, silently continue
          })
        }
      }
    }
  }
  
  if (is.null(evals) || length(evals) == 0) {
    return(NULL)
  }

  # evals structure: evals[[dataset]][[metric]] is a numeric vector per iteration.
  dataset_names <- names(evals)
  if (is.null(dataset_names) || length(dataset_names) == 0) {
    return(NULL)
  }
  first_ds <- dataset_names[[1]]
  metric_names <- names(evals[[first_ds]])
  if (is.null(metric_names) || length(metric_names) == 0) {
    return(NULL)
  }
  n_iter <- length(evals[[first_ds]][[metric_names[[1]]]])
  if (n_iter == 0) {
    return(NULL)
  }

  df <- data.frame(iter = seq_len(n_iter))
  for (ds in dataset_names) {
    for (met in names(evals[[ds]])) {
      colname <- paste0(ds, "_", met)
      df[[colname]] <- evals[[ds]][[met]]
    }
  }

  df
}

#' formula version of LightGBM (binary classification)
#' @param data Dataframe to create model
#' @param formula Formula for model
#' @param output_type Type of output. Default is "probability". Other options include "class" for class predictions
#' @param eval_metric Evaluation metric to use. Default is "auc". Other options include "binary_logloss", "binary_error", "average_precision"
#' @param params List of additional parameters to pass to LightGBM. These will be merged with the eval_metric parameter.
#' @param ... Additional arguments passed to fml_lightgbm, such as:
#'   \itemize{
#'     \item{nrounds: Maximum number of iteration of training (default: 10)}
#'     \item{weights: Weight of data for modeling}
#'     \item{watchlist_rate: Ratio of validation data to watch learning process (default: 0)}
#'     \item{na.action: How to handle data with NA. Can be na.omit, na.pass, na.fail (default: na.pass)}
#'     \item{sparse: If matrix should be sparse. As default, it becomes sparse if there is any categorical value}
#'     \item{early_stopping_rounds: Number of rounds for early stopping}
#'   }
#'   Other LightGBM parameters can also be passed through ...
#' @export
lightgbm_binary <- function(data, formula, output_type = "probability", eval_metric = "auc", params = list(), ...) {
  loadNamespace("lightgbm")

  # there can be more than 1 metric by adding to params, but keep one explicit.
  metric_list <- list(metric = eval_metric)
  params <- append(metric_list, params)

  vars <- all.vars(formula)
  y_name <- vars[[1]]
  y_vals <- data[[y_name]]

  # this is used to get back original values from predicted output
  label_levels <- c(0, 1)
  if (is.logical(y_vals)) {
    label_levels <- c(FALSE, TRUE)
    y_vals <- as.numeric(y_vals)
  } else if (!all(y_vals[!is.na(y_vals)] %in% c(0, 1))) {
    factored <- as.factor(data[[y_name]])
    label_levels <- to_same_type(levels(factored), data[[y_name]])
    if (length(label_levels) != 2) {
      stop("target variable must have 2 unique values")
    }
    y_vals <- as.numeric(factored) - 1
  }

  data[[y_name]] <- y_vals

  # LightGBM objective is "binary"; output_type controls predict mode (handled in augment if needed).
  objective <- "binary"

  ret <- fml_lightgbm(
    data = data,
    formula = formula,
    params = append(list(objective = objective), params),
    ...
  )

  class(ret) <- c("lightgbm_binary", class(ret))
  ret$y_levels <- label_levels
  ret$output_type <- output_type
  ret$params <- append(list(objective = objective), params)

  original_colnames <- colnames(data)
  terms_mapping <- original_colnames
  names(terms_mapping) <- original_colnames
  ret$terms_mapping <- terms_mapping
  ret
}

#' formula version of LightGBM (multiclass classification)
#' @param data Dataframe to create model
#' @param formula Formula for model
#' @param output_type Type of output. Default is "softprob". Other options include "class" for class predictions
#' @param eval_metric Evaluation metric to use. Default is "multi_error". Other options include "multi_logloss", "mlogloss", "cross_entropy"
#' @param params List of additional parameters to pass to LightGBM. These will be merged with the eval_metric parameter.
#' @param ... Additional arguments passed to fml_lightgbm, such as:
#'   \itemize{
#'     \item{nrounds: Maximum number of iteration of training (default: 10)}
#'     \item{weights: Weight of data for modeling}
#'     \item{watchlist_rate: Ratio of validation data to watch learning process (default: 0)}
#'     \item{na.action: How to handle data with NA. Can be na.omit, na.pass, na.fail (default: na.pass)}
#'     \item{sparse: If matrix should be sparse. As default, it becomes sparse if there is any categorical value}
#'     \item{early_stopping_rounds: Number of rounds for early stopping}
#'   }
#'   Other LightGBM parameters can also be passed through ...
#' @export
lightgbm_multi <- function(data, formula, output_type = "softprob", eval_metric = "multi_error", params = list(), ...) {
  loadNamespace("lightgbm")

  metric_list <- list(metric = eval_metric)
  params <- append(metric_list, params)

  vars <- all.vars(formula)
  y_name <- vars[[1]]
  y_vals <- data[[y_name]]

  label_levels <- if (is.logical(y_vals)) {
    c(FALSE, TRUE)
  } else if (is.factor(y_vals)) {
    sort(unique(data[[y_name]]))
  } else {
    factored <- as.factor(data[[y_name]])
    to_same_type(levels(factored), data[[y_name]])
  }

  if (is.logical(y_vals)) {
    y_vals <- as.numeric(y_vals)
  } else if (is.factor(y_vals)) {
    mapping <- 0:(length(label_levels) - 1)
    names(mapping) <- as.character(label_levels)
    y_vals <- mapping[as.character(y_vals)]
  } else {
    factored <- as.factor(data[[y_name]])
    y_vals <- as.numeric(factored) - 1
  }

  data[[y_name]] <- y_vals

  objective <- "multiclass"

  ret <- fml_lightgbm(
    data = data,
    formula = formula,
    params = append(list(objective = objective, num_class = length(label_levels)), params),
    ...
  )

  class(ret) <- c("lightgbm_multi", class(ret))
  ret$y_levels <- label_levels
  ret$output_type <- output_type
  ret$params <- append(list(objective = objective, num_class = length(label_levels)), params)

  original_colnames <- colnames(data)
  terms_mapping <- original_colnames
  names(terms_mapping) <- original_colnames
  ret$terms_mapping <- terms_mapping
  ret
}

#' formula version of LightGBM (regression)
#' @param data Dataframe to create model
#' @param formula Formula for model
#' @param output_type Type of output. Default is "regression"
#' @param eval_metric Evaluation metric to use. Default is "rmse". Other options include "mae", "mape", "huber", "fair", "poisson", "quantile", "gamma", "tweedie"
#' @param params List of additional parameters to pass to LightGBM. These will be merged with the eval_metric parameter.
#' @param ... Additional arguments passed to fml_lightgbm, such as:
#'   \itemize{
#'     \item{nrounds: Maximum number of iteration of training (default: 10)}
#'     \item{weights: Weight of data for modeling}
#'     \item{watchlist_rate: Ratio of validation data to watch learning process (default: 0)}
#'     \item{na.action: How to handle data with NA. Can be na.omit, na.pass, na.fail (default: na.pass)}
#'     \item{sparse: If matrix should be sparse. As default, it becomes sparse if there is any categorical value}
#'     \item{early_stopping_rounds: Number of rounds for early stopping}
#'   }
#'   Other LightGBM parameters can also be passed through ...
#' @export
lightgbm_reg <- function(data, formula, output_type = "regression", eval_metric = "rmse", params = list(), ...) {
  loadNamespace("lightgbm")

  metric_list <- list(metric = eval_metric)
  params <- append(metric_list, params)

  vars <- all.vars(formula)
  y_name <- vars[[1]]
  data[[y_name]] <- as.numeric(data[[y_name]])

  objective <- output_type

  ret <- fml_lightgbm(
    data = data,
    formula = formula,
    params = append(list(objective = objective), params),
    ...
  )

  class(ret) <- c("lightgbm_reg", class(ret))
  ret$output_type <- output_type
  ret$params <- append(list(objective = objective), params)

  original_colnames <- colnames(data)
  terms_mapping <- original_colnames
  names(terms_mapping) <- original_colnames
  ret$terms_mapping <- terms_mapping
  ret
}

# LightGBM prediction function that takes data frame rather than matrix.
predict_lightgbm <- function(model, df, predraw = FALSE) {
  loadNamespace("lightgbm")

  booster <- if (!is.null(model$booster)) model$booster else model
  predict_method <- getS3method("predict", "lgb.Booster")
  pred_formals <- names(formals(predict_method))

  y_name <- all.vars(model$terms)[[1]]
  if (is.null(df[[y_name]])) {
    df[[y_name]] <- rep(0, nrow(df))
  }

  mat_data <- if (!is.null(model$is_sparse) && model$is_sparse) {
    Matrix::sparse.model.matrix(model$terms, data = model.frame(df, na.action = na.pass, xlev = model$xlevels))
  } else {
    model.matrix(model$terms, model.frame(df, na.action = na.pass, xlev = model$xlevels))
  }

  # Sanitize column names for LightGBM if the model was trained with sanitized names
  # (for backward compatibility, check if x_names_sanitized exists)
  if (!is.null(model$x_names_sanitized)) {
    # Model was trained with sanitized names, so we need to match them
    # The column names from model.matrix should match x_names (original)
    # We need to map them to sanitized names
    if (!is.null(model$colname_mapping)) {
      # colname_mapping has sanitized names as keys and original names as values
      # We need to reverse lookup: find sanitized name for each original name
      current_names <- colnames(mat_data)
      # Reverse the mapping: create a lookup from original to sanitized
      reverse_mapping <- names(model$colname_mapping)
      names(reverse_mapping) <- model$colname_mapping
      sanitized_names <- reverse_mapping[current_names]
      # If mapping doesn't have an entry, sanitize on the fly
      missing_idx <- is.na(sanitized_names)
      if (any(missing_idx)) {
        sanitized_names[missing_idx] <- sanitize_lightgbm_colnames(current_names[missing_idx])
      }
      colnames(mat_data) <- sanitized_names
    } else {
      # Fallback: sanitize directly
      colnames(mat_data) <- sanitize_lightgbm_colnames(colnames(mat_data))
    }
  } else {
    # For backward compatibility: if model doesn't have sanitized names,
    # check if column names need sanitization (they might if they have special chars)
    # Only sanitize if we detect special JSON characters
    current_names <- colnames(mat_data)
    has_special_chars <- has_special_json_chars(current_names) || any(grepl("[[:cntrl:]]", current_names))
    if (has_special_chars) {
      colnames(mat_data) <- sanitize_lightgbm_colnames(current_names)
    }
  }

  # Ensure numeric matrix for dense case
  if (!inherits(mat_data, "Matrix")) {
    if (is.integer(mat_data) || is.logical(mat_data)) {
      mat_data <- matrix(as.numeric(mat_data), ncol = ncol(mat_data))
    } else {
      storage.mode(mat_data) <- "double"
    }
  }

  # LightGBM's predict API changed: some versions use `rawscore`, newer uses `type`.
  if ("rawscore" %in% pred_formals) {
    pred <- stats::predict(booster, mat_data, rawscore = predraw)
  } else if ("type" %in% pred_formals) {
    pred_type <- if (isTRUE(predraw)) "raw" else "response"
    pred <- stats::predict(booster, mat_data, type = pred_type)
  } else {
    pred <- stats::predict(booster, mat_data)
  }

  obj <- NULL
  if (!is.null(model$params) && !is.null(model$params$objective)) {
    obj <- model$params$objective
  }

  if (!is.null(obj) && obj == "multiclass") {
    probs <- matrix(pred, ncol = length(model$y_levels), byrow = TRUE)
    probs <- fill_mat_NA(1:nrow(df), probs, nrow(df))
    colnames(probs) <- model$y_levels
    return(probs)
  }

  # vector case
  if (length(pred) != nrow(df)) {
    pred <- fill_vec_NA(1:length(pred), pred, nrow(df))
  }
  pred
}

#' @export
augment.lightgbm_multi <- function(x, data = NULL, newdata = NULL, data_type = "training", ...) {
  loadNamespace("lightgbm")

  predictor_variables <- all.vars(x$terms)[-1]
  predictor_variables_orig <- x$terms_mapping[predictor_variables]

  if (!is.null(newdata)) {
    if (!is.null(x$predictor_funs)) {
      newdata <- newdata %>% mutate_predictors(x$orig_predictor_cols, x$predictor_funs)
    }

    original_data <- newdata
    cleaned_data <- original_data %>% dplyr::select(predictor_variables_orig)
    colnames(cleaned_data) <- predictor_variables

    if (!is.null(x$df)) {
      cleaned_data <- align_predictor_factor_levels(cleaned_data, x$df, predictor_variables)
    }

    if (nrow(cleaned_data) == 0) {
      return(data.frame())
    }

    predicted <- predict_lightgbm(x, cleaned_data)

    colmax <- max.col(predicted)
    max_prob <- predicted[(colmax - 1) * nrow(predicted) + seq(nrow(predicted))]
    predicted_label <- x$y_levels[colmax]

    original_data <- ranger.set_multi_predicted_values(original_data, as.data.frame(predicted), predicted_label, max_prob, c())
    original_data
  } else if (!is.null(data)) {
    if (nrow(data) == 0) {
      return(data.frame())
    }
    data <- data %>% dplyr::relocate(!!rlang::sym(x$orig_target_col), .after = last_col())
    switch(data_type,
      training = {
        predicted_value <- extract_predicted_multiclass_labels(x, type = "training")
        predicted <- extract_predicted(x, type = "training")
        colmax <- max.col(predicted)
        max_prob <- predicted[(colmax - 1) * nrow(predicted) + seq(nrow(predicted))]
        data <- ranger.set_multi_predicted_values(data, as.data.frame(predicted), predicted_value, max_prob, c())
        data
      },
      test = {
        predicted_value_nona <- extract_predicted_multiclass_labels(x, type = "test")
        predicted_value_nona <- restore_na(predicted_value_nona, attr(x$prediction_test, "unknown_category_rows_index"))
        predicted_value <- restore_na(predicted_value_nona, attr(x$prediction_test, "na.action"))

        predicted <- extract_predicted(x, type = "test")
        colmax <- max.col(predicted)
        max_prob_nona <- predicted[(colmax - 1) * nrow(predicted) + seq(nrow(predicted))]
        max_prob_nona <- restore_na(max_prob_nona, attr(x$prediction_test, "unknown_category_rows_index"))
        max_prob <- restore_na(max_prob_nona, attr(x$prediction_test, "na.action"))

        data <- ranger.set_multi_predicted_values(
          data, as.data.frame(predicted), predicted_value, max_prob,
          attr(x$prediction_test, "na.action"), attr(x$prediction_test, "unknown_category_rows_index")
        )
        data
      }
    )
  }
}

#' @export
augment.lightgbm_binary <- function(x, data = NULL, newdata = NULL, data_type = "training", binary_classification_threshold = 0.5, ...) {
  loadNamespace("lightgbm")

  predictor_variables <- all.vars(x$terms)[-1]
  predictor_variables_orig <- x$terms_mapping[predictor_variables]

  if (!is.null(newdata)) {
    if (!is.null(x$predictor_funs)) {
      newdata <- newdata %>% mutate_predictors(x$orig_predictor_cols, x$predictor_funs)
    }

    original_data <- newdata
    cleaned_data <- original_data %>% dplyr::select(predictor_variables_orig)
    colnames(cleaned_data) <- predictor_variables

    if (!is.null(x$df)) {
      cleaned_data <- align_predictor_factor_levels(cleaned_data, x$df, predictor_variables)
    }

    if (nrow(cleaned_data) == 0) {
      return(data.frame())
    }

    predraw <- identical(x$output_type, "rawscore")
    predicted_val <- predict_lightgbm(x, cleaned_data, predraw = predraw)

    predicted_val_col <- avoid_conflict(colnames(original_data), "predicted_label")
    predicted_prob_col <- avoid_conflict(colnames(original_data), "predicted_probability")

    if (predraw) {
      prob <- boot::inv.logit(predicted_val)
      original_data[[predicted_prob_col]] <- prob
      original_data[[predicted_val_col]] <- predicted_val
    } else {
      original_data[[predicted_prob_col]] <- predicted_val
      original_data[[predicted_val_col]] <- predicted_val > binary_classification_threshold
    }
    original_data
  } else if (!is.null(data)) {
    if (nrow(data) == 0) {
      return(data.frame())
    }
    data <- data %>% dplyr::relocate(!!rlang::sym(x$orig_target_col), .after = last_col())
    predicted_value_col <- avoid_conflict(colnames(data), "predicted_label")
    predicted_probability_col <- avoid_conflict(colnames(data), "predicted_probability")
    switch(data_type,
      training = {
        predicted_prob <- extract_predicted(x, type = "training")
        predicted_value <- extract_predicted_binary_labels(x, threshold = binary_classification_threshold, type = "training")
      },
      test = {
        predicted_prob_nona <- extract_predicted(x, type = "test")
        predicted_value_nona <- extract_predicted_binary_labels(x, threshold = binary_classification_threshold, type = "test")

        predicted_prob_nona <- restore_na(predicted_prob_nona, attr(x$prediction_test, "unknown_category_rows_index"))
        predicted_value_nona <- restore_na(predicted_value_nona, attr(x$prediction_test, "unknown_category_rows_index"))

        predicted_prob <- restore_na(predicted_prob_nona, attr(x$prediction_test, "na.action"))
        predicted_value <- restore_na(predicted_value_nona, attr(x$prediction_test, "na.action"))
      }
    )
    data[[predicted_value_col]] <- predicted_value
    data[[predicted_probability_col]] <- predicted_prob
    data
  }
}

#' @export
augment.lightgbm_reg <- function(x, data = NULL, newdata = NULL, data_type = "training", ...) {
  loadNamespace("lightgbm")

  predictor_variables <- all.vars(x$terms)[-1]
  predictor_variables_orig <- x$terms_mapping[predictor_variables]

  if (!is.null(newdata)) {
    predicted_value_col <- avoid_conflict(colnames(newdata), "predicted_value")
    if (!is.null(x$predictor_funs)) {
      newdata <- newdata %>% mutate_predictors(x$orig_predictor_cols, x$predictor_funs)
    }

    original_data <- newdata
    cleaned_data <- original_data %>% dplyr::select(predictor_variables_orig)
    colnames(cleaned_data) <- predictor_variables

    if (!is.null(x$df)) {
      cleaned_data <- align_predictor_factor_levels(cleaned_data, x$df, predictor_variables)
    }

    if (nrow(cleaned_data) == 0) {
      return(data.frame())
    }

    predicted_val <- predict_lightgbm(x, cleaned_data)
    original_data[[predicted_value_col]] <- predicted_val
    original_data
  } else if (!is.null(data)) {
    if (nrow(data) == 0) {
      return(data.frame())
    }
    data <- data %>% dplyr::relocate(!!rlang::sym(x$orig_target_col), .after = last_col())
    switch(data_type,
      training = {
        predicted_value_col <- avoid_conflict(colnames(data), "predicted_value")
        predicted <- extract_predicted(x, type = "training")
        data[[predicted_value_col]] <- predicted
        data
      },
      test = {
        predicted_value_col <- avoid_conflict(colnames(data), "predicted_value")
        predicted_nona <- extract_predicted(x, type = "test")
        predicted_nona <- restore_na(predicted_nona, attr(x$prediction_test, "unknown_category_rows_index"))
        predicted <- restore_na(predicted_nona, attr(x$prediction_test, "na.action"))
        data[[predicted_value_col]] <- predicted
        data
      }
    )
  }
}

#' @export
augment.lightgbm_exp <- function(x, data = NULL, newdata = NULL, ...) {
  if ("lightgbm_reg" %in% class(x)) {
    augment.lightgbm_reg(x, data, newdata, ...)
  } else if ("lightgbm_binary" %in% class(x)) {
    augment.lightgbm_binary(x, data, newdata, ...)
  } else if ("lightgbm_multi" %in% class(x)) {
    augment.lightgbm_multi(x, data, newdata, ...)
  }
}

#' @export
augment.lgb.Booster <- function(x, data = NULL, newdata = NULL, ...) {
  loadNamespace("lightgbm")

  # Remove our wrapper classes, if any.
  class(x) <- class(x)[!class(x) %in% c("lightgbm_binary", "lightgbm_multi", "lightgbm_reg", "fml_lightgbm")]

  if (!is.null(newdata)) {
    data <- newdata
  }

  mat_data <- if (!is.null(x$x_names)) {
    data[x$x_names]
  } else {
    data
  }

  mat <- as.matrix(mat_data)
  if (is.integer(mat) || is.logical(mat)) {
    mat <- matrix(as.numeric(mat), ncol = ncol(mat))
  } else {
    storage.mode(mat) <- "double"
  }

  predicted <- stats::predict(x, mat)
  predicted_value_col <- avoid_conflict(colnames(data), "predicted_value")
  data[[predicted_value_col]] <- predicted
  data
}

#' Tidy method for LightGBM output
#' @param x Fitted lgb.Booster model
#' @param type Can be "weight" or "log".
#' "weight" returns importances of features and "log" returns evaluation log.
#' @export
tidy.lgb.Booster <- function(x, type = "weight", pretty.name = FALSE, ...) {
  loadNamespace("lightgbm")

  if (type == "log") {
    elog <- lightgbm_build_evaluation_log(x)
    if (is.null(elog)) {
      return(data.frame())
    }
    return(elog)
  }

  ret <- tryCatch({
    imp <- lightgbm::lgb.importance(x) %>% as.data.frame()
    # Expected columns: Feature, Gain, Cover, Frequency (depending on version)
    if (!"Feature" %in% colnames(imp)) {
      stop("LightGBM importance does not contain Feature column.")
    }
    if (pretty.name) {
      if ("Gain" %in% colnames(imp)) colnames(imp)[colnames(imp) == "Gain"] <- "Importance"
      imp
    } else {
      if ("Feature" %in% colnames(imp)) colnames(imp)[colnames(imp) == "Feature"] <- "feature"
      if ("Gain" %in% colnames(imp)) colnames(imp)[colnames(imp) == "Gain"] <- "importance"
      if ("Cover" %in% colnames(imp)) colnames(imp)[colnames(imp) == "Cover"] <- "coverage"
      if ("Frequency" %in% colnames(imp)) colnames(imp)[colnames(imp) == "Frequency"] <- "frequency"
      imp
    }
  }, error = function(e) {
    data.frame(Error = e$message)
  })

  ret
}

#' Glance for lgb.Booster model
#' @export
glance.lgb.Booster <- function(x, pretty.name = FALSE, ...) {
  elog <- lightgbm_build_evaluation_log(x)
  if (is.null(elog)) {
    return(data.frame())
  }
  elog
}

importance_lightgbm <- function(model) {
  booster <- if (!is.null(model$booster)) model$booster else model
  imp <- tidy.lgb.Booster(booster)
  if ("Error" %in% colnames(imp)) {
    ret <- simpleError(imp$Error)
    return(ret)
  }
  ret <- imp %>% dplyr::rename(variable = feature)
  # Aggregate dummy-expanded features back to original c{n}_ variable.
  # For variables that don't match the pattern (e.g., numeric features), keep the original name.
  ret <- ret %>%
    dplyr::mutate(
      variable_extracted = stringr::str_extract(variable, "c\\d+_"),
      variable = dplyr::if_else(is.na(variable_extracted), variable, variable_extracted)
    ) %>%
    dplyr::select(-variable_extracted) %>%
    dplyr::group_by(variable) %>%
    dplyr::summarize(importance = sum(importance, na.rm = TRUE))
  ret <- ret %>% dplyr::arrange(-importance)
  ret
}

calc_permutation_importance_lightgbm_regression <- function(fit, target, vars, data) {
  var_list <- as.list(vars)
  importances <- purrr::map(var_list, function(var) {
    mmpf::permutationImportance(
      data, var, target, fit, nperm = 1,
      predict.fun = function(object, newdata) { predict_lightgbm(object, newdata) },
      loss.fun = function(x, y) { sum((x - y) ^ 2, na.rm = TRUE) / length(x) }
    )
  })
  importances <- purrr::flatten_dbl(importances)
  importances_df <- tibble::tibble(variable = vars, importance = pmax(importances, 0))
  importances_df <- importances_df %>% dplyr::arrange(-importance)
  importances_df
}

calc_permutation_importance_lightgbm_binary <- function(fit, target, vars, data) {
  var_list <- as.list(vars)
  importances <- purrr::map(var_list, function(var) {
    mmpf::permutationImportance(
      data, var, target, fit, nperm = 1,
      predict.fun = function(object, newdata) { predict_lightgbm(object, newdata) },
      loss.fun = function(x, y) { -sum(log(1 - abs(x - y[[1]])), na.rm = TRUE) }
    )
  })
  importances <- purrr::flatten_dbl(importances)
  importances_df <- tibble(variable = vars, importance = pmax(importances, 0))
  importances_df <- importances_df %>% dplyr::arrange(-importance)
  importances_df
}

calc_permutation_importance_lightgbm_multiclass <- function(fit, target, vars, data) {
  var_list <- as.list(vars)
  importances <- purrr::map(var_list, function(var) {
    mmpf::permutationImportance(
      data, var, target, fit, nperm = 1,
      predict.fun = function(object, newdata) { predict_lightgbm(object, newdata) },
      loss.fun = function(x, y) {
        sum(-log(x[match(y[[1]][row(x)], colnames(x)) == col(x)]), na.rm = TRUE)
      }
    )
  })
  importances <- purrr::flatten_dbl(importances)
  importances_df <- tibble(variable = vars, importance = pmax(importances, 0))
  importances_df <- importances_df %>% dplyr::arrange(-importance)
  importances_df
}

# Builds partial dependence data (LightGBM).
partial_dependence.lightgbm <- function(fit, vars = colnames(data),
                                       n = c(min(nrow(unique(data[, vars, drop = FALSE])), 25L), nrow(data)),
                                       classification = FALSE, interaction = FALSE, uniform = TRUE, data, ...) {
  target <- all.vars(fit$terms)[[1]]

  predict.fun <- function(object, newdata) {
    predict_lightgbm(object, newdata)
  }

  points <- list()
  quantile_points <- list()
  for (cname in vars) {
    if (is.numeric(data[[cname]])) {
      coldata <- data[[cname]]
      minv <- min(coldata, na.rm = TRUE)
      maxv <- max(coldata, na.rm = TRUE)
      grid <- minv + (0:20) / 20 * (maxv - minv)
      quantile_grid <- quantile(coldata, probs = 1:24 / 25)
      quantile_points[[cname]] <- quantile_grid
      points[[cname]] <- sort(unique(c(grid, quantile_grid)))
    } else {
      points[[cname]] <- unique(data[[cname]])
    }
  }

  args <- list(
    "data" = data,
    "vars" = vars,
    "n" = n,
    "model" = fit,
    "points" = points,
    "predict.fun" = predict.fun,
    ...
  )

  if (length(vars) > 1L & !interaction) {
    pd <- rbindlist(sapply(vars, function(x) {
      args$vars <- x
      if ("points" %in% names(args)) args$points <- args$points[x]
      mp <- do.call(mmpf::marginalPrediction, args)
      if (!classification) names(mp)[ncol(mp)] <- target
      mp
    }, simplify = FALSE), fill = TRUE)
    data.table::setcolorder(pd, c(vars, colnames(pd)[!colnames(pd) %in% vars]))
  } else {
    pd <- do.call(mmpf::marginalPrediction, args)
    if (!classification) names(pd)[ncol(pd)] <- target
  }

  attr(pd, "class") <- c("pd", "data.frame")
  attr(pd, "interaction") <- interaction == TRUE
  attr(pd, "target") <- if (!classification) target else as.character(fit$y_levels)
  attr(pd, "vars") <- vars
  attr(pd, "points") <- points
  attr(pd, "quantile_points") <- quantile_points
  pd
}

#' Build LightGBM model for Analytics View.
#' @export
exp_lightgbm <- function(df,
                         target,
                         ...,
                         target_fun = NULL,
                         predictor_funs = NULL,
                         max_nrow = 50000,
                         # LightGBM-specific parameters
                         nrounds = 10,
                         watchlist_rate = 0,
                         sparse = FALSE,
                         early_stopping_rounds = NULL,
                         num_leaves = 31,
                         max_depth = -1,
                         min_data_in_leaf = 20,
                         min_gain_to_split = 0,
                         feature_fraction = 1,
                         bagging_fraction = 1,
                         bagging_freq = 0,
                         learning_rate = 0.1,
                         lambda_l1 = 0,
                         lambda_l2 = 0,
                         output_type_regression = "regression",
                         eval_metric_regression = "rmse",
                         output_type_binary = "probability",
                         eval_metric_binary = "auc",
                         output_type_multiclass = "softprob",
                         eval_metric_multiclass = "multi_error",
                         # Model agnostic parameters
                         target_n = 20,
                         predictor_n = 12,
                         smote = FALSE,
                         smote_target_minority_perc = 40,
                         smote_max_synth_perc = 200,
                         smote_k = 5,
                         importance_measure = "permutation",
                         max_pd_vars = NULL,
                         pd_sample_size = 500,
                         pd_grid_resolution = 20,
                         pd_with_bin_means = TRUE, # Default is TRUE to match XGBoost behavior and show confidence intervals
                         seed = 1,
                         test_rate = 0.0,
                         test_split_type = "random") {
  loadNamespace("lightgbm")

  if (test_rate < 0 | 1 < test_rate) {
    stop("test_rate must be between 0 and 1")
  } else if (test_rate == 1) {
    stop("test_rate must be less than 1")
  }

  target_col <- tidyselect::vars_select(names(df), !!rlang::enquo(target))
  orig_selected_cols <- tidyselect::vars_select(names(df), !!!rlang::quos(...))

  target_funs <- NULL
  if (!is.null(target_fun)) {
    target_funs <- list(target_fun)
    names(target_funs) <- target_col
    df <- df %>% mutate_predictors(target_col, target_funs)
  }

  if (!is.null(predictor_funs)) {
    df <- df %>% mutate_predictors(orig_selected_cols, predictor_funs)
    selected_cols <- names(unlist(predictor_funs))
  } else {
    selected_cols <- orig_selected_cols
  }

  grouped_cols <- grouped_by(df)
  selected_cols <- stringr::str_sort(selected_cols)

  is_target_numeric <- is.numeric(df[[target_col]])
  is_target_logical <- is.logical(df[[target_col]])

  orig_levels <- NULL
  if (is.factor(df[[target_col]])) {
    orig_levels <- levels(df[[target_col]])
  } else if (is.logical(df[[target_col]])) {
    orig_levels <- c("TRUE", "FALSE")
  }

  clean_ret <- cleanup_df(df, target_col, selected_cols, grouped_cols, target_n, predictor_n)

  clean_df <- clean_ret$clean_df
  name_map <- clean_ret$name_map
  clean_target_col <- clean_ret$clean_target_col
  clean_cols <- clean_ret$clean_cols

  classification_type <- get_classification_type(clean_df[[clean_target_col]])

  each_func <- function(df) {
    tryCatch({
      if (!is.null(seed)) {
        set.seed(seed)
      }

      unique_val <- unique(df[[clean_target_col]])
      if (smote && length(unique_val[!is.na(unique_val)]) == 2) {
        sample_size <- NULL
      } else {
        sample_size <- max_nrow
      }

      orig_predictor_classes <- capture_df_column_classes(df, clean_cols)

      # LightGBM can handle NAs in numeric predictors; keep NA filtering off (match xgboost).
      clean_df_ret <- cleanup_df_per_group(
        df, clean_target_col, sample_size, clean_cols, name_map, predictor_n,
        filter_numeric_na = FALSE, convert_logical = FALSE
      )
      if (is.null(clean_df_ret)) {
        return(NULL)
      }

      df <- clean_df_ret$df
      c_cols <- clean_df_ret$c_cols
      if (length(c_cols) == 0) {
        stop("Invalid Predictors: Only one unique value.")
      }
      name_map <- clean_df_ret$name_map

      unique_val <- unique(df[[clean_target_col]])
      if (smote && length(unique_val[!is.na(unique_val)]) == 2) {
        df <- df %>% exp_balance(clean_target_col, target_size = max_nrow, target_minority_perc = smote_target_minority_perc, max_synth_perc = smote_max_synth_perc, k = smote_k)
        df <- df %>% dplyr::select(-synthesized)
      }

      source_data <- df
      test_index <- sample_df_index(source_data, rate = test_rate, ordered = (test_split_type == "ordered"))
      df <- safe_slice(source_data, test_index, remove = TRUE)
      if (test_rate > 0) {
        df_test <- safe_slice(source_data, test_index, remove = FALSE)
      }

      # Restore source_data column name to original column name
      rev_name_map <- names(name_map)
      names(rev_name_map) <- name_map
      colnames(source_data) <- rev_name_map[colnames(source_data)]

      rhs <- paste0("`", c_cols, "`", collapse = " + ")
      fml <- as.formula(paste(clean_target_col, " ~ ", rhs))

      common_params <- list(
        num_leaves = num_leaves,
        max_depth = max_depth,
        min_data_in_leaf = min_data_in_leaf,
        min_gain_to_split = min_gain_to_split,
        feature_fraction = feature_fraction,
        bagging_fraction = bagging_fraction,
        bagging_freq = bagging_freq,
        learning_rate = learning_rate,
        lambda_l1 = lambda_l1,
        lambda_l2 = lambda_l2
      )

      if (is_target_logical) {
        model <- lightgbm_binary(
          df, fml,
          nrounds = nrounds,
          watchlist_rate = watchlist_rate,
          sparse = sparse,
          early_stopping_rounds = early_stopping_rounds,
          output_type = output_type_binary,
          eval_metric = eval_metric_binary,
          params = common_params
        )
      } else if (is_target_numeric) {
        model <- lightgbm_reg(
          df, fml,
          nrounds = nrounds,
          watchlist_rate = watchlist_rate,
          sparse = sparse,
          early_stopping_rounds = early_stopping_rounds,
          output_type = output_type_regression,
          eval_metric = eval_metric_regression,
          params = common_params
        )
      } else {
        model <- lightgbm_multi(
          df, fml,
          nrounds = nrounds,
          watchlist_rate = watchlist_rate,
          sparse = sparse,
          early_stopping_rounds = early_stopping_rounds,
          output_type = output_type_multiclass,
          eval_metric = eval_metric_multiclass,
          params = common_params
        )
      }

      class(model) <- c("lightgbm_exp", class(model))

      # Store evaluation log in XGBoost-compatible shape (wide, with iter).
      model$evaluation_log <- lightgbm_build_evaluation_log(model)

      model$prediction_training <- predict_lightgbm(model, df)

      if (test_rate > 0) {
        df_test_clean <- cleanup_df_for_test(df_test, df, c_cols)
        na_row_numbers_test <- attr(df_test_clean, "na_row_numbers")
        unknown_category_rows_index <- attr(df_test_clean, "unknown_category_rows_index")

        prediction_test <- predict_lightgbm(model, df_test_clean)

        attr(prediction_test, "na.action") <- na_row_numbers_test
        attr(prediction_test, "unknown_category_rows_index") <- unknown_category_rows_index
        model$prediction_test <- prediction_test
        model$df_test <- df_test_clean
      }

      if (is.null(max_pd_vars)) {
        max_pd_vars <- 20
      }
      # Guard against the surprising behavior of 1:0 (which yields c(1, 0)).
      # Treat non-positive values as "no partial dependence vars".
      max_pd_vars <- suppressWarnings(as.integer(max_pd_vars))
      if (is.na(max_pd_vars) || max_pd_vars < 0) {
        max_pd_vars <- 0
      }

      if (length(c_cols) > 1) {
        if (importance_measure == "permutation") {
          if (is_target_logical) {
            imp_df <- calc_permutation_importance_lightgbm_binary(model, clean_target_col, c_cols, df)
          } else if (is_target_numeric) {
            imp_df <- calc_permutation_importance_lightgbm_regression(model, clean_target_col, c_cols, df)
          } else {
            imp_df <- calc_permutation_importance_lightgbm_multiclass(model, clean_target_col, c_cols, df)
          }
          model$imp_df <- imp_df
        } else if (importance_measure == "impurity" || importance_measure == "lightgbm") {
          imp_df <- importance_lightgbm(model)
          model$imp_df <- imp_df
        } else if (importance_measure == "firm") {
          # For firm importance, imp_df will be computed later from partial dependence
          # Initialize to NULL to ensure it's defined
          model$imp_df <- NULL
        }

        if (importance_measure == "firm") {
          imp_vars <- c_cols
        } else if ("error" %in% class(model$imp_df)) {
          imp_vars <- c_cols
          imp_vars <- imp_vars[seq_len(min(length(imp_vars), max_pd_vars))]
        } else {
          imp_vars <- model$imp_df$variable
          imp_vars <- imp_vars[seq_len(min(length(imp_vars), max_pd_vars))]
        }
      } else {
        error <- simpleError("Variable importance requires two or more variables.")
        model$imp_df <- error
        imp_vars <- c_cols
      }

      imp_vars <- as.character(imp_vars)
      model$imp_vars <- imp_vars
      if (length(imp_vars) > 0) {
        model$partial_dependence <- partial_dependence.lightgbm(
          model, vars = imp_vars, data = df,
          n = c(pd_grid_resolution, min(nrow(df), pd_sample_size)),
          classification = !(is_target_numeric || is_target_logical)
        )
      } else {
        model$partial_dependence <- NULL
      }

      if (importance_measure == "firm") {
        if (length(c_cols) > 1) {
          pdp_target_col <- attr(model$partial_dependence, "target")
          imp_df <- importance_firm(model$partial_dependence, pdp_target_col, imp_vars)
          model$imp_df <- imp_df
          imp_vars <- imp_df$variable
        } else {
          error <- simpleError("Variable importance requires two or more variables.")
          model$imp_df <- error
          imp_vars <- c_cols
        }

        imp_vars <- imp_vars[seq_len(min(length(imp_vars), max_pd_vars))]
        model$imp_vars <- imp_vars
        model$partial_dependence <- shrink_partial_dependence_data(model$partial_dependence, imp_vars)
      }

      if (length(imp_vars) > 0) {
        if (pd_with_bin_means && (is_target_logical || is_target_numeric)) {
          # We calculate means of bins only for logical or numeric target to keep the visualization simple.
          # imp_vars contains processed column names (like "c9_") which match the column names in df
          model$partial_binning <- calc_partial_binning_data(df, clean_target_col, imp_vars)
        }
      }

      model$classification_type <- classification_type
      model$orig_levels <- orig_levels
      model$terms_mapping <- names(name_map)
      names(model$terms_mapping) <- name_map
      model$df <- df
      model$formula_terms <- terms(fml)
      attr(model$formula_terms, ".Environment") <- NULL
      model$sampled_nrow <- clean_df_ret$sampled_nrow

      model$orig_target_col <- target_col
      if (!is.null(target_funs)) {
        model$target_funs <- target_funs
      }
      if (!is.null(predictor_funs)) {
        model$orig_predictor_cols <- orig_selected_cols
        attr(predictor_funs, "LC_TIME") <- Sys.getlocale("LC_TIME")
        attr(predictor_funs, "sysname") <- Sys.info()[["sysname"]]
        attr(predictor_funs, "lubridate.week.start") <- getOption("lubridate.week.start")
        model$predictor_funs <- predictor_funs
      }
      model$orig_predictor_classes <- orig_predictor_classes

      list(model = model, test_index = test_index, source_data = source_data)
    }, error = function(e) {
      if (length(grouped_cols) > 0) {
        class(e) <- c("lightgbm_exp", class(e))
        list(model = e, test_index = NULL, source_data = NULL)
      } else {
        stop(e)
      }
    })
  }

  model_and_data_col <- "model_and_data"
  ret <- do_on_each_group(clean_df, each_func, name = model_and_data_col, with_unnest = FALSE)

  if (length(grouped_cols) > 0) {
    ret <- ret %>% tidyr::nest(-grouped_cols)
  } else {
    ret <- ret %>% tidyr::nest()
  }

  ret <- ret %>% dplyr::ungroup()
  ret <- ret %>%
    dplyr::mutate(model = purrr::map(data, function(df) { df[[model_and_data_col]][[1]]$model })) %>%
    dplyr::mutate(.test_index = purrr::map(data, function(df) { df[[model_and_data_col]][[1]]$test_index })) %>%
    dplyr::mutate(source.data = purrr::map(data, function(df) {
      data <- df[[model_and_data_col]][[1]]$source_data
      if (length(grouped_cols) > 0 && !is.null(data)) {
        data %>% dplyr::select(-grouped_cols)
      } else {
        data
      }
    })) %>%
    dplyr::select(-data)

  if (length(grouped_cols) > 0) {
    ret <- ret %>% dplyr::rowwise(grouped_cols)
  } else {
    ret <- ret %>% dplyr::rowwise()
  }

  if (purrr::every(ret$model, function(x) { "error" %in% class(x) })) {
    stop(ret$model[[1]])
  }

  ret
}

# This is used from Analytics View only when classification type is regression.
#' @export
glance.lightgbm_exp <- function(x, pretty.name = FALSE, ...) {
  if ("error" %in% class(x)) {
    ret <- data.frame(Note = x$message)
    return(ret)
  }
  if ("lightgbm_reg" %in% class(x)) {
    glance.method <- glance.lightgbm_exp.regression
  } else {
    stop("glance.lightgbm_exp should not be called for classification")
  }
  glance.method(x, pretty.name = pretty.name, ...)
}

#' @export
glance.lightgbm_exp.regression <- function(x, pretty.name, ...) {
  predicted <- extract_predicted(x)
  actual <- extract_actual(x)
  root_mean_square_error <- rmse(predicted, actual)
  rsq <- r_squared(actual, predicted)
  n <- length(actual)
  ret <- data.frame(r_squared = rsq, root_mean_square_error = root_mean_square_error, n = n)
  if (pretty.name) {
    map <- list(`R Squared` = as.symbol("r_squared"), `RMSE` = as.symbol("root_mean_square_error"), `Rows` = as.symbol("n"))
    ret <- ret %>% dplyr::rename(!!!map)
  }
  ret
}

#' @export
#' @param type "importance", "evaluation", "conf_mat", "partial_dependence", "evaluation_log"
tidy.lightgbm_exp <- function(x, type = "importance", pretty.name = FALSE, binary_classification_threshold = 0.5, ...) {
  if ("error" %in% class(x) && type != "evaluation") {
    return(data.frame())
  }
  switch(type,
    importance = {
      if ("error" %in% class(x$imp_df)) {
        return(data.frame())
      }
      ret <- x$imp_df
      ret <- ret %>% dplyr::mutate(variable = x$terms_mapping[variable])
      ret
    },
    evaluation = {
      if ("error" %in% class(x)) {
        return(glance(x, pretty.name = pretty.name, ...))
      }
      actual <- extract_actual(x)
      if (is.numeric(actual)) {
        glance(x, pretty.name = pretty.name, ...)
      } else {
        if (x$classification_type == "binary") {
          predicted <- extract_predicted_binary_labels(x, threshold = binary_classification_threshold)
          predicted_probability <- extract_predicted(x)
          evaluate_binary_classification(actual, predicted, predicted_probability, pretty.name = pretty.name)
        } else {
          predicted <- extract_predicted_multiclass_labels(x)
          evaluate_multi_(data.frame(predicted = predicted, actual = actual), "predicted", "actual", pretty.name = pretty.name)
        }
      }
    },
    evaluation_by_class = {
      if ("error" %in% class(x)) {
        return(glance(x, pretty.name = pretty.name, ...))
      }
      actual <- extract_actual(x)
      predicted <- extract_predicted_multiclass_labels(x)
      per_level <- function(level) {
        evaluate_classification(actual, predicted, level, pretty.name = pretty.name)
      }
      dplyr::bind_rows(lapply(levels(actual), per_level))
    },
    conf_mat = {
      actual <- extract_actual(x)
      if (x$classification_type == "binary") {
        predicted <- extract_predicted_binary_labels(x, threshold = binary_classification_threshold)
      } else {
        predicted <- extract_predicted_multiclass_labels(x)
      }
      calc_conf_mat(actual, predicted)
    },
    partial_dependence = {
      handle_partial_dependence(x)
    },
    evaluation_log = {
      if (is.null(x$evaluation_log)) {
        return(data.frame())
      }
      ret <- x$evaluation_log %>% as.data.frame()
      ret <- ret %>% tidyr::pivot_longer(cols = c(-iter))
      # Columns are like "train_rmse", "validation_binary_logloss", etc.
      # Some metrics contain underscores; split only on the first "_" so we keep the full metric name.
      ret <- ret %>% tidyr::separate(col = "name", into = c("type", "name"), sep = "_", extra = "merge", fill = "right")
      # Keep legacy column name used by existing tests and downstream code.
      colnames(ret)[colnames(ret) == "iter"] <- "Iter"
      ret
    },
    {
      stop(paste0("type ", type, " is not defined"))
    }
  )
}

# Model specific S3 accessors (reusing generics from build_xgboost.R)
extract_actual.lightgbm_exp <- function(x, type = "training") {
  if (type == "training") {
    x$df[[all.vars(x$terms)[[1]]]]
  } else {
    x$df_test[[all.vars(x$terms)[[1]]]]
  }
}

extract_predicted.lightgbm_exp <- function(x, type = "training") {
  if (type == "training") x$prediction_training else x$prediction_test
}

extract_predicted.lightgbm_reg <- extract_predicted.lightgbm_exp
extract_predicted.lightgbm_binary <- extract_predicted.lightgbm_exp
extract_predicted.lightgbm_multi <- extract_predicted.lightgbm_exp

extract_predicted_binary_labels.lightgbm_exp <- function(x, threshold = 0.5, type = "training") {
  if (type == "training") (x$prediction_training > threshold) else (x$prediction_test > threshold)
}
extract_predicted_binary_labels.lightgbm_binary <- extract_predicted_binary_labels.lightgbm_exp

extract_predicted_multiclass_labels.lightgbm_exp <- function(x, type = "training") {
  pred <- if (type == "training") x$prediction_training else x$prediction_test
  x$y_levels[apply(pred, 1, which.max)]
}
extract_predicted_multiclass_labels.lightgbm_multi <- extract_predicted_multiclass_labels.lightgbm_exp

get_prediction_type.lightgbm_exp <- function(x) {
  if ("lightgbm_reg" %in% class(x)) {
    return("regression")
  }
  if (x$classification_type == "binary") "binary" else "multiclass"
}
