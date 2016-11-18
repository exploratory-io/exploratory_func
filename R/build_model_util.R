#' function to retrieve model object from rds
getModelSetFromRDS <- function(modelset_name, key, index){
  tryCatch({
    modelset_path <- getOption("tam.modelset_cache")
    # the file path is like model_path/glm_019u0931_3.rds
    object <- readRDS(file.path(modelset_path, paste0(paste0(modelset_name, key, index, sep = "_")), ".rds"))
    object
  }, error = function(e){
    NULL
  })
}

#' function to save model object from rds
saveModelSetToRDS <- function(object, modelset_name, key, index){
  tryCatch({
    modelset_path <- getOption("tam.modelset_cache")
    # the file path is like model_path/glm_019u0931_3.rds
    saveRDS(object, file.path(modelset_path, paste0(paste0(modelset_name, key, index, sep = "_")), ".rds"))
    TRUE
  }, error = function(e){
    NULL
  })
}
