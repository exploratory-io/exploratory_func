
# augment function just to filter out unknown categories in predictors to avoid error.
augment.glm_exploratory_0 <- function(x, data = NULL, newdata = NULL, ...) {
  if (!is.null(newdata) && length(x$xlevels) > 0) {
    for (i in 1:length(x$xlevels)) {
      newdata <- newdata %>% dplyr::filter(!!rlang::sym(names(x$xlevels)[[i]]) %in% !!x$xlevels[[i]])
    }
  }
  if (is.null(data)) { # Giving data argument when it is NULL causes error from augment.glm.
    ret <- tryCatch({
      broom:::augment.glm(x, newdata = newdata, se = TRUE, ...)
    }, error = function(e){
      # se=TRUE throws error that looks like "singular matrix 'a' in solve",
      # in some subset of cases of perfect collinearity.
      # Try recovering from it by running with se=FALSE.
      broom:::augment.glm(x, newdata = newdata, se = FALSE, ...)
    })
    if (!is.null(ret$.rownames)) { # Clean up .rownames column augment.glm adds for some reason.
      ret$.rownames <- NULL
    }
  }
  else {
    ret <- tryCatch({
      broom:::augment.glm(x, data = data, newdata = newdata, se = TRUE, ...)
    }, error = function(e){
      # se=TRUE throws error that looks like "singular matrix 'a' in solve",
      # in some subset of cases of perfect collinearity.
      # Try recovering from it by running with se=FALSE.
      broom:::augment.glm(x, data = data, newdata = newdata, se = FALSE, ...)
    })
  }
  ret
}
