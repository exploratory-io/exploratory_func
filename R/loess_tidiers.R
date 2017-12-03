#' loess tidiers
#' @export
glance.loess <- function(x, ...) {
  ret <- data.frame(
    n_observations=x$n, # Number of Observations
    enp=x$enp, # Equivalent Number of Parameters
    residual_std_error=x$s, # Residual Standard Error
    trace_of_smoother_matrix=x$trace.hat # Trace of smoother matrix (exact)
  )
  ret
}
