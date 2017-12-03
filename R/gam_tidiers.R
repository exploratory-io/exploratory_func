#' gam tidiers
#' @export
glance.gam_exploratory <- function(x, ...) {
  summary_ret <- summary(x)

  ret <- data.frame(
    r_squared=summary_ret$r.sq, # R-sq.(adj)
    deviance_explained=summary_ret$dev.expl, # Deviance explained
    gcv=summary_ret$sp.criterion, # GCV
    scale_est=summary_ret$scale # Scale est.
  )
  ret
}
