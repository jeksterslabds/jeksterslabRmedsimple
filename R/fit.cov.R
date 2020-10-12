#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Fit Saturated Model - Structural Equation Modeling
#'
#' @description Fits covariances and means of \eqn{x}, \eqn{m}, and \eqn{y} using structural equation modeling.
#'
#' @family model fit functions
#' @keywords fit
#' @inheritParams fit.sem
#' @importFrom lavaan sem
#' @examples
#' library(lavaan)
#' taskid <- 1
#'
#' # Complete Data ----------------------------------------------------
#' original <- mvn_dat(taskid = taskid)
#' cov(original)
#' colMeans(original)
#'
#' # Missing completely at random -------------------------------------
#' data <- mvn_dat_mcar(data = original, taskid = taskid)
#' out <- fit.cov(data = data)
#' summary(out)
#'
#' # Missing at random ------------------------------------------------
#' data <- mvn_dat_mar(data = original, taskid = taskid)
#' out <- fit.cov(data = data)
#' summary(out)
#' @export
fit.cov <- function(data) {
  data <- as.data.frame(data)
  colnames(data) <- c("x", "m", "y")
  model <- "
      # variances
      x ~~ sigma2xhat * x
      m ~~ sigma2mhat * m
      y ~~ sigma2yhat * y
      # covariances
      x ~~ sigmaxmhat * m
      x ~~ sigmaxyhat * y
      m ~~ sigmamyhat * y
      # means of x, m, and y
      y ~ muyhat * 1
      m ~ mumhat * 1
      x ~ muxhat * 1
    "
  out <- sem(
    model = model,
    data = data,
    estimator = "ML",
    likelihood = "wishart",
    missing = "fiml",
    fixed.x = FALSE
  )
}
