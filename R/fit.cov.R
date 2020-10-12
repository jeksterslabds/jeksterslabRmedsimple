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
#' cov(jeksterslabRdatarepo::thirst)
#' colMeans(jeksterslabRdatarepo::thirst)
#' summary(fit.cov(data = jeksterslabRdatarepo::thirst))
#'
#' taskid <- 1
#' data <- mvn_dat(taskid = taskid)
#'
#' # Complete Data ----------------------------------------------------
#' cov(data)
#' colMeans(data)
#' summary(fit.cov(data = data))
#'
#' # Missing completely at random -------------------------------------
#' ## 10% missing
#' summary(fit.cov(data = mvn_mcar_10_dat(data = data, taskid = taskid)))
#'
#' ## 20% missing
#' summary(fit.cov(data = mvn_mcar_20_dat(data = data, taskid = taskid)))
#'
#' ## 30% missing
#' summary(fit.cov(data = mvn_mcar_30_dat(data = data, taskid = taskid)))
#'
#' # Missing at random ------------------------------------------------
#' ## 10% missing
#' summary(fit.cov(data = mvn_mar_10_dat(data = data, taskid = taskid)))
#'
#' ## 20% missing
#' summary(fit.cov(data = mvn_mar_20_dat(data = data, taskid = taskid)))
#'
#' ## 30% missing
#' summary(fit.cov(data = mvn_mar_30_dat(data = data, taskid = taskid)))
#'
#' # Missing Not at random --------------------------------------------
#' ## 10% missing
#' summary(fit.cov(data = mvn_mnar_10_dat(data = data, taskid = taskid)))
#'
#' ## 20% missing
#' summary(fit.cov(data = mvn_mnar_20_dat(data = data, taskid = taskid)))
#'
#' ## 30% missing
#' summary(fit.cov(data = mvn_mnar_30_dat(data = data, taskid = taskid)))
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
  sem(
    model = model,
    data = data,
    estimator = "ML",
    likelihood = "wishart",
    missing = "fiml",
    fixed.x = FALSE
  )
}
