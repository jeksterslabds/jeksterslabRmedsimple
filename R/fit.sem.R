#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Fit Simple Mediation Model - Structural Equation Modeling
#'
#' @description Fits the simple mediation model using structural equation modeling.
#'
#' @family model fit functions
#' @keywords fit
#' @inheritParams fit.ols
#' @import lavaan
#' @param std Logical.
#'   If `TRUE`, estimate standardized simple mediation model
#'   using latent variables and nonlinear constraints.
#' @param fiml Logical.
#'   If `TRUE`, use `missing = "fiml"` to handle missing values.
#'   Note that using `missing = "fiml"` sets `fixed.x = FALSE`.
#' @examples
#' library(lavaan)
#' summary(fit.sem(data = jeksterslabRdatarepo::thirst))
#' summary(fit.sem(data = jeksterslabRdatarepo::thirst, std = TRUE))
#' summary(fit.sem(data = jeksterslabRdatarepo::thirst, minimal = TRUE))
#' summary(fit.sem(data = jeksterslabRdatarepo::thirst, minimal = TRUE, std = TRUE))
#'
#' taskid <- 1
#' data <- mvn_dat(taskid = taskid)
#'
#' # Unstandaradized ##################################################
#' # Complete Data ----------------------------------------------------
#' summary(fit.sem(data = data))
#'
#' # Missing completely at random -------------------------------------
#' ## 10% missing
#' summary(fit.sem(data = mvn_mcar_10_dat(data = data, taskid = taskid), fiml = TRUE))
#'
#' ## 20% missing
#' summary(fit.sem(data = mvn_mcar_20_dat(data = data, taskid = taskid), fiml = TRUE))
#'
#' ## 30% missing
#' summary(fit.sem(data = mvn_mcar_30_dat(data = data, taskid = taskid), fiml = TRUE))
#'
#' # Missing at random ------------------------------------------------
#' ## 10% missing
#' summary(fit.sem(data = mvn_mar_10_dat(data = data, taskid = taskid), fiml = TRUE))
#'
#' ## 20% missing
#' summary(fit.sem(data = mvn_mar_20_dat(data = data, taskid = taskid), fiml = TRUE))
#'
#' ## 30% missing
#' summary(fit.sem(data = mvn_mar_30_dat(data = data, taskid = taskid), fiml = TRUE))
#'
#' # Missing Not at random --------------------------------------------
#' ## 10% missing
#' summary(fit.sem(data = mvn_mnar_10_dat(data = data, taskid = taskid), fiml = TRUE))
#'
#' ## 20% missing
#' summary(fit.sem(data = mvn_mnar_20_dat(data = data, taskid = taskid), fiml = TRUE))
#'
#' ## 30% missing
#' summary(fit.sem(data = mvn_mnar_30_dat(data = data, taskid = taskid), fiml = TRUE))
#'
#' # Standaradized ####################################################
#' # Complete Data ----------------------------------------------------
#' summary(fit.sem(data = data, std = TRUE))
#'
#' # Missing completely at random -------------------------------------
#' ## 10% missing
#' summary(fit.sem(data = mvn_mcar_10_dat(data = data, taskid = taskid), std = TRUE, fiml = TRUE))
#'
#' ## 20% missing
#' summary(fit.sem(data = mvn_mcar_20_dat(data = data, taskid = taskid), std = TRUE, fiml = TRUE))
#'
#' ## 30% missing
#' summary(fit.sem(data = mvn_mcar_30_dat(data = data, taskid = taskid), std = TRUE, fiml = TRUE))
#'
#' # Missing at random ------------------------------------------------
#' ## 10% missing
#' summary(fit.sem(data = mvn_mar_10_dat(data = data, taskid = taskid), std = TRUE, fiml = TRUE))
#'
#' ## 20% missing
#' summary(fit.sem(data = mvn_mar_20_dat(data = data, taskid = taskid), std = TRUE, fiml = TRUE))
#'
#' ## 30% missing
#' summary(fit.sem(data = mvn_mar_30_dat(data = data, taskid = taskid), std = TRUE, fiml = TRUE))
#'
#' # Missing Not at random --------------------------------------------
#' ## 10% missing
#' summary(fit.sem(data = mvn_mnar_10_dat(data = data, taskid = taskid), std = TRUE, fiml = TRUE))
#'
#' ## 20% missing
#' summary(fit.sem(data = mvn_mnar_20_dat(data = data, taskid = taskid), std = TRUE, fiml = TRUE))
#'
#' ## 30% missing
#' summary(fit.sem(data = mvn_mnar_30_dat(data = data, taskid = taskid), std = TRUE, fiml = TRUE))
#' @export
fit.sem <- function(data,
                    minimal = FALSE,
                    std = FALSE,
                    fiml = FALSE) {
  data <- as.data.frame(data)
  colnames(data) <- c("x", "m", "y")
  if (std) {
    model <- "
      # measurement model
      ## `NA *` to estimate factor loading
      xlatent =~ NA * x + lambdaxhat * x
      mlatent =~ NA * m + lambdamhat * m
      ylatent =~ NA * y + lambdayhat * y
      # fix measurement error to zero
      ## `0 *` to fix the value to zero
      x ~~ 0 * x
      m ~~ 0 * m
      y ~~ 0 * y
      # regression
      ylatent ~ taudothatprime * xlatent + betahatprime * mlatent
      mlatent ~ alphahatprime * xlatent
      # fix variance of xlatent to 1
      ## `1 *` to fix the value to 1
      xlatent ~~ 1 * xlatent
      # nonlinear constraints
      ylatent ~~ sigma2hatepsilonylatenthat * ylatent
      mlatent ~~ sigma2hatepsilonmlatenthat * mlatent
      sigma2hatepsilonylatenthat == 1 - taudothatprime^2 - betahatprime^2 -  (2 * taudothatprime * betahatprime * alphahatprime)
      sigma2hatepsilonmlatenthat == 1 - alphahatprime^2
      # indirect effect
      alphahatprimebetahatprime := alphahatprime * betahatprime
    "
  } else {
    model <- "
      # regression
      y ~ taudothat * x + betahat * m
      m ~ alphahat * x
      # residual variances
      y ~~ sigma2hatepsilonyhat * y
      m ~~ sigma2hatepsilonmhat * m
      # indirect effect
      alphahatbetahat := alphahat * betahat
    "
  }
  if (fiml) {
    if (std) {
      means <- "
        # means of x, m, and y
        y ~ muyhat * 1
        m ~ mumhat * 1
        x ~ muxhat * 1
      "
      model <- paste(
        model,
        means
      )
    } else {
      means <- "
        # mean of x and intercepts of m and y
        y ~ deltayhat * 1
        m ~ deltamhat * 1
        x ~ muxhat * 1
      "
      vars <- "
        # variance of x
        x ~~ sigma2xhat * x
      "
      model <- paste(
        model,
        means,
        vars
      )
    }
    out <- sem(
      model = model,
      data = data,
      estimator = "ML",
      likelihood = "wishart",
      missing = "fiml",
      fixed.x = FALSE
    )
  } else {
    out <- sem(
      model = model,
      data = data,
      estimator = "ML",
      likelihood = "wishart"
    )
  }
  if (minimal) {
    estimates <- coef(out)
    if (std) {
      out <- unname(estimates["alphahatprime"] * estimates["betahatprime"])
    } else {
      out <- unname(estimates["alphahat"] * estimates["betahat"])
    }
  }
  out
}
