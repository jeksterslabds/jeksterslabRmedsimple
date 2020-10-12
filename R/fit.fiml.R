#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Fit Simple Mediation Model - Structural Equation Modeling with FIML
#'
#' @family model fit functions
#' @keywords fit
#' @inheritParams .fitsem
#' @export
.fitfiml <- function(data,
                     std = FALSE) {
  data <- as.data.frame(data)
  colnames(data) <- c("x", "m", "y")
  if (std) {
    model <- "
      # measurement model
      xlatent =~ NA * x
      mlatent =~ NA * m
      ylatent =~ NA * y
      # no measurement error
      x ~~ 0 * x
      m ~~ 0 * m
      y ~~ 0 * y
      # regression
      ylatent ~ taudothatprime * xlatent + betahatprime * mlatent
      mlatent ~ alphahatprime * xlatent
      # constraints
      xlatent ~~ sigma2xlatent * xlatent
      ylatent ~~ sigma2hatepsilonyhat * ylatent
      mlatent ~~ sigma2hatepsilonmhat * mlatent
      sigma2xlatent == 1
      sigma2hatepsilonyhat == 1 - taudothatprime^2 - betahatprime^2 -  2 * alphahatprime * taudothatprime * betahatprime
      sigma2hatepsilonmhat == 1 - alphahatprime^2
    "
  } else {
    model <- "
      y ~ taudothat * x + betahat * m
      m ~ alphahat * x
    "
  }
  sem(
    model = model,
    data = data,
    estimator = "ML",
    likelihood = "wishart",
    missing = "fiml",
    fixed.x = FALSE
  )
}
