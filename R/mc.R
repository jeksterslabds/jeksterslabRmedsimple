#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Monte Carlo Method for Indirect Effect in a Simple Mediation Model
#'   Assuming \eqn{\alpha} and \eqn{\beta} Follow a Multivariate Normal Distribution (Sampling Distribution)
#'
#' @description In this method \eqn{\alpha} and \eqn{\beta} are assumed to follow
#'   a multivariate normal distribution.
#'
#' @family monte carlo method functions
#' @keywords mc
#' @importFrom mvnfast rmvn
#' @param R Integer.
#'   Monte Carlo replications.
#' @param alphahat Numeric.
#'   Estimated slope of path from `x` to `m` \eqn{\left( \hat{\alpha} \right)} .
#' @param sehatalphahat Numeric.
#'   Estimated standard error of slope of path from `x` to `m` \eqn{\left( \widehat{se}_{\hat{\alpha}} \right)} .
#' @param betahat Numeric.
#'   Estimated slope of path from `m` to `y` \eqn{\left( \hat{\beta} \right)} .
#' @param sehatbetahat Numeric.
#'   Estimated standard error of slope of path from `m` to `y` \eqn{\left( \widehat{se}_{\hat{\beta}} \right)} .
#' @examples
#' thetahatstar <- mc.mvn(
#'   R = 20000L, alphahat = 0.338593, sehatalphahat = 0.12236736,
#'   betahat = 0.451039, sehatbetahat = 0.14597405
#' )
#' hist(thetahatstar)
#' @export
mc.mvn <- function(R = 20000L,
                   alphahat,
                   sehatalphahat,
                   betahat,
                   sehatbetahat) {
  mu <- c(
    alphahat,
    betahat
  )
  Sigma <- matrix(
    data = c(
      sehatalphahat^2,
      0,
      0,
      sehatbetahat^2
    ),
    ncol = 2
  )
  mc <- mvrnorm(
    n = R,
    mu = mu,
    Sigma = Sigma
  )
  as.vector(
    unname(mc[, 1] * mc[, 2])
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Monte Carlo Method for Indirect Effect in a Simple Mediation Model
#'   Assuming \eqn{\alpha} and \eqn{\beta} Follow a \eqn{t} Distribution (Sampling Distribution)
#'
#' @description In this method \eqn{\alpha} and \eqn{\beta} are assumed to follow a \eqn{t} distribution
#'   with \eqn{df = n - 2} and \eqn{df = n - 3} respectively.
#'
#' @family monte carlo method functions
#' @keywords mc
#' @inheritParams mc.mvn
#' @importFrom stats rt
#' @param n Integer.
#'   Sample size.
#' @examples
#' thetahatstar <- mc.t(
#'   R = 20000L, alphahat = 0.338593, sehatalphahat = 0.12236736,
#'   betahat = 0.451039, sehatbetahat = 0.14597405,
#'   n = 20
#' )
#' hist(thetahatstar)
#' @export
mc.t <- function(R = 20000L,
                 alphahat,
                 sehatalphahat,
                 betahat,
                 sehatbetahat,
                 n) {
  alphahatstar <- rt(n = R, df = n - 2) * sehatalphahat + alphahat
  betahatstar <- rt(n = R, df = n - 3) * sehatbetahat + betahat
  as.vector(
    unname(alphahatstar * betahatstar)
  )
}
