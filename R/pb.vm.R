#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Parametric Bootstrapping Assuming Multivariate Normal Distribution (Sampling Distribution)
#'
#' @family parametric functions
#' @keywords pb
#' @importFrom semTools mvrnonnorm
#' @importFrom stats cov complete.cases
#' @importFrom jeksterslabRplots .hist.plot
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams fit.ols
#' @inheritParams jeksterslabRpar::par_lapply
#' @param muthetahat Numeric vector.
#'   Model-implied mean vector \eqn{ \boldsymbol{\mu} \left( \boldsymbol{\hat{\theta}} \right) } .
#' @param Sigmathetahat Numeric matrix.
#'   Model-implied variance-covariance matrix \eqn{ \boldsymbol{\Sigma} \left( \boldsymbol{\hat{\theta}} \right) } .
#' @param skewnessthetahat Numeric vector. Estimated skewness.
#' @param kurtosisthetahat Numeric vector. Estimated excess kurtosis
#' @param n Integer.
#'   Sample size.
#' @param B Integer.
#'   Number of bootstrap samples.
#' @examples
#' muthetahat <- mutheta(
#'   mux = 70.18000,
#'   deltam = 26.82246,
#'   deltay = 29.91071,
#'   taudot = 0.207648,
#'   beta = 0.451039,
#'   alpha = 0.338593
#' )
#' Sigmathetahat <- Sigmatheta(
#'   taudot = 0.207648,
#'   beta = 0.451039,
#'   alpha = 0.338593,
#'   sigma2x = 1.293469,
#'   sigma2epsilonm = 0.9296691,
#'   sigma2epsilony = 0.9310597
#' )
#' skewnessthetahat <- c(
#'   -0.542156,
#'   -0.2377815,
#'   -0.06016235
#' )
#' kurtosisthetahat <- c(
#'   0.2215535,
#'   -0.4361385,
#'   -0.6906525
#' )
#'
#' # Unstandardized -------------------------------------------------------------
#' thetahatstar <- pb.vm(
#'   muthetahat = muthetahat, Sigmathetahat = Sigmathetahat,
#'   skewnessthetahat = skewnessthetahat, kurtosisthetahat = kurtosisthetahat,
#'   n = 50, B = 5000, par = FALSE
#' )
#' hist(thetahatstar)
#'
#' # Standardized ---------------------------------------------------------------
#' thetahatstar <- pb.vm(
#'   muthetahat = muthetahat, Sigmathetahat = Sigmathetahat,
#'   skewnessthetahat = skewnessthetahat, kurtosisthetahat = kurtosisthetahat,
#'   n = 50, std = TRUE, B = 5000, par = FALSE
#' )
#' hist(thetahatstar)
#' @export
pb.vm <- function(muthetahat,
                  Sigmathetahat,
                  skewnessthetahat,
                  kurtosisthetahat,
                  n,
                  std = FALSE,
                  B = 5000,
                  par = TRUE,
                  ncores = NULL,
                  blas_threads = TRUE,
                  mc = TRUE,
                  lb = FALSE) {
  foo <- function(iter,
                  muthetahat,
                  Sigmathetahat,
                  skewnessthetahat,
                  kurtosisthetahat,
                  n,
                  std) {
    data <- mvrnonnorm(
      n = n,
      mu = muthetahat,
      Sigma = Sigmathetahat,
      skewness = skewnessthetahat,
      kurtosis = kurtosisthetahat
    )
    fit.ols(
      data = data,
      minimal = TRUE,
      std = std
    )
  }
  thetahatstar <- par_lapply(
    X = 1:B,
    FUN = foo,
    muthetahat = muthetahat,
    Sigmathetahat = Sigmathetahat,
    skewnessthetahat = skewnessthetahat,
    kurtosisthetahat = kurtosisthetahat,
    n = n,
    std = std,
    par = par,
    ncores = ncores,
    blas_threads = blas_threads,
    mc = mc,
    lb = lb,
    rbind = TRUE
  )
  as.vector(thetahatstar)
}
