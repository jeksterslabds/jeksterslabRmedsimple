#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title M Vector
#'
#' @description Mean of `x` and intercepts from the simple mediation model.
#'
#' @family reticular action model functions
#' @keywords ram
#' @param mux Numeric.
#'   Mean of `x` \eqn{\left( \mu_x \right)} .
#' @param deltam Numeric.
#'   Intercept of `m` \eqn{\left( \delta_m \right)} .
#' @param deltay Numeric.
#'   Intercept of `y` \eqn{\left( \delta_y \right)} .
#' @examples
#' M(
#'   mux = 70.18000,
#'   deltam = 26.82246,
#'   deltay = 29.91071
#' )
#' @export
M <- function(mux,
              deltam,
              deltay) {
  M <- c(
    mux,
    deltam,
    deltay
  )
  names(M) <- c("x", "m", "y")
  M
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title M Vector
#'
#' @description Mean of `x` and intercepts from the simple mediation model
#'   from means of `x`, `m` and `y`.
#'
#' @family reticular action model functions
#' @keywords ram
#' @inheritParams A
#' @inheritParams M
#' @inherit A details
#' @param mum Numeric.
#'   Mean of `m` \eqn{\left( \mu_m \right)} .
#' @param muy Numeric.
#'   Mean of `y` \eqn{\left( \mu_y \right)} .
#' @examples
#' Mfrommu(
#'   mux = 70.18,
#'   mum = 3.06,
#'   muy = 3.24,
#'   taudot = 0.207648,
#'   beta = 0.451039,
#'   alpha = 0.338593
#' )
#' @export
Mfrommu <- function(mux,
                    mum,
                    muy,
                    taudot,
                    beta,
                    alpha) {
  A <- A(
    taudot = taudot,
    beta = beta,
    alpha = alpha
  )
  x <- mux
  m <- mum - mux * alpha
  y <- drop(
    muy - sum(crossprod(c(mux, mum), c(taudot, beta)))
  )
  M <- c(
    x,
    m,
    y
  )
  names(M) <- c("x", "m", "y")
  M
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Model-Implied Mean Vector
#'
#' @family reticular action model functions
#' @keywords ram
#' @description Model-Implied Mean Vector from the simple mediation model.
#'
#' @inheritParams M
#' @inheritParams A
#' @inherit A details
#' @examples
#' mutheta(
#'   mux = 70.18000,
#'   deltam = 26.82246,
#'   deltay = 29.91071,
#'   taudot = 0.207648,
#'   beta = 0.451039,
#'   alpha = 0.338593
#' )
#' colMeans(jeksterslabRdatarepo::thirst)
#' @export
mutheta <- function(mux,
                    deltam,
                    deltay,
                    taudot,
                    beta,
                    alpha) {
  M <- M(
    mux = mux,
    deltam = deltam,
    deltay = deltay
  )
  A <- A(
    taudot = taudot,
    beta = beta,
    alpha = alpha
  )
  mutheta <- drop(
    solve(diag(nrow(A)) - A) %*% M
  )
  names(mutheta) <- c("x", "m", "y")
  mutheta
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title A Matrix
#'
#' @description Matrix of regression slopes from the simple mediation model.
#'
#' @details The simple mediation model is given by
#'   \deqn{
#'     y_i = \delta_y + \dot{\tau} x_i + \beta m_i + \varepsilon_{y_{i}}
#'   }
#'
#'   \deqn{
#'     m_i = \delta_m + \alpha x_i + \varepsilon_{m_{i}}
#'   }
#'
#'   The parameters for the mean structure are
#'   \deqn{
#'     \boldsymbol{\theta}_{\text{mean structure}} = \left\{ \mu_x, \delta_m, \delta_y \right\} .
#'   }
#'
#'   The parameters for the covariance strcuture are
#'   \deqn{
#'     \boldsymbol{\theta}_{\text{covariance structure}} = \left\{ \dot{\tau}, \beta, \alpha, \sigma_{x}^{2},
#'     \sigma_{\varepsilon_{m}}^{2}, \sigma_{\varepsilon_{y}}^{2} \right\} .
#'   }
#'
#' @family reticular action model functions
#' @keywords ram
#' @param taudot Numeric.
#'   Slope of path from `x` to `y` \eqn{\left( \dot{\tau} \right)}.
#' @param beta Numeric.
#'   Slope of path from `m` to `y` \eqn{\left( \beta \right)} .
#' @param alpha Numeric.
#'   Slope of path from `x` to `m` \eqn{\left( \alpha \right)} .
#' @examples
#' A(
#'   taudot = 0.207648,
#'   beta = 0.451039,
#'   alpha = 0.338593
#' )
#' @export
A <- function(taudot,
              beta,
              alpha) {
  A <- matrix(
    data = c(
      0, alpha, taudot,
      0, 0, beta,
      0, 0, 0
    ),
    ncol = 3
  )
  varnames <- c("x", "m", "y")
  colnames(A) <- varnames
  rownames(A) <- varnames
  A
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Standardized A Matrix
#'
#' @description Matrix of regression slopes from the standardized simple mediation model.
#'
#' @family reticular action model functions
#' @keywords ram
#' @param taudotprime Numeric.
#'   Standardized slope of path from `x` to `y` \eqn{\left( \dot{\tau}^{\prime} \right)}.
#' @param betaprime Numeric.
#'   Standardized slope of path from `m` to `y` \eqn{\left( \beta^{\prime} \right)} .
#' @param alphaprime Numeric.
#'   Standardized slope of path from `x` to `m` \eqn{\left( \alpha^{\prime} \right)} .
#' @param lambdax Numeric.
#'   Factor loading `xlatent ~ x` \eqn{ \left( \lambda_x \right)} .
#'   Numerically equivalent to the standard deviation of `x`.
#' @param lambdam Numeric.
#'   Factor loading `mlatent ~ m` \eqn{ \left( \lambda_m \right)} .
#'   Numerically equivalent to the standard deviation of `m`.
#' @param lambday Numeric.
#'   Factor loading `ylatent ~ y` \eqn{ \left( \lambda_y \right)} .
#'   Numerically equivalent to the standard deviation of `y`.
#' @examples
#' A.std(
#'   taudotprime = 0.2080748,
#'   betaprime = 0.4126006,
#'   alphaprime = 0.3708979,
#'   lambdax = 1.137308,
#'   lambdam = 1.038248,
#'   lambday = 1.134973
#' )
#' @export
A.std <- function(taudotprime,
                  betaprime,
                  alphaprime,
                  lambdax,
                  lambdam,
                  lambday) {
  A <- matrix(
    data = 0,
    ncol = 6,
    nrow = 6
  )
  A[1, 4] <- lambdax
  A[2, 5] <- lambdam
  A[3, 6] <- lambday
  A[5, 4] <- alphaprime
  A[6, 4] <- taudotprime
  A[6, 5] <- betaprime
  varnames <- c("x", "m", "y", "xlatent", "mlatent", "ylatent")
  colnames(A) <- varnames
  rownames(A) <- varnames
  A
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title S Matrix
#'
#' @description Matrix of variance of `x` and error variances from the simple mediation model.
#'
#' @family reticular action model functions
#' @keywords ram
#' @inherit A details
#' @param sigma2x Numeric.
#'   Variance of `x` \eqn{\left( \sigma_{x}^{2} \right)}.
#' @param sigma2epsilonm Numeric.
#'   Error variance of \eqn{\varepsilon_{m_{i}}} \eqn{\left( \sigma_{\varepsilon_m}^{2} \right)}.
#' @param sigma2epsilony Numeric.
#'   Error variance of \eqn{\varepsilon_{y_{i}}} \eqn{\left( \sigma_{\varepsilon_y}^{2} \right)}.
#' @examples
#' S(
#'   sigma2x = 1.293469,
#'   sigma2epsilonm = 0.9296691,
#'   sigma2epsilony = 0.9310597
#' )
#' @export
S <- function(sigma2x,
              sigma2epsilonm,
              sigma2epsilony) {
  S <- diag(c(sigma2x, sigma2epsilonm, sigma2epsilony))
  varnames <- c("x", "m", "y")
  colnames(S) <- varnames
  rownames(S) <- varnames
  S
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Standardized S Matrix
#'
#' @description Matrix of error variances from the standardized simple mediation model.
#'
#' @family reticular action model functions
#' @keywords ram
#' @param sigma2epsilonm Numeric.
#'   Error variance of \eqn{\varepsilon_{m_{latent_{i}}}} \eqn{\left( \sigma_{\varepsilon_{m_{latent}}}^{2} \right)}.
#' @param sigma2epsilony Numeric.
#'   Error variance of \eqn{\varepsilon_{y_{latent_{i}}}} \eqn{\left( \sigma_{\varepsilon_{y_{latent}}}^{2} \right)}.
#' @examples
#' S.std(sigma2epsilonm = 0.8624347, sigma2epsilony = 0.7227811)
#' @export
S.std <- function(sigma2epsilonm,
                  sigma2epsilony) {
  S <- matrix(
    data = 0,
    ncol = 6,
    nrow = 6
  )
  S[4, 4] <- 1
  S[5, 5] <- sigma2epsilonm
  S[6, 6] <- sigma2epsilony
  varnames <- c("x", "m", "y", "xlatent", "mlatent", "ylatent")
  colnames(S) <- varnames
  rownames(S) <- varnames
  S
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title S Matrix from Variances of `x`, `m`, and `y` and the `A` matrix
#'
#' @description Matrix of variance of `x` and error variances from the simple mediation model
#'   from variances of `x`, `m`, and `y` and the `A` matrix.
#'
#' @family reticular action model functions
#' @keywords ram
#' @inheritParams A
#' @inheritParams S
#' @inherit A details
#' @param sigma2m Numeric.
#'   Variance of `m` \eqn{\left( \sigma_{m}^{2} \right)} .
#' @param sigma2y Numeric.
#'   Variance of `y` \eqn{\left( \sigma_{y}^{2} \right)} .
#' @examples
#' Sfromsigma2(
#'   taudot = 0.207648,
#'   beta = 0.451039,
#'   alpha = 0.338593,
#'   sigma2x = 1.2934694,
#'   sigma2m = 1.0779592,
#'   sigma2y = 1.2881633
#' )
#' @export
Sfromsigma2 <- function(taudot,
                        beta,
                        alpha,
                        sigma2x,
                        sigma2m,
                        sigma2y) {
  ramSigmatheta <- function(A,
                            S) {
    invIminusA <- solve(diag(nrow(A)) - A)
    invIminusA %*% S %*% t(invIminusA)
  }
  sigma2 <- c(
    sigma2x,
    sigma2m,
    sigma2y
  )
  A <- A(
    taudot = taudot,
    beta = beta,
    alpha = alpha
  )
  S <- matrix(
    data = 0,
    ncol = ncol(A),
    nrow = nrow(A)
  )
  Sigmatheta_temp <- ramSigmatheta(
    A = A,
    S = S
  )
  index <- 1:nrow(A)
  for (i in index) {
    S[i, i] <- sigma2[i] - Sigmatheta_temp[i, i]
    Sigmatheta_temp <- ramSigmatheta(
      A = A,
      S = S
    )
  }
  varnames <- c("x", "m", "y")
  colnames(S) <- varnames
  rownames(S) <- varnames
  S
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Model-Implied Variance-Covariance Matrix
#'
#' @description Model-implied variance-covariance matrix from the simple mediation model.
#'
#' @family reticular action model functions
#' @keywords ram
#' @inheritParams A
#' @inheritParams S
#' @inherit A details
#' @examples
#' Sigmatheta(
#'   taudot = 0.207648,
#'   beta = 0.451039,
#'   alpha = 0.338593,
#'   sigma2x = 1.293469,
#'   sigma2epsilonm = 0.9296691,
#'   sigma2epsilony = 0.9310597
#' )
#' cov(jeksterslabRdatarepo::thirst)
#' @export
Sigmatheta <- function(taudot,
                       beta,
                       alpha,
                       sigma2x,
                       sigma2epsilonm,
                       sigma2epsilony) {
  A <- A(
    taudot = taudot,
    beta = beta,
    alpha = alpha
  )
  S <- S(
    sigma2x = sigma2x,
    sigma2epsilonm = sigma2epsilonm,
    sigma2epsilony = sigma2epsilony
  )
  invIminusA <- solve(
    diag(nrow(A)) - A
  )
  Sigmatheta <- invIminusA %*% S %*% t(invIminusA)
  colnames(Sigmatheta) <- c("x", "m", "y")
  rownames(Sigmatheta) <- c("x", "m", "y")
  Sigmatheta
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Model-Implied Variance-Covariance Matrix
#'
#' @description Model-implied variance-covariance matrix from the simple mediation model.
#'
#' @family reticular action model functions
#' @keywords ram
#' @inheritParams Sfromsigma2
#' @inherit A details
#' @examples
#' Sigmathetafromsigma2(
#'   taudot = 0.207648,
#'   beta = 0.451039,
#'   alpha = 0.338593,
#'   sigma2x = 1.2934694,
#'   sigma2m = 1.0779592,
#'   sigma2y = 1.2881633
#' )
#' cov(jeksterslabRdatarepo::thirst)
#' @export
Sigmathetafromsigma2 <- function(taudot,
                                 beta,
                                 alpha,
                                 sigma2x,
                                 sigma2m,
                                 sigma2y) {
  A <- A(
    taudot = taudot,
    beta = beta,
    alpha = alpha
  )
  S <- Sfromsigma2(
    taudot = taudot,
    beta = beta,
    alpha = alpha,
    sigma2x = sigma2x,
    sigma2m = sigma2m,
    sigma2y = sigma2y
  )
  invIminusA <- solve(
    diag(nrow(A)) - A
  )
  Sigmatheta <- invIminusA %*% S %*% t(invIminusA)
  colnames(Sigmatheta) <- c("x", "m", "y")
  rownames(Sigmatheta) <- c("x", "m", "y")
  Sigmatheta
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Standardized Model-Implied Variance-Covariance Matrix
#'
#' @description Model-implied variance-covariance matrix from the standardized simple mediation model.
#'
#' @family reticular action model functions
#' @keywords ram
#' @inheritParams A.std
#' @inheritParams S.std
#' @examples
#' Sigmatheta.std(
#'   taudotprime = 0.2080748,
#'   betaprime = 0.4126006,
#'   alphaprime = 0.3708979,
#'   lambdax = 1.137308,
#'   lambdam = 1.038248,
#'   lambday = 1.134973,
#'   sigma2epsilonm = 0.8624347,
#'   sigma2epsilony = 0.7227811
#' )
#' cov(jeksterslabRdatarepo::thirst)
#' @export
Sigmatheta.std <- function(taudotprime,
                           betaprime,
                           alphaprime,
                           lambdax,
                           lambdam,
                           lambday,
                           sigma2epsilonm,
                           sigma2epsilony) {
  A <- A.std(
    taudotprime = taudotprime,
    betaprime = betaprime,
    alphaprime = alphaprime,
    lambdax = lambdax,
    lambdam = lambdam,
    lambday = lambday
  )
  S <- S.std(
    sigma2epsilonm = sigma2epsilonm,
    sigma2epsilony = sigma2epsilony
  )
  F <- matrix(
    data = 0,
    ncol = 6,
    nrow = 3
  )
  F[1, 1] <- 1
  F[2, 2] <- 1
  F[3, 3] <- 1
  invIminusA <- solve(
    diag(nrow(A)) - A
  )
  Sigmatheta <- F %*% invIminusA %*% S %*% t(invIminusA) %*% t(F)
  colnames(Sigmatheta) <- c("x", "m", "y")
  rownames(Sigmatheta) <- c("x", "m", "y")
  Sigmatheta
}
