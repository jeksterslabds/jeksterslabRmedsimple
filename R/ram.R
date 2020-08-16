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
#' @title S Matrix
#'
#' @description Matrix of variance of `x` and residual variances from the simple mediation model.
#'
#' @family reticular action model functions
#' @keywords ram
#' @inherit A details
#' @param sigma2x Numeric.
#'   Variance of `x` \eqn{\left( \sigma_{x}^{2} \right)}.
#' @param sigma2epsilonm Numeric.
#'   Residual variance of \eqn{\varepsilon_{m_{i}}} \eqn{\left( \sigma_{\varepsilon_m}^{2} \right)}.
#' @param sigma2epsilony Numeric.
#'   Residual variance of \eqn{\varepsilon_{y_{i}}} \eqn{\left( \sigma_{\varepsilon_y}^{2} \right)}.
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
#' @title S Matrix from Variances of `x`, `m`, and `y` and the `A` matrix
#'
#' @description Matrix of variance of `x` and residual variances from the simple mediation model
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
