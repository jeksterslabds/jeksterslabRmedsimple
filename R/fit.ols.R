#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Fit Simple Mediation Model - Minimal
#'
#' @description Fits the simple mediation model and returns the indirect effect.
#'
#' @details The fitted simple mediation model is given by
#'   \deqn{
#'     y_i
#'     =
#'      \hat{\delta}_{y} + \hat{\dot{\tau}} x_i + \hat{\beta} m_i +
#'      \hat{\varepsilon}_{y_{i}}
#'   }
#'
#'   \deqn{
#'     m_i = \hat{\delta}_{m} + \hat{\alpha} x_i + \hat{\varepsilon}_{m_{i}}
#'   }
#'
#'   The estimated parameters for the mean structure are
#'   \deqn{
#'     \boldsymbol{\hat{\theta}}_{\text{mean structure}}
#'     =
#'      \left\{ \hat{\mu}_{x}, \hat{\delta}_{m}, \hat{\delta}_{y} \right\} .
#'   }
#'
#'   The estimated parameters for the covariance structure are
#'   \deqn{
#'     \boldsymbol{\hat{\theta}}_{\text{covariance structure}}
#'     =
#'      \left\{ \hat{\dot{\tau}}, \hat{\beta}, \hat{\alpha},
#'      \hat{\sigma}_{x}^{2}, \hat{\sigma}_{\hat{\varepsilon}_{m}}^{2},
#'      \hat{\sigma}_{\hat{\varepsilon}_{y}}^{2} \right\} .
#'   }
#'
#' @family model fit functions
#' @keywords fit
#' @import jeksterslabRlinreg
#' @importFrom stats var
#' @param data `n` by 3 matrix or data frame.
#'   `data[, 1]` correspond to values for `x`.
#'   `data[, 2]` correspond to values for `m`.
#'   `data[, 3]` correspond to values for `y`.
#' @param minimal Logical.
#'   If `TRUE`, only returns the estimate of the indirect effect
#'   \eqn{\left( \hat{\alpha} \hat{\beta} \right)}.
#'   If `FALSE`, returns more information.
#' @param std Logical.
#'   Standardize the indirect effect
#'   \eqn{
#'     \hat{\alpha}^{\prime} \hat{\beta}^{\prime}
#'     = \hat{\alpha} \hat{\beta} \frac{\hat{\sigma}_x}{\hat{\sigma}_y}}.
#' @examples
#' data <- jeksterslabRdatarepo::thirst
#' .fit(data, minimal = TRUE)
#' .fit(data, minimal = TRUE, std = TRUE)
#' .fit(data, minimal = FALSE)
#' @export
.fit <- function(data,
                 minimal = TRUE,
                 std = FALSE) {
  x <- data[, 1]
  m <- data[, 2]
  y <- data[, 3]
  n <- nrow(data)
  # y ~ x + m -------------------------------------------------------------------
  X1 <- cbind(
    constant = 1,
    x,
    m
  )
  y1 <- y
  betahat1 <- betahat(
    X = X1,
    y = y1
  )
  # m ~ x -----------------------------------------------------------------------
  X2 <- cbind(
    constant = 1,
    x
  )
  y2 <- m
  betahat2 <- betahat(
    X = X2,
    y = y2
  )
  indirect <- unname(
    betahat1[3] * betahat2[2]
  )
  if (minimal) {
    if (std) {
      return(
        indirect * (sd(x) / sd(y))
      )
    } else {
      return(
        indirect
      )
    }
  }
  RSS1 <- RSS(
    X = X1,
    y = y1
  )
  RSS2 <- RSS(
    X = X2,
    y = y2
  )
  sigma2hatepsilonhat1 <- .sigma2hatepsilonhat(
    RSS = RSS1,
    n = n,
    k = ncol(X1)
  )
  sigma2hatepsilonhat2 <- .sigma2hatepsilonhat(
    RSS = RSS2,
    n = n,
    k = ncol(X2)
  )
  se1 <- sqrt(
    diag(
      .vcovhatbetahat(
        sigma2hatepsilonhat = sigma2hatepsilonhat1,
        X = X1
      )
    )
  )
  se2 <- sqrt(
    diag(
      .vcovhatbetahat(
        sigma2hatepsilonhat = sigma2hatepsilonhat2,
        X = X2
      )
    )
  )
  # standardized estimates and standard errors
  slopeshatprime1 <- slopeshatprime(
    X = X1,
    y = y1
  )
  slopeshatprime2 <- slopeshatprime(
    X = X2,
    y = y2
  )
  seprime1 <- sehatslopeshatprimedelta(
    X = X1,
    y = y1
  )
  seprime2 <- sehatslopeshatprimedelta(
    X = X2,
    y = y2
  )
  # collate results
  est <- c(
    betahat1,
    betahat2,
    indirect
  )
  estprime <- c(
    slopeshatprime1,
    slopeshatprime2,
    indirect * (sd(x) / sd(y))
  )
  names(est) <- c(
    "deltayhat",
    "taudothat",
    "betahat",
    "deltamhat",
    "alphahat",
    "alphahatbetahat"
  )
  names(estprime) <- c(
    "taudothatprime",
    "betahatprime",
    "alphahatprime",
    "alphahatprimebetahatprime"
  )
  S <- c(
    sigma2xhat = var(x),
    sigma2epsilonmhat = sigma2hatepsilonhat2,
    sigma2epsilonyhat = sigma2hatepsilonhat1
  )
  se <- c(
    se1,
    se2
  )
  names(se) <- c(
    "sehatdeltayhat",
    "sehattaudothat",
    "sehatbetahat",
    "sehatdeltamhat",
    "sehatalphahat"
  )
  seprime <- c(
    seprime1,
    seprime2
  )
  names(seprime) <- c(
    "sehattaudothatprime",
    "sehatbetahatprime",
    "sehatalphahatprime"
  )
  out <- c(
    est,
    estprime,
    S,
    muxhat = mean(x),
    se,
    seprime
  )
  out
}
