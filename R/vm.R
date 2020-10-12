#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Generate Nonnormal Data Using the Vale and Maurelli (1983) Approach
#'
#' @family multivariate nonnormal data functions
#' @keywords vm
#' @importFrom semTools mvrnonnorm
#' @inheritParams mvn
#' @param skewness Numeric vector. Skewness.
#' @param kurtosis Numeric vector. Kurtosis.
#' @examples
#' n <- 100
#' mu <- c(0, 0)
#' Sigma <- matrix(data = c(1, 0.50, 0.50, 1), ncol = 2)
#' skewness <- c(5, 2)
#' kurtosis <- c(3, 3)
#' data <- vm(n = n, mu = mu, Sigma = Sigma, skewness = skewness, kurtosis = kurtosis)
#' colMeans(data)
#' cov(data)
#' apply(X = data, MARGIN = 2, FUN = jeksterslabRdist::skew)
#' apply(X = data, MARGIN = 2, FUN = jeksterslabRdist::kurt)
#' data
#' @export
vm <- function(n,
               mu,
               Sigma,
               skewness,
               kurtosis) {
  mvrnonnorm(
    n = n,
    mu = mu,
    Sigma = Sigma,
    skewness = skewness,
    kurtosis = kurtosis
  )
}
