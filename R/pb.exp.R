#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Parametric Bootstrapping Assuming Exponential X
#'
#' @family parametric functions
#' @keywords pb
#' @importFrom MASS mvrnorm
#' @importFrom stats cov complete.cases
#' @importFrom jeksterslabRplots .hist.plot
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams fit.ols
#' @inheritParams pb.mvn
#' @inheritParams jeksterslabRpar::par_lapply
#' @examples
#' data <- genexp(taskid = 1)
#'
#' # Unstandardized -------------------------------------------------------------
#' thetahatstar <- pb.exp(
#'   data = data, B = 5000, par = FALSE
#' )
#' hist(thetahatstar)
#'
#' # Standardized ---------------------------------------------------------------
#' thetahatstar <- pb.exp(
#'   data = data, std = TRUE, B = 5000, par = FALSE
#' )
#' hist(thetahatstar)
#' @export
pb.exp <- function(data,
                   std = FALSE,
                   B = 5000,
                   par = TRUE,
                   ncores = NULL,
                   blas_threads = TRUE,
                   mc = TRUE,
                   lb = FALSE) {
  n <- dim(data)[1]
  fit_X <- MASS::fitdistr(
    x = data[, 1],
    densfun = "exponential"
  )
  rate <- fit_X$estimate["rate"]
  fit <- fit.ols(
    data = data,
    minimal = FALSE
  )
  taudot <- fit["taudothat"]
  beta <- fit["betahatprime"]
  alpha <- fit["alphahatprime"]
  sigma2epsilony <- fit["sigma2hatepsilonyhat"]
  sigma2epsilonm <- fit["sigma2hatepsilonmhat"]
  deltay <- fit["deltayhat"]
  deltam <- fit["deltamhat"]
  foo <- function(iter,
                  n,
                  rate,
                  taudot,
                  beta,
                  alpha,
                  sigma2epsilony,
                  sigma2epsilonm,
                  deltay,
                  deltam,
                  std) {
    epsilony <- rnorm(n = n, sd = sqrt(sigma2epsilony))
    epsilonm <- rnorm(n = n, sd = sqrt(sigma2epsilonm))
    x <- rexp(n = n, rate = rate)
    m <- deltam + alpha * x + epsilonm
    y <- deltay + taudot * x + beta * m + epsilony
    data <- cbind(
      x = x,
      m = m,
      y = y
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
    n = n,
    rate = rate,
    taudot = taudot,
    beta = beta,
    alpha = alpha,
    sigma2epsilony = sigma2epsilony,
    sigma2epsilonm = sigma2epsilonm,
    deltay = deltay,
    deltam = deltam,
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
