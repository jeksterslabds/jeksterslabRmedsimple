#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Nonparametric Bootstrapping
#'
#' @family nonparametric functions
#' @keywords nb
#' @inheritParams fit.ols
#' @inheritParams nb.del
#' @examples
#' data <- jeksterslabRdatarepo::thirst
#'
#' # Unstandardized -------------------------------------------------------------
#' thetahatstar <- nb.fiml(data = data, B = 5000, par = FALSE)
#' hist(thetahatstar)
#'
#' # Standardized ---------------------------------------------------------------
#' thetahatstar <- nb.fiml(data = data, std = TRUE, B = 5000, par = FALSE)
#' hist(thetahatstar)
#' @export
nb.fiml <- function(data,
                    std = FALSE,
                    B = 5000,
                    par = TRUE,
                    ncores = NULL,
                    blas_threads = TRUE,
                    mc = TRUE,
                    lb = FALSE) {
  foo <- function(iter,
                  data,
                  std) {
    len <- 1:nrow(data)
    i <- sample(
      x = len,
      replace = TRUE
    )
    data <- data[i, ]
    fit.sem(
      data = data,
      minimal = TRUE,
      std = std,
      fiml = TRUE
    )
  }
  thetahatstar <- par_lapply(
    X = 1:B,
    FUN = foo,
    data = data,
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
