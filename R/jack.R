#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Jackknife Estimates of Indirect Effect for a Simple Mediation Model
#'
#' @family jackknife functions
#' @keywords jack
#' @inheritParams fit.ols
#' @inheritParams nb
#' @examples
#' data <- jeksterslabRdatarepo::thirst
#' jack(data, par = FALSE)
#' jack(data, std = TRUE, par = FALSE)
#' @export
jack <- function(data,
                 std = FALSE,
                 complete = TRUE,
                 par = TRUE,
                 ncores = NULL,
                 blas_threads = TRUE,
                 mc = TRUE,
                 lb = FALSE) {
  if (complete) {
    data <- data[complete.cases(data), ]
    foo <- function(i,
                    data,
                    std) {
      fit.ols(
        data = data[-i, ],
        minimal = TRUE,
        std = std
      )
    }
  } else {
    foo <- function(i,
                    data,
                    std) {
      fit.sem(
        data = data[-i, ],
        minimal = TRUE,
        std = std,
        fiml = TRUE
      )
    }
  }
  thetahatstar <- par_lapply(
    X = 1:nrow(data),
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
