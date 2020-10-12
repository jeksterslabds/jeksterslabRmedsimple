#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Nonparametric Bootstrapping
#'
#' @family nonparametric functions
#' @keywords nb
#' @inheritParams fit.ols
#' @inheritParams pb.mvn
#' @param std Logical.
#'   Standardize the indirect effect \eqn{\hat{\alpha} \hat{\beta} \frac{\sigma_x}{\sigma_y}}.
#' @param complete Logical.
#'   If `TRUE`, create complete data set before bootstrapping.
#'   If 'FALSE`' bootstrap incomplete data set and fit the model with FIML.
#' @examples
#' data <- jeksterslabRdatarepo::thirst
#'
#' # Unstandardized -------------------------------------------------------------
#' thetahatstar <- nb(data = data, B = 5000, par = FALSE)
#' hist(thetahatstar)
#'
#' # Standardized ---------------------------------------------------------------
#' thetahatstar <- nb(data = data, std = TRUE, B = 5000, par = FALSE)
#' hist(thetahatstar)
#' @export
nb <- function(data,
               std = FALSE,
               complete = TRUE,
               B = 5000,
               par = TRUE,
               ncores = NULL,
               blas_threads = TRUE,
               mc = TRUE,
               lb = FALSE) {
  if (complete) {
    data <- data[complete.cases(data), ]
    foo <- function(iter,
                    data,
                    std) {
      len <- 1:nrow(data)
      i <- sample(
        x = len,
        replace = TRUE
      )
      data <- data[i, ]
      fit.ols(
        data = data,
        minimal = TRUE,
        std = std
      )
    }
  } else {
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
