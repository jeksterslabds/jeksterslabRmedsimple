#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Nonparametric Bootstrapping (Listwise Deletion)
#'
#' @family nonparametric functions
#' @keywords nb
#' @inheritParams fit.ols
#' @inheritParams pb.mvn
#' @param std Logical.
#'   Standardize the indirect effect \eqn{\hat{\alpha} \hat{\beta} \frac{\sigma_x}{\sigma_y}}.
#' @examples
#' data <- jeksterslabRdatarepo::thirst
#'
#' # Unstandardized -------------------------------------------------------------
#' thetahatstar <- nb.del(data = data, B = 5000, par = FALSE)
#' hist(thetahatstar)
#'
#' # Standardized ---------------------------------------------------------------
#' thetahatstar <- nb.del(data = data, std = TRUE, B = 5000, par = FALSE)
#' hist(thetahatstar)
#' @export
nb.del <- function(data,
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
    # fit.ols performs listwise deletion
    fit.ols(
      data = data,
      minimal = TRUE,
      std = std
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
