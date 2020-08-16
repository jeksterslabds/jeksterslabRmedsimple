#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Jackknife Estimates of Indirect Effect for a Simple Mediation Model
#'
#' @family confidence intervals functions
#' @keywords ci
#' @inheritParams .fit
#' @examples
#' data <- jeksterslabRdatarepo::thirst
#' jackknife(data)
#' jackknife(data, std = TRUE)
#' @export
jackknife <- function(data,
                      std = FALSE) {
  out <- lapply(
    X = 1:nrow(data),
    FUN = function(x, data) data[-x, ],
    data = data
  )
  as.vector(
    sapply(
      X = out,
      FUN = .fit,
      minimal = TRUE,
      std = std
    )
  )
}
