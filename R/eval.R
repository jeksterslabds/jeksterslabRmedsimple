#' Confidence Interval - Theta Hit
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @family confidence intervals functions
#' @keywords ci
#' @param lo Numeric.
#'   Lower limit of the estimated confidence interval
#'   \eqn{\left( \hat{\theta}_{\mathrm{lo}} \right)}.
#' @param theta Numeric.
#'   Population parameter
#'   \eqn{\left( \theta \right)}.
#' @param up Numeric.
#'   Upper limit of the estimated confidence interval
#'   \eqn{\left( \hat{\theta}_{\mathrm{up}} \right)}.
#' @return Returns
#'   `TRUE` if `theta` \eqn{\left( \theta \right)} is between the interval
#'   `lo`
#'   \eqn{\left( \hat{\theta}_{\mathrm{lo}} \right)}
#'   to
#'   `up`
#'   \eqn{\left( \hat{\theta}_{\mathrm{up}} \right)}.
#'   Returns
#'   `FALSE` if `theta` \eqn{\left( \theta \right)} is outside the interval
#'   `lo`
#'   \eqn{\left( \hat{\theta}_{\mathrm{lo}} \right)}
#'   to
#'   `up`
#'   \eqn{\left( \hat{\theta}_{\mathrm{up}} \right)}.
#' @examples
#' # FALSE
#' theta_hit(lo = 1, theta = 0, up = 2)
#' # TRUE
#' theta_hit(lo = -1, theta = 0, up = 1)
#' @export
theta_hit <- function(lo,
                      theta,
                      up) {
  lo < theta & theta < up
}

#' Confidence Interval - Zero Hit
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @family confidence intervals functions
#' @keywords ci
#' @inheritParams theta_hit
#' @return Returns
#'   `TRUE` if zero is between the interval
#'   `lo`
#'   \eqn{\left( \hat{\theta}_{\mathrm{lo}} \right)}
#'   to
#'   `up`
#'   \eqn{\left( \hat{\theta}_{\mathrm{up}} \right)}.
#'   Returns
#'   `FALSE` if zero is outside the interval
#'   `lo`
#'   \eqn{\left( \hat{\theta}_{\mathrm{lo}} \right)}
#'   to
#'   `up`
#'   \eqn{\left( \hat{\theta}_{\mathrm{up}} \right)}.
#' @examples
#' # FALSE
#' zero_hit(lo = 1, up = 2)
#' # TRUE
#' zero_hit(lo = -1, up = 1)
#' @export
zero_hit <- function(lo,
                     up) {
  theta_hit(
    lo,
    theta = 0,
    up
  )
}

#' Confidence Interval - Length
#'
#' Calculates confidence interval length.
#'
#' The confidence interval length is given by
#' \deqn{
#'   \mathrm{
#'     confidence \ interval \ length
#'   }
#'   =
#'   \hat{
#'     \theta
#'   }_{
#'     \mathrm{
#'       up
#'     }
#'   }
#'   -
#'   \hat{
#'     \theta
#'   }_{
#'     \mathrm{
#'       lo
#'     }
#'   }
#' }
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @family confidence intervals functions
#' @keywords ci
#' @inheritParams theta_hit
#' @export
len <- function(lo,
                up) {
  up - lo
}

#' Confidence Interval - Shape
#'
#' Calculates confidence interval shape.
#'
#' The confidence interval shape is given by
#' \deqn{
#'   \mathrm{
#'     confidence \ interval \ shape
#'   }
#'   =
#'   \frac{
#'     \hat{
#'       \theta
#'     }_{
#'       \mathrm{
#'         up
#'       }
#'     }
#'     -
#'     \hat{
#'       \theta
#'     }
#'   }
#'   {
#'     \hat{
#'       \theta
#'     }
#'     -
#'     \hat{
#'       \theta
#'     }_{
#'       \mathrm{
#'         lo
#'       }
#'     }
#'   }
#' }
#'
#' The shape measures the asymmetry of the confidence interval
#' around the point estimate
#' \eqn{
#'   \hat{
#'     \theta
#'   }
#' }.
#' Shape
#' \eqn{
#'   >
#'   1.00
#' }
#' is indicative of greater distance between
#' \eqn{
#'   \hat{
#'     \theta
#'   }_{
#'     \mathrm{
#'       up
#'     }
#'   }
#' }
#' and
#' \eqn{
#'   \hat{
#'     \theta
#'   }
#' }
#' than
#' \eqn{
#'   \hat{
#'     \theta
#'   }
#' }
#' and
#' \eqn{
#'   \hat{
#'     \theta
#'   }_{
#'     \mathrm{
#'       lo
#'     }
#'   }
#' } .
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @family confidence intervals functions
#' @keywords ci
#' @inheritParams theta_hit
#' @param thetahat Numeric.
#' Parameter estimate
#' \eqn{
#'   \left(
#'     \hat{
#'       \theta
#'     }
#'   \right)
#' } .
#' @export
shape <- function(lo,
                  thetahat,
                  up) {
  (up - thetahat) / (thetahat - lo)
}
