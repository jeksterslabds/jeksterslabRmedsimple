#' Monte Carlo Simulation Parameters (Exponential X)
#'
#' @family parameters functions
#' @keywords parameters
#' @inherit A details
#' @format A data frame with 12 variables
#' \describe{
#'   \item{taskid}{
#'     Simulation task identification number.
#'   }
#'   \item{rate}{
#'     Rate. Exponential distribution parameter \eqn{\lambda}.
#'   }
#'   \item{n}{
#'     Sample size.
#'   }
#'   \item{mux}{
#'     Population mean of `x` \eqn{\left( \mu_x \right)}.
#'   }
#'   \item{mum}{
#'     Population mean of `m` \eqn{\left( \mu_m \right)}.
#'   }
#'   \item{muy}{
#'     Population mean of `y` \eqn{\left( \mu_y \right)}.
#'   }
#'   \item{taudot}{
#'     Population slope of path from `x` to `y` \eqn{\left( \dot{\tau} \right)}
#'   }
#'   \item{beta}{
#'     Population slope of path from `m` to `y` \eqn{\left( \beta \right)}
#'   }
#'   \item{alpha}{
#'     Population slope of path from `x` to `m` \eqn{\left( \alpha \right)}
#'   }
#'   \item{sigma2x}{
#'     Population variance of `x` \eqn{\left( \sigma_{x}^{2} \right)}
#'   }
#'   \item{sigma2m}{
#'     Population variance of `m` \eqn{\left( \sigma_{m}^{2} \right)}
#'   }
#'   \item{sigma2y}{
#'     Population variance of `y` \eqn{\left( \sigma_{y}^{2} \right)}
#'   }
#'   \item{reps}{
#'     Monte Carlo replications.
#'   }
#' }
#' @examples
#' data(paramsexp, package = "jeksterslabRmedsimple")
#' head(paramsexp)
#' str(paramsexp)
"paramsexp"
