% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ci.R
\name{evalci}
\alias{evalci}
\title{Evaluate Confidence Intervals}
\usage{
evalci(ci, alpha = c(0.001, 0.01, 0.05), thetahat, theta = NULL)
}
\arguments{
\item{ci}{Vector of confidence intervals
arranged from smallest to largest.
Length should be twice the length of alpha.}

\item{alpha}{Numeric vector.
Alpha level.
By default \code{alpha = c(0.001, 0.01, 0.05)}.}

\item{thetahat}{Numeric.
Parameter estimate.}

\item{theta}{Numeric.
Parameter.
Optional argument.}
}
\description{
Evaluates the confidence intervals using the following functions
\code{\link[=zero_hit]{zero_hit()}}, \code{\link[=len]{len()}}, and \code{\link[=shape]{shape()}}.
}
\seealso{
Other confidence intervals functions: 
\code{\link{bcaci}()},
\code{\link{bcci}()},
\code{\link{len}()},
\code{\link{pcci}()},
\code{\link{shape}()},
\code{\link{theta_hit}()},
\code{\link{zero_hit}()}
}
\author{
Ivan Jacob Agaloos Pesigan
}
\concept{confidence intervals functions}
\keyword{ci}
