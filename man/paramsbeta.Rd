% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/paramsbeta.R
\docType{data}
\name{paramsbeta}
\alias{paramsbeta}
\title{Monte Carlo Simulation Parameters (Beta X)}
\format{
A data frame with 12 variables
\describe{
\item{taskid}{
Simulation task identification number.
}
\item{shape1}{
Shape 1. Beta distribution parameter \eqn{\alpha}.
}
\item{shape2}{
Shape 2. Beta distribution parameter \eqn{\beta}.
}
\item{n}{
Sample size.
}
\item{mux}{
Population mean of \code{x} \eqn{\left( \mu_x \right)}.
}
\item{mum}{
Population mean of \code{m} \eqn{\left( \mu_m \right)}.
}
\item{muy}{
Population mean of \code{y} \eqn{\left( \mu_y \right)}.
}
\item{taudot}{
Population slope of path from \code{x} to \code{y} \eqn{\left( \dot{\tau} \right)}
}
\item{beta}{
Population slope of path from \code{m} to \code{y} \eqn{\left( \beta \right)}
}
\item{alpha}{
Population slope of path from \code{x} to \code{m} \eqn{\left( \alpha \right)}
}
\item{sigma2x}{
Population variance of \code{x} \eqn{\left( \sigma_{x}^{2} \right)}
}
\item{sigma2m}{
Population variance of \code{m} \eqn{\left( \sigma_{m}^{2} \right)}
}
\item{sigma2y}{
Population variance of \code{y} \eqn{\left( \sigma_{y}^{2} \right)}
}
\item{reps}{
Monte Carlo replications.
}
}
}
\usage{
paramsbeta
}
\description{
Monte Carlo Simulation Parameters (Beta X)
}
\details{
The simple mediation model is given by
\deqn{
    y_i = \delta_y + \dot{\tau} x_i + \beta m_i + \varepsilon_{y_{i}}
  }

\deqn{
    m_i = \delta_m + \alpha x_i + \varepsilon_{m_{i}}
  }

The parameters for the mean structure are
\deqn{
    \boldsymbol{\theta}_{\text{mean structure}} = \left\{ \mu_x, \delta_m, \delta_y \right\} .
  }

The parameters for the covariance structure are
\deqn{
    \boldsymbol{\theta}_{\text{covariance structure}} = \left\{ \dot{\tau}, \beta, \alpha, \sigma_{x}^{2},
    \sigma_{\varepsilon_{m}}^{2}, \sigma_{\varepsilon_{y}}^{2} \right\} .
  }
}
\examples{
data(paramsbeta, package = "jeksterslabRmedsimple")
head(paramsbeta)
str(paramsbeta)
}
\seealso{
Other parameters functions: 
\code{\link{paramsexp}},
\code{\link{paramsmvn}}
}
\concept{parameters functions}
\keyword{parameters}
