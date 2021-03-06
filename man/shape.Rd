% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eval.R
\name{shape}
\alias{shape}
\title{Confidence Interval - Shape}
\usage{
shape(lo, thetahat, up)
}
\arguments{
\item{lo}{Numeric.
Lower limit of the estimated confidence interval
\eqn{\left( \hat{\theta}_{\mathrm{lo}} \right)}.}

\item{thetahat}{Numeric.
Parameter estimate
\eqn{
  \left(
    \hat{
      \theta
    }
  \right)
} .}

\item{up}{Numeric.
Upper limit of the estimated confidence interval
\eqn{\left( \hat{\theta}_{\mathrm{up}} \right)}.}
}
\description{
Calculates confidence interval shape.
}
\details{
The confidence interval shape is given by
\deqn{
  \mathrm{
    confidence \ interval \ shape
  }
  =
  \frac{
    \hat{
      \theta
    }_{
      \mathrm{
        up
      }
    }
    -
    \hat{
      \theta
    }
  }
  {
    \hat{
      \theta
    }
    -
    \hat{
      \theta
    }_{
      \mathrm{
        lo
      }
    }
  }
}

The shape measures the asymmetry of the confidence interval
around the point estimate
\eqn{
  \hat{
    \theta
  }
}.
Shape
\eqn{
  >
  1.00
}
is indicative of greater distance between
\eqn{
  \hat{
    \theta
  }_{
    \mathrm{
      up
    }
  }
}
and
\eqn{
  \hat{
    \theta
  }
}
than
\eqn{
  \hat{
    \theta
  }
}
and
\eqn{
  \hat{
    \theta
  }_{
    \mathrm{
      lo
    }
  }
} .
}
\seealso{
Other confidence intervals functions: 
\code{\link{bcaci}()},
\code{\link{bcci}()},
\code{\link{evalci}()},
\code{\link{len}()},
\code{\link{pcci}()},
\code{\link{theta_hit}()},
\code{\link{zero_hit}()}
}
\author{
Ivan Jacob Agaloos Pesigan
}
\concept{confidence intervals functions}
\keyword{ci}
