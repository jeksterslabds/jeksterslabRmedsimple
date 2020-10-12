#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Percentile Confidence Intervals
#'
#' @family confidence intervals functions
#' @keywords ci
#' @importFrom stats sd
#' @importFrom stats quantile
#' @param thetahatstar Numeric vector.
#'   Sampling distribution of thetahat.
#' @param thetahat Numeric.
#'   Parameter estimate.
#' @param theta Numeric.
#'   Parameter.
#'   Optional argument.
#' @param alpha Numeric vector.
#'   Alpha level.
#'   By default `alpha = c(0.001, 0.01, 0.05)`.
#' @examples
#' B <- 5000
#' data <- jeksterslabRdatarepo::thirst
#' thetahat <- fit.ols(data, minimal = TRUE)
#' n <- nrow(data)
#' muthetahat <- colMeans(data)
#' Sigmathetahat <- cov(data)
#' thetahatstar <- pb.mvn(
#'   muthetahat = muthetahat, Sigmathetahat = Sigmathetahat,
#'   n = n, B = 5000, par = FALSE
#' )
#' pcci(
#'   thetahatstar = thetahatstar,
#'   thetahat = thetahat,
#'   theta = 0.15 # assuming that the true indirect effect is 0.15
#' )
#' @export
pcci <- function(thetahatstar,
                 thetahat,
                 theta = NULL,
                 alpha = c(0.001, 0.01, 0.05)) {
  thetahatstar <- as.vector(thetahatstar)
  alpha <- sort(alpha)
  prob_ll <- alpha / 2
  prob_ul <- rev(1 - prob_ll)
  probs <- c(prob_ll, prob_ul)
  ci <- quantile(
    x = thetahatstar,
    probs = probs
  )
  eval <- evalci(
    ci = ci,
    alpha = alpha,
    thetahat = thetahat,
    theta = theta
  )
  evalnames <- names(eval)
  out <- c(
    thetahat,
    sd(thetahatstar),
    length(thetahatstar),
    ci,
    eval
  )
  names(out) <- c(
    "est",
    "se",
    "reps",
    paste0(
      "ci_",
      probs * 100
    ),
    evalnames
  )
  out
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Bias-Corrected Confidence Intervals
#'
#' @family confidence intervals functions
#' @keywords ci
#' @importFrom stats sd
#' @importFrom stats quantile
#' @importFrom stats qnorm
#' @importFrom stats pnorm
#' @inheritParams pcci
#' @examples
#' B <- 5000
#' data <- jeksterslabRdatarepo::thirst
#' thetahat <- fit.ols(data, minimal = TRUE)
#' n <- nrow(data)
#' muthetahat <- colMeans(data)
#' Sigmathetahat <- cov(data)
#' thetahatstar <- pb.mvn(
#'   muthetahat = muthetahat, Sigmathetahat = Sigmathetahat,
#'   n = n, B = 5000, par = FALSE
#' )
#' bcci(
#'   thetahatstar = thetahatstar,
#'   thetahat = thetahat,
#'   theta = 0.15 # assuming that the true indirect effect is 0.15
#' )
#' @export
bcci <- function(thetahatstar,
                 thetahat,
                 theta = NULL,
                 alpha = c(0.001, 0.01, 0.05)) {
  thetahatstar <- as.vector(thetahatstar)
  alpha <- sort(alpha)
  prob_ll <- alpha / 2
  prob_ul <- rev(1 - prob_ll)
  probs <- c(prob_ll, prob_ul)
  # bias-correction
  z0hat <- qnorm(
    sum(thetahatstar < thetahat) / length(thetahatstar)
  )
  bc_probs <- pnorm(
    q = 2 * z0hat + qnorm(
      p = probs
    )
  )
  ci <- quantile(
    x = thetahatstar,
    probs = bc_probs,
    names = FALSE
  )
  eval <- evalci(
    ci = ci,
    alpha = alpha,
    thetahat = thetahat,
    theta = theta
  )
  evalnames <- names(eval)
  out <- c(
    thetahat,
    sd(thetahatstar),
    length(thetahatstar),
    ci,
    eval
  )
  names(out) <- c(
    "est",
    "se",
    "reps",
    paste0(
      "ci_",
      probs * 100
    ),
    evalnames
  )
  out
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Bias-Corrected and Accelerated Confidence Intervals
#'
#' @family confidence intervals functions
#' @keywords ci
#' @importFrom stats sd
#' @importFrom stats quantile
#' @importFrom stats qnorm
#' @importFrom stats pnorm
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams pcci
#' @inheritParams nb
#' @inheritParams fit.ols
#' @inheritParams jeksterslabRpar::par_lapply
#' @param thetahatstarjack Numeric vector.
#'   Jackknife vector of parameter estimates.
#'   If `thetahatstarjack = NULL`,
#'   `thetahatstarjack` is calculated using [`jack()`].
#' @examples
#' B <- 5000
#' data <- jeksterslabRdatarepo::thirst
#' n <- nrow(data)
#' muthetahat <- colMeans(data)
#' Sigmathetahat <- cov(data)
#' thetahat <- fit.ols(data, minimal = TRUE)
#' thetahatstar <- pb.mvn(
#'   muthetahat = muthetahat, Sigmathetahat = Sigmathetahat,
#'   n = n, B = 5000, par = FALSE
#' )
#' bcaci(
#'   thetahatstar = thetahatstar,
#'   thetahat = thetahat,
#'   theta = 0.15, # assuming that the true indirect effect is 0.15
#'   data = data,
#'   par = FALSE
#' )
#' thetahat <- fit.ols(data, minimal = TRUE, std = TRUE)
#' thetahatstar <- pb.mvn(
#'   muthetahat = muthetahat, Sigmathetahat = Sigmathetahat,
#'   n = n, std = TRUE, B = 5000, par = FALSE
#' )
#' bcaci(
#'   thetahatstar = thetahatstar,
#'   thetahat = thetahat,
#'   theta = 0.15, # assuming that the true indirect effect is 0.15
#'   data = data,
#'   std = TRUE,
#'   par = FALSE
#' )
#' @export
bcaci <- function(thetahatstar,
                  thetahatstarjack = NULL,
                  thetahat,
                  theta = NULL,
                  data,
                  std = FALSE,
                  complete = TRUE,
                  alpha = c(0.001, 0.01, 0.05),
                  par = TRUE,
                  ncores = NULL,
                  blas_threads = TRUE,
                  mc = TRUE,
                  lb = FALSE) {
  alpha <- sort(alpha)
  prob_ll <- alpha / 2
  prob_ul <- rev(1 - prob_ll)
  probs <- c(prob_ll, prob_ul)
  # bias-correction
  z0hat <- qnorm(
    sum(thetahatstar < thetahat) / length(thetahatstar)
  )
  # acceleration
  if (is.null(thetahatstarjack)) {
    thetahatstarjack <- jack(
      data = data,
      std = std,
      complete = complete,
      par = par,
      ncores = ncores,
      blas_threads = blas_threads,
      mc = mc,
      lb = lb
    )
  }
  parenthesis <- mean(thetahatstarjack) - thetahatstarjack
  numerator <- sum(parenthesis^3)
  denominator <- 6 * ((sum(parenthesis^2))^(3 / 2))
  ahat <- numerator / denominator
  z1 <- qnorm(
    p = probs
  )
  bca_probs <- pnorm(
    z0hat + (z0hat + z1) / (1 - ahat * (z0hat + z1))
  )
  ci <- quantile(
    x = thetahatstar,
    probs = bca_probs,
    names = FALSE
  )
  eval <- evalci(
    ci = ci,
    alpha = alpha,
    thetahat = thetahat,
    theta = theta
  )
  evalnames <- names(eval)
  out <- c(
    thetahat,
    sd(thetahatstar),
    length(thetahatstar),
    ci,
    eval
  )
  names(out) <- c(
    "est",
    "se",
    "reps",
    paste0(
      "ci_",
      probs * 100
    ),
    evalnames
  )
  out
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Evaluate Confidence Intervals
#'
#' @family confidence intervals functions
#' @keywords ci
#' @description Evaluates the confidence intervals using the following functions
#'   [`zero_hit()`], [`len()`], and [`shape()`].
#'
#' @param ci Vector of confidence intervals
#'   arranged from smallest to largest.
#'   Length should be twice the length of alpha.
#' @inheritParams pcci
#' @export
evalci <- function(ci,
                   alpha = c(0.001, 0.01, 0.05),
                   thetahat,
                   theta = NULL) {
  alpha <- sort(alpha)
  ci <- sort(ci)
  n <- length(ci)
  nhalf <- n / 2
  if (nhalf != length(alpha)) {
    stop(
      "Length of \`ci\` should be twice the length of \`alpha\`."
    )
  }
  lo_start <- 1
  lo_end <- nhalf
  up_start <- nhalf + 1
  up_end <- n
  lo_index <- lo_start:lo_end
  up_index <- up_start:up_end
  lo <- unname(ci[lo_index])
  up <- unname(rev(ci[up_index]))
  zero_hit_results <- rep(x = NA, times = length(lo))
  len_results <- rep(x = NA, times = length(lo))
  shape_results <- rep(x = NA, times = length(lo))
  if (!is.null(theta)) {
    theta_hit_results <- rep(x = NA, times = length(lo))
    theta_miss_results <- rep(x = NA, times = length(lo))
  }
  for (i in seq_along(lo)) {
    if (!is.null(theta)) {
      theta_hit_results[i] <- theta_hit(
        lo = lo[i],
        theta = theta,
        up = up[i]
      )
      theta_miss_results[i] <- !theta_hit_results[i]
    }
    zero_hit_results[i] <- zero_hit(
      lo = lo[i],
      up = up[i]
    )
    len_results[i] <- len(
      lo = lo[i],
      up = up[i]
    )
    shape_results[i] <- shape(
      lo = lo[i],
      thetahat = thetahat,
      up = up[i]
    )
  }
  label <- 100 * (1 - alpha)
  names(zero_hit_results) <- paste0("zero_hit_", label)
  names(len_results) <- paste0("len_", label)
  names(shape_results) <- paste0("shape_", label)
  out <- c(
    zero_hit_results,
    len_results,
    shape_results
  )
  if (!is.null(theta)) {
    names(theta_hit_results) <- paste0("theta_hit_", label)
    names(theta_miss_results) <- paste0("theta_miss_", label)
    out <- c(
      out,
      theta_hit_results,
      theta_miss_results,
      theta = theta
    )
  }
  out
}
