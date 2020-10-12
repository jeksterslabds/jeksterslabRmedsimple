#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Set Parameters for Data Generated from a Multivariate Normal Distribution
#'
#' @family multivariate normal data functions
#' @keywords mvn
#' @inherit A details
#' @param taskid Numeric.
#'   Task ID.
#' @examples
#' setparamsmvn(
#'   taskid = 1
#' )
#' @export
setparamsmvn <- function(taskid) {
  paramsmvn <- jeksterslabRmedsimple::paramsmvn
  paramsmvn <- paramsmvn[which(paramsmvn$taskid == taskid), ]
  n <- paramsmvn$n
  reps <- paramsmvn$reps
  mux <- paramsmvn$mux
  mum <- paramsmvn$mum
  muy <- paramsmvn$muy
  taudot <- paramsmvn$taudot
  beta <- paramsmvn$beta
  alpha <- paramsmvn$alpha
  sigma2x <- paramsmvn$sigma2x
  sigma2m <- paramsmvn$sigma2m
  sigma2y <- paramsmvn$sigma2y
  M <- Mfrommu(
    mux = mux,
    mum = mum,
    muy = muy,
    taudot = taudot,
    beta = beta,
    alpha = alpha
  )
  A <- A(
    taudot = taudot,
    beta = beta,
    alpha = alpha
  )
  S <- Sfromsigma2(
    taudot = taudot,
    beta = beta,
    alpha = alpha,
    sigma2x = sigma2x,
    sigma2m = sigma2m,
    sigma2y = sigma2y
  )
  mutheta <- c(
    x = mux,
    m = mum,
    y = muy
  )
  Sigmatheta <- Sigmathetafromsigma2(
    taudot = taudot,
    beta = beta,
    alpha = alpha,
    sigma2x = sigma2x,
    sigma2m = sigma2m,
    sigma2y = sigma2y
  )
  M <- as.vector(M)
  A <- as.vector(A)
  S <- as.vector(S)
  mutheta <- as.vector(mutheta)
  Sigmatheta <- as.vector(Sigmatheta)
  out <- c(
    M,
    A,
    S,
    mutheta,
    Sigmatheta,
    taskid,
    n,
    reps
  )
  names(out) <- c(
    "M[x]", "M[m]", "M[y]",
    "A[x, x]", "A[m, x]", "A[y, x]", "A[x, m]", "A[m, m]", "A[y, m]", "A[x, y]", "A[m, y]", "A[y, y]",
    "S[x, x]", "S[m, x]", "S[y, x]", "S[x, m]", "S[m, m]", "S[y, m]", "S[x, y]", "S[m, y]", "S[y, y]",
    "E[x]", "E[m]", "E[y]",
    "Cov[x, x]", "Cov[m, x]", "Cov[y, x]", "Cov[x, m]", "Cov[m, m]", "Cov[y, m]", "Cov[x, y]", "Cov[m, y]", "Cov[y, y]",
    "taskid", "n", "reps"
  )
  out
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Use Parameters for Data Generated from a Multivariate Normal Distribution
#'
#' @family multivariate normal data functions
#' @keywords mvn
#' @inherit A details
#' @inheritParams setparamsmvn
#' @examples
#' useparamsmvn(taskid = 1)
#' @export
useparamsmvn <- function(taskid) {
  paramsmvn <- setparamsmvn(taskid = taskid)
  M <- unname(paramsmvn[1:3])
  A <- unname(paramsmvn[4:12])
  S <- unname(paramsmvn[13:21])
  mutheta <- unname(paramsmvn[22:24])
  Sigmatheta <- unname(paramsmvn[25:33])
  taskid <- unname(paramsmvn[34])
  n <- unname(paramsmvn[35])
  reps <- unname(paramsmvn[36])
  A <- matrix(
    data = A,
    ncol = 3
  )
  S <- matrix(
    data = S,
    ncol = 3
  )
  Sigmatheta <- matrix(
    data = Sigmatheta,
    ncol = 3
  )
  varnames <- c("x", "m", "y")
  names(M) <- varnames
  names(mutheta) <- varnames
  colnames(A) <- varnames
  colnames(S) <- varnames
  colnames(Sigmatheta) <- varnames
  rownames(A) <- varnames
  rownames(S) <- varnames
  rownames(Sigmatheta) <- varnames
  list(
    taskid = taskid,
    n = n,
    reps = reps,
    taudot = unname(A["y", "x"]),
    beta = unname(A["y", "m"]),
    alpha = unname(A["m", "x"]),
    alphabeta = unname(A["m", "x"] * A["y", "m"]),
    sigma2x = unname(S["x", "x"]),
    sigma2epsilonm = unname(S["m", "m"]),
    sigma2epsilony = unname(S["y", "y"]),
    mux = unname(M["x"]),
    deltam = unname(M["m"]),
    deltay = unname(M["y"]),
    M = M,
    A = A,
    S = S,
    mutheta = mutheta,
    Sigmatheta = Sigmatheta
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Generate Data from a Multivariate Normal Distribution
#'
#' @family multivariate normal data functions
#' @keywords mvn
#' @importFrom MASS mvrnorm
#' @param n Integer.
#'   Samples size.
#' @param mu Numeric vector.
#'   Mean vector.
#' @param Sigma Numeric matrix.
#'   Variance-covariance matrix.
#' @examples
#' n <- 100
#' mu <- c(0, 0)
#' Sigma <- matrix(data = c(1, 0.50, 0.50, 1), ncol = 2)
#' mvn(n = n, mu = mu, Sigma = Sigma)
#' @export
mvn <- function(n,
                mu,
                Sigma) {
  mvrnorm(
    n = n,
    mu = mu,
    Sigma = Sigma
  )
}
