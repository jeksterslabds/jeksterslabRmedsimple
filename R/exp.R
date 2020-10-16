#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Set Parameters (Exponential)
#'
#' @family exponential X data functions
#' @keywords exp
#' @inherit A details
#' @param taskid Numeric.
#'   Task ID.
#' @examples
#' setparamsexp(
#'   taskid = 1
#' )
#' @export
setparamsexp <- function(taskid) {
  paramsexp <- jeksterslabRmedsimple::paramsexp
  paramsexp <- paramsexp[which(paramsexp$taskid == taskid), ]
  rate <- paramsexp$rate
  n <- paramsexp$n
  reps <- paramsexp$reps
  mux <- paramsexp$mux
  mum <- paramsexp$mum
  muy <- paramsexp$muy
  taudot <- paramsexp$taudot
  beta <- paramsexp$beta
  alpha <- paramsexp$alpha
  sigma2x <- paramsexp$sigma2x
  sigma2m <- paramsexp$sigma2m
  sigma2y <- paramsexp$sigma2y
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
    reps,
    rate
  )
  names(out) <- c(
    "M[x]", "M[m]", "M[y]",
    "A[x, x]", "A[m, x]", "A[y, x]", "A[x, m]", "A[m, m]", "A[y, m]", "A[x, y]", "A[m, y]", "A[y, y]",
    "S[x, x]", "S[m, x]", "S[y, x]", "S[x, m]", "S[m, m]", "S[y, m]", "S[x, y]", "S[m, y]", "S[y, y]",
    "E[x]", "E[m]", "E[y]",
    "Cov[x, x]", "Cov[m, x]", "Cov[y, x]", "Cov[x, m]", "Cov[m, m]", "Cov[y, m]", "Cov[x, y]", "Cov[m, y]", "Cov[y, y]",
    "taskid", "n", "reps", "rate"
  )
  out
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Use Parameters (Exponential)
#'
#' @family exponential X data functions
#' @keywords exp
#' @inherit A details
#' @inheritParams setparamsexp
#' @examples
#' useparamsexp(taskid = 1)
#' @export
useparamsexp <- function(taskid) {
  paramsexp <- setparamsexp(taskid = taskid)
  M <- unname(paramsexp[1:3])
  A <- unname(paramsexp[4:12])
  S <- unname(paramsexp[13:21])
  mutheta <- unname(paramsexp[22:24])
  Sigmatheta <- unname(paramsexp[25:33])
  taskid <- unname(paramsexp[34])
  n <- unname(paramsexp[35])
  reps <- unname(paramsexp[36])
  rate <- unname(paramsexp[37])
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
    Sigmatheta = Sigmatheta,
    rate = rate
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Exponential
#'
#' @family exponential X data functions
#' @keywords exp
#' @importFrom stats rnorm
#' @importFrom stats rexp
#' @inheritParams useparamsexp
#' @examples
#' data <- genexp(taskid = 1)
#' fit.ols(data)
#' @export
genexp <- function(taskid) {
  paramsexp <- useparamsexp(taskid = taskid)
  n <- paramsexp$n
  rate <- paramsexp$rate
  taudot <- paramsexp$taudot
  beta <- paramsexp$beta
  alpha <- paramsexp$alpha
  sigma2epsilonm <- paramsexp$sigma2epsilonm
  sigma2epsilony <- paramsexp$sigma2epsilony
  deltam <- paramsexp$deltam
  deltay <- paramsexp$deltay
  epsilonm <- rnorm(n = n, sd = sqrt(sigma2epsilonm))
  epsilony <- rnorm(n = n, sd = sqrt(sigma2epsilony))
  x <- rexp(n = n, rate = rate)
  m <- deltam + alpha * x + epsilonm
  y <- deltay + taudot * x + beta * m + epsilony
  cbind(
    x = x,
    m = m,
    y = y
  )
}
