#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Set Parameters (Beta)
#'
#' @family beta X data functions
#' @keywords beta
#' @inherit A details
#' @param taskid Numeric.
#'   Task ID.
#' @examples
#' setparamsbeta(
#'   taskid = 1
#' )
#' @export
setparamsbeta <- function(taskid) {
  paramsbeta <- paramsbeta[which(paramsbeta$taskid == taskid), ]
  shape1 <- paramsbeta$shape1
  shape2 <- paramsbeta$shape2
  n <- paramsbeta$n
  reps <- paramsbeta$reps
  mux <- paramsbeta$mux
  mum <- paramsbeta$mum
  muy <- paramsbeta$muy
  taudot <- paramsbeta$taudot
  beta <- paramsbeta$beta
  alpha <- paramsbeta$alpha
  sigma2x <- paramsbeta$sigma2x
  sigma2m <- paramsbeta$sigma2m
  sigma2y <- paramsbeta$sigma2y
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
    shape1,
    shape2
  )
  names(out) <- c(
    "M[x]", "M[m]", "M[y]",
    "A[x, x]", "A[m, x]", "A[y, x]", "A[x, m]", "A[m, m]", "A[y, m]", "A[x, y]", "A[m, y]", "A[y, y]",
    "S[x, x]", "S[m, x]", "S[y, x]", "S[x, m]", "S[m, m]", "S[y, m]", "S[x, y]", "S[m, y]", "S[y, y]",
    "E[x]", "E[m]", "E[y]",
    "Cov[x, x]", "Cov[m, x]", "Cov[y, x]", "Cov[x, m]", "Cov[m, m]", "Cov[y, m]", "Cov[x, y]", "Cov[m, y]", "Cov[y, y]",
    "taskid", "n", "reps", "shape1", "shape2"
  )
  out
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Use Parameters (Beta)
#'
#' @family beta X data functions
#' @keywords beta
#' @inherit A details
#' @inheritParams setparamsbeta
#' @examples
#' useparamsbeta(taskid = 1)
#' @export
useparamsbeta <- function(taskid) {
  paramsbeta <- setparamsbeta(taskid = taskid)
  M <- unname(paramsbeta[1:3])
  A <- unname(paramsbeta[4:12])
  S <- unname(paramsbeta[13:21])
  mutheta <- unname(paramsbeta[22:24])
  Sigmatheta <- unname(paramsbeta[25:33])
  taskid <- unname(paramsbeta[34])
  n <- unname(paramsbeta[35])
  reps <- unname(paramsbeta[36])
  shape1 <- unname(paramsbeta[37])
  shape2 <- unname(paramsbeta[38])
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
    shape1 = shape1,
    shape2 = shape2
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Exponential
#'
#' @family beta X data functions
#' @keywords beta
#' @importFrom stats rnorm
#' @importFrom stats rbeta
#' @inheritParams useparamsbeta
#' @examples
#' data <- genbeta(taskid = 1)
#' fit.ols(data)
#' @export
genbeta <- function(taskid) {
  paramsbeta <- useparamsbeta(taskid = taskid)
  n <- paramsbeta$n
  shape1 <- paramsbeta$shape1
  shape2 <- paramsbeta$shape2
  taudot <- paramsbeta$taudot
  beta <- paramsbeta$beta
  alpha <- paramsbeta$alpha
  sigma2epsilonm <- paramsbeta$sigma2epsilonm
  sigma2epsilony <- paramsbeta$sigma2epsilony
  deltam <- paramsbeta$deltam
  deltay <- paramsbeta$deltay
  epsilonm <- rnorm(n = n, sd = sqrt(sigma2epsilonm))
  epsilony <- rnorm(n = n, sd = sqrt(sigma2epsilony))
  x <- rbeta(n = n, shape1 = shape1, shape2 = shape2)
  m <- deltam + alpha * x + epsilonm
  y <- deltay + taudot * x + beta * m + epsilony
  cbind(
    x = x,
    m = m,
    y = y
  )
}
