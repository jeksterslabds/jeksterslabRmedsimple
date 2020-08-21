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
#' @importFrom mvnfast rmvn
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
  rmvn(
    n = n,
    mu = mu,
    sigma = Sigma
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Generate Data from a Multivariate Normal Distribution - Simple Mediation Model
#'
#' @family multivariate normal data functions
#' @keywords mvn
#' @inheritParams useparamsmvn
#' @examples
#' data <- mvn_dat(taskid = 1)
#' .fit(data, minimal = TRUE)
#' .fit(data, minimal = TRUE, std = TRUE)
#' .fit(data, minimal = FALSE)
#' @export
mvn_dat <- function(taskid) {
  paramsmvn <- useparamsmvn(taskid = taskid)
  mvn(
    n = paramsmvn$n,
    mu = paramsmvn$mutheta,
    Sigma = paramsmvn$Sigmatheta
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Generate Data from a Multivariate Normal Distribution - Simple Mediation Model
#'   (Single Task)
#'
#' @family multivariate normal data functions
#' @keywords mvn
#' @inheritParams mvn_dat
#' @param dir Character string.
#'   Directory where results of `mvn_dat_*` are stored.
#' @param overwrite Logical.
#'   Overwrite existing results.
#' @export
mvn_dat_task <- function(taskid,
                         dir = getwd(),
                         overwrite = FALSE) {
  # for socks to load package in the namespace
  requireNamespace(
    "jeksterslabRmedsimple",
    quietly = TRUE
  )
  wd <- getwd()
  setwd(dir)
  fn <- paste0(
    "medsimple_mvn_dat_",
    sprintf(
      "%05.0f",
      taskid
    ),
    ".Rds"
  )
  # Resolve overwrite -----------------------------------------------------------
  if (overwrite) {
    run <- TRUE
  } else {
    # Check if result exists ----------------------------------------------------
    if (file.exists(fn)) {
      run <- FALSE
      tryCatch(
        {
          existing_results <- readRDS(fn)
        },
        error = function(e) {
          run <- TRUE
        }
      )
    } else {
      run <- TRUE
    }
  }
  # Execute ---------------------------------------------------------------------
  if (run) {
    foo <- function(iter,
                    taskid) {
      mvn_dat(taskid)
    }
    paramsmvn <- useparamsmvn(taskid = taskid)
    R <- paramsmvn$reps
    X <- 1:R
    out <- invisible(
      par_lapply(
        X = X,
        FUN = foo,
        taskid = taskid,
        par = FALSE, # should always be FALSE since this is wrapped around a parallel par_lapply
        blas_threads = FALSE # should always be FALSE since this is wrapped around a parallel par_lapply
      )
    )
    saveRDS(
      object = out,
      file = fn
    )
  }
  invisible(
    setwd(wd)
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Generate Data from a Multivariate Normal Distribution - Simple Mediation Model
#'   (Simulation)
#'
#' @family multivariate normal data functions
#' @keywords mvn
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams mvn_dat_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @param all Logical.
#'   Process all results.
#' @param taskid Vector of integers.
#'   `taskid` to use when `all = FALSE`.
#' @export
mvn_dat_simulation <- function(dir = getwd(),
                               all = TRUE,
                               taskid = NULL,
                               par = TRUE,
                               ncores = NULL,
                               blas_threads = TRUE,
                               mc = TRUE,
                               lb = FALSE,
                               cl_eval = FALSE,
                               cl_export = FALSE,
                               cl_expr,
                               cl_vars) {
  if (all) {
    ncase <- nrow(jeksterslabRmedsimple::paramsmvn)
    taskid <- 1:ncase
  } else {
    if (is.null(taskid)) {
      stop(
        "If \`all = FALSE\` \`taskid\` should be provided."
      )
    }
  }
  out <- invisible(
    par_lapply(
      X = taskid,
      FUN = mvn_dat_task,
      dir = dir,
      par = par,
      ncores = ncores,
      blas_threads = blas_threads,
      mc = mc,
      lb = lb,
      cl_eval = cl_eval,
      cl_export = cl_eval,
      cl_expr = cl_expr,
      cl_vars = cl_vars,
      rbind = NULL
    )
  )
}
