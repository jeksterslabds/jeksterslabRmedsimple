#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Generate Nonnormal Data Using the Vale and Maurelli (1983) Approach
#'   following a Simple Mediation Model (Skewness = 2, Kurtosis = 7)
#'
#' @family multivariate normal data functions
#' @keywords vm_dat
#' @inheritParams useparamsmvn
#' @examples
#' data <- vm_mod_dat(taskid = 1)
#' colMeans(data)
#' cov(data)
#' apply(X = data, MARGIN = 2, FUN = jeksterslabRdist::skew)
#' apply(X = data, MARGIN = 2, FUN = jeksterslabRdist::kurt)
#' fit.ols(data, minimal = TRUE)
#' fit.ols(data, minimal = TRUE, std = TRUE)
#' fit.ols(data, minimal = FALSE)
#' @export
vm_mod_dat <- function(taskid) {
  paramsmvn <- useparamsmvn(taskid = taskid)
  vm(
    n = paramsmvn$n,
    mu = paramsmvn$mutheta,
    Sigma = paramsmvn$Sigmatheta,
    skewness = c(2.0, 2.0, 2.0),
    kurtosis = c(7.0, 7.0, 7.0)
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Generate Nonnormal Data Using the Vale and Maurelli (1983) Approach
#'   following a Simple Mediation Model (Skewness = 2, Kurtosis = 7)
#'   (Single Task)
#'
#' @family multivariate normal data functions
#' @keywords vm_dat
#' @inheritParams vm_mod_dat
#' @param dir Character string.
#'   Directory where results of `vm_mod_dat_*` are stored.
#' @param overwrite Logical.
#'   Overwrite existing results.
#' @export
vm_mod_dat_task <- function(taskid,
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
    "medsimple_vm_mod_dat_",
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
      vm_mod_dat(taskid)
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
#' @title Generate Nonnormal Data Using the Vale and Maurelli (1983) Approach
#'   following a Simple Mediation Model (Skewness = 2, Kurtosis = 7)
#'   (Simulation)
#'
#' @family multivariate normal data functions
#' @keywords vm_dat
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams vm_sev_dat_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @param all Logical.
#'   Process all results.
#' @param taskid Vector of integers.
#'   `taskid` to use when `all = FALSE`.
#' @export
vm_mod_dat_simulation <- function(dir = getwd(),
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
      FUN = vm_mod_dat_task,
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

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Generate Nonnormal Data Using the Vale and Maurelli (1983) Approach
#'   following a Simple Mediation Model (Skewness = 2, Kurtosis = 7)
#'   (Summary - Task)
#'
#' @family multivariate normal data functions
#' @keywords vm_dat
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams vm_mod_dat_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @export
vm_mod_dat_simulation_task_summary <- function(taskid,
                                               dir = getwd()) {
  # for socks to load package in the namespace
  requireNamespace(
    "jeksterslabRmedsimple",
    quietly = TRUE
  )
  wd <- getwd()
  setwd(dir)
  foo <- function(X, taskid) {
    params <- useparamsmvn(taskid)
    out <- c(
      unname(colMeans(X)),
      unname(as.vector(cov(X))),
      unname(skew(X[, 1])),
      unname(skew(X[, 2])),
      unname(skew(X[, 3])),
      unname(kurt(X[, 1])),
      unname(kurt(X[, 2])),
      unname(kurt(X[, 3]))
    )
    names(out) <- c(
      "muxhat",
      "mumhat",
      "muyhat",
      "sigma2xhat",
      "sigmaxmhat",
      "sigmaxyhat",
      "sigmamxhat",
      "sigma2mhat",
      "sigmamyhat",
      "sigmayxhat",
      "sigmaymhat",
      "sigma2yhat",
      "skewxhat",
      "skewmhat",
      "skewyhat",
      "kurtxhat",
      "kurtmhat",
      "kurtyhat"
    )
    c(
      taskid = params$taskid,
      n = params$n,
      reps = params$reps,
      taudot = params$taudot,
      beta = params$beta,
      alpha = params$alpha,
      alphabeta = params$alphabeta,
      sigma2x = params$sigma2x,
      sigma2epsilonm = params$sigma2epsilonm,
      sigma2epsilony = params$sigma2epsilony,
      mux = params$mux,
      deltam = params$deltam,
      deltay = params$deltay,
      out
    )
  }
  fndata <- paste0(
    "medsimple_vm_mod_dat_",
    sprintf(
      "%05.0f",
      taskid
    ),
    ".Rds"
  )
  # Check if data exists --------------------------------------------------------
  if (file.exists(fndata)) {
    X <- readRDS(fndata)
  } else {
    stop(
      paste(
        fndata,
        "does not exist in",
        dir
      )
    )
  }
  out <- invisible(
    par_lapply(
      X = X,
      FUN = foo,
      taskid = taskid,
      par = FALSE, # should always be FALSE since this is wrapped around a parallel par_lapply
      blas_threads = FALSE, # should always be FALSE since this is wrapped around a parallel par_lapply
      rbind = TRUE
    )
  )
  colMeans(out)
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Generate Nonnormal Data Using the Vale and Maurelli (1983) Approach
#'   following a Simple Mediation Model (Skewness = 2, Kurtosis = 7)
#'   (Summary - Simulation)
#'
#' @family multivariate normal data functions
#' @keywords vm_dat
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams vm_mod_dat_simulation
#' @inheritParams jeksterslabRpar::par_lapply
#' @export
vm_mod_dat_simulation_simulation_summary <- function(dir = getwd(),
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
      FUN = vm_mod_dat_simulation_task_summary,
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
      rbind = TRUE
    )
  )
  out <- label(
    out = out,
    method = "vm.mod",
    model = "Simple mediation model",
    std = FALSE
  )
  fn <- "summary_medsimple_vm_mod_dat.Rds"
  saveRDS(
    object = out,
    file = fn
  )
}
