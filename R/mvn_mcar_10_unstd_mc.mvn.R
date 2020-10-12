#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Monte Carlo Method Assuming Multivariate Normal Distribution for Indirect Effect in a Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution with Data Missing Completely at Random - 10%
#'
#' @family monte carlo method functions
#' @keywords mc
#' @inheritParams mc.mvn
#' @inheritParams useparamsmvn
#' @examples
#' taskid <- 1
#' data <- mvn_dat(taskid = taskid)
#' data <- mvn_mcar_10_dat(data = data, taskid = taskid)
#' fit.sem(data = data, minimal = TRUE, std = FALSE, fiml = TRUE)
#'
#' fit <- mvn_mcar_10_fit.sem(data = data, taskid = taskid)
#' thetahatstar <- mvn_mcar_10_mc.mvn(
#'   taskid = taskid, R = 20000L,
#'   alphahat = fit["alphahat"], sehatalphahat = fit["sehatalphahat"],
#'   betahat = fit["betahat"], sehatbetahat = fit["sehatbetahat"]
#' )
#' hist(thetahatstar)
#' @export
mvn_mcar_10_mc.mvn <- function(taskid,
                               R = 20000L,
                               alphahat,
                               sehatalphahat,
                               betahat,
                               sehatbetahat) {
  paramsmvn <- useparamsmvn(taskid = taskid)
  out <- mc.mvn(
    R = R,
    alphahat = alphahat,
    sehatalphahat = sehatalphahat,
    betahat = betahat,
    sehatbetahat = sehatbetahat
  )
  attributes(out)$taskid <- paramsmvn$taskid
  attributes(out)$theta <- paramsmvn$alphabeta
  attributes(out)$thetahat <- alphahat * betahat
  out
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Monte Carlo Method Assuming Multivariate Normal Distribution for Indirect Effect in a Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution with Data Missing Completely at Random - 10%
#'   (Single Task)
#'
#' @family monte carlo method functions
#' @keywords mc
#' @inheritParams mvn_dat_task
#' @export
mvn_mcar_10_mc.mvn_task <- function(taskid,
                                    dir = getwd(),
                                    overwrite = FALSE) {
  # for socks to load package in the namespace
  requireNamespace(
    "jeksterslabRmedsimple",
    quietly = TRUE
  )
  wd <- getwd()
  setwd(dir)
  fnest <- paste0(
    "medsimple_mvn_mcar_10_fit.sem_",
    sprintf(
      "%05.0f",
      taskid
    ),
    ".Rds"
  )
  fn <- paste0(
    "medsimple_mvn_mcar_10_mc.mvn_",
    sprintf(
      "%05.0f",
      taskid
    ),
    ".Rds"
  )
  # Check if data exists --------------------------------------------------------
  if (file.exists(fnest)) {
    X <- readRDS(fnest)
  } else {
    stop(
      paste(
        fnest,
        "does not exist in",
        dir
      )
    )
  }
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
  if (run) {
    out <- invisible(
      mapply(
        FUN = mvn_mcar_10_mc.mvn,
        taskid = X[, "taskid"],
        alphahat = X[, "alphahat"],
        sehatalphahat = X[, "sehatalphahat"],
        betahat = X[, "betahat"],
        sehatbetahat = X[, "sehatbetahat"],
        SIMPLIFY = FALSE
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
#' @title Monte Carlo Method Assuming Multivariate Normal Distribution for Indirect Effect in a Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution with Data Missing Completely at Random - 10%
#'   (Simulation)
#'
#' @family monte carlo method functions
#' @keywords mc
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams mvn_mcar_10_mc.mvn_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams mvn_dat_simulation
#' @export
mvn_mcar_10_mc.mvn_simulation <- function(dir = getwd(),
                                          all = TRUE,
                                          taskid = NULL,
                                          overwrite = FALSE,
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
      FUN = mvn_mcar_10_mc.mvn_task,
      dir = dir,
      overwrite = overwrite,
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
#' @title Monte Carlo Method Confidence Intervals Assuming Multivariate Normal Distribution for Indirect Effect in a Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution with Data Missing Completely at Random - 10%
#'   (Single Task)
#'
#' @family monte carlo method functions
#' @keywords mc
#' @inheritParams mvn_dat_task
#' @export
mvn_mcar_10_mc.mvn_pcci_task <- function(taskid,
                                         dir = getwd()) {
  # for socks to load package in the namespace
  requireNamespace(
    "jeksterslabRmedsimple",
    quietly = TRUE
  )
  foo <- function(thetahatstar) {
    pcci(
      thetahatstar = thetahatstar,
      thetahat = attributes(thetahatstar)$thetahat,
      theta = attributes(thetahatstar)$theta,
      alpha = c(0.001, 0.01, 0.05)
    )
  }
  wd <- getwd()
  setwd(dir)
  fndata <- paste0(
    "medsimple_mvn_mcar_10_mc.mvn_",
    sprintf(
      "%05.0f",
      taskid
    ),
    ".Rds"
  )
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
      par = FALSE, # should always be FALSE since this is wrapped around a parallel par_lapply
      blas_threads = FALSE, # should always be FALSE since this is wrapped around a parallel par_lapply
      rbind = TRUE
    )
  )
  setwd(wd)
  process(
    taskid = taskid,
    out = out
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Monte Carlo Method Confidence Intervals Assuming Multivariate Normal Distribution for Indirect Effect in a Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution with Data Missing Completely at Random - 10%
#'   (Simulation)
#'
#' @family monte carlo method functions
#' @keywords mc
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams mvn_mcar_10_mc.mvn_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams mvn_dat_simulation
#' @export
mvn_mcar_10_mc.mvn_pcci_simulation <- function(dir = getwd(),
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
      FUN = mvn_mcar_10_mc.mvn_pcci_task,
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
    method = "MC.MCAR.10",
    model = "Simple mediation model",
    std = FALSE,
    missing = "MCAR.10"
  )
  fn <- "summary_medsimple_mvn_mcar_10_mc.mvn_pcci.Rds"
  saveRDS(
    object = out,
    file = fn
  )
}
