#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Jackknife Estimates of Indirect Effect in a Simple Mediation Model for Data with Exponential X
#'
#' @family jackknife functions
#' @keywords jack
#' @inheritParams fit.ols
#' @inheritParams jack
#' @inheritParams useparamsexp
#' @examples
#' taskid <- 1
#' data <- exp_dat(taskid = taskid)
#' exp_jack(data = data, taskid = taskid)
#' @export
exp_jack <- function(data,
                     taskid) {
  paramsexp <- useparamsexp(taskid = taskid)
  out <- jack(
    data = data,
    std = FALSE, # always FALSE for unstandardized coefficients
    complete = TRUE, # always TRUE for complete data set
    par = FALSE, # should always be FALSE since this is wrapped around a parallel par_lapply
    blas_threads = FALSE # should always be FALSE since this is wrapped around a parallel par_lapply
  )
  attributes(out)$taskid <- paramsexp$taskid
  attributes(out)$theta <- paramsexp$alphabeta
  attributes(out)$thetahat <- fit.ols(
    data = data,
    minimal = TRUE,
    std = FALSE # always FALSE for unstandardized coefficients
  )
  out
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Jackknife Estimates of Indirect Effect in a Simple Mediation Model for Data with Exponential X (Single Task)
#'
#' @family jackknife functions
#' @keywords jack
#' @inheritParams exp_jack
#' @inheritParams exp_fit.ols_task
#' @export
exp_jack_task <- function(taskid,
                          dir = getwd(),
                          overwrite = FALSE) {
  # for socks to load package in the namespace
  requireNamespace(
    "jeksterslabRmedsimple",
    quietly = TRUE
  )
  wd <- getwd()
  setwd(dir)
  fndata <- paste0(
    "medsimple_exp_dat_",
    sprintf(
      "%05.0f",
      taskid
    ),
    ".Rds"
  )
  fn <- paste0(
    "medsimple_exp_jack_",
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
  # Resolve overwrite ---------------------------------------------------------------------------
  if (overwrite) {
    run <- TRUE
  } else {
    # Check if result exists -------------------------------------------------------------
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
      par_lapply(
        X = X,
        FUN = exp_jack,
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
#' @title Jackknife Estimates of Indirect Effect in a Simple Mediation Model for Data with Exponential X (Simulation)
#'
#' @family jackknife functions
#' @keywords jack
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams exp_jack_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams exp_dat_simulation
#' @export
exp_jack_simulation <- function(dir = getwd(),
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
    ncase <- nrow(jeksterslabRmedsimple::paramsexp)
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
      FUN = exp_jack_task,
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
