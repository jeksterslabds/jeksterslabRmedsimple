#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Jackknife Estimates of Indirect Effect in a Simple Mediation Model for Data Generated Using the Vale and Maurelli (1983) Approach (Skewness = 3, Kurtosis = 21)
#'
#' @family jackknife functions
#' @keywords jack
#' @inheritParams fit.ols
#' @inheritParams jack
#' @inheritParams useparamsmvn
#' @examples
#' taskid <- 1
#' data <- vm_sev_dat(taskid = taskid)
#' vm_sev_std_jack(data = data, taskid = taskid)
#' @export
vm_sev_std_jack <- function(data,
                            taskid) {
  paramsmvn <- useparamsmvn(taskid = taskid)
  out <- jack(
    data = data,
    std = TRUE, # always TRUE for standardized coefficients
    complete = TRUE, # always TRUE for complete data set
    par = FALSE, # should always be FALSE since this is wrapped around a parallel par_lapply
    blas_threads = FALSE # should always be FALSE since this is wrapped around a parallel par_lapply
  )
  attributes(out)$taskid <- paramsmvn$taskid
  attributes(out)$theta <- paramsmvn$alphabeta
  attributes(out)$thetahat <- fit.ols(
    data = data,
    minimal = TRUE,
    std = TRUE # always TRUE for standardized coefficients
  )
  out
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Jackknife Estimates of Indirect Effect in a Simple Mediation Model for Data Generated Using the Vale and Maurelli (1983) Approach (Skewness = 3, Kurtosis = 21) (Single Task)
#'
#' @family jackknife functions
#' @keywords jack
#' @inheritParams vm_sev_std_jack
#' @inheritParams mvn_fit.ols_task
#' @export
vm_sev_std_jack_task <- function(taskid,
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
    "medsimple_vm_sev_dat_",
    sprintf(
      "%05.0f",
      taskid
    ),
    ".Rds"
  )
  fn <- paste0(
    "medsimple_vm_sev_std_jack_",
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
        FUN = vm_sev_std_jack,
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
#' @title Jackknife Estimates of Indirect Effect in a Simple Mediation Model for Data Generated Using the Vale and Maurelli (1983) Approach (Skewness = 3, Kurtosis = 21) (Simulation)
#'
#' @family jackknife functions
#' @keywords jack
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams vm_sev_std_jack_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams vm_sev_dat_simulation
#' @export
vm_sev_std_jack_simulation <- function(dir = getwd(),
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
      FUN = vm_sev_std_jack_task,
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
