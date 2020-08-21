#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Fit Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution
#'
#' @family model fit functions
#' @keywords fit
#' @inheritParams .fit
#' @inheritParams useparamsmvn
#' @examples
#' taskid <- 1
#' data <- mvn_dat(taskid = taskid)
#' mvn_fitfiml_mar(data = data, taskid = taskid)
#' @export
mvn_fitfiml_mar <- function(data,
                            taskid) {
  params <- useparamsmvn(taskid)
  estimates <- .fitfiml(
    data,
    std = FALSE # always FALSE for unstandardized
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
    estimates
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Fit Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution
#'   (Single Task)
#'
#' @family model fit functions
#' @keywords fit
#' @inheritParams mvn_fitfiml_mar
#' @inheritParams mvn_dat_task
#' @export
mvn_fitfiml_mar_task <- function(taskid,
                                 dir = getwd(),
                                 overwrite = FALSE) {
  # for socks to load package in the namespace
  require(
    "jeksterslabRmedsimple"
  )
  wd <- getwd()
  setwd(dir)
  fndata <- paste0(
    "medsimple_mvn_dat_mar_",
    sprintf(
      "%05.0f",
      taskid
    ),
    ".Rds"
  )
  fn <- paste0(
    "medsimple_mvn_fitfiml_mar_",
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
      par_lapply(
        X = X,
        FUN = mvn_fitfiml_mar,
        taskid = taskid,
        par = FALSE, # should always be FALSE since this is wrapped around a parallel par_lapply
        blas_threads = FALSE, # should always be FALSE since this is wrapped around a parallel par_lapply
        rbind = TRUE
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
#' @title Fit Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution
#'   (Simulation)
#'
#' @family model fit functions
#' @keywords fit
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams mvn_fitfiml_mar_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams mvn_dat_simulation
#' @export
mvn_fitfiml_mar_simulation <- function(dir = getwd(),
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
      FUN = mvn_fitfiml_mar_task,
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
