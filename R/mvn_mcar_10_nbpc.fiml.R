#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Nonparametric Bootstrap with Percentile Confidence Intervals for Indirect Effect in a Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution with Data Missing Completely at Random - 10%
#'
#' @family nonparametric functions
#' @keywords nb
#' @inheritParams useparamsmvn
#' @inheritParams mvn_dat_task
#' @export
mvn_mcar_10_nbpc.fiml_task <- function(taskid,
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
    "medsimple_mvn_dat_mcar_10_",
    sprintf(
      "%05.0f",
      taskid
    ),
    ".Rds"
  )
  fn <- paste0(
    "medsimple_mvn_mcar_10_nbpc.fiml_",
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
        FUN = nbpc.fiml,
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
#' @title Nonparametric Bootstrap with Percentile Confidence Intervals for Indirect Effect in a Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution with Data Missing Completely at Random - 10%
#'   (Simulation)
#'
#' @family nonparametric functions
#' @keywords nb
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams mvn_mcar_10_mc.mvn_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams mvn_dat_simulation
#' @export
mvn_mcar_10_nbpc.fiml_simulation <- function(dir = getwd(),
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
      FUN = mvn_mcar_10_nbpc.fiml_task,
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
#' @title Nonparametric Bootstrap with Percentile Confidence Intervals for Indirect Effect in a Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution with Data Missing Completely at Random - 10%
#'   (Simulation)
#'
#' @family nonparametric functions
#' @keywords nb
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams mvn_mcar_10_mc.mvn_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams mvn_dat_simulation
#' @export
mvn_mcar_10_nb.fiml_pcci_simulation <- function(dir = getwd(),
                                                all = TRUE,
                                                taskid = NULL) {
  # for socks to load package in the namespace
  requireNamespace(
    "jeksterslabRmedsimple",
    quietly = TRUE
  )
  wd <- getwd()
  setwd(dir)
  foo <- function(taskid) {
    fndata <- paste0(
      "medsimple_mvn_mcar_10_nbpc.fiml_",
      sprintf(
        "%05.0f",
        taskid
      ),
      ".Rds"
    )
    if (file.exists(fndata)) {
      return(readRDS(fndata))
    } else {
      stop(
        paste(
          fndata,
          "does not exist in",
          dir
        )
      )
    }
  }
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
  out <- lapply(
    X = taskid,
    FUN = foo
  )
  out <- mapply(
    FUN = process,
    out = out,
    taskid = taskid,
    SIMPLIFY = FALSE
  )
  out <- do.call(
    what = "rbind",
    args = out
  )
  out <- label(
    out = out,
    method = "NBPC.FIML.MCAR.10",
    model = "Simple Mediation Model",
    std = FALSE,
    missing = "MCAR.10"
  )
  fn <- "summary_medsimple_mvn_mcar_10_nb.fiml_pcci.Rds"
  saveRDS(
    object = out,
    file = fn
  )
  invisible(setwd(wd))
}
