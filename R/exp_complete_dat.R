#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Generate Data with Exponential X
#'   following a Simple Mediation Model
#'
#' @family exponential X data functions
#' @keywords exp_dat
#' @inheritParams useparamsexp
#' @examples
#' data <- exp_dat(taskid = 1)
#' fit.ols(data, minimal = TRUE)
#' fit.ols(data, minimal = TRUE, std = TRUE)
#' fit.ols(data, minimal = FALSE)
#' @export
exp_dat <- function(taskid) {
  genexp(taskid = taskid)
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Generate Data with Exponential X
#'   following a Simple Mediation Model
#'   (Single Task)
#'
#' @family exponential X data functions
#' @keywords exp_dat
#' @inheritParams exp_dat
#' @param dir Character string.
#'   Directory where results of `exp_dat_*` are stored.
#' @param overwrite Logical.
#'   Overwrite existing results.
#' @export
exp_dat_task <- function(taskid,
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
    "medsimple_exp_dat_",
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
      exp_dat(taskid)
    }
    paramsexp <- useparamsexp(taskid = taskid)
    R <- paramsexp$reps
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
#' @title Generate Data with Exponential X
#'   following a Simple Mediation Model
#'   (Simulation)
#'
#' @family exponential X data functions
#' @keywords exp_dat
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams exp_dat_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @param all Logical.
#'   Process all results.
#' @param taskid Vector of integers.
#'   `taskid` to use when `all = FALSE`.
#' @export
exp_dat_simulation <- function(dir = getwd(),
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
      FUN = exp_dat_task,
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
