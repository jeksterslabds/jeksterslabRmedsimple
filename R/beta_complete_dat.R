#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Generate Data with Beta X
#'   following a Simple Mediation Model
#'
#' @family beta X data functions
#' @keywords beta_dat
#' @inheritParams useparamsbeta
#' @examples
#' data <- beta_dat(taskid = 1)
#' fit.ols(data, minimal = TRUE)
#' fit.ols(data, minimal = TRUE, std = TRUE)
#' fit.ols(data, minimal = FALSE)
#' @export
beta_dat <- function(taskid) {
  genbeta(taskid = taskid)
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Generate Data with Beta X
#'   following a Simple Mediation Model
#'   (Single Task)
#'
#' @family beta X data functions
#' @keywords beta_dat
#' @inheritParams beta_dat
#' @param dir Character string.
#'   Directory where results of `beta_dat_*` are stored.
#' @param overwrite Logical.
#'   Overwrite existing results.
#' @export
beta_dat_task <- function(taskid,
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
    "medsimple_beta_dat_",
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
      beta_dat(taskid)
    }
    paramsbeta <- useparamsbeta(taskid = taskid)
    R <- paramsbeta$reps
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
#' @title Generate Data with Beta X
#'   following a Simple Mediation Model
#'   (Simulation)
#'
#' @family beta X data functions
#' @keywords beta_dat
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams beta_dat_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @param all Logical.
#'   Process all results.
#' @param taskid Vector of integers.
#'   `taskid` to use when `all = FALSE`.
#' @export
beta_dat_simulation <- function(dir = getwd(),
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
    ncase <- nrow(jeksterslabRmedsimple::paramsbeta)
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
      FUN = beta_dat_task,
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
