#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Jackknife Estimates of Indirect Effect for a Simple Mediation Model
#'
#' @family jackknife functions
#' @keywords jack
#' @inheritParams .fit
#' @inheritParams .nb
#' @examples
#' data <- jeksterslabRdatarepo::thirst
#' .jack(data, par = FALSE)
#' .jack(data, std = TRUE, par = FALSE)
#' @export
.jack <- function(data,
                  std = FALSE,
                  par = TRUE,
                  ncores = NULL,
                  blas_threads = TRUE,
                  mc = TRUE,
                  lb = FALSE) {
  foo <- function(i,
                  data,
                  std) {
    .fit(
      data = data[-i, ],
      minimal = TRUE,
      std = std
    )
  }
  thetahatstar <- par_lapply(
    X = 1:nrow(data),
    FUN = foo,
    data = data,
    std = std,
    par = par,
    ncores = ncores,
    blas_threads = blas_threads,
    mc = mc,
    lb = lb,
    rbind = TRUE
  )
  as.vector(thetahatstar)
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Jackknife Estimates of Indirect Effect in a Simple Mediation Model for Data Generated from a Multivariate Normal Distribution
#'
#' @family jackknife functions
#' @keywords jack
#' @inheritParams .fit
#' @inheritParams .jack
#' @inheritParams useparamsmvn
#' @examples
#' taskid <- 1
#' data <- mvn_dat(taskid = taskid)
#' mvn_jack(data = data, taskid = taskid)
#' @export
mvn_jack <- function(data,
                     taskid) {
  paramsmvn <- useparamsmvn(taskid = taskid)
  out <- .jack(
    data = data,
    std = FALSE, # always FALSE for unstandardized coefficients
    par = FALSE, # should always be FALSE since this is wrapped around a parallel par_lapply
    blas_threads = FALSE # should always be FALSE since this is wrapped around a parallel par_lapply
  )
  attributes(out)$taskid <- paramsmvn$taskid
  attributes(out)$theta <- paramsmvn$alphabeta
  attributes(out)$thetahat <- .fit(
    data = data,
    minimal = TRUE,
    std = FALSE # always FALSE for unstandardized coefficients
  )
  out
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Jackknife Estimates of Indirect Effect in a Simple Mediation Model for Data Generated from a Multivariate Normal Distribution (Single Task)
#'
#' @family jackknife functions
#' @keywords jack
#' @inheritParams mvn_jack
#' @inheritParams mvn_fit_task
#' @export
mvn_jack_task <- function(taskid,
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
    "medsimple_mvn_dat_",
    sprintf(
      "%05.0f",
      taskid
    ),
    ".Rds"
  )
  fn <- paste0(
    "medsimple_mvn_jack_",
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
        FUN = mvn_jack,
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
#' @title Jackknife Estimates of Indirect Effect in a Simple Mediation Model for Data Generated from a Multivariate Normal Distribution (Simulation)
#'
#' @family jackknife functions
#' @keywords jack
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams mvn_jack_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams mvn_dat_simulation
#' @export
mvn_jack_simulation <- function(dir = getwd(),
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
      FUN = mvn_jack_task,
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
