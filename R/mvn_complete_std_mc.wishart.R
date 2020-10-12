#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Monte Carlo Method Assuming Wishart Distribution using Estimated Covariance Matrix
#'   for Data Generated from a Multivariate Normal Distribution
#'
#' @family monte carlo method functions
#' @keywords mc
#' @inheritParams mc.mvn
#' @inheritParams useparamsmvn
#' @inheritParams fit.ols
#' @examples
#' taskid <- 1
#' data <- mvn_dat(taskid = taskid)
#' fit.ols(data = data, minimal = TRUE, std = TRUE)
#'
#' thetahatstar <- mvn_std_mc.wishart(
#'   data = data, R = 20000L, taskid = taskid
#' )
#' hist(thetahatstar)
#' @export
mvn_std_mc.wishart <- function(data,
                               R = 20000L,
                               taskid) {
  paramsmvn <- useparamsmvn(taskid = taskid)
  Sigmahat <- cov(data)
  n <- dim(data)[1]
  out <- mc.wishart(
    R = R,
    Sigmahat = Sigmahat,
    n = n,
    std = TRUE
  )
  attributes(out)$taskid <- paramsmvn$taskid
  attributes(out)$theta <- paramsmvn$alphabeta
  attributes(out)$thetahat <- fit.ols(
    data = data,
    minimal = TRUE,
    std = TRUE
  )
  out
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Monte Carlo Method Assuming Wishart Distribution using Estimated Covariance Matrix
#'   for Data Generated from a Multivariate Normal Distribution
#'   (Single Task)
#'
#' @family monte carlo method functions
#' @keywords mc
#' @inheritParams mvn_dat_task
#' @export
mvn_std_mc.wishart_task <- function(taskid,
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
    "medsimple_mvn_std_mc.wishart_",
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
        FUN = mvn_std_mc.wishart,
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
#' @title Monte Carlo Method Assuming Wishart Distribution using Estimated Covariance Matrix
#'   for Data Generated from a Multivariate Normal Distribution
#'   (Simulation)
#'
#' @family monte carlo method functions
#' @keywords mc
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams mvn_std_mc.wishart_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams mvn_dat_simulation
#' @export
mvn_std_mc.wishart_simulation <- function(dir = getwd(),
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
      FUN = mvn_std_mc.wishart_task,
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
#' @title Monte Carlo Method Confidence Intervals Assuming Wishart Distribution using Estimated Covariance Matrix
#'   for Data Generated from a Multivariate Normal Distribution
#'   (Single Task)
#'
#' @family monte carlo method functions
#' @keywords mc
#' @inheritParams mvn_dat_task
#' @export
mvn_std_mc.wishart_pcci_task <- function(taskid,
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
    "medsimple_mvn_std_mc.wishart_",
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
#' @title Monte Carlo Method Confidence Intervals Assuming Wishart Distribution using Estimated Covariance Matrix
#'   for Data Generated from a Multivariate Normal Distribution
#'   (Simulation)
#'
#' @family monte carlo method functions
#' @keywords mc
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams mvn_std_mc.wishart_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams mvn_dat_simulation
#' @export
mvn_std_mc.wishart_pcci_simulation <- function(dir = getwd(),
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
      FUN = mvn_std_mc.wishart_pcci_task,
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
    method = "MC.WISHART",
    model = "Simple mediation model",
    std = TRUE
  )
  fn <- "summary_medsimple_mvn_std_mc.wishart_pcci.Rds"
  saveRDS(
    object = out,
    file = fn
  )
}
