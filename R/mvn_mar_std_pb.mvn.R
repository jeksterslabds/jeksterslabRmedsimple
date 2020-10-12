#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Parametric Bootstrap Estimates of Indirect Effect in a Standardized Simple Mediation Model for Data Generated from a Multivariate Normal Distribution
#'   with Data Missing at Random
#'
#' @family parametric functions
#' @keywords mvn_mar_std
#' @inheritParams fit.ols
#' @inheritParams useparamsmvn
#' @examples
#' taskid <- 1
#' data <- mvn_dat(taskid = taskid)
#' data <- mvn_dat_mar(data = data, taskid = taskid)
#' mvn_mar_std_pb.mvn(data = data, taskid = taskid)
#' @export
mvn_mar_std_pb.mvn <- function(data,
                               taskid) {
  paramsmvn <- useparamsmvn(taskid = taskid)
  fit <- coef(fit.cov(data))
  Sigmathetahat <- matrix(
    data = c(
      fit["sigma2xhat"],
      fit["sigmaxmhat"],
      fit["sigmaxyhat"],
      fit["sigmaxmhat"],
      fit["sigma2mhat"],
      fit["sigmamyhat"],
      fit["sigmaxyhat"],
      fit["sigmamyhat"],
      fit["sigma2yhat"]
    ),
    ncol = 3
  )
  muthetahat <- c(
    x = fit["muxhat"],
    m = fit["mumhat"],
    y = fit["muyhat"]
  )
  out <- pb.mvn(
    muthetahat = muthetahat,
    Sigmathetahat = Sigmathetahat,
    n = nrow(data),
    std = TRUE, # always TRUE for standardized coefficients
    par = FALSE, # should always be FALSE since this is wrapped around a parallel par_lapply
    blas_threads = FALSE # should always be FALSE since this is wrapped around a parallel par_lapply
  )
  attributes(out)$taskid <- paramsmvn$taskid
  attributes(out)$theta <- paramsmvn$alphabeta
  attributes(out)$thetahat <- fit.sem(
    data = data,
    minimal = TRUE,
    std = TRUE, # always TRUE for standardized coefficients
    fiml = TRUE
  )
  out
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Parametric Bootstrap Estimates of Indirect Effect in a Standardized Simple Mediation Model for Data Generated from a Multivariate Normal Distribution
#'   with Data Missing at Random
#'   (Single Task)
#'
#' @family parametric functions
#' @keywords mvn_mar_std
#' @inheritParams mvn_mar_std_pb.mvn
#' @inheritParams mvn_fit.ols_task
#' @export
mvn_mar_std_pb.mvn_task <- function(taskid,
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
    "medsimple_mvn_mar_dat_",
    sprintf(
      "%05.0f",
      taskid
    ),
    ".Rds"
  )
  fn <- paste0(
    "medsimple_mvn_mar_std_pb.mvn_",
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
        FUN = mvn_mar_std_pb.mvn,
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
#' @title Parametric Bootstrap Estimates of Indirect Effect in a Standardized Simple Mediation Model for Data Generated from a Multivariate Normal Distribution
#'   with Data Missing at Random
#'   (Simulation)
#'
#' @family parametric functions
#' @keywords mvn_mar_std
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams mvn_mar_std_pb.mvn_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams mvn_dat_simulation
#' @export
mvn_mar_std_pb.mvn_simulation <- function(dir = getwd(),
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
      FUN = mvn_mar_std_pb.mvn_task,
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
#' @title Parametric Bootstrap Percentile Confidence Intervals for Indirect Effect in a Standardized Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution
#'   with Data Missing at Random
#'   (Single Task)
#'
#' @family parametric functions
#' @keywords mvn_mar_std
#' @inheritParams mvn_dat_task
#' @export
mvn_mar_std_pb.mvn_pcci_task <- function(taskid,
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
    "medsimple_mvn_mar_std_pb.mvn_",
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
#' @title Parametric Bootstrap Percentile Confidence Intervals for Indirect Effect in a Standardized Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution
#'   with Data Missing at Random
#'   (Simulation)
#'
#' @family parametric functions
#' @keywords mvn_mar_std
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams mvn_mar_std_pb.mvn_pcci_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams mvn_dat_simulation
#' @export
mvn_mar_std_pb.mvn_pcci_simulation <- function(dir = getwd(),
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
      FUN = mvn_mar_std_pb.mvn_pcci_task,
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
    method = "PBPC",
    model = "Simple mediation model",
    std = TRUE,
    missing = "MAR"
  )
  fn <- "summary_medsimple_mvn_mar_std_pb.mvn_pcci.Rds"
  saveRDS(
    object = out,
    file = fn
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Parametric Bootstrap Bias-Corrected Confidence Intervals for Indirect Effect in a Standardized Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution
#'   with Data Missing at Random
#'   (Single Task)
#'
#' @family parametric functions
#' @keywords mvn_mar_std
#' @inheritParams mvn_dat_task
#' @export
mvn_mar_std_pb.mvn_bcci_task <- function(taskid,
                                         dir = getwd()) {
  # for socks to load package in the namespace
  requireNamespace(
    "jeksterslabRmedsimple",
    quietly = TRUE
  )
  foo <- function(thetahatstar) {
    bcci(
      thetahatstar = thetahatstar,
      thetahat = attributes(thetahatstar)$thetahat,
      theta = attributes(thetahatstar)$theta,
      alpha = c(0.001, 0.01, 0.05)
    )
  }
  wd <- getwd()
  setwd(dir)
  fndata <- paste0(
    "medsimple_mvn_mar_std_pb.mvn_",
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
#' @title Parametric Bootstrap Bias-Corrected Confidence Intervals for Indirect Effect in a Standardized Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution
#'   with Data Missing at Random
#'   (Simulation)
#'
#' @family parametric functions
#' @keywords mvn_mar_std
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams mvn_mar_std_pb.mvn_bcci_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams mvn_dat_simulation
#' @export
mvn_mar_std_pb.mvn_bcci_simulation <- function(dir = getwd(),
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
      FUN = mvn_mar_std_pb.mvn_bcci_task,
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
    method = "PBBC",
    model = "Simple mediation model",
    std = TRUE,
    missing = "MAR"
  )
  fn <- "summary_medsimple_mvn_mar_std_pb.mvn_bcci.Rds"
  saveRDS(
    object = out,
    file = fn
  )
}
