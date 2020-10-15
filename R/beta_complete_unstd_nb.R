#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Nonparametric Bootstrap Estimates of Indirect Effect in a Simple Mediation Model for Data with Exponential X
#'
#' @family nonparametric functions
#' @keywords nb
#' @inheritParams fit.ols
#' @inheritParams nb
#' @inheritParams useparamsbeta
#' @examples
#' taskid <- 1
#' data <- beta_dat(taskid = taskid)
#' beta_nb(data = data, taskid = taskid)
#' @export
beta_nb <- function(data,
                    taskid) {
  paramsbeta <- useparamsbeta(taskid = taskid)
  out <- nb(
    data = data,
    std = FALSE, # always FALSE for unstandardized coefficients
    complete = TRUE, # always TRUE for complete data set
    par = FALSE, # should always be FALSE since this is wrapped around a parallel par_lapply
    blas_threads = FALSE # should always be FALSE since this is wrapped around a parallel par_lapply
  )
  attributes(out)$taskid <- paramsbeta$taskid
  attributes(out)$theta <- paramsbeta$alphabeta
  attributes(out)$thetahat <- fit.ols(
    data = data,
    minimal = TRUE,
    std = FALSE # always FALSE for unstandardized coefficients
  )
  out
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Nonparametric Bootstrap Estimates of Indirect Effect in a Simple Mediation Model for Data with Exponential X
#'   (Single Task)
#'
#' @family nonparametric functions
#' @keywords nb
#' @inheritParams beta_nb
#' @inheritParams beta_fit.ols_task
#' @export
beta_nb_task <- function(taskid,
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
    "medsimple_beta_dat_",
    sprintf(
      "%05.0f",
      taskid
    ),
    ".Rds"
  )
  fn <- paste0(
    "medsimple_beta_nb_",
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
        FUN = beta_nb,
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
#' @title Nonparametric Bootstrap Estimates of Indirect Effect in a Simple Mediation Model for Data with Exponential X
#'   (Simulation)
#'
#' @family nonparametric functions
#' @keywords nb
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams beta_nb_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams beta_dat_simulation
#' @export
beta_nb_simulation <- function(dir = getwd(),
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
      FUN = beta_nb_task,
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
#' @title Nonparametric Bootstrap Percentile Confidence Intervals for Indirect Effect in a Simple Mediation Model
#'   for Data with Exponential X
#'   (Single Task)
#'
#' @family nonparametric functions
#' @keywords nb
#' @inheritParams beta_dat_task
#' @export
beta_nb_pcci_task <- function(taskid,
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
    "medsimple_beta_nb_",
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
    out = out,
    dist = "beta"
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Nonparametric Bootstrap Percentile Confidence Intervals for Indirect Effect in a Simple Mediation Model
#'   for Data with Exponential X
#'   (Simulation)
#'
#' @family nonparametric functions
#' @keywords nb
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams beta_nb_pcci_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams beta_dat_simulation
#' @export
beta_nb_pcci_simulation <- function(dir = getwd(),
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
      FUN = beta_nb_pcci_task,
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
    method = "NBPC",
    model = "Simple mediation model",
    std = FALSE
  )
  fn <- "summary_medsimple_beta_nb_pcci.Rds"
  saveRDS(
    object = out,
    file = fn
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Nonparametric Bootstrap Bias-Corrected Confidence Intervals for Indirect Effect in a Simple Mediation Model
#'   for Data with Exponential X
#'   (Single Task)
#'
#' @family nonparametric functions
#' @keywords nb
#' @inheritParams beta_dat_task
#' @export
beta_nb_bcci_task <- function(taskid,
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
    "medsimple_beta_nb_",
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
    out = out,
    dist = "beta"
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Nonparametric Bootstrap Bias-Corrected Confidence Intervals for Indirect Effect in a Simple Mediation Model
#'   for Data with Exponential X
#'   (Simulation)
#'
#' @family nonparametric functions
#' @keywords nb
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams beta_nb_bcci_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams beta_dat_simulation
#' @export
beta_nb_bcci_simulation <- function(dir = getwd(),
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
      FUN = beta_nb_bcci_task,
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
    method = "NBBC",
    model = "Simple mediation model",
    std = FALSE
  )
  fn <- "summary_medsimple_beta_nb_bcci.Rds"
  saveRDS(
    object = out,
    file = fn
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Nonparametric Bootstrap Bias-Corrected and Accelerated Confidence Intervals for Indirect Effect in a Simple Mediation Model
#'   for Data with Exponential X
#'   (Single Task)
#'
#' @family nonparametric functions
#' @keywords nb
#' @inheritParams beta_dat_task
#' @export
beta_nb_bcaci_task <- function(taskid,
                               dir = getwd()) {
  # for socks to load package in the namespace
  requireNamespace(
    "jeksterslabRmedsimple",
    quietly = TRUE
  )
  foo <- function(thetahatstar,
                  thetahatstarjack,
                  data) {
    bcaci(
      thetahatstar = thetahatstar,
      thetahatstarjack = thetahatstarjack,
      thetahat = attributes(thetahatstar)$thetahat,
      theta = attributes(thetahatstar)$theta,
      data = data,
      std = FALSE,
      alpha = c(0.001, 0.01, 0.05),
      par = FALSE, # should always be FALSE since this is wrapped around a parallel par_lapply
      blas_threads = FALSE # should always be FALSE since this is wrapped around a parallel par_lapply
    )
  }
  wd <- getwd()
  setwd(dir)
  fndata <- paste0(
    "medsimple_beta_dat_",
    sprintf(
      "%05.0f",
      taskid
    ),
    ".Rds"
  )
  fnnb <- paste0(
    "medsimple_beta_nb_",
    sprintf(
      "%05.0f",
      taskid
    ),
    ".Rds"
  )
  fnjack <- paste0(
    "medsimple_beta_jack_",
    sprintf(
      "%05.0f",
      taskid
    ),
    ".Rds"
  )
  if (file.exists(fndata)) {
    data <- readRDS(fndata)
  } else {
    stop(
      paste(
        fndata,
        "does not exist in",
        dir
      )
    )
  }
  if (file.exists(fnnb)) {
    thetahatstar <- readRDS(fnnb)
  } else {
    stop(
      paste(
        fnnb,
        "does not exist in",
        dir
      )
    )
  }
  if (file.exists(fnjack)) {
    thetahatstarjack <- readRDS(fnjack)
  } else {
    stop(
      paste(
        fnjack,
        "does not exist in",
        dir
      )
    )
  }
  out <- invisible(
    t(
      mapply(
        FUN = foo,
        thetahatstar = thetahatstar,
        thetahatstarjack = thetahatstarjack,
        data = data
      )
    )
  )
  setwd(wd)
  process(
    taskid = taskid,
    out = out,
    dist = "beta"
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Nonparametric Bootstrap Bias-Corrected and Accelerated Confidence Intervals for Indirect Effect in a Simple Mediation Model
#'   for Data with Exponential X
#'   (Simulation)
#'
#' @family nonparametric functions
#' @keywords nb
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams beta_nb_bcaci_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams beta_dat_simulation
#' @export
beta_nb_bcaci_simulation <- function(dir = getwd(),
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
      FUN = beta_nb_bcaci_task,
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
    method = "NBBCA",
    model = "Simple mediation model",
    std = FALSE
  )
  fn <- "summary_medsimple_beta_nb_bcaci.Rds"
  saveRDS(
    object = out,
    file = fn
  )
}
