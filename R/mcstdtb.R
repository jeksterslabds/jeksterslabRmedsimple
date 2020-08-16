#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Monte Carlo Method for Standardized Indirect Effect in a Simple Mediation Model Assuming Multivariate Normal Distribution
#'   for Data Generated from a Multivariate Normal Distribution
#'
#' @family monte carlo method functions
#' @keywords mc
#' @importFrom lavaan coef
#' @importFrom lavaan vcov
#' @inheritParams .fit
#' @inheritParams .mcmvn
#' @inheritParams useparamsmvn
#' @examples
#' taskid <- 1
#' data <- mvn_dat(taskid = taskid)
#' mvn_std_mcmvntb(data = data, taskid = taskid, R = 200)
#' @export
mvn_std_mcmvntb <- function(data,
                            taskid,
                            R = 5000) {
  paramsmvn <- useparamsmvn(taskid = taskid)
  estimates <- .fit(
    data,
    minimal = FALSE,
    std = FALSE # always FALSE for unstandardized coefficients
    # unstandardized estimates are standardized here
  )
  alphahat <- estimates["alphahat"]
  sehatalphahat <- estimates["sehatalphahat"]
  betahat <- estimates["betahat"]
  sehatbetahat <- estimates["sehatbetahat"]
  x <- data[, 1]
  m <- data[, 2]
  y <- data[, 3]
  alphahatprime <- alphahat * (sd(x) / sd(m))
  betahatprime <- betahat * (sd(m) / sd(y))
  sehatalphahatprime <- sehatalphahat * (alphahatprime / alphahat)
  sehatbetahatprime <- sehatbetahat * (betahatprime / betahat)
  out <- .mcmvn(
    R = R,
    alphahat = alphahatprime,
    sehatalphahat = sehatalphahatprime,
    betahat = betahatprime,
    sehatbetahat = sehatbetahatprime
  )
  attributes(out)$taskid <- paramsmvn$taskid
  attributes(out)$theta <- paramsmvn$alphabeta
  attributes(out)$thetahat <- alphahatprime * betahatprime
  out
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Monte Carlo Method for Standardized Indirect Effect in a Simple Mediation Model Assuming \eqn{t} Distribution
#'   for Data Generated from a Multivariate Normal Distribution
#'
#' @family monte carlo method functions
#' @keywords mc
#' @importFrom lavaan coef
#' @importFrom lavaan vcov
#' @inheritParams .fit
#' @inheritParams .mct
#' @inheritParams useparamsmvn
#' @examples
#' taskid <- 1
#' data <- mvn_dat(taskid = taskid)
#' mvn_std_mcttb(data = data, taskid = taskid, R = 200)
#' @export
mvn_std_mcttb <- function(data,
                          taskid,
                          R = 5000) {
  paramsmvn <- useparamsmvn(taskid = taskid)
  estimates <- .fit(
    data,
    minimal = FALSE,
    std = FALSE # always FALSE for unstandardized coefficients
    # unstandardized estimates are standardized here
  )
  alphahat <- estimates["alphahat"]
  sehatalphahat <- estimates["sehatalphahat"]
  betahat <- estimates["betahat"]
  sehatbetahat <- estimates["sehatbetahat"]
  x <- data[, 1]
  m <- data[, 2]
  y <- data[, 3]
  alphahatprime <- alphahat * (sd(x) / sd(m))
  betahatprime <- betahat * (sd(m) / sd(y))
  sehatalphahatprime <- sehatalphahat * (alphahatprime / alphahat)
  sehatbetahatprime <- sehatbetahat * (betahatprime / betahat)
  n <- nrow(data)
  out <- .mct(
    R = R,
    alphahat = alphahatprime,
    sehatalphahat = sehatalphahatprime,
    betahat = betahatprime,
    sehatbetahat = sehatbetahatprime,
    n = n
  )
  attributes(out)$taskid <- paramsmvn$taskid
  attributes(out)$theta <- paramsmvn$alphabeta
  attributes(out)$thetahat <- alphahatprime * betahatprime
  out
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Monte Carlo Method for Indirect Effect in a Simple Mediation Model Assuming Multivariate Normal Distribution
#'   for Data Generated from a Multivariate Normal Distribution (Single Task)
#'
#' @family monte carlo method functions
#' @keywords mc
#' @inheritParams mvn_std_mcmvntb
#' @inheritParams mvn_fit_task
#' @export
mvn_std_mcmvntb_task <- function(taskid,
                                 R = 5000,
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
    "medsimple_mvn_std_mcmvntb_",
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
        FUN = mvn_std_mcmvntb,
        taskid = taskid,
        R = R,
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
#' @title Monte Carlo Method for Indirect Effect in a Simple Mediation Model Assuming Multivariate Normal Distribution
#'   for Data Generated from a Multivariate Normal Distribution (Simulation)
#'
#' @family monte carlo method functions
#' @keywords mc
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams mvn_std_mcmvntb_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams mvn_dat_simulation
#' @export
mvn_std_mcmvntb_simulation <- function(dir = getwd(),
                                       all = TRUE,
                                       taskid = NULL,
                                       overwrite = FALSE,
                                       R = 5000,
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
      FUN = mvn_std_mcmvntb_task,
      dir = dir,
      overwrite = overwrite,
      R = R,
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
#' @title Monte Carlo Method for Indirect Effect in a Simple Mediation Model Assuming \eqn{t} Distribution
#'   for Data Generated from a Multivariate Normal Distribution (Single Task)
#'
#' @family monte carlo method functions
#' @keywords mc
#' @inheritParams mvn_std_mcttb
#' @inheritParams mvn_fit_task
#' @export
mvn_std_mcttb_task <- function(taskid,
                               R = 5000,
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
    "medsimple_mvn_std_mcttb_",
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
        FUN = mvn_std_mcttb,
        taskid = taskid,
        R = R,
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
#' @title Monte Carlo Method for Indirect Effect in a Simple Mediation Model Assuming \eqn{t} Distribution
#'   for Data Generated from a Multivariate Normal Distribution (Simulation)
#'
#' @family monte carlo method functions
#' @keywords mc
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams mvn_std_mcttb_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams mvn_dat_simulation
#' @export
mvn_std_mcttb_simulation <- function(dir = getwd(),
                                     all = TRUE,
                                     taskid = NULL,
                                     overwrite = FALSE,
                                     R = 5000,
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
      FUN = mvn_std_mcttb_task,
      dir = dir,
      overwrite = overwrite,
      R = R,
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
#' @title Monte Carlo Method Confidence Intervals for Indirect Effect in a Simple Mediation Model Assuming \eqn{t} Distribution
#'   for Data Generated from a Multivariate Normal Distribution (Single Task)
#'
#' @family monte carlo method functions
#' @keywords mc
#' @inheritParams useparamsmvn
#' @inheritParams mvn_fit_task
#' @inheritParams pcci
#' @export
mvn_std_mcmvntb_pcci_task <- function(taskid,
                                      dir = getwd(),
                                      alpha = c(0.001, 0.01, 0.05)) {
  # for socks to load package in the namespace
  requireNamespace(
    "jeksterslabRmedsimple",
    quietly = TRUE
  )
  foo <- function(thetahatstar,
                  alpha) {
    pcci(
      thetahatstar = thetahatstar,
      thetahat = attributes(thetahatstar)$thetahat,
      theta = attributes(thetahatstar)$theta,
      alpha = alpha
    )
  }
  wd <- getwd()
  setwd(dir)
  fndata <- paste0(
    "medsimple_mvn_std_mcmvntb_",
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
      alpha = alpha,
      par = FALSE, # should always be FALSE since this is wrapped around a parallel par_lapply
      blas_threads = FALSE, # should always be FALSE since this is wrapped around a parallel par_lapply
      rbind = TRUE
    )
  )
  process(
    taskid = taskid,
    out = out
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Monte Carlo Method Confidence Intervals for Indirect Effect in a Simple Mediation Model Assuming \eqn{t} Distribution
#'   for Data Generated from a Multivariate Normal Distribution (Simulation)
#'
#' @family monte carlo method functions
#' @keywords mc
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams mvn_std_mcmvntb_pcci_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams mvn_dat_simulation
#' @export
mvn_std_mcmvntb_pcci_simulation <- function(dir = getwd(),
                                            all = TRUE,
                                            taskid = NULL,
                                            alpha = c(0.001, 0.01, 0.05),
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
      FUN = mvn_std_mcmvntb_pcci_task,
      dir = dir,
      alpha = alpha,
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
    method = "MCMVNTB",
    model = "Simple mediation model",
    std = TRUE
  )
  fn <- "summary_medsimple_mvn_std_mcmvntb_pcci.Rds"
  saveRDS(
    object = out,
    file = fn
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Monte Carlo Method Confidence Intervals for Indirect Effect in a Simple Mediation Model Assuming \eqn{t} Distribution
#'   for Data Generated from a Multivariate Normal Distribution (Single Task)
#'
#' @family monte carlo method functions
#' @keywords mc
#' @inheritParams useparamsmvn
#' @inheritParams mvn_fit_task
#' @inheritParams pcci
#' @export
mvn_std_mcttb_pcci_task <- function(taskid,
                                    dir = getwd(),
                                    alpha = c(0.001, 0.01, 0.05)) {
  # for socks to load package in the namespace
  requireNamespace(
    "jeksterslabRmedsimple",
    quietly = TRUE
  )
  foo <- function(thetahatstar,
                  alpha) {
    pcci(
      thetahatstar = thetahatstar,
      thetahat = attributes(thetahatstar)$thetahat,
      theta = attributes(thetahatstar)$theta,
      alpha = alpha
    )
  }
  wd <- getwd()
  setwd(dir)
  fndata <- paste0(
    "medsimple_mvn_std_mcttb_",
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
      alpha = alpha,
      par = FALSE, # should always be FALSE since this is wrapped around a parallel par_lapply
      blas_threads = FALSE, # should always be FALSE since this is wrapped around a parallel par_lapply
      rbind = TRUE
    )
  )
  process(
    taskid = taskid,
    out = out
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Monte Carlo Method Confidence Intervals for Indirect Effect in a Simple Mediation Model Assuming \eqn{t} Distribution
#'   for Data Generated from a Multivariate Normal Distribution (Simulation)
#'
#' @family monte carlo method functions
#' @keywords mc
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams mvn_std_mcttb_pcci_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams mvn_dat_simulation
#' @export
mvn_std_mcttb_pcci_simulation <- function(dir = getwd(),
                                          all = TRUE,
                                          taskid = NULL,
                                          alpha = c(0.001, 0.01, 0.05),
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
      FUN = mvn_std_mcttb_pcci_task,
      dir = dir,
      alpha = alpha,
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
    method = "MCtTB",
    model = "Simple mediation model",
    std = TRUE
  )
  fn <- "summary_medsimple_mvn_std_mcttb_pcci.Rds"
  saveRDS(
    object = out,
    file = fn
  )
}
