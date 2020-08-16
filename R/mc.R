#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Monte Carlo Method for Indirect Effect in a Simple Mediation Model
#'   Assuming \eqn{\alpha} and \eqn{\beta} Follow a Multivariate Normal Distribution (Sampling Distribution)
#'
#' @description In this method \eqn{\alpha} and \eqn{\beta} are assumed to follow
#'   a multivariate normal distribution.
#'
#' @family monte carlo method functions
#' @keywords mc
#' @importFrom mvnfast rmvn
#' @importFrom mvnfast rmvt
#' @param R Integer.
#'   Monte Carlo replications.
#' @param alphahat Numeric.
#'   Estimated slope of path from `x` to `m` \eqn{\left( \hat{\alpha} \right)} .
#' @param sehatalphahat Numeric.
#'   Estimated standard error of slope of path from `x` to `m` \eqn{\left( \widehat{se}_{\hat{\alpha}} \right)} .
#' @param betahat Numeric.
#'   Estimated slope of path from `m` to `y` \eqn{\left( \hat{\beta} \right)} .
#' @param sehatbetahat Numeric.
#'   Estimated standard error of slope of path from `m` to `y` \eqn{\left( \widehat{se}_{\hat{\beta}} \right)} .
#' @examples
#' thetahatstar <- .mcmvn(
#'   R = 5000, alphahat = 0.338593, sehatalphahat = 0.12236736,
#'   betahat = 0.451039, sehatbetahat = 0.14597405
#' )
#' hist(thetahatstar)
#' @export
.mcmvn <- function(R = 200,
                   alphahat,
                   sehatalphahat,
                   betahat,
                   sehatbetahat) {
  mu <- c(
    alphahat,
    betahat
  )
  Sigma <- matrix(
    data = c(
      sehatalphahat^2,
      0,
      0,
      sehatbetahat^2
    ),
    ncol = 2
  )
  mc <- rmvn(
    n = R,
    mu = mu,
    sigma = Sigma
  )
  as.vector(
    unname(mc[, 1] * mc[, 2])
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Monte Carlo Method for Indirect Effect in a Simple Mediation Model
#'   Assuming \eqn{\alpha} and \eqn{\beta} Follow a \eqn{t} Distribution (Sampling Distribution)
#'
#' @description In this method \eqn{\alpha} and \eqn{\beta} are assumed to follow a \eqn{t} distribution
#'   with \eqn{df = n - 2} and \eqn{df = n - 3} respectively.
#'
#' @family monte carlo method functions
#' @keywords mc
#' @inheritParams .mcmvn
#' @importFrom stats rt
#' @param n Integer.
#'   Sample size.
#' @examples
#' thetahatstar <- .mct(
#'   R = 5000, alphahat = 0.338593, sehatalphahat = 0.12236736,
#'   betahat = 0.451039, sehatbetahat = 0.14597405,
#'   n = 20
#' )
#' hist(thetahatstar)
#' @export
.mct <- function(R = 200,
                 alphahat,
                 sehatalphahat,
                 betahat,
                 sehatbetahat,
                 n) {
  alphahatstar <- rt(n = R, df = n - 2) * sehatalphahat + alphahat
  betahatstar <- rt(n = R, df = n - 3) * sehatbetahat + betahat
  as.vector(
    unname(alphahatstar * betahatstar)
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Monte Carlo Method for Indirect Effect in a Simple Mediation Model
#'
#' @family monte carlo method functions
#' @keywords mc
#' @inheritParams .mcmvn
#' @inheritParams .mct
#' @inheritParams pcci
#' @importFrom jeksterslabRplots .hist.plot
#' @param R Integer.
#'   Monte Carlo replications.
#' @param alphahat Numeric.
#'   Estimated slope of path from `x` to `m` \eqn{\left( \hat{\alpha} \right)} .
#' @param sehatalphahat Numeric.
#'   Estimated standard error of slope of path from `x` to `m` \eqn{\left( \widehat{se}_{\hat{\alpha}} \right)} .
#' @param betahat Numeric.
#'   Estimated slope of path from `m` to `y` \eqn{\left( \hat{\beta} \right)} .
#' @param sehatbetahat Numeric.
#'   Estimated standard error of slope of path from `m` to `y` \eqn{\left( \widehat{se}_{\hat{\beta}} \right)} .
#' @param mvn Logical.
#'   If `TRUE`, generate data from a multivariate normal distribution.
#'   If `FALSE`, generate \eqn{\hat{\alpha}^{*}} from a \eqn{t} distribution with \eqn{df = n - 2}
#'   and generate \eqn{\hat{\beta}^{*}} from a \eqn{t} distribution with \eqn{df = n - 3} .
#' @param plot Logical.
#'   Plot the sampling distribution of the indirect effect.
#' @examples
#' # mvn --------------------------------------------------------------
#' mc(
#'   R = 5000, alphahat = 0.338593, sehatalphahat = 0.12236736,
#'   betahat = 0.451039, sehatbetahat = 0.14597405
#' )
#' # mvt --------------------------------------------------------------
#' mc(
#'   R = 5000, alphahat = 0.338593, sehatalphahat = 0.12236736,
#'   betahat = 0.451039, sehatbetahat = 0.14597405,
#'   mvn = FALSE, n = 20
#' )
#' @export
mc <- function(R = 200,
               alphahat,
               sehatalphahat,
               betahat,
               sehatbetahat,
               mvn = TRUE,
               n,
               alpha = c(0.001, 0.01, 0.05),
               plot = TRUE) {
  thetahat <- alphahat * betahat
  if (mvn) {
    thetahatstar <- .mcmvn(
      R = R,
      alphahat = alphahat,
      sehatalphahat = sehatalphahat,
      betahat = betahat,
      sehatbetahat = sehatbetahat
    )
  } else {
    thetahatstar <- .mct(
      R = R,
      alphahat = alphahat,
      sehatalphahat = sehatalphahat,
      betahat = betahat,
      sehatbetahat = sehatbetahat,
      n = n
    )
  }
  if (plot) {
    if (mvn) {
      .hist.plot(
        x = thetahatstar,
        main = expression(Monte ~ Carlo ~ MVN ~ paste(hat(alpha), hat(beta))),
        xlab = expression(paste(hat(alpha), hat(beta)))
      )
    } else {
      .hist.plot(
        x = thetahatstar,
        main = expression(Monte ~ Carlo ~ t ~ paste(hat(alpha), hat(beta))),
        xlab = expression(paste(hat(alpha), hat(beta)))
      )
    }
  }
  pcci(
    thetahatstar = thetahatstar,
    thetahat = thetahat,
    alpha = alpha
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Monte Carlo Method for Indirect Effect in a Simple Mediation Model Assuming Multivariate Normal Distribution
#'   for Data Generated from a Multivariate Normal Distribution
#'
#' @family monte carlo method functions
#' @keywords mc
#' @inheritParams .fit
#' @inheritParams .mcmvn
#' @inheritParams useparamsmvn
#' @examples
#' taskid <- 1
#' data <- mvn_dat(taskid = taskid)
#' mvn_mcmvn(data = data, taskid = taskid, R = 200)
#' @export
mvn_mcmvn <- function(data,
                      taskid,
                      R = 5000) {
  paramsmvn <- useparamsmvn(taskid = taskid)
  estimates <- .fit(
    data,
    minimal = FALSE,
    std = FALSE # always FALSE for unstandardized coefficients
  )
  out <- .mcmvn(
    R = R,
    alphahat = estimates["alphahat"],
    sehatalphahat = estimates["sehatalphahat"],
    betahat = estimates["betahat"],
    sehatbetahat = estimates["sehatbetahat"]
  )
  attributes(out)$taskid <- paramsmvn$taskid
  attributes(out)$theta <- paramsmvn$alphabeta
  attributes(out)$thetahat <- estimates["alphahat"] * estimates["betahat"]
  out
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Monte Carlo Method for Indirect Effect in a Simple Mediation Model Assuming \eqn{t} Distribution
#'   for Data Generated from a Multivariate Normal Distribution
#'
#' @family monte carlo method functions
#' @keywords mc
#' @inheritParams .fit
#' @inheritParams .mct
#' @inheritParams useparamsmvn
#' @examples
#' taskid <- 1
#' data <- mvn_dat(taskid = taskid)
#' mvn_mct(data = data, taskid = taskid, R = 200)
#' @export
mvn_mct <- function(data,
                    taskid,
                    R = 5000) {
  paramsmvn <- useparamsmvn(taskid = taskid)
  estimates <- .fit(
    data,
    minimal = FALSE,
    std = FALSE # always FALSE for unstandardized coefficients
  )
  n <- nrow(data)
  out <- .mct(
    R = R,
    alphahat = estimates["alphahat"],
    sehatalphahat = estimates["sehatalphahat"],
    betahat = estimates["betahat"],
    sehatbetahat = estimates["sehatbetahat"],
    n = n
  )
  attributes(out)$taskid <- paramsmvn$taskid
  attributes(out)$theta <- paramsmvn$alphabeta
  attributes(out)$thetahat <- estimates["alphahat"] * estimates["betahat"]
  out
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Monte Carlo Method for Indirect Effect in a Simple Mediation Model Assuming Multivariate Normal Distribution
#'   for Data Generated from a Multivariate Normal Distribution (Single Task)
#'
#' @family monte carlo method functions
#' @keywords mc
#' @inheritParams mvn_mcmvn
#' @inheritParams mvn_fit_task
#' @export
mvn_mcmvn_task <- function(taskid,
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
    "medsimple_mvn_mcmvn_",
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
        FUN = mvn_mcmvn,
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
#' @inheritParams mvn_mcmvn_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams mvn_dat_simulation
#' @export
mvn_mcmvn_simulation <- function(dir = getwd(),
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
      FUN = mvn_mcmvn_task,
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
#' @inheritParams mvn_mct
#' @inheritParams mvn_fit_task
#' @export
mvn_mct_task <- function(taskid,
                         dir = getwd(),
                         R = 5000,
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
    "medsimple_mvn_mct_",
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
        FUN = mvn_mct,
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
#' @inheritParams mvn_mct_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams mvn_dat_simulation
#' @export
mvn_mct_simulation <- function(dir = getwd(),
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
      FUN = mvn_mct_task,
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
#' @title Monte Carlo Method Confidence Intervals for Indirect Effect in a Simple Mediation Model Assuming Multivariate Normal Distribution
#'   for Data Generated from a Multivariate Normal Distribution (Single Task)
#'
#' @family monte carlo method functions
#' @keywords mc
#' @inheritParams useparamsmvn
#' @inheritParams mvn_fit_task
#' @inheritParams pcci
#' @export
mvn_mcmvn_pcci_task <- function(taskid,
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
    "medsimple_mvn_mcmvn_",
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
#' @title Monte Carlo Method Confidence Intervals for Indirect Effect in a Simple Mediation Model Assuming Multivariate Normal Distribution
#'   for Data Generated from a Multivariate Normal Distribution (Simulation)
#'
#' @family monte carlo method functions
#' @keywords mc
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams mvn_mcmvn_pcci_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams mvn_dat_simulation
#' @export
mvn_mcmvn_pcci_simulation <- function(dir = getwd(),
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
      FUN = mvn_mcmvn_pcci_task,
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
    method = "MCMVN",
    model = "Simple mediation model",
    std = FALSE
  )
  fn <- "summary_medsimple_mvn_mcmvn_pcci.Rds"
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
mvn_mct_pcci_task <- function(taskid,
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
    "medsimple_mvn_mct_",
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
#' @inheritParams mvn_mct_pcci_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams mvn_dat_simulation
#' @export
mvn_mct_pcci_simulation <- function(dir = getwd(),
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
      FUN = mvn_mct_pcci_task,
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
    method = "MCt",
    model = "Simple mediation model",
    std = FALSE
  )
  fn <- "summary_medsimple_mvn_mct_pcci.Rds"
  saveRDS(
    object = out,
    file = fn
  )
}
