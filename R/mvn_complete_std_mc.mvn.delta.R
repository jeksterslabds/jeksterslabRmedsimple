#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Monte Carlo Method Assuming Multivariate Normal Distribution using Delta Method Standard Errors for Indirect Effect in a Standardized Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution
#'
#' @family monte carlo method functions
#' @keywords mc
#' @inheritParams mc.mvn
#' @inheritParams useparamsmvn
#' @param alphahatprime Numeric.
#'   Estimated standardized slope of path from `x` to `m` \eqn{\left( \hat{\alpha}^{\prime} \right)} .
#' @param sehatalphahatprimedelta Numeric.
#'   Estimated delta method standard error of standardized slope of path from `x` to `m` \eqn{\left( \widehat{se}_{\hat{\alpha}}^{\prime} \right)} .
#' @param betahatprime Numeric.
#'   Estimated standardized slope of path from `m` to `y` \eqn{\left( \hat{\beta}^{\prime} \right)} .
#' @param sehatbetahatprimedelta Numeric.
#'   Estimated delta method standard error of standardized slope of path from `m` to `y` \eqn{\left( \widehat{se}_{\hat{\beta}}^{\prime} \right)} .
#' @examples
#' taskid <- 1
#' data <- mvn_dat(taskid = taskid)
#' fit.ols(data = data, minimal = TRUE)
#'
#' fit <- mvn_fit.ols(data = data, taskid = taskid)
#' thetahatstar <- mvn_std_mc.mvn.delta(
#'   taskid = taskid, R = 20000L,
#'   alphahatprime = fit["alphahatprime"], sehatalphahatprimedelta = fit["sehatalphahatprimedelta"],
#'   betahatprime = fit["betahatprime"], sehatbetahatprimedelta = fit["sehatbetahatprimedelta"]
#' )
#' hist(thetahatstar)
#' @export
mvn_std_mc.mvn.delta <- function(taskid,
                                 R = 20000L,
                                 alphahatprime,
                                 sehatalphahatprimedelta,
                                 betahatprime,
                                 sehatbetahatprimedelta) {
  paramsmvn <- useparamsmvn(taskid = taskid)
  out <- mc.mvn(
    R = R,
    alphahat = alphahatprime,
    sehatalphahat = sehatalphahatprimedelta,
    betahat = betahatprime,
    sehatbetahat = sehatbetahatprimedelta
  )
  attributes(out)$taskid <- paramsmvn$taskid
  attributes(out)$theta <- paramsmvn$alphabeta
  attributes(out)$thetahat <- alphahatprime * betahatprime
  out
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Monte Carlo Method Assuming Multivariate Normal Distribution using Delta Method Standard Errors for Indirect Effect in a Standardized Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution
#'   (Single Task)
#'
#' @family monte carlo method functions
#' @keywords mc
#' @inheritParams mvn_dat_task
#' @export
mvn_std_mc.mvn.delta_task <- function(taskid,
                                      dir = getwd(),
                                      overwrite = FALSE) {
  # for socks to load package in the namespace
  requireNamespace(
    "jeksterslabRmedsimple",
    quietly = TRUE
  )
  wd <- getwd()
  setwd(dir)
  fnest <- paste0(
    "medsimple_mvn_fit.ols_",
    sprintf(
      "%05.0f",
      taskid
    ),
    ".Rds"
  )
  fn <- paste0(
    "medsimple_mvn_std_mc.mvn.delta_",
    sprintf(
      "%05.0f",
      taskid
    ),
    ".Rds"
  )
  # Check if data exists --------------------------------------------------------
  if (file.exists(fnest)) {
    X <- readRDS(fnest)
  } else {
    stop(
      paste(
        fnest,
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
      mapply(
        FUN = mvn_std_mc.mvn.delta,
        taskid = X[, "taskid"],
        alphahatprime = X[, "alphahatprime"],
        sehatalphahatprimedelta = X[, "sehatalphahatprimedelta"],
        betahatprime = X[, "betahatprime"],
        sehatbetahatprimedelta = X[, "sehatbetahatprimedelta"],
        SIMPLIFY = FALSE
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
#' @title Monte Carlo Method Assuming Multivariate Normal Distribution using Delta Method Standard Errors for Indirect Effect in a Standardized Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution
#'   (Simulation)
#'
#' @family monte carlo method functions
#' @keywords mc
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams mvn_std_mc.mvn.delta_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams mvn_dat_simulation
#' @export
mvn_std_mc.mvn.delta_simulation <- function(dir = getwd(),
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
      FUN = mvn_std_mc.mvn.delta_task,
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
#' @title Monte Carlo Method Confidence Intervals Assuming Multivariate Normal Distribution using Delta Method Standard Errors for Indirect Effect in a Standardized Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution
#'   (Single Task)
#'
#' @family monte carlo method functions
#' @keywords mc
#' @inheritParams mvn_dat_task
#' @export
mvn_std_mc.mvn.delta_pcci_task <- function(taskid,
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
    "medsimple_mvn_std_mc.mvn.delta_",
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
#' @title Monte Carlo Method Confidence Intervals Assuming Multivariate Normal Distribution using Delta Method Standard Errors for Indirect Effect in a Standardized Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution
#'   (Simulation)
#'
#' @family monte carlo method functions
#' @keywords mc
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams mvn_std_mc.mvn.delta_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams mvn_dat_simulation
#' @export
mvn_std_mc.mvn.delta_pcci_simulation <- function(dir = getwd(),
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
      FUN = mvn_std_mc.mvn.delta_pcci_task,
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
    method = "MC.DELTA",
    model = "Simple mediation model",
    std = TRUE
  )
  fn <- "summary_medsimple_mvn_std_mc.mvn.delta_pcci.Rds"
  saveRDS(
    object = out,
    file = fn
  )
}
