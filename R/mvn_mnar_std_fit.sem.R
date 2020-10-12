#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Fit Standardized Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution
#'   with Data Missing Not at Random - Structural Equation Modeling
#'
#' @family model fit functions
#' @keywords mvn_mnar_std
#' @importFrom lavaan coef lavInspect vcov
#' @inheritParams fit.sem
#' @inheritParams useparamsmvn
#' @examples
#' taskid <- 1
#' data <- mvn_dat(taskid = taskid)
#' data <- mvn_dat_mnar(data = data, taskid = taskid)
#'
#' # SEM with FIML -------------------------------------------------------------
#' mvn_mnar_std_fit.sem(data = data, taskid = taskid)
#'
#' # OLS with listwise deletion ------------------------------------------------
#' mvn_fit.ols(data = data, taskid = taskid)
#' @export
mvn_mnar_std_fit.sem <- function(data,
                                 taskid) {
  params <- useparamsmvn(taskid)
  object <- fit.sem(
    data = data,
    std = TRUE, # should always be TRUE for standardized coefficients
    fiml = TRUE # should always be TRUE for `missing = "fiml"`
  )
  converge <- lavInspect(
    object = object,
    what = "converged"
  )
  if (converge) {
    estimates <- coef(object)
    se <- unname(
      as.vector(
        sqrt(
          diag(
            vcov(
              object
            )
          )
        )
      )
    )
    names(se) <- paste0("sehat", names(estimates))
    c(
      taskid = params$taskid,
      n = params$n,
      reps = params$reps,
      taudot = params$taudot,
      beta = params$beta,
      alpha = params$alpha,
      alphabeta = params$alphabeta,
      sigma2x = params$sigma2x,
      sigma2epsilonm = params$sigma2epsilonm,
      sigma2epsilony = params$sigma2epsilony,
      mux = params$mux,
      deltam = params$deltam,
      deltay = params$deltay,
      estimates,
      alphahatprimebetahatprime = unname(estimates["alphahatprime"] * estimates["betahatprime"]),
      se
    )
  } else {
    stop(
      "\`post.check\` or \`converged\` is not TRUE."
    )
  }
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Fit Standardized Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution
#'   with Data Missing Not at Random - Structural Equation Modeling
#'   (Single Task)
#'
#' @family model fit functions
#' @keywords mvn_mnar_std
#' @inheritParams mvn_mnar_std_fit.sem
#' @inheritParams mvn_dat_task
#' @export
mvn_mnar_std_fit.sem_task <- function(taskid,
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
    "medsimple_mvn_dat_mnar_",
    sprintf(
      "%05.0f",
      taskid
    ),
    ".Rds"
  )
  fn <- paste0(
    "medsimple_mvn_mnar_std_fit.sem_",
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
        FUN = mvn_mnar_std_fit.sem,
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
#' @title Fit Standardized Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution
#'   with Data Missing Not at Random - Structural Equation Modeling
#'   (Simulation)
#'
#' @family model fit functions
#' @keywords mvn_mnar_std
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams mvn_mnar_std_fit.sem_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams mvn_dat_simulation
#' @export
mvn_mnar_std_fit.sem_simulation <- function(dir = getwd(),
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
      FUN = mvn_mnar_std_fit.sem_task,
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
#' @title Fit Standardized Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution
#'   with Data Missing Not at Random - Structural Equation Modeling
#'   (Single Task Summary)
#'
#' @family model fit functions
#' @keywords mvn_mnar_std
#' @inheritParams mvn_mnar_std_fit.sem_task
#' @importFrom jeksterslabRdist skew
#' @importFrom jeksterslabRdist kurt
#' @export
mvn_mnar_std_fit.sem_task_summary <- function(taskid,
                                              dir = getwd()) {
  # for socks to load package in the namespace
  requireNamespace(
    "jeksterslabRmedsimple",
    quietly = TRUE
  )
  wd <- getwd()
  setwd(dir)
  fn <- paste0(
    "medsimple_mvn_mnar_std_fit.sem_",
    sprintf(
      "%05.0f",
      taskid
    ),
    ".Rds"
  )
  X <- readRDS(fn)
  setwd(wd)
  thetahat <- c(
    "taudothatprime",
    "betahatprime",
    "alphahatprime",
    "alphahatprimebetahatprime"
  )
  theta <- c(
    "taudot",
    "beta",
    "alpha",
    "alphabeta"
  )
  means <- colMeans(X)
  vars <- sds <- skews <- kurts <- bias <- mse <- rmse <- rep(x = NA, times = length(thetahat))
  for (i in seq_along(thetahat)) {
    vars[i] <- var(X[, thetahat[i]])
    sds[i] <- sd(X[, thetahat[i]])
    skews[i] <- skew(X[, thetahat[i]])
    kurts[i] <- kurt(X[, thetahat[i]])
    bias[i] <- mean(X[, thetahat[i]] - X[, theta[i]])
    mse[i] <- mean((X[, thetahat[i]] - X[, theta[i]])^2)
    rmse[i] <- sqrt(mean((X[, thetahat[i]] - X[, theta[i]])^2))
  }
  names(vars) <- paste0(thetahat, "_var")
  names(sds) <- paste0(thetahat, "_sd")
  names(skews) <- paste0(thetahat, "_skew")
  names(kurts) <- paste0(thetahat, "_kurt")
  names(bias) <- paste0(thetahat, "_bias")
  names(mse) <- paste0(thetahat, "_mse")
  names(rmse) <- paste0(thetahat, "_rmse")
  c(
    means,
    theta = unname(means["alphahatprimebetahatprime"]),
    vars,
    sds,
    skews,
    kurts,
    bias,
    mse,
    rmse
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Fit Standardized Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution
#'   with Data Missing Not at Random - Structural Equation Modeling
#'   (Simulation Summary)
#'
#' @family model fit functions
#' @keywords mvn_mnar_std
#' @inheritParams mvn_mnar_std_fit.sem_simulation
#' @inheritParams mvn_dat_simulation
#' @export
mvn_mnar_std_fit.sem_simulation_summary <- function(dir = getwd(),
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
      FUN = mvn_mnar_std_fit.sem_task_summary,
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
    method = "fit",
    model = "Simple mediation model",
    std = TRUE,
    missing = "MNAR"
  )
  saveRDS(
    object = out,
    file = file.path(
      dir,
      "summary_medsimple_mvn_mnar_std_fit.sem.Rds"
    )
  )
}
