#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Fit Simple Mediation Model - Minimal
#'
#' @description Fits the simple mediation model and returns the indirect effect.
#'
#' @details The fitted simple mediation model is given by
#'   \deqn{
#'     y_i
#'     =
#'      \hat{\delta}_{y} + \hat{\dot{\tau}} x_i + \hat{\beta} m_i +
#'      \hat{\varepsilon}_{y_{i}}
#'   }
#'
#'   \deqn{
#'     m_i = \hat{\delta}_{m} + \hat{\alpha} x_i + \hat{\varepsilon}_{m_{i}}
#'   }
#'
#'   The estimated parameters for the mean structure are
#'   \deqn{
#'     \boldsymbol{\hat{\theta}}_{\text{mean structure}}
#'     =
#'      \left\{ \hat{\mu}_{x}, \hat{\delta}_{m}, \hat{\delta}_{y} \right\} .
#'   }
#'
#'   The estimated parameters for the covariance structure are
#'   \deqn{
#'     \boldsymbol{\hat{\theta}}_{\text{covariance structure}}
#'     =
#'      \left\{ \hat{\dot{\tau}}, \hat{\beta}, \hat{\alpha},
#'      \hat{\sigma}_{x}^{2}, \hat{\sigma}_{\hat{\varepsilon}_{m}}^{2},
#'      \hat{\sigma}_{\hat{\varepsilon}_{y}}^{2} \right\} .
#'   }
#'
#' @family model fit functions
#' @keywords fit
#' @import jeksterslabRlinreg
#' @importFrom stats var
#' @param data `n` by 3 matrix or data frame.
#'   `data[, 1]` correspond to values for `x`.
#'   `data[, 2]` correspond to values for `m`.
#'   `data[, 3]` correspond to values for `y`.
#' @param minimal Logical.
#'   If `TRUE`, only returns the estimate of the indirect effect
#'   \eqn{\left( \hat{\alpha} \hat{\beta} \right)}.
#'   If `FALSE`, returns more information.
#' @param std Logical.
#'   Standardize the indirect effect
#'   \eqn{
#'     \hat{\alpha}^{\prime} \hat{\beta}^{\prime}
#'     = \hat{\alpha} \hat{\beta} \frac{\hat{\sigma}_x}{\hat{\sigma}_y}}.
#' @examples
#' data <- jeksterslabRdatarepo::thirst
#' .fit(data, minimal = TRUE)
#' .fit(data, minimal = TRUE, std = TRUE)
#' .fit(data, minimal = FALSE)
#' @export
.fit <- function(data,
                 minimal = TRUE,
                 std = FALSE) {
  x <- data[, 1]
  m <- data[, 2]
  y <- data[, 3]
  n <- nrow(data)
  # y ~ x + m -------------------------------------------------------------------
  X1 <- cbind(
    constant = 1,
    x,
    m
  )
  y1 <- y
  betahat1 <- betahat(
    X = X1,
    y = y1
  )
  # m ~ x -----------------------------------------------------------------------
  X2 <- cbind(
    constant = 1,
    x
  )
  y2 <- m
  betahat2 <- betahat(
    X = X2,
    y = y2
  )
  indirect <- unname(
    betahat1[3] * betahat2[2]
  )
  if (minimal) {
    if (std) {
      return(
        indirect * (sd(x) / sd(y))
      )
    } else {
      return(
        indirect
      )
    }
  }
  # add standardized estimates and standard errors
  RSS1 <- RSS(
    X = X1,
    y = y1
  )
  RSS2 <- RSS(
    X = X2,
    y = y2
  )
  sigma2hatepsilonhat1 <- .sigma2hatepsilonhat(
    RSS = RSS1,
    n = n,
    k = ncol(X1)
  )
  sigma2hatepsilonhat2 <- .sigma2hatepsilonhat(
    RSS = RSS2,
    n = n,
    k = ncol(X2)
  )
  se1 <- sqrt(
    diag(
      .vcovhatbetahat(
        sigma2hatepsilonhat = sigma2hatepsilonhat1,
        X = X1
      )
    )
  )
  se2 <- sqrt(
    diag(
      .vcovhatbetahat(
        sigma2hatepsilonhat = sigma2hatepsilonhat2,
        X = X2
      )
    )
  )
  est <- c(
    betahat1,
    betahat2,
    indirect,
    indirect * (sd(x) / sd(y))
  )
  names(est) <- c(
    "deltayhat",
    "taudothat",
    "betahat",
    "deltamhat",
    "alphahat",
    "alphahatbetahat",
    "alphahatprimebetahatprime"
  )
  S <- c(
    sigma2xhat = var(x),
    sigma2epsilonmhat = sigma2hatepsilonhat2,
    sigma2epsilonyhat = sigma2hatepsilonhat1
  )
  se <- c(
    se1,
    se2
  )
  names(se) <- c(
    "sehatdeltayhat",
    "sehattaudothat",
    "sehatbetahat",
    "sehatdeltamhat",
    "sehatalphahat"
  )
  out <- c(
    est,
    S,
    muxhat = mean(x),
    se
  )
  out
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Fit Simple Mediation Model - Structural Equation Modeling
#'
#' @family model fit functions
#' @keywords fit
#' @inheritParams .fit
#' @importFrom lavaan sem
#' @param std Logical.
#'   If `TRUE`, estimate standardized simple mediation model
#'   using latent variables and nonlinear constraints.
#' @examples
#' library(lavaan)
#' data <- jeksterslabRdatarepo::thirst
#' .fit(data, minimal = FALSE)
#' coef(.fitsem(data = data))
#' coef(.fitsem(data = data, std = TRUE))
#' @export
.fitsem <- function(data,
                    std = FALSE) {
  data <- as.data.frame(data)
  colnames(data) <- c("x", "m", "y")
  if (std) {
    model <- "
      # measurement model
      xlatent =~ NA * x
      mlatent =~ NA * m
      ylatent =~ NA * y
      # no measurement error
      x ~~ 0 * x
      m ~~ 0 * m
      y ~~ 0 * y
      # regression
      ylatent ~ taudothatprime * xlatent + betahatprime * mlatent
      mlatent ~ alphahatprime * xlatent
      # constraints
      xlatent ~~ sigma2xlatent * xlatent
      ylatent ~~ sigma2hatepsilonyhat * ylatent
      mlatent ~~ sigma2hatepsilonmhat * mlatent
      sigma2xlatent == 1
      sigma2hatepsilonyhat == 1 - taudothatprime^2 - betahatprime^2 -  2 * alphahatprime * taudothatprime * betahatprime
      sigma2hatepsilonmhat == 1 - alphahatprime^2
    "
  } else {
    model <- "
      y ~ taudothat * x + betahat * m
      m ~ alphahat * x
    "
  }
  sem(
    model = model,
    data = data,
    estimator = "ML",
    likelihood = "wishart"
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Fit Simple Mediation Model
#'
#' @description Fits the simple mediation model.
#'
#' @family model fit functions
#' @keywords fit
#' @import jeksterslabRlinreg
#' @inheritParams .fit
#' @inheritParams mc
#' @inheritParams pbmvn
#' @inherit .fit details
#' @examples
#' data <- jeksterslabRdatarepo::thirst
#' fit(data, par = FALSE)
#' @export
fit <- function(data,
                alpha = c(0.001, 0.01, 0.05),
                R = 5000,
                B = 5000,
                plot = TRUE,
                par = TRUE,
                ncores = NULL,
                blas_threads = TRUE,
                mc = TRUE,
                lb = FALSE) {
  x <- data[, 1]
  m <- data[, 2]
  y <- data[, 3]
  n <- nrow(data)
  # y ~ x + m -------------------------------------------------------------------
  X1 <- cbind(
    constant = 1,
    x,
    m
  )
  y1 <- y
  cat("\ny ~ x + m #######################################################\n")
  eq1 <- linreg(
    X = X1,
    y = y1
  )
  # m ~ x -----------------------------------------------------------------------
  X2 <- cbind(
    constant = 1,
    x
  )
  y2 <- m
  cat("\nm ~ x ###########################################################\n")
  eq2 <- linreg(
    X = X2,
    y = y2
  )
  betahat <- eq1[["betahat"]][["m"]]
  alphahat <- eq2[["betahat"]][["x"]]
  alphahatbetahat <- alphahat * betahat
  sehatbetahat <- eq1[["sehatbetahat"]][["m"]]
  sehatalphahat <- eq2[["sehatbetahat"]][["x"]]
  # indirect --------------------------------------------------------------------
  mcmvn <- mc(
    R = R,
    alphahat = alphahat,
    sehatalphahat = sehatalphahat,
    betahat = betahat,
    sehatbetahat = sehatbetahat,
    mvn = TRUE,
    n = nrow(data),
    alpha = alpha,
    plot = plot
  )
  mct <- mc(
    R = R,
    alphahat = alphahat,
    sehatalphahat = sehatalphahat,
    betahat = betahat,
    sehatbetahat = sehatbetahat,
    mvn = FALSE,
    n = nrow(data),
    alpha = alpha,
    plot = plot
  )
  pbmvn <- pbmvn(
    data = data,
    B = B,
    alphahatbetahat = alphahatbetahat,
    alpha = alpha,
    plot = plot,
    par = par,
    ncores = ncores,
    blas_threads = blas_threads,
    mc = mc,
    lb = lb
  )
  nb <- nb(
    data = data,
    B = B,
    alphahatbetahat = alphahatbetahat,
    alpha = alpha,
    plot = plot,
    par = par,
    ncores = ncores,
    blas_threads = blas_threads,
    mc = mc,
    lb = lb
  )
  indirect <- rbind(
    mcmvn,
    mct,
    pbmvn,
    nb
  )
  rownames(indirect) <- c(
    "mcmvn",
    "mct",
    "pbpc",
    "pbbc",
    "pbbca",
    "nbpc",
    "nbbc",
    "nbbca"
  )
  cat("\nIndirect Effect #################################################\n")
  print(
    indirect
  )
  invisible(
    list(
      eq1,
      eq2,
      indirect = indirect
    )
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Fit Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution
#'
#' @family model fit functions
#' @keywords fit
#' @inheritParams .fit
#' @inheritParams useparamsmvn
#' @examples
#' taskid <- 1
#' data <- mvn_dat(taskid = taskid)
#' mvn_fit(data = data, taskid = taskid)
#' @export
mvn_fit <- function(data,
                    taskid) {
  params <- useparamsmvn(taskid)
  estimates <- .fit(
    data,
    minimal = FALSE,
    std = FALSE # always FALSE for unstandardized
  )
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
    estimates
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Fit Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution
#'   (Single Task)
#'
#' @family model fit functions
#' @keywords fit
#' @inheritParams mvn_fit
#' @inheritParams mvn_dat_task
#' @export
mvn_fit_task <- function(taskid,
                         dir = getwd(),
                         overwrite = FALSE) {
  # for socks to load package in the namespace
  require(
    "jeksterslabRmedsimple"
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
    "medsimple_mvn_fit_",
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
        FUN = mvn_fit,
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
#' @title Fit Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution
#'   (Simulation)
#'
#' @family model fit functions
#' @keywords fit
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams mvn_fit_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams mvn_dat_simulation
#' @export
mvn_fit_simulation <- function(dir = getwd(),
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
      FUN = mvn_fit_task,
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
#' @title Fit Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution
#'   (Single Task Summary)
#'
#' @family model fit functions
#' @keywords fit
#' @inheritParams mvn_fit_task
#' @importFrom jeksterslabRdist skew
#' @importFrom jeksterslabRdist kurt
#' @export
mvn_fit_task_summary <- function(taskid,
                                 dir = getwd()) {
  # for socks to load package in the namespace
  require(
    "jeksterslabRmedsimple"
  )
  wd <- getwd()
  setwd(dir)
  fn <- paste0(
    "medsimple_mvn_fit_",
    sprintf(
      "%05.0f",
      taskid
    ),
    ".Rds"
  )
  X <- readRDS(fn)
  setwd(wd)
  thetahat <- c(
    "deltayhat",
    "taudothat",
    "betahat",
    "deltamhat",
    "alphahat",
    "alphahatbetahat",
    "alphahatprimebetahatprime"
  )
  theta <- c(
    "deltay",
    "taudot",
    "beta",
    "deltam",
    "alpha",
    "alphabeta",
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
    theta = unname(means["alphabeta"]),
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
#' @title Fit Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution
#'   (Simulation Summary)
#'
#' @family model fit functions
#' @keywords fit
#' @inheritParams mvn_fit_simulation
#' @inheritParams mvn_dat_simulation
#' @export
mvn_fit_simulation_summary <- function(dir = getwd(),
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
      FUN = mvn_fit_task_summary,
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
    std = FALSE
  )
  saveRDS(
    object = out,
    file = file.path(
      dir,
      "summary_medsimple_mvn_fit.Rds"
    )
  )
}
