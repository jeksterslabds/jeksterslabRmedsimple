#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Nonparametric Bootstrapping
#'
#' @family nonparametric functions
#' @keywords nb
#' @inheritParams .fit
#' @inheritParams .pbmvn
#' @param std Logical.
#'   Standardize the indirect effect \eqn{\hat{\alpha} \hat{\beta} \frac{\sigma_x}{\sigma_y}}.
#' @examples
#' data <- jeksterslabRdatarepo::thirst
#' thetahatstar <- .nb(data = data, B = 5000, par = FALSE)
#' hist(thetahatstar)
#' thetahatstar <- .nb(data = data, std = TRUE, B = 5000, par = FALSE)
#' hist(thetahatstar)
#' @export
.nb <- function(data,
                std = FALSE,
                B = 200,
                par = TRUE,
                ncores = NULL,
                blas_threads = TRUE,
                mc = TRUE,
                lb = FALSE) {
  foo <- function(iter,
                  data,
                  std) {
    len <- 1:nrow(data)
    i <- sample(
      x = len,
      replace = TRUE
    )
    data <- data[i, ]
    .fit(
      data = data,
      minimal = TRUE,
      std = std
    )
  }
  thetahatstar <- par_lapply(
    X = 1:B,
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
#' @title Nonparametric Bootstrapping
#'
#' @family nonparametric functions
#' @keywords nb
#' @importFrom jeksterslabRplots .hist.plot
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams .nb
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams pcci
#' @param alphahatbetahat Numeric.
#'   Estimate of the indirect effect \eqn{\left( \hat{\alpha} \hat{\beta} \right)}.
#' @param plot Logical.
#'   Plot the sampling distribution of the indirect effect.
#' @examples
#' data <- jeksterslabRdatarepo::thirst
#' alphahatbetahat <- .fit(data, minimal = TRUE)
#' nb(data = data, B = 5000, alphahatbetahat = alphahatbetahat, par = FALSE)
#' alphahatbetahat <- .fit(data, minimal = TRUE, std = TRUE)
#' nb(data = data, std = TRUE, B = 5000, alphahatbetahat = alphahatbetahat, par = FALSE)
#' @export
nb <- function(data,
               std = FALSE,
               B = 200,
               alphahatbetahat,
               alpha = c(0.001, 0.01, 0.05),
               plot = TRUE,
               par = TRUE,
               ncores = NULL,
               blas_threads = TRUE,
               mc = TRUE,
               lb = FALSE) {
  thetahat <- alphahatbetahat
  thetahatstar <- .nb(
    data = data,
    B = B,
    std = std,
    par = par,
    ncores = ncores,
    blas_threads = blas_threads,
    mc = mc,
    lb = lb
  )
  if (plot) {
    .hist.plot(
      x = thetahatstar,
      main = expression(Nonparametric ~ Bootstrapping ~ paste(hat(alpha), hat(beta))),
      xlab = expression(paste(hat(alpha), hat(beta)))
    )
  }
  pc <- pcci(
    thetahatstar = thetahatstar,
    thetahat = thetahat,
    alpha = alpha
  )
  bc <- bcci(
    thetahatstar = thetahatstar,
    thetahat = thetahat,
    alpha = alpha
  )
  bca <- bcaci(
    thetahatstar = thetahatstar,
    thetahatstarjack = NULL,
    thetahat = thetahat,
    data = data,
    std = std,
    alpha = alpha,
    par = par,
    ncores = ncores,
    blas_threads = blas_threads,
    mc = mc,
    lb = lb
  )
  rbind(
    pc = pc,
    bc = bc,
    bca = bca
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Nonparametric Bootstrapping Estimates of Indirect Effect in a Simple Mediation Model for Data Generated from a Multivariate Normal Distribution
#'
#' @family nonparametric functions
#' @keywords nb
#' @inheritParams .fit
#' @inheritParams .nb
#' @inheritParams useparamsmvn
#' @examples
#' taskid <- 1
#' data <- mvn_dat(taskid = taskid)
#' mvn_nb(data = data, taskid = taskid, B = 200)
#' @export
mvn_nb <- function(data,
                   taskid,
                   B = 5000) {
  paramsmvn <- useparamsmvn(taskid = taskid)
  out <- .nb(
    data = data,
    B = B,
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
#' @title Nonparametric Bootstrapping Estimates of Indirect Effect in a Simple Mediation Model for Data Generated from a Multivariate Normal Distribution (Single Task)
#'
#' @family nonparametric functions
#' @keywords nb
#' @inheritParams mvn_nb
#' @inheritParams mvn_fit_task
#' @export
mvn_nb_task <- function(taskid,
                        dir = getwd(),
                        B = 5000,
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
    "medsimple_mvn_nb_",
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
        FUN = mvn_nb,
        taskid = taskid,
        B = B,
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
#' @title Nonparametric Bootstrapping Estimates of Indirect Effect in a Simple Mediation Model for Data Generated from a Multivariate Normal Distribution (Simulation)
#'
#' @family nonparametric functions
#' @keywords nb
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams mvn_nb_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams mvn_dat_simulation
#' @export
mvn_nb_simulation <- function(dir = getwd(),
                              all = TRUE,
                              taskid = NULL,
                              overwrite = FALSE,
                              B = 5000,
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
      FUN = mvn_nb_task,
      dir = dir,
      overwrite = overwrite,
      B = B,
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
#'   for Data Generated from a Multivariate Normal Distribution (Single Task)
#'
#' @family nonparametric functions
#' @keywords nb
#' @inheritParams useparamsmvn
#' @inheritParams mvn_fit_task
#' @inheritParams pcci
#' @export
mvn_nb_pcci_task <- function(taskid,
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
    "medsimple_mvn_nb_",
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
  setwd(wd)
  process(
    taskid = taskid,
    out = out
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Nonparametric Bootstrap Percentile Confidence Intervals for Indirect Effect in a Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution (Simulation)
#'
#' @family nonparametric functions
#' @keywords nb
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams mvn_nb_pcci_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams mvn_dat_simulation
#' @export
mvn_nb_pcci_simulation <- function(dir = getwd(),
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
      FUN = mvn_nb_pcci_task,
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
    method = "NBPC",
    model = "Simple mediation model",
    std = FALSE
  )
  fn <- "summary_medsimple_mvn_nb_pcci.Rds"
  saveRDS(
    object = out,
    file = fn
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Nonparametric Bootstrap Bias-Corrected Confidence Intervals for Indirect Effect in a Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution (Single Task)
#'
#' @family nonparametric functions
#' @keywords nb
#' @inheritParams useparamsmvn
#' @inheritParams mvn_fit_task
#' @inheritParams bcci
#' @export
mvn_nb_bcci_task <- function(taskid,
                             dir = getwd(),
                             alpha = c(0.001, 0.01, 0.05)) {
  # for socks to load package in the namespace
  requireNamespace(
    "jeksterslabRmedsimple",
    quietly = TRUE
  )
  foo <- function(thetahatstar,
                  alpha) {
    bcci(
      thetahatstar = thetahatstar,
      thetahat = attributes(thetahatstar)$thetahat,
      theta = attributes(thetahatstar)$theta,
      alpha = alpha
    )
  }
  wd <- getwd()
  setwd(dir)
  fndata <- paste0(
    "medsimple_mvn_nb_",
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
  setwd(wd)
  process(
    taskid = taskid,
    out = out
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Nonparametric Bootstrap Bias-Corrected Confidence Intervals for Indirect Effect in a Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution (Simulation)
#'
#' @family nonparametric functions
#' @keywords nb
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams mvn_nb_bcci_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams mvn_dat_simulation
#' @export
mvn_nb_bcci_simulation <- function(dir = getwd(),
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
      FUN = mvn_nb_bcci_task,
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
    method = "NBBC",
    model = "Simple mediation model",
    std = FALSE
  )
  fn <- "summary_medsimple_mvn_nb_bcci.Rds"
  saveRDS(
    object = out,
    file = fn
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Nonparametric Bootstrap Bias-Corrected and Accelerated Confidence Intervals for Indirect Effect in a Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution (Single Task)
#'
#' @family nonparametric functions
#' @keywords nb
#' @inheritParams useparamsmvn
#' @inheritParams mvn_fit_task
#' @inheritParams bcaci
#' @export
mvn_nb_bcaci_task <- function(taskid,
                              dir = getwd(),
                              alpha = c(0.001, 0.01, 0.05)) {
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
    "medsimple_mvn_dat_",
    sprintf(
      "%05.0f",
      taskid
    ),
    ".Rds"
  )
  fnnb <- paste0(
    "medsimple_mvn_nb_",
    sprintf(
      "%05.0f",
      taskid
    ),
    ".Rds"
  )
  fnjack <- paste0(
    "medsimple_mvn_jack_",
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
    out = out
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Nonparametric Bootstrap Bias-Corrected and Accelerated Confidence Intervals for Indirect Effect in a Simple Mediation Model
#'   for Data Generated from a Multivariate Normal Distribution (Simulation)
#'
#' @family nonparametric functions
#' @keywords nb
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams mvn_nb_bcaci_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams mvn_dat_simulation
#' @export
mvn_nb_bcaci_simulation <- function(dir = getwd(),
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
      FUN = mvn_nb_bcaci_task,
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
  fn <- "summary_medsimple_mvn_nb_bcaci.Rds"
  saveRDS(
    object = out,
    file = fn
  )
}
