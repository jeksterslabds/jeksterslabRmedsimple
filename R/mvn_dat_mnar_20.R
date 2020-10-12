#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Generate Data from a Multivariate Normal Distribution
#'   following a Simple Mediation Model
#'   with Data Missing Not at Random
#'
#' @family multivariate normal data functions
#' @keywords mvn
#' @importFrom mice ampute
#' @importFrom stats complete.cases
#' @inheritParams fit.ols
#' @inheritParams useparamsmvn
#' @examples
#' taskid <- 1
#' data <- mvn_dat(taskid = taskid)
#' mvn_dat_mnar_20(data = data, taskid = taskid)
#' @export
mvn_dat_mnar_20 <- function(data,
                            taskid) {
  paramsmvn <- useparamsmvn(taskid = taskid)
  missing <- ampute(
    data,
    prop = 0.20,
    mech = "MNAR"
  )
  missing <- as.matrix(
    missing$amp
  )
  colnames(missing) <- c("x", "m", "y")
  attributes(missing)$taskid <- paramsmvn$taskid
  attributes(missing)$n <- paramsmvn$n
  attributes(missing)$n.complete <- nrow(missing[complete.cases(missing), ])
  missing
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Generate Data from a Multivariate Normal Distribution
#'   following a Simple Mediation Model
#'   with Data Missing Not at Random
#'   (Single Task)
#'
#' @family multivariate normal data functions
#' @keywords mvn
#' @inheritParams mvn_dat_mnar_20
#' @inheritParams mvn_dat_task
#' @export
mvn_dat_mnar_20_task <- function(taskid,
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
    "medsimple_mvn_dat_mnar_20_",
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
        FUN = mvn_dat_mnar_20,
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
#' @title Generate Data from a Multivariate Normal Distribution
#'   following a Simple Mediation Model
#'   with Data Missing Not at Random
#'   (Simulation)
#'
#' @family multivariate normal data functions
#' @keywords mvn
#' @importFrom jeksterslabRpar par_lapply
#' @inheritParams mvn_dat_mnar_20_task
#' @inheritParams jeksterslabRpar::par_lapply
#' @inheritParams mvn_dat_simulation
#' @export
mvn_dat_mnar_20_simulation <- function(dir = getwd(),
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
      FUN = mvn_dat_mnar_20_task,
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
