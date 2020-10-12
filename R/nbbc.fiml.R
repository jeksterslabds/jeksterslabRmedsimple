#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Nonparametric Bootstrap Estimates with FIML of Indirect Effect in a Simple Mediation Model for Data Generated from a Multivariate Normal Distribution
#'
#' @family nonparametric functions
#' @keywords nb
#' @inheritParams nb
#' @inheritParams useparamsmvn
#' @importFrom utils write.table
#' @import MplusAutomation
#' @param mpluspath Mplus path.
#' @examples
#' taskid <- 1
#' data <- mvn_dat(taskid = taskid)
#' nbbc.fiml(data = mvn_mcar_10_dat(data = data, taskid = taskid), taskid = taskid)
#' nbbc.fiml(data = mvn_mcar_20_dat(data = data, taskid = taskid), taskid = taskid)
#' nbbc.fiml(data = mvn_mcar_30_dat(data = data, taskid = taskid), taskid = taskid)
#' nbbc.fiml(data = mvn_mar_10_dat(data = data, taskid = taskid), taskid = taskid)
#' nbbc.fiml(data = mvn_mar_20_dat(data = data, taskid = taskid), taskid = taskid)
#' nbbc.fiml(data = mvn_mar_30_dat(data = data, taskid = taskid), taskid = taskid)
#' nbbc.fiml(data = mvn_mnar_10_dat(data = data, taskid = taskid), taskid = taskid)
#' nbbc.fiml(data = mvn_mnar_20_dat(data = data, taskid = taskid), taskid = taskid)
#' nbbc.fiml(data = mvn_mnar_30_dat(data = data, taskid = taskid), taskid = taskid)
#' @export
nbbc.fiml <- function(data,
                      B = 5000L,
                      taskid,
                      mpluspath = "~/.mplus/mplus") {
  wd <- getwd()
  setwd(tempdir())
  requireNamespace(
    "MplusAutomation"
  )
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {
      if (is.na(data[i, j])) {
        data[i, j] <- -999
      }
    }
  }
  fn.data <- tempfile()
  fn.data <- paste0(fn.data, ".csv")
  write.table(data, file = fn.data, row.names = FALSE, col.names = FALSE, sep = ",")
  input <- paste0(
    "DATA:", "\n",
    "\t", "FILE = ", fn.data, ";", "\n",
    "VARIABLE:", "\n",
    "\t", "NAMES = X M Y;", "\n",
    "\t", "USEVARIABLES = X M Y;", "\n",
    "\t", "MISSING = ALL (-999);", "\n",
    "ANALYSIS:", "\n",
    "\t", "TYPE = GENERAL;", "\n",
    "\t", "ESTIMATOR = ML;", "\n",
    "\t", "BOOTSTRAP = ", B, ";", "\n",
    "MODEL:", "\n",
    "\t", "Y ON X (CP);", "\n",
    "\t", "Y ON M (B);", "\n",
    "\t", "M ON X (A);", "\n",
    "\t", "X WITH X;", "\n",
    "MODEL CONSTRAINT:", "\n",
    "\t", "NEW(AB);", "\n",
    "\t", "AB = A*B;", "\n",
    "OUTPUT:", "\n",
    "\t", "CINT(bcbootstrap);", "\n"
  )
  fn.inp <- tempfile()
  fn.inp <- paste0(fn.inp, ".inp")
  fileConn <- file(fn.inp)
  writeLines(input, fileConn)
  close(fileConn)
  fn.out <- tempfile()
  fn.out <- paste0(fn.out, ".out")
  system(paste(mpluspath, fn.inp, fn.out), ignore.stdout = TRUE, ignore.stderr = TRUE)
  result <- readModels(fn.out, quiet = TRUE)
  coefficients <- coef(result, params = "new")
  est <- coefficients[, "est"]
  se <- coefficients[, "se"]
  reps <- B
  ci_95 <- confint(result, level = .95, params = "new")
  ci_99 <- confint(result, level = .99, params = "new")
  ci_0.05 <- NA
  ci_0.5 <- ci_99[, "LowerCI"]
  ci_2.5 <- ci_95[, "LowerCI"]
  ci_97.5 <- ci_95[, "UpperCI"]
  ci_99.5 <- ci_99[, "UpperCI"]
  ci_99.95 <- NA
  paramsmvn <- useparamsmvn(taskid = taskid)
  theta <- paramsmvn$alphabeta
  zero_hit_99.9 <- NA
  zero_hit_99 <- zero_hit(lo = ci_0.5, up = ci_99.5)
  zero_hit_95 <- zero_hit(lo = ci_2.5, up = ci_97.5)
  len_99.9 <- NA
  len_99 <- len(lo = ci_0.5, up = ci_99.5)
  len_95 <- len(lo = ci_2.5, up = ci_97.5)
  shape_99.9 <- NA
  shape_99 <- shape(lo = ci_0.5, thetahat = est, up = ci_99.5)
  shape_95 <- shape(lo = ci_2.5, thetahat = est, up = ci_97.5)
  theta_hit_99.9 <- NA
  theta_hit_99 <- theta_hit(lo = ci_0.5, theta = theta, up = ci_99.5)
  theta_hit_95 <- theta_hit(lo = ci_2.5, theta = theta, up = ci_97.5)
  theta_miss_99.9 <- NA
  theta_miss_99 <- 1 - theta_hit_99
  theta_miss_95 <- 1 - theta_hit_95
  on.exit(
    unlink(c(fn.data, fn.inp, fn.out))
  )
  setwd(wd)
  c(
    est = est,
    se = se,
    reps = reps,
    ci_0.05 = ci_0.05,
    ci_0.5 = ci_0.5,
    ci_2.5 = ci_2.5,
    ci_97.5 = ci_97.5,
    ci_99.5 = ci_99.5,
    ci_99.95 = ci_99.95,
    zero_hit_99.9 = zero_hit_99.9,
    zero_hit_99 = zero_hit_99,
    zero_hit_95 = zero_hit_95,
    len_99.9 = len_99.9,
    len_99 = len_99,
    len_95 = len_95,
    shape_99.9 = shape_99.9,
    shape_99 = shape_99,
    shape_95 = shape_95,
    theta_hit_99.9 = theta_hit_99.9,
    theta_hit_99 = theta_hit_99,
    theta_hit_95 = theta_hit_95,
    theta_miss_99.9 = theta_miss_99.9,
    theta_miss_99 = theta_miss_99,
    theta_miss_95 = theta_miss_95,
    theta = theta
  )
}
