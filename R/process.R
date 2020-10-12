#' @author Ivan Jacob Agaloos Pesign
#'
#' @title Process Task Results
#'
#' @family process results functions
#' @keywords process
#' @inheritParams mvn_dat
#' @param out Object.
#'   Output of `*_task`.
#' @export
process <- function(taskid,
                    out) {
  meansout <- colMeans(out)
  liberal_ll_99.9 <- 0.001 - (0.001 / 2)
  liberal_ul_99.9 <- 0.001 + (0.001 / 2)
  moderate_ll_99.9 <- 0.001 - (0.001 / 5)
  moderate_ul_99.9 <- 0.001 + (0.001 / 5)
  strict_ll_99.9 <- 0.001 - (0.001 / 10)
  strict_ul_99.9 <- 0.001 + (0.001 / 10)
  liberal_ll_99 <- 0.01 - (0.01 / 2)
  liberal_ul_99 <- 0.01 + (0.01 / 2)
  moderate_ll_99 <- 0.01 - (0.01 / 5)
  moderate_ul_99 <- 0.01 + (0.01 / 5)
  strict_ll_99 <- 0.01 - (0.01 / 10)
  strict_ul_99 <- 0.01 + (0.01 / 10)
  liberal_ll_95 <- 0.05 - (0.05 / 2)
  liberal_ul_95 <- 0.05 + (0.05 / 2)
  moderate_ll_95 <- 0.05 - (0.05 / 5)
  moderate_ul_95 <- 0.05 + (0.05 / 5)
  strict_ll_95 <- 0.05 - (0.05 / 10)
  strict_ul_95 <- 0.05 + (0.05 / 10)
  serlin_ll_95 <- 0.035
  serlin_ul_95 <- 0.065
  paramsmvn <- useparamsmvn(taskid = taskid)
  c(
    taskid = paramsmvn$taskid,
    n = paramsmvn$n,
    simreps = paramsmvn$reps,
    taudot = paramsmvn$taudot,
    beta = paramsmvn$beta,
    alpha = paramsmvn$alpha,
    alphabeta = paramsmvn$alphabeta,
    sigma2x = paramsmvn$sigma2x,
    sigma2epsilonm = paramsmvn$sigma2epsilonm,
    sigma2epsilony = paramsmvn$sigma2epsilony,
    mux = paramsmvn$mux,
    deltam = paramsmvn$deltam,
    deltay = paramsmvn$deltay,
    meansout,
    power_99.9 = unname(mean((1 - out[, "zero_hit_99.9"]))),
    power_99 = unname(mean((1 - out[, "zero_hit_99"]))),
    power_95 = unname(mean((1 - out[, "zero_hit_95"]))),
    liberal_ll_99.9 = liberal_ll_99.9,
    liberal_ul_99.9 = liberal_ul_99.9,
    moderate_ll_99.9 = moderate_ll_99.9,
    moderate_ul_99.9 = moderate_ul_99.9,
    strict_ll_99.9 = strict_ll_99.9,
    strict_ul_99.9 = strict_ul_99.9,
    liberal_ll_99 = liberal_ll_99,
    liberal_ul_99 = liberal_ul_99,
    moderate_ll_99 = moderate_ll_99,
    moderate_ul_99 = moderate_ul_99,
    strict_ll_99 = strict_ll_99,
    strict_ul_99 = strict_ul_99,
    liberal_ll_95 = liberal_ll_95,
    liberal_ul_95 = liberal_ul_95,
    moderate_ll_95 = moderate_ll_95,
    moderate_ul_95 = moderate_ul_95,
    strict_ll_95 = strict_ll_95,
    strict_ul_95 = strict_ul_95,
    serlin_ll_95 = serlin_ll_95,
    serlin_ul_95 = serlin_ul_95,
    liberal_99.9 = unname(meansout["theta_miss_99.9"] > liberal_ll_99.9 & meansout["theta_miss_99.9"] < liberal_ul_99.9),
    liberal_99 = unname(meansout["theta_miss_99"] > liberal_ll_99 & meansout["theta_miss_99"] < liberal_ul_99),
    liberal_95 = unname(meansout["theta_miss_95"] > liberal_ll_95 & meansout["theta_miss_95"] < liberal_ul_95),
    moderate_99.9 = unname(meansout["theta_miss_99.9"] > moderate_ll_99.9 & meansout["theta_miss_99.9"] < moderate_ul_99.9),
    moderate_99 = unname(meansout["theta_miss_99"] > moderate_ll_99 & meansout["theta_miss_99"] < moderate_ul_99),
    moderate_95 = unname(meansout["theta_miss_95"] > moderate_ll_95 & meansout["theta_miss_95"] < moderate_ul_95),
    strict_99.9 = unname(meansout["theta_miss_99.9"] > strict_ll_99.9 & meansout["theta_miss_99.9"] < strict_ul_99.9),
    strict_99 = unname(meansout["theta_miss_99"] > strict_ll_99 & meansout["theta_miss_99"] < strict_ul_99),
    strict_95 = unname(meansout["theta_miss_95"] > strict_ll_95 & meansout["theta_miss_95"] < strict_ul_95),
    serlin_95 = unname(meansout["theta_miss_95"] > serlin_ll_95 & meansout["theta_miss_95"] < serlin_ul_95)
  )
}
