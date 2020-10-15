#' ---
#' title: "Data: Simple Mediation Model - Exponential X lambda = 1 - Complete Data - Monte Carlo Method Confidence Intervals with Ordinary Least Squares Parameter Estimates and Standard Errors"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' vignette: >
#'   %\VignetteIndexEntry{Data: Simple Mediation Model - Exponential X lambda = 1 - Complete Data - Monte Carlo Method Confidence Intervals with Ordinary Least Squares Parameter Estimates and Standard Errors}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' ---
#'
#+ data
results_exp_ols_mc.mvn_ci <- readRDS("summary_medsimple_exp_ols_mc.mvn_pcci.Rds")
# subset
# results_exp_ols_mc.mvn_ci <- results_exp_ols_mc.mvn_ci[which(results_exp_ols_mc.mvn_ci$taskid < 145 | results_exp_ols_mc.mvn_ci$taskid > 153), ]
# results_exp_ols_mc.mvn_ci <- results_exp_ols_mc.mvn_ci[which(results_exp_ols_mc.mvn_ci$n > 20), ]
head(results_exp_ols_mc.mvn_ci)
str(results_exp_ols_mc.mvn_ci)
#'
#+ usedata
usethis::use_data(
  results_exp_ols_mc.mvn_ci,
  overwrite = TRUE
)
