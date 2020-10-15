#' ---
#' title: "Data: Simple Mediation Model - Multivariate Normal Distribution - Standardized - Complete Data - Monte Carlo Method Confidence Intervals with Ordinary Least Squares Parameter Estimates and Delta Method Standard Errors"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' vignette: >
#'   %\VignetteIndexEntry{Data: Simple Mediation Model - Multivariate Normal Distribution - Standardized - Complete Data - Monte Carlo Method Confidence Intervals with Ordinary Least Squares Parameter Estimates and Delta Method Standard Errors}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' ---
#'
#+ data
results_mvn_std_mc.mvn.delta_ci <- readRDS("summary_medsimple_mvn_std_mc.mvn.delta_pcci.Rds")
# subset
# results_mvn_std_mc.mvn.delta_ci <- results_mvn_std_mc.mvn.delta_ci[which(results_mvn_std_mc.mvn.delta_ci$taskid < 145 | results_mvn_std_mc.mvn.delta_ci$taskid > 153), ]
# results_mvn_std_mc.mvn.delta_ci <- results_mvn_std_mc.mvn.delta_ci[which(results_mvn_std_mc.mvn.delta_ci$n > 20), ]
head(results_mvn_std_mc.mvn.delta_ci)
str(results_mvn_std_mc.mvn.delta_ci)
#'
#+ usedata
usethis::use_data(
  results_mvn_std_mc.mvn.delta_ci,
  overwrite = TRUE
)
