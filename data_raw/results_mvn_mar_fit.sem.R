#' ---
#' title: "Data: Simple Mediation Model - Multivariate Normal Distribution - Data Missing at Random - Fit Structural Equation Modeling with Full Information Maximum Likelihood"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' vignette: >
#'   %\VignetteIndexEntry{Data: Simple Mediation Model - Multivariate Normal Distribution - Data Missing at Random - Fit Structural Equation Modeling with Full Information Maximum Likelihood}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' ---
#'
#+ data
results_mvn_mar_10_fit.sem <- readRDS("summary_medsimple_mvn_mar_10_fit.sem.Rds")
results_mvn_mar_20_fit.sem <- readRDS("summary_medsimple_mvn_mar_20_fit.sem.Rds")
results_mvn_mar_30_fit.sem <- readRDS("summary_medsimple_mvn_mar_30_fit.sem.Rds")
results_mvn_mar_fit.sem <- rbind(
  results_mvn_mar_10_fit.sem,
  results_mvn_mar_20_fit.sem,
  results_mvn_mar_30_fit.sem
)
# subset
# results_mvn_mar_fit.sem <- results_mvn_mar_fit.sem[which(results_mvn_mar_fit.sem$taskid < 145 | results_mvn_mar_fit.sem$taskid > 153), ]
# results_mvn_mar_fit.sem <- results_mvn_mar_fit.sem[which(results_mvn_mar_fit.sem$n > 20), ]
head(results_mvn_mar_fit.sem)
str(results_mvn_mar_fit.sem)
#'
#+ usedata
usethis::use_data(
  results_mvn_mar_fit.sem,
  overwrite = TRUE
)
