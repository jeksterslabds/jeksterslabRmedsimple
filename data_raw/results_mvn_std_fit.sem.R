#' ---
#' title: "Data: Simple Mediation Model - Multivariate Normal Distribution - Standardized - Complete Data - Fit Structural Equation Modeling"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' vignette: >
#'   %\VignetteIndexEntry{Data: Simple Mediation Model - Multivariate Normal Distribution - Standardized - Complete Data - Fit Structural Equation Modeling}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' ---
#'
#+ data
results_mvn_std_fit.sem <- readRDS("summary_medsimple_mvn_std_fit.sem.Rds")
# subset
# results_mvn_std_fit.sem <- results_mvn_std_fit.sem[which(results_mvn_std_fit.sem$taskid < 145 | results_mvn_std_fit.sem$taskid > 153), ]
# results_mvn_std_fit.sem <- results_mvn_std_fit.sem[which(results_mvn_std_fit.sem$n > 20), ]
head(results_mvn_std_fit.sem)
str(results_mvn_std_fit.sem)
#'
#+ usedata
usethis::use_data(
  results_mvn_std_fit.sem,
  overwrite = TRUE
)
