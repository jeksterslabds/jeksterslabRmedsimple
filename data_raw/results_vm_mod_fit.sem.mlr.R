#' ---
#' title: "Data: Simple Mediation Model - Vale and Maurelli (1983) - Skewness = 2, Kurtosis = 7 - Complete Data - Fit Structural Equation Modeling with Robust Standard Errors"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' vignette: >
#'   %\VignetteIndexEntry{Data: Simple Mediation Model - Vale and Maurelli (1983) - Skewness = 2, Kurtosis = 7 - Complete Data - Fit Structural Equation Modeling with Robust Standard Errors}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' ---
#'
#+ data
results_vm_mod_fit.sem.mlr <- readRDS("summary_medsimple_vm_mod_fit.sem.mlr.Rds")
# subset
# results_vm_mod_fit.sem.mlr <- results_vm_mod_fit.sem.mlr[which(results_vm_mod_fit.sem.mlr$taskid < 145 | results_vm_mod_fit.sem.mlr$taskid > 153), ]
# results_vm_mod_fit.sem.mlr <- results_vm_mod_fit.sem.mlr[which(results_vm_mod_fit.sem.mlr$n > 20), ]
head(results_vm_mod_fit.sem.mlr)
str(results_vm_mod_fit.sem.mlr)
#'
#+ usedata
usethis::use_data(
  results_vm_mod_fit.sem.mlr,
  overwrite = TRUE
)
