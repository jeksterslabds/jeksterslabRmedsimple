#' ---
#' title: "Data: Simple Mediation Model - Vale and Maurelli (1983) - Skewness = 2, Kurtosis = 7 - Complete Data - Fit Ordinary Least Squares"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' vignette: >
#'   %\VignetteIndexEntry{Data: Simple Mediation Model - Vale and Maurelli (1983) - Skewness = 2, Kurtosis = 7 - Complete Data - Fit Ordinary Least Squares}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' ---
#'
#+ data
results_vm_mod_fit.ols <- readRDS("summary_medsimple_vm_mod_fit.ols.Rds")
# subset
# results_vm_mod_fit.ols <- results_vm_mod_fit.ols[which(results_vm_mod_fit.ols$taskid < 145 | results_vm_mod_fit.ols$taskid > 153), ]
# results_vm_mod_fit.ols <- results_vm_mod_fit.ols[which(results_vm_mod_fit.ols$n > 20), ]
head(results_vm_mod_fit.ols)
str(results_vm_mod_fit.ols)
#'
#+ usedata
usethis::use_data(
  results_vm_mod_fit.ols,
  overwrite = TRUE
)
