#' ---
#' title: "Data: Simple Mediation Model - Beta X alpha = beta = 1.5 - Complete Data - Fit Ordinary Least Squares"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' vignette: >
#'   %\VignetteIndexEntry{Data: Simple Mediation Model - Beta X alpha = beta = 1.5 - Complete Data - Fit Ordinary Least Squares}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' ---
#'
#+ data
results_beta_fit.ols <- readRDS("summary_medsimple_beta_fit.ols.Rds")
# subset
# results_beta_fit.ols <- results_beta_fit.ols[which(results_beta_fit.ols$taskid < 145 | results_beta_fit.ols$taskid > 153), ]
# results_beta_fit.ols <- results_beta_fit.ols[which(results_beta_fit.ols$n > 20), ]
head(results_beta_fit.ols)
str(results_beta_fit.ols)
#'
#+ usedata
usethis::use_data(
  results_beta_fit.ols,
  overwrite = TRUE
)
