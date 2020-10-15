#' ---
#' title: "Data: Simple Mediation Model - Exponential X lambda = 1 - Complete Data - Fit Ordinary Least Squares"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' vignette: >
#'   %\VignetteIndexEntry{Data: Simple Mediation Model - Exponential X lambda = 1 - Complete Data - Fit Ordinary Least Squares}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' ---
#'
#+ data
results_exp_fit.ols <- readRDS("summary_medsimple_exp_fit.ols.Rds")
# subset
# results_exp_fit.ols <- results_exp_fit.ols[which(results_exp_fit.ols$taskid < 145 | results_exp_fit.ols$taskid > 153), ]
# results_exp_fit.ols <- results_exp_fit.ols[which(results_exp_fit.ols$n > 20), ]
head(results_exp_fit.ols)
str(results_exp_fit.ols)
#'
#+ usedata
usethis::use_data(
  results_exp_fit.ols,
  overwrite = TRUE
)
