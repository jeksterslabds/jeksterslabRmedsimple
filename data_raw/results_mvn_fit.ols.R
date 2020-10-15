#' ---
#' title: "Data: Simple Mediation Model - Multivariate Normal Distribution - Complete Data - Fit Ordinary Least Squares"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' vignette: >
#'   %\VignetteIndexEntry{Data: Simple Mediation Model - Multivariate Normal Distribution - Complete Data - Fit Ordinary Least Squares}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' ---
#'
#+ data
results_mvn_fit.ols <- readRDS("summary_medsimple_mvn_fit.ols.Rds")
# subset
# results_mvn_fit.ols <- results_mvn_fit.ols[which(results_mvn_fit.ols$taskid < 145 | results_mvn_fit.ols$taskid > 153), ]
# results_mvn_fit.ols <- results_mvn_fit.ols[which(results_mvn_fit.ols$n > 20), ]
head(results_mvn_fit.ols)
str(results_mvn_fit.ols)
#'
#+ usedata
usethis::use_data(
  results_mvn_fit.ols,
  overwrite = TRUE
)
