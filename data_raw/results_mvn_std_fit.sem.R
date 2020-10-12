#' ---
#' title: "Results: Fit Standardized Simple Mediation Model for Data Generated from a Multivariate Normal Distribution - Structural Equation Modeling"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' ---
#'
#+ data
results_mvn_std_fit.sem <- readRDS("summary_medsimple_mvn_std_fit.sem.Rds")
head(results_mvn_std_fit.sem)
str(results_mvn_std_fit.sem)
#'
#+ usedata
usethis::use_data(
  results_mvn_std_fit.sem,
  overwrite = TRUE
)
