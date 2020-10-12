#' ---
#' title: "Results: Fit Simple Mediation Model for Data Generated from a Multivariate Normal Distribution with Data Missing at Random - Structural Equation Modeling"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' ---
#'
#+ data
results_mvn_mar_fit.sem <- readRDS("summary_medsimple_mvn_mar_fit.sem.Rds")
head(results_mvn_mar_fit.sem)
str(results_mvn_mar_fit.sem)
#'
#+ usedata
usethis::use_data(
  results_mvn_mar_fit.sem,
  overwrite = TRUE
)
