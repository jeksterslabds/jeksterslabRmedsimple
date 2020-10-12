#' ---
#' title: "Results: Fit Simple Mediation Model for Data Generated from a Multivariate Normal Distribution with Data Missing Not at Random - Structural Equation Modeling"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' ---
#'
#+ data
results_mvn_mnar_fit.sem <- readRDS("summary_medsimple_mvn_mnar_fit.sem.Rds")
head(results_mvn_mnar_fit.sem)
str(results_mvn_mnar_fit.sem)
#'
#+ usedata
usethis::use_data(
  results_mvn_mnar_fit.sem,
  overwrite = TRUE
)
