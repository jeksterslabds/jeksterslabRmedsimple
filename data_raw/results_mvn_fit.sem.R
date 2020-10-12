#' ---
#' title: "Results: Fit Simple Mediation Model for Data Generated from a Multivariate Normal Distribution - Structural Equation Modeling"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' ---
#'
#+ data
mvn_complete_fit.sem <- readRDS("summary_medsimple_mvn_complete_fit.sem.Rds")
mvn_mar_10_fit.sem <- readRDS("summary_medsimple_mvn_mar_10_fit.sem.Rds")
mvn_mar_30_fit.sem <- readRDS("summary_medsimple_mvn_mar_30_fit.sem.Rds")
mvn_mcar_10_fit.sem <- readRDS("summary_medsimple_mvn_mcar_10_fit.sem.Rds")
mvn_mcar_30_fit.sem <- readRDS("summary_medsimple_mvn_mcar_30_fit.sem.Rds")
mvn_mnar_10_fit.sem <- readRDS("summary_medsimple_mvn_mnar_10_fit.sem.Rds")
mvn_mnar_30_fit.sem <- readRDS("summary_medsimple_mvn_mnar_30_fit.sem.Rds")
results_mvn_fit.sem <- dplyr::bind_rows(
  mvn_mar_10_fit.sem,
  mvn_mar_30_fit.sem,
  mvn_mcar_10_fit.sem,
  mvn_mcar_30_fit.sem,
  mvn_mnar_10_fit.sem,
  mvn_mnar_30_fit.sem,
  mvn_complete_fit.sem
)
head(results_mvn_fit.sem)
str(results_mvn_fit.sem)
#'
#+ usedata
usethis::use_data(
  results_mvn_fit.sem,
  overwrite = TRUE
)
