#' ---
#' title: "Data: Fit Simple Mediation Model for Data Generated from a Multivariate Normal Distribution - Structural Equation Modeling"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' ---
#'
#+ data
results_mvn_fit.sem <- readRDS("summary_medsimple_mvn_fit.sem.Rds")
# subset
# results_mvn_fit.sem <- results_mvn_fit.sem[which(results_mvn_fit.sem$taskid < 145 | results_mvn_fit.sem$taskid > 153), ]
# results_mvn_fit.sem <- results_mvn_fit.sem[which(results_mvn_fit.sem$n > 20), ]
head(results_mvn_fit.sem)
str(results_mvn_fit.sem)
#'
#+ usedata
usethis::use_data(
  results_mvn_fit.sem,
  overwrite = TRUE
)
