#' ---
#' title: "Data: Fit - Multivariate Normal"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' ---
#'
#+ data
results_mvn_fit <- readRDS("summary_medsimple_mvn_fit.Rds")
head(results_mvn_fit)
str(results_mvn_fit)
#'
#+ usedata
usethis::use_data(
  results_mvn_fit,
  overwrite = TRUE
)
