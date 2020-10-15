#' ---
#' title: "Data: Simple Mediation Model - Multivariate Normal Distribution - Complete Data - Nonparametric Bootstrap Confidence Intervals"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' vignette: >
#'   %\VignetteIndexEntry{Data: Simple Mediation Model - Multivariate Normal Distribution - Complete Data - Nonparametric Bootstrap Confidence Intervals}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' ---
#'
#+ data
mvn_nb_pcci <- readRDS("summary_medsimple_mvn_nb_pcci.Rds")
mvn_nb_bcci <- readRDS("summary_medsimple_mvn_nb_bcci.Rds")
mvn_nb_bcaci <- readRDS("summary_medsimple_mvn_nb_bcaci.Rds")
results_mvn_nb_ci <- rbind(
  mvn_nb_pcci,
  mvn_nb_bcci,
  mvn_nb_bcaci
)
# subset
# results_mvn_nb_ci <- results_mvn_nb_ci[which(results_mvn_nb_ci$taskid < 145 | results_mvn_nb_ci$taskid > 153), ]
# results_mvn_nb_ci <- results_mvn_nb_ci[which(results_mvn_nb_ci$n > 20), ]
head(results_mvn_nb_ci)
str(results_mvn_nb_ci)
#'
#+ usedata
usethis::use_data(
  results_mvn_nb_ci,
  overwrite = TRUE
)
