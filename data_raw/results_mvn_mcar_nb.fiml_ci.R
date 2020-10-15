#' ---
#' title: "Data: Simple Mediation Model - Multivariate Normal Distribution - Data Missing Completely at Random - Nonparametric Bootstrap Confidence Intervals using Full Information Maximum Likelihood"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' vignette: >
#'   %\VignetteIndexEntry{Data: Simple Mediation Model - Multivariate Normal Distribution - Data Missing Completely at Random - Nonparametric Bootstrap Confidence Intervals using Full Information Maximum Likelihood}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' ---
#'
#+ data
mvn_mcar_10_nb.fiml_pcci <- readRDS("summary_medsimple_mvn_mcar_10_nb.fiml_pcci.Rds")
mvn_mcar_20_nb.fiml_pcci <- readRDS("summary_medsimple_mvn_mcar_20_nb.fiml_pcci.Rds")
mvn_mcar_30_nb.fiml_pcci <- readRDS("summary_medsimple_mvn_mcar_30_nb.fiml_pcci.Rds")
mvn_mcar_10_nb.fiml_bcci <- readRDS("summary_medsimple_mvn_mcar_10_nb.fiml_bcci.Rds")
mvn_mcar_20_nb.fiml_bcci <- readRDS("summary_medsimple_mvn_mcar_20_nb.fiml_bcci.Rds")
mvn_mcar_30_nb.fiml_bcci <- readRDS("summary_medsimple_mvn_mcar_30_nb.fiml_bcci.Rds")
results_mvn_mcar_nb.fiml_ci <- rbind(
  mvn_mcar_10_nb.fiml_pcci,
  mvn_mcar_20_nb.fiml_pcci,
  mvn_mcar_30_nb.fiml_pcci,
  mvn_mcar_10_nb.fiml_bcci,
  mvn_mcar_20_nb.fiml_bcci,
  mvn_mcar_30_nb.fiml_bcci
)
# subset
# results_mvn_mcar_nb.fiml_ci <- results_mvn_mcar_nb.fiml_ci[which(results_mvn_mcar_nb.fiml_ci$taskid < 145 | results_mvn_mcar_nb.fiml_ci$taskid > 153), ]
# results_mvn_mcar_nb.fiml_ci <- results_mvn_mcar_nb.fiml_ci[which(results_mvn_mcar_nb.fiml_ci$n > 20), ]
head(results_mvn_mcar_nb.fiml_ci)
str(results_mvn_mcar_nb.fiml_ci)
#'
#+ usedata
usethis::use_data(
  results_mvn_mcar_nb.fiml_ci,
  overwrite = TRUE
)
