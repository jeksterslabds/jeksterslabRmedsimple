#' ---
#' title: "Data: Simple Mediation Model - Multivariate Normal Distribution - Data Missing at Random - Nonparametric Bootstrap Confidence Intervals using Full Information Maximum Likelihood"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' vignette: >
#'   %\VignetteIndexEntry{Data: Simple Mediation Model - Multivariate Normal Distribution - Data Missing at Random - Nonparametric Bootstrap Confidence Intervals using Full Information Maximum Likelihood}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' ---
#'
#+ data
mvn_mar_10_nb.fiml_pcci <- readRDS("summary_medsimple_mvn_mar_10_nb.fiml_pcci.Rds")
mvn_mar_20_nb.fiml_pcci <- readRDS("summary_medsimple_mvn_mar_20_nb.fiml_pcci.Rds")
mvn_mar_30_nb.fiml_pcci <- readRDS("summary_medsimple_mvn_mar_30_nb.fiml_pcci.Rds")
mvn_mar_10_nb.fiml_bcci <- readRDS("summary_medsimple_mvn_mar_10_nb.fiml_bcci.Rds")
mvn_mar_20_nb.fiml_bcci <- readRDS("summary_medsimple_mvn_mar_20_nb.fiml_bcci.Rds")
mvn_mar_30_nb.fiml_bcci <- readRDS("summary_medsimple_mvn_mar_30_nb.fiml_bcci.Rds")
results_mvn_mar_nb.fiml_ci <- rbind(
  mvn_mar_10_nb.fiml_pcci,
  mvn_mar_20_nb.fiml_pcci,
  mvn_mar_30_nb.fiml_pcci,
  mvn_mar_10_nb.fiml_bcci,
  mvn_mar_20_nb.fiml_bcci,
  mvn_mar_30_nb.fiml_bcci
)
# subset
# results_mvn_mar_nb.fiml_ci <- results_mvn_mar_nb.fiml_ci[which(results_mvn_mar_nb.fiml_ci$taskid < 145 | results_mvn_mar_nb.fiml_ci$taskid > 153), ]
# results_mvn_mar_nb.fiml_ci <- results_mvn_mar_nb.fiml_ci[which(results_mvn_mar_nb.fiml_ci$n > 20), ]
head(results_mvn_mar_nb.fiml_ci)
str(results_mvn_mar_nb.fiml_ci)
#'
#+ usedata
usethis::use_data(
  results_mvn_mar_nb.fiml_ci,
  overwrite = TRUE
)
