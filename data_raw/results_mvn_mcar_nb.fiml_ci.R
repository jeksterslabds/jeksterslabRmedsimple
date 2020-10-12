#' ---
#' title: "Data: Nonparametric Bootstrap using FIML Confidence Intervals - Multivariate Normal - Data Missing Completely at Random"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
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
