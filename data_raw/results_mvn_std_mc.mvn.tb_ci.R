#' ---
#' title: "Data: Monte Carlo Method Confidence Intervals with Text Book Standard Errors for Standardized Indirect Effect - Multivariate Normal - Complete Data"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' ---
#'
#+ data
results_mvn_std_mc.mvn.tb_ci <- readRDS("summary_medsimple_mvn_std_mc.mvn.tb_pcci.Rds")
# subset
# results_mvn_std_mc.mvn.tb_ci <- results_mvn_std_mc.mvn.tb_ci[which(results_mvn_std_mc.mvn.tb_ci$taskid < 145 | results_mvn_std_mc.mvn.tb_ci$taskid > 153), ]
# results_mvn_std_mc.mvn.tb_ci <- results_mvn_std_mc.mvn.tb_ci[which(results_mvn_std_mc.mvn.tb_ci$n > 20), ]
head(results_mvn_std_mc.mvn.tb_ci)
str(results_mvn_std_mc.mvn.tb_ci)
#'
#+ usedata
usethis::use_data(
  results_mvn_std_mc.mvn.tb_ci,
  overwrite = TRUE
)
