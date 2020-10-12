#' ---
#' title: "Data: Monte Carlo Method Confidence Intervals - Multivariate Normal - Complete Data - Structural Equation Modeling Estimates and Standard Errors"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' ---
#'
#+ data
results_mvn_sem_mc.mvn_ci <- readRDS("summary_medsimple_mvn_sem_mc.mvn_pcci.Rds")
# subset
# results_mvn_sem_mc.mvn_ci <- results_mvn_sem_mc.mvn_ci[which(results_mvn_sem_mc.mvn_ci$taskid < 145 | results_mvn_sem_mc.mvn_ci$taskid > 153), ]
# results_mvn_sem_mc.mvn_ci <- results_mvn_sem_mc.mvn_ci[which(results_mvn_sem_mc.mvn_ci$n > 20), ]
head(results_mvn_sem_mc.mvn_ci)
str(results_mvn_sem_mc.mvn_ci)
#'
#+ usedata
usethis::use_data(
  results_mvn_sem_mc.mvn_ci,
  overwrite = TRUE
)
