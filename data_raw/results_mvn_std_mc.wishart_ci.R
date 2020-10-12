#' ---
#' title: "Data: Monte Carlo Method Confidence Intervals with Sigma (Wishart) - Multivariate Normal - Complete Data"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' ---
#'
#+ data
results_mvn_std_mc.wishart_ci <- readRDS("summary_medsimple_mvn_std_mc.wishart_pcci.Rds")
# subset
# results_mvn_std_mc.wishart_ci <- results_mvn_std_mc.wishart_ci[which(results_mvn_std_mc.wishart_ci$taskid < 145 | results_mvn_std_mc.wishart_ci$taskid > 153), ]
# results_mvn_std_mc.wishart_ci <- results_mvn_std_mc.wishart_ci[which(results_mvn_std_mc.wishart_ci$n > 20), ]
head(results_mvn_std_mc.wishart_ci)
str(results_mvn_std_mc.wishart_ci)
#'
#+ usedata
usethis::use_data(
  results_mvn_std_mc.wishart_ci,
  overwrite = TRUE
)
