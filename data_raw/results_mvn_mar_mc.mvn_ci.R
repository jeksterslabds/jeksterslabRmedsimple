#' ---
#' title: "Data: Monte Carlo Method Confidence Intervals - Multivariate Normal - Data Missing at Random"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' ---
#'
#+ data
mvn_mar_10_mc.mvn_pcci <- readRDS("summary_medsimple_mvn_mar_10_mc.mvn_pcci.Rds")
mvn_mar_20_mc.mvn_pcci <- readRDS("summary_medsimple_mvn_mar_20_mc.mvn_pcci.Rds")
mvn_mar_30_mc.mvn_pcci <- readRDS("summary_medsimple_mvn_mar_30_mc.mvn_pcci.Rds")
results_mvn_mar_mc.mvn_ci <- rbind(
  mvn_mar_10_mc.mvn_pcci,
  mvn_mar_20_mc.mvn_pcci,
  mvn_mar_30_mc.mvn_pcci
)
# subset
# results_mvn_mar_mc.mvn_ci <- results_mvn_mar_mc.mvn_ci[which(results_mvn_mar_mc.mvn_ci$taskid < 145 | results_mvn_mar_mc.mvn_ci$taskid > 153), ]
# results_mvn_mar_mc.mvn_ci <- results_mvn_mar_mc.mvn_ci[which(results_mvn_mar_mc.mvn_ci$n > 20), ]
head(results_mvn_mar_mc.mvn_ci)
str(results_mvn_mar_mc.mvn_ci)
#'
#+ usedata
usethis::use_data(
  results_mvn_mar_mc.mvn_ci,
  overwrite = TRUE
)
