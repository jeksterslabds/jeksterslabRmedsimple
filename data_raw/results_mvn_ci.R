#' ---
#' title: "Data: Confidence Intervals - Multivariate Normal"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' ---
#'
#+ data
mcmvn_pcci <- readRDS("summary_medsimple_mvn_mcmvn_pcci.Rds")
mct_pcci <- readRDS("summary_medsimple_mvn_mct_pcci.Rds")
results_mvn_ci <- rbind(
  mcmvn_pcci,
  mct_pcci
)
head(results_mvn_ci)
str(results_mvn_ci)
#'
#+ usedata
usethis::use_data(
  results_mvn_ci,
  overwrite = TRUE
)
