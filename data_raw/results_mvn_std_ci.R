#' ---
#' title: "Data: Standardized Confidence Intervals - Multivariate Normal"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' ---
#'
#+ data
mcmvn_pcci <- readRDS("summary_medsimple_mvn_std_mcmvntb_pcci.Rds")
mct_pcci <- readRDS("summary_medsimple_mvn_std_mcttb_pcci.Rds")
results_mvn_std_ci <- rbind(
  mcmvn_pcci,
  mct_pcci
)
head(results_mvn_std_ci)
str(results_mvn_std_ci)
#'
#+ usedata
usethis::use_data(
  results_mvn_std_ci,
  overwrite = TRUE
)
