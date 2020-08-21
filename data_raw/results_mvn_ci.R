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
mvn_mcmvn_pcci <- readRDS("summary_medsimple_mvn_mcmvn_pcci.Rds")
mvn_mct_pcci <- readRDS("summary_medsimple_mvn_mct_pcci.Rds")
mvn_nb_bcaci <- readRDS("summary_medsimple_mvn_nb_bcaci.Rds")
mvn_nb_bcci <- readRDS("summary_medsimple_mvn_nb_bcci.Rds")
mvn_nb_pcci <- readRDS("summary_medsimple_mvn_nb_pcci.Rds")
mvn_pbmvn_bcaci <- readRDS("summary_medsimple_mvn_pbmvn_bcaci.Rds")
mvn_pbmvn_bcci <- readRDS("summary_medsimple_mvn_pbmvn_bcci.Rds")
mvn_pbmvn_pcci <- readRDS("summary_medsimple_mvn_pbmvn_pcci.Rds")
results_mvn_ci <- rbind(
  mvn_mcmvn_pcci,
  mvn_mct_pcci,
  #  mvn_nb_bcaci,
  #  mvn_nb_bcci,
  mvn_nb_pcci,
  #  mvn_pbmvn_bcaci,
  #  mvn_pbmvn_bcci,
  mvn_pbmvn_pcci
)
head(results_mvn_ci)
str(results_mvn_ci)
#'
#+ usedata
usethis::use_data(
  results_mvn_ci,
  overwrite = TRUE
)
