#' ---
#' title: "Data: Confidence Intervals - Multivariate Normal - Missing Completely at Random"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' ---
#'
#+ data
mvn_mcar_mc.mvn_pcci <- readRDS("summary_medsimple_mvn_mcar_mc.mvn_pcci.Rds")
mvn_mcar_nb_pcci <- readRDS("summary_medsimple_mvn_mcar_nb_pcci.Rds")
mvn_mcar_nb_bcci <- readRDS("summary_medsimple_mvn_mcar_nb_bcci.Rds")
mvn_mcar_pb.mvn_pcci <- readRDS("summary_medsimple_mvn_mcar_pb.mvn_pcci.Rds")
mvn_mcar_pb.mvn_bcci <- readRDS("summary_medsimple_mvn_mcar_pb.mvn_bcci.Rds")

results_mvn_mcar_ci <- rbind(
  mvn_mcar_mc.mvn_pcci,
  mvn_mcar_nb_pcci,
  mvn_mcar_nb_bcci
  # mvn_mcar_pb.mvn_pcci ,
  # mvn_mcar_pb.mvn_bcci
)
head(results_mvn_mcar_ci)
str(results_mvn_mcar_ci)
#'
#+ usedata
usethis::use_data(
  results_mvn_mcar_ci,
  overwrite = TRUE
)
