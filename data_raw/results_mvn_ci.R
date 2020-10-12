#' ---
#' title: "Data: Confidence Intervals - Multivariate Normal - Complete Data"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' ---
#'
#+ data
mvn_mc.mvn_pcci <- readRDS("summary_medsimple_mvn_mc.mvn_pcci.Rds")
mvn_nb_pcci <- readRDS("summary_medsimple_mvn_nb_pcci.Rds")
mvn_nb_bcci <- readRDS("summary_medsimple_mvn_nb_bcci.Rds")
mvn_nb_bcaci <- readRDS("summary_medsimple_mvn_nb_bcaci.Rds")
mvn_pb.mvn_pcci <- readRDS("summary_medsimple_mvn_pb.mvn_pcci.Rds")
mvn_pb.mvn_bcci <- readRDS("summary_medsimple_mvn_pb.mvn_bcci.Rds")
mvn_pb.mvn_bcaci <- readRDS("summary_medsimple_mvn_pb.mvn_bcaci.Rds")
results_mvn_ci <- rbind(
  mvn_mc.mvn_pcci,
  mvn_nb_pcci,
  mvn_nb_bcci,
  mvn_nb_bcaci,
  mvn_pb.mvn_pcci,
  mvn_pb.mvn_bcci,
  mvn_pb.mvn_bcaci
)
head(results_mvn_ci)
str(results_mvn_ci)
#'
#+ usedata
usethis::use_data(
  results_mvn_ci,
  overwrite = TRUE
)
