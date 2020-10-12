#' ---
#' title: "Data: Standardized Confidence Intervals - Multivariate Normal - Complete Data"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' ---
#'
#+ data
mvn_std_mc.t.tb_pcci <- readRDS("summary_medsimple_mvn_std_mc.t.tb_pcci.Rds")
mvn_std_mc.t.delta_pcci <- readRDS("summary_medsimple_mvn_std_mc.t.delta_pcci.Rds")
mvn_std_mc.t.sem_pcci <- readRDS("summary_medsimple_mvn_std_mc.t.sem_pcci.Rds")
mvn_std_mc.mvn.tb_pcci <- readRDS("summary_medsimple_mvn_std_mc.mvn.tb_pcci.Rds")
mvn_std_mc.mvn.delta_pcci <- readRDS("summary_medsimple_mvn_std_mc.mvn.delta_pcci.Rds")
mvn_std_mc.mvn.sem_pcci <- readRDS("summary_medsimple_mvn_std_mc.mvn.sem_pcci.Rds")
mvn_std_nb_pcci <- readRDS("summary_medsimple_mvn_std_nb_pcci.Rds")
mvn_std_nb_bcci <- readRDS("summary_medsimple_mvn_std_nb_bcci.Rds")
mvn_std_nb_bcaci <- readRDS("summary_medsimple_mvn_std_nb_bcaci.Rds")
mvn_std_pb.mvn_pcci <- readRDS("summary_medsimple_mvn_std_pb.mvn_pcci.Rds")
mvn_std_pb.mvn_bcci <- readRDS("summary_medsimple_mvn_std_pb.mvn_bcci.Rds")
mvn_std_pb.mvn_bcaci <- readRDS("summary_medsimple_mvn_std_pb.mvn_bcaci.Rds")
results_mvn_std_ci <- rbind(
  # mvn_std_mc.t.tb_pcci,
  # mvn_std_mc.t.delta_pcci,
  # mvn_std_mc.t.sem_pcci,
  mvn_std_mc.mvn.delta_pcci,
  # mvn_std_mc.mvn.sem_pcci,
  # mvn_std_mc.mvn.tb_pcci,
  mvn_std_nb_pcci,
  # mvn_std_nb_bcci,
  # mvn_std_nb_bcaci,
  mvn_std_pb.mvn_pcci
  # mvn_std_pb.mvn_bcci,
  # mvn_std_pb.mvn_bcaci
)
head(results_mvn_std_ci)
str(results_mvn_std_ci)
#'
#+ usedata
usethis::use_data(
  results_mvn_std_ci,
  overwrite = TRUE
)
