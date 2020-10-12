#' ---
#' title: "Data: Confidence Intervals - Multivariate Normal - Missing at Random"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' ---
#'
#+ data
mvn_mar_mc.mvn_pcci <- readRDS("summary_medsimple_mvn_mar_mc.mvn_pcci.Rds")
mvn_mar_nb_pcci <- readRDS("summary_medsimple_mvn_mar_nb_pcci.Rds")
mvn_mar_nb_bcci <- readRDS("summary_medsimple_mvn_mar_nb_bcci.Rds")
mvn_mar_pb.mvn_pcci <- readRDS("summary_medsimple_mvn_mar_pb.mvn_pcci.Rds")
mvn_mar_pb.mvn_bcci <- readRDS("summary_medsimple_mvn_mar_pb.mvn_bcci.Rds")

results_mvn_mar_ci <- rbind(
  mvn_mar_mc.mvn_pcci,
  mvn_mar_nb_pcci,
  mvn_mar_nb_bcci
  # mvn_mar_pb.mvn_pcci ,
  # mvn_mar_pb.mvn_bcci
)
head(results_mvn_mar_ci)
str(results_mvn_mar_ci)
#'
#+ usedata
usethis::use_data(
  results_mvn_mar_ci,
  overwrite = TRUE
)
