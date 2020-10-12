#' ---
#' title: "Data: Confidence Intervals - Multivariate Normal - Missing Not at Random"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' ---
#'
#+ data
mvn_mnar_mc.mvn_pcci <- readRDS("summary_medsimple_mvn_mnar_mc.mvn_pcci.Rds")
# mvn_mnar_nb_pcci <- readRDS("summary_medsimple_mvn_mnar_nb_pcci.Rds")
# mvn_mnar_nb_bcci <- readRDS("summary_medsimple_mvn_mnar_nb_bcci.Rds")
# mvn_mnar_pb.mvn_pcci <- readRDS("summary_medsimple_mvn_mnar_pb.mvn_pcci.Rds")
# mvn_mnar_pb.mvn_bcci <- readRDS("summary_medsimple_mvn_mnar_pb.mvn_bcci.Rds")

results_mvn_mnar_ci <- mvn_mnar_mc.mvn_pcci

# results_mvn_mnar_ci <- rbind(
#  mvn_mnar_mc.mvn_pcci,
#  mvn_mnar_nb_pcci,
#  mvn_mnar_nb_bcci
# mvn_mnar_pb.mvn_pcci ,
# mvn_mnar_pb.mvn_bcci
# )
head(results_mvn_mnar_ci)
str(results_mvn_mnar_ci)
#'
#+ usedata
usethis::use_data(
  results_mvn_mnar_ci,
  overwrite = TRUE
)
