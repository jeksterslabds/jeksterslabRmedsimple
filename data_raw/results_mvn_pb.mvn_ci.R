#' ---
#' title: "Data: Parametric Bootstrap Confidence Intervals - Multivariate Normal - Complete Data"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' ---
#'
#+ data
mvn_pb.mvn_pcci <- readRDS("summary_medsimple_mvn_pb.mvn_pcci.Rds")
mvn_pb.mvn_bcci <- readRDS("summary_medsimple_mvn_pb.mvn_bcci.Rds")
mvn_pb.mvn_bcaci <- readRDS("summary_medsimple_mvn_pb.mvn_bcaci.Rds")
results_mvn_pb.mvn_ci <- rbind(
  mvn_pb.mvn_pcci,
  mvn_pb.mvn_bcci,
  mvn_pb.mvn_bcaci
)
# subset
# results_mvn_pb.mvn_ci <- results_mvn_pb.mvn_ci[which(results_mvn_pb.mvn_ci$taskid < 145 | results_mvn_pb.mvn_ci$taskid > 153), ]
# results_mvn_pb.mvn_ci <- results_mvn_pb.mvn_ci[which(results_mvn_pb.mvn_ci$n > 20), ]
head(results_mvn_pb.mvn_ci)
str(results_mvn_pb.mvn_ci)
#'
#+ usedata
usethis::use_data(
  results_mvn_pb.mvn_ci,
  overwrite = TRUE
)
