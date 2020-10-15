#' ---
#' title: "Data: Simple Mediation Model - Multivariate Normal Distribution - Data Missing Completely at Random - Parametric Bootstrap Confidence Intervals using Full Information Maximum Likelihood"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' vignette: >
#'   %\VignetteIndexEntry{Data: Simple Mediation Model - Multivariate Normal Distribution - Data Missing Completely at Random - Parametric Bootstrap Confidence Intervals using Full Information Maximum Likelihood}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' ---
#'
#+ data
mvn_mcar_10_pb.mvn_pcci <- readRDS("summary_medsimple_mvn_mcar_10_pb.mvn_pcci.Rds")
mvn_mcar_20_pb.mvn_pcci <- readRDS("summary_medsimple_mvn_mcar_20_pb.mvn_pcci.Rds")
mvn_mcar_30_pb.mvn_pcci <- readRDS("summary_medsimple_mvn_mcar_30_pb.mvn_pcci.Rds")
mvn_mcar_10_pb.mvn_bcci <- readRDS("summary_medsimple_mvn_mcar_10_pb.mvn_bcci.Rds")
mvn_mcar_20_pb.mvn_bcci <- readRDS("summary_medsimple_mvn_mcar_20_pb.mvn_bcci.Rds")
mvn_mcar_30_pb.mvn_bcci <- readRDS("summary_medsimple_mvn_mcar_30_pb.mvn_bcci.Rds")
results_mvn_mcar_pb.mvn_ci <- rbind(
  mvn_mcar_10_pb.mvn_pcci,
  mvn_mcar_20_pb.mvn_pcci,
  mvn_mcar_30_pb.mvn_pcci,
  mvn_mcar_10_pb.mvn_bcci,
  mvn_mcar_20_pb.mvn_bcci,
  mvn_mcar_30_pb.mvn_bcci
)
# subset
# results_mvn_mcar_pb.mvn_ci <- results_mvn_mcar_pb.mvn_ci[which(results_mvn_mcar_pb.mvn_ci$taskid < 145 | results_mvn_mcar_pb.mvn_ci$taskid > 153), ]
# results_mvn_mcar_pb.mvn_ci <- results_mvn_mcar_pb.mvn_ci[which(results_mvn_mcar_pb.mvn_ci$n > 20), ]
head(results_mvn_mcar_pb.mvn_ci)
str(results_mvn_mcar_pb.mvn_ci)
#'
#+ usedata
usethis::use_data(
  results_mvn_mcar_pb.mvn_ci,
  overwrite = TRUE
)
