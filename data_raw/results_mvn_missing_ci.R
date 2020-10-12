#' ---
#' title: "Data: Confidence Intervals - Multivariate Normal - Missing Data"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' ---
#'
#+ data
mc.mvn_pcci <- readRDS("summary_medsimple_mvn_mc.mvn_pcci.Rds")
mcar_10_mc.mvn_pcci <- readRDS("summary_medsimple_mvn_mcar_10_mc.mvn_pcci.Rds")
mcar_20_mc.mvn_pcci <- readRDS("summary_medsimple_mvn_mcar_20_mc.mvn_pcci.Rds")
mcar_30_mc.mvn_pcci <- readRDS("summary_medsimple_mvn_mcar_30_mc.mvn_pcci.Rds")
mar_10_mc.mvn_pcci <- readRDS("summary_medsimple_mvn_mar_10_mc.mvn_pcci.Rds")
mar_20_mc.mvn_pcci <- readRDS("summary_medsimple_mvn_mar_20_mc.mvn_pcci.Rds")
mar_30_mc.mvn_pcci <- readRDS("summary_medsimple_mvn_mar_30_mc.mvn_pcci.Rds")
mnar_10_mc.mvn_pcci <- readRDS("summary_medsimple_mvn_mnar_10_mc.mvn_pcci.Rds")
mnar_20_mc.mvn_pcci <- readRDS("summary_medsimple_mvn_mnar_20_mc.mvn_pcci.Rds")
mnar_30_mc.mvn_pcci <- readRDS("summary_medsimple_mvn_mnar_30_mc.mvn_pcci.Rds")
mar_10_nbpc.fiml <- readRDS("summary_medsimple_mvn_mar_10_nbpc.fiml.Rds")
mar_30_nbpc.fiml <- readRDS("summary_medsimple_mvn_mar_30_nbpc.fiml.Rds")
mcar_10_nbpc.fiml <- readRDS("summary_medsimple_mvn_mcar_10_nbpc.fiml.Rds")
mcar_30_nbpc.fiml <- readRDS("summary_medsimple_mvn_mcar_30_nbpc.fiml.Rds")
mnar_10_nbpc.fiml <- readRDS("summary_medsimple_mvn_mnar_10_nbpc.fiml.Rds")
mnar_30_nbpc.fiml <- readRDS("summary_medsimple_mvn_mnar_30_nbpc.fiml.Rds")



results_mvn_missing_ci <- rbind(
  mc.mvn_pcci,
  mcar_10_mc.mvn_pcci,
  mcar_20_mc.mvn_pcci,
  mcar_30_mc.mvn_pcci,
  mar_10_mc.mvn_pcci,
  mar_20_mc.mvn_pcci,
  mar_30_mc.mvn_pcci,
  mnar_10_mc.mvn_pcci,
  mnar_20_mc.mvn_pcci,
  mnar_30_mc.mvn_pcci,
  mar_10_nbpc.fiml,
  mar_30_nbpc.fiml,
  mcar_10_nbpc.fiml,
  mcar_30_nbpc.fiml,
  mnar_10_nbpc.fiml,
  mnar_30_nbpc.fiml
)
head(results_mvn_missing_ci)
str(results_mvn_missing_ci)
#'
#+ usedata
usethis::use_data(
  results_mvn_missing_ci,
  overwrite = TRUE
)
