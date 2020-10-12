#' ---
#' title: "Data: Nonparametric Bootstrap Confidence Intervals - Data Generated from a Nonnormal Distribution (Vale and Maurelli (1983) - Skewness = 2, Kurtosis = 7) - Complete Data"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' ---
#'
#+ data
vm_mod_nb_pcci <- readRDS("summary_medsimple_vm_mod_nb_pcci.Rds")
vm_mod_nb_bcci <- readRDS("summary_medsimple_vm_mod_nb_bcci.Rds")
# vm_mod_nb_bcaci <- readRDS("summary_medsimple_vm_mod_nb_bcaci.Rds")
results_vm_mod_nb_ci <- rbind(
  vm_mod_nb_pcci,
  vm_mod_nb_bcci
  #  vm_mod_nb_bcaci
)
# subset
# results_vm_mod_nb_ci <- results_vm_mod_nb_ci[which(results_vm_mod_nb_ci$taskid < 145 | results_vm_mod_nb_ci$taskid > 153), ]
# results_vm_mod_nb_ci <- results_vm_mod_nb_ci[which(results_vm_mod_nb_ci$n > 20), ]
head(results_vm_mod_nb_ci)
str(results_vm_mod_nb_ci)
#'
#+ usedata
usethis::use_data(
  results_vm_mod_nb_ci,
  overwrite = TRUE
)
