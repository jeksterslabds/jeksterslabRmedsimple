#' ---
#' title: "Data: Parameters"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' ---
#'
#+ common
es <- c(
  0.26,
  0.13,
  0.02,
  0.00
)
sqrtes <- sqrt(es)
n <- c(
  1000,
  500,
  250,
  200,
  150,
  100,
  75,
  50,
  20
)
reps <- 5000
#'
#' ## Multivariate Normal
#'
#+
paramsmvn <- expand.grid(
  n = n,
  mux = 100,
  mum = 100,
  muy = 100,
  taudot = es,
  beta = sqrtes,
  alpha = sqrtes,
  sigma2x = 15^2,
  sigma2m = 15^2,
  sigma2y = 15^2,
  reps = reps
)
paramsmvn <- data.frame(
  taskid = 1:nrow(paramsmvn),
  paramsmvn
)
usethis::use_data(
  paramsmvn,
  overwrite = TRUE
)
