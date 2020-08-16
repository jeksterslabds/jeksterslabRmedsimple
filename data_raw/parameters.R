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
#'
#' ## Exponential
#'
#+
rate <- 1
paramsexp <- expand.grid(
  n = n,
  rate = rate,
  mux = 1 / rate,
  mum = 1 / rate,
  muy = 1 / rate,
  taudot = es,
  beta = sqrtes,
  alpha = sqrtes,
  sigma2x = 1 / rate^2,
  sigma2m = 1 / rate^2,
  sigma2y = 1 / rate^2,
  reps = reps
)
paramsexp <- data.frame(
  taskid = 1:nrow(paramsexp),
  paramsexp
)
usethis::use_data(
  paramsexp,
  overwrite = TRUE
)
#'
#' ## Poisson
#'
#+
lambda <- 1
paramspois <- expand.grid(
  n = n,
  lambda = lambda,
  mux = lambda,
  mum = lambda,
  muy = lambda,
  taudot = es,
  beta = sqrtes,
  alpha = sqrtes,
  sigma2x = lambda,
  sigma2m = lambda,
  sigma2y = lambda,
  reps = reps
)
paramspois <- data.frame(
  taskid = 1:nrow(paramspois),
  paramspois
)
usethis::use_data(
  paramspois,
  overwrite = TRUE
)
