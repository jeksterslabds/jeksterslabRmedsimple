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
  0.26^(1 / 4),
  0.13^(1 / 4),
  0.02^(1 / 4),
  0.00
)
taudot <- c(
  0.26^(1 / 2),
  0.13^(1 / 2),
  0.02^(1 / 2),
  0.00
)
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
  taudot = taudot,
  beta = es,
  alpha = es,
  sigma2x = 15^2,
  sigma2m = 15^2,
  sigma2y = 15^2,
  reps = reps
)
sigma2epsilonm <- paramsmvn$sigma2m - ((paramsmvn$alpha^2) * paramsmvn$sigma2x)
sigma2epsilony <- paramsmvn$sigma2y - ((paramsmvn$taudot^2) * paramsmvn$sigma2x) - ((paramsmvn$beta^2) * (paramsmvn$alpha^2) * paramsmvn$sigma2x) - ((paramsmvn$beta^2) * sigma2epsilonm) - (2 * paramsmvn$taudot * paramsmvn$beta * paramsmvn$alpha * paramsmvn$sigma2x)

paramsmvn <- data.frame(
  paramsmvn,
  sigma2epsilonm = sigma2epsilonm,
  sigma2epsilony = sigma2epsilony
)
paramsmvn <- paramsmvn[paramsmvn$sigma2epsilony >= 0, ]
paramsmvn <- paramsmvn[paramsmvn$sigma2epsilonm >= 0, ]
paramsmvn$sigma2epsilony <- NULL
paramsmvn$sigma2epsilonm <- NULL
rownames(paramsmvn) <- NULL
paramsmvn <- data.frame(
  taskid = 1:nrow(paramsmvn),
  paramsmvn
)
usethis::use_data(
  paramsmvn,
  overwrite = TRUE
)
#'
#' ## Exponential X
#'
#+
rate <- 1
paramsexp <- expand.grid(
  rate = rate,
  n = n,
  mux = 1 / rate,
  mum = 1 / rate,
  muy = 1 / rate,
  taudot = taudot,
  beta = es,
  alpha = es,
  sigma2x = 1 / (rate^2),
  sigma2m = 1 / (rate^2),
  sigma2y = 1 / (rate^2),
  reps = reps
)
sigma2epsilonm <- paramsexp$sigma2m - ((paramsexp$alpha^2) * paramsexp$sigma2x)
sigma2epsilony <- paramsexp$sigma2y - ((paramsexp$taudot^2) * paramsexp$sigma2x) - ((paramsexp$beta^2) * (paramsexp$alpha^2) * paramsexp$sigma2x) - ((paramsexp$beta^2) * sigma2epsilonm) - (2 * paramsexp$taudot * paramsexp$beta * paramsexp$alpha * paramsexp$sigma2x)

paramsexp <- data.frame(
  paramsexp,
  sigma2epsilonm = sigma2epsilonm,
  sigma2epsilony = sigma2epsilony
)
paramsexp <- paramsexp[paramsexp$sigma2epsilony >= 0, ]
paramsexp <- paramsexp[paramsexp$sigma2epsilonm >= 0, ]
paramsexp$sigma2epsilony <- NULL
paramsexp$sigma2epsilonm <- NULL
rownames(paramsexp) <- NULL
paramsexp <- data.frame(
  taskid = 1:nrow(paramsexp),
  paramsexp
)
usethis::use_data(
  paramsexp,
  overwrite = TRUE
)
