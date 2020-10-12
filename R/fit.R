#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Fit Simple Mediation Model
#'
#' @description Fits the simple mediation model.
#'
#' @family model fit functions
#' @keywords fit
#' @import jeksterslabRlinreg
#' @inheritParams .fit
#' @inheritParams mc
#' @inheritParams pbmvn
#' @inherit .fit details
#' @examples
#' data <- jeksterslabRdatarepo::thirst
#' fit(data, par = FALSE)
#' @export
fit <- function(data,
                alpha = c(0.001, 0.01, 0.05),
                R = 5000,
                B = 5000,
                plot = TRUE,
                par = TRUE,
                ncores = NULL,
                blas_threads = TRUE,
                mc = TRUE,
                lb = FALSE) {
  x <- data[, 1]
  m <- data[, 2]
  y <- data[, 3]
  n <- nrow(data)
  # y ~ x + m -------------------------------------------------------------------
  X1 <- cbind(
    constant = 1,
    x,
    m
  )
  y1 <- y
  cat("\ny ~ x + m #######################################################\n")
  eq1 <- linreg(
    X = X1,
    y = y1
  )
  # m ~ x -----------------------------------------------------------------------
  X2 <- cbind(
    constant = 1,
    x
  )
  y2 <- m
  cat("\nm ~ x ###########################################################\n")
  eq2 <- linreg(
    X = X2,
    y = y2
  )
  betahat <- eq1[["betahat"]][["m"]]
  alphahat <- eq2[["betahat"]][["x"]]
  alphahatbetahat <- alphahat * betahat
  sehatbetahat <- eq1[["sehatbetahat"]][["m"]]
  sehatalphahat <- eq2[["sehatbetahat"]][["x"]]
  # indirect --------------------------------------------------------------------
  mcmvn <- mc(
    R = R,
    alphahat = alphahat,
    sehatalphahat = sehatalphahat,
    betahat = betahat,
    sehatbetahat = sehatbetahat,
    mvn = TRUE,
    n = nrow(data),
    alpha = alpha,
    plot = plot
  )
  mct <- mc(
    R = R,
    alphahat = alphahat,
    sehatalphahat = sehatalphahat,
    betahat = betahat,
    sehatbetahat = sehatbetahat,
    mvn = FALSE,
    n = nrow(data),
    alpha = alpha,
    plot = plot
  )
  pbmvn <- pbmvn(
    data = data,
    B = B,
    alphahatbetahat = alphahatbetahat,
    alpha = alpha,
    plot = plot,
    par = par,
    ncores = ncores,
    blas_threads = blas_threads,
    mc = mc,
    lb = lb
  )
  nb <- nb(
    data = data,
    B = B,
    alphahatbetahat = alphahatbetahat,
    alpha = alpha,
    plot = plot,
    par = par,
    ncores = ncores,
    blas_threads = blas_threads,
    mc = mc,
    lb = lb
  )
  indirect <- rbind(
    mcmvn,
    mct,
    pbmvn,
    nb
  )
  rownames(indirect) <- c(
    "mcmvn",
    "mct",
    "pbpc",
    "pbbc",
    "pbbca",
    "nbpc",
    "nbbc",
    "nbbca"
  )
  cat("\nIndirect Effect #################################################\n")
  print(
    indirect
  )
  invisible(
    list(
      eq1,
      eq2,
      indirect = indirect
    )
  )
}
