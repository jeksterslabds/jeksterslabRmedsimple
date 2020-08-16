delta <- function(alphahat,
                  sehatalphahat,
                  betahat,
                  sehatbetahat,
                  std = FALSE,
                  sigmax = NULL,
                  sigmay = NULL) {
  alphahat2 <- alphahat^2
  betahat2 <- betahat^2
  sehatalphahat2 <- sehatalphahat^2
  sehatbetahat2 <- sehatbetahat^2
  Sobel <- sqrt(
    alphahat2 * sehatbetahat2 + betahat2 * sehatalphahat2
  )
  Aroaian <- sqrt(
    alphahat2 * sehatbetahat2 + betahat2 * sehatalphahat2 + sehatalphahat2 * sehatbetahat2
  )
  Goodman <- sqrt(
    alphahat2 * sehatbetahat2 + betahat2 * sehatalphahat2 - sehatalphahat2 * sehatbetahat2
  )
  out <- c(
    Sobel = Sobel,
    Aroaian = Aroaian,
    Goodman = Goodman
  )
  if (std) {
    sigmax_over_sigmay <- sigmax / sigmay
    Sobel <- sigmax_over_sigmay * Sobel
    Aroaian <- sigmax_over_sigmay * Aroaian
    Goodman <- sigmax_over_sigmay * Goodman
  }
  c(
    Sobel = Sobel,
    Aroaian = Aroaian,
    Goodman = Goodman
  )
}
