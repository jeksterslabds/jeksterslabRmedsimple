#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Generate Labels
#'
#' @family process results functions
#' @keywords process
#' @param out Object.
#'   Output of `*_summary`.
#' @param method Character string.
#'   Name of the method used.
#' @param model Character string.
#'   Model used.
#' @param std Logical.
#'   If `TRUE`, standardized indirect effect.
#'   If `FALSE`, unstandardized indirect effect.
#' @param missing Character string.
#'   Type of missingness.
#'   `missing = "NON"` for complete data.
#'   `missing = "MCAR"` for missing completely at random.
#'   `missingess = "MAR"` for missing at random.
#' @export
label <- function(out,
                  method,
                  model,
                  std,
                  missing = "NON") {
  out <- as.data.frame(out)
  out$missing <- missing
  if (std) {
    out$std <- "Standardized"
  } else {
    out$std <- "Unstandardized"
  }
  out$Method <- method
  out$n_label <- paste0(
    "n: ",
    out$n
  )
  out$n_label <- factor(
    out$n_label,
    levels = c(
      "n: 20",
      "n: 50",
      "n: 75",
      "n: 100",
      "n: 150",
      "n: 200",
      "n: 250",
      "n: 500",
      "n: 1000"
    )
  )
  out$alpha_label <- paste0(
    "\u03b1: ",
    sprintf("%.02f", out$alpha)
  )
  out$alpha_label <- factor(
    out$alpha_label,
    levels = sort(
      unique(
        out$alpha_label
      )
    )
  )
  out$beta_label <- paste0(
    "\u03b2: ",
    sprintf("%.02f", out$beta)
  )
  out$beta_label <- factor(
    out$beta_label,
    levels = sort(
      unique(
        out$beta_label
      )
    )
  )
  out$taudot_label <- paste0(
    "\u03C4\u0307: ",
    sprintf("%.02f", out$taudot)
  )
  out$taudot_label <- factor(
    out$taudot_label,
    levels = sort(
      unique(
        out$taudot_label
      )
    )
  )
  out$theta_label <- paste0(
    sprintf("%.02f", out$theta),
    "(",
    sprintf("%.02f", out$alpha),
    ",",
    sprintf("%.02f", out$beta),
    ")"
  )
  out
}
