#' Results: Simple Mediation Model - Multivariate Normal Distribution - Complete Data - Fit Structural Equation Modeling
#'
#' @family results
#' @keywords results
#' @inherit A details
#' @format A data frame with the following variables
#' \describe{
#'   \item{taskid}{
#'     Simulation task identification number.
#'   }
#'   \item{n}{
#'     Sample size.
#'   }
#'   \item{reps}{
#'     Monte Carlo replications.
#'   }
#'   \item{taudot}{
#'     Population slope of path from `x` to `y` \eqn{\left( \dot{\tau} \right)}.
#'   }
#'   \item{beta}{
#'     Population slope of path from `m` to `y` \eqn{\left( \beta \right)}.
#'   }
#'   \item{alpha}{
#'     Population slope of path from `x` to `m` \eqn{\left( \alpha \right)}.
#'   }
#'   \item{alphabeta}{
#'     Population indirect effect of `x` on `y` through `m` \eqn{\left( \alpha \beta \right)}.
#'   }
#'   \item{sigma2x}{
#'     Population variance of `x` \eqn{\left( \sigma_{x}^{2} \right)}.
#'   }
#'   \item{sigma2epsilonm}{
#'     Population error variance of `m` \eqn{\left( \sigma_{\varepsilon_{m}}^{2} \right)}.
#'   }
#'   \item{sigma2epsilony}{
#'     Population error variance of `y` \eqn{\left( \sigma_{\varepsilon_{y}}^{2} \right)}.
#'   }
#'   \item{mux}{
#'     Population mean of `x` \eqn{\left( \mu_x \right)}.
#'   }
#'   \item{deltam}{
#'     Population intercept of `m` \eqn{\left( \delta_m \right)}.
#'   }
#'   \item{deltay}{
#'     Population intercept of `y` \eqn{\left( \delta_y \right)}.
#'   }
#'   \item{taudothat}{
#'     Mean of estimated slope of path from `x` to `y` \eqn{\left( \hat{\dot{\tau}} \right)}.
#'   }
#'   \item{betahat}{
#'     Mean of estimated slope of path from `m` to `y` \eqn{\left( \hat{\beta} \right)}.
#'   }
#'   \item{alphahat}{
#'     Mean of estimated slope of path from `x` to `m` \eqn{\left( \hat{\alpha} \right)}.
#'   }
#'   \item{sigma2hatepsilonyhat}{
#'     Mean of estimated error variance of `y` \eqn{\left( \hat{\sigma}_{\varepsilon_{y}}^{2} \right)}.
#'   }
#'   \item{sigma2hatepsilonmhat}{
#'     Mean of estimated error variance of `m` \eqn{\left( \hat{\sigma}_{\varepsilon_{m}}^{2} \right)}.
#'   }
#'   \item{alphahatbetahat}{
#'     Mean of estimated indirect effect of `x` on `y` through `m` \eqn{\left( \hat{\alpha} \hat{\beta} \right)}.
#'   }
#'   \item{sehattaudothat}{
#'     Mean of estimated standard error of \eqn{\hat{\dot{\tau}}}.
#'   }
#'   \item{sehatbetahat}{
#'     Mean of estimated standard error of \eqn{\hat{\beta}}.
#'   }
#'   \item{sehatalphahat}{
#'     Mean of estimated standard error of \eqn{\hat{\alpha}}.
#'   }
#'   \item{sehatsigma2hatepsilonyhat}{
#'     Mean of estimated standard error of error variance of `y` \eqn{\left( \hat{\sigma}_{\varepsilon_{y}}^{2} \right)}.
#'   }
#'   \item{sehatsigma2hatepsilonmhat}{
#'     Mean of estimated standard error of error variance of `m` \eqn{\left( \hat{\sigma}_{\varepsilon_{m}}^{2} \right)}.
#'   }
#'   \item{theta}{
#'     Population parameter \eqn{\alpha \beta}.
#'   }
#'   \item{taudothat_var}{
#'     Variance of estimated slope of path from `x` to `y` \eqn{\left( \hat{\dot{\tau}} \right)}.
#'   }
#'   \item{betahat_var}{
#'     Variance of estimated slope of path from `m` to `y` \eqn{\left( \hat{\beta} \right)}.
#'   }
#'   \item{alphahat_var}{
#'     Variance of estimated slope of path from `x` to `m` \eqn{\left( \hat{\alpha} \right)}.
#'   }
#'   \item{alphahatbetahat_var}{
#'     Variance of estimated indirect effect of `x` on `y` through `m` \eqn{\left( \hat{\alpha} \hat{\beta} \right)}.
#'   }
#'   \item{taudothat_sd}{
#'     Standard deviation of estimated slope of path from `x` to `y` \eqn{\left( \hat{\dot{\tau}} \right)}.
#'   }
#'   \item{betahat_sd}{
#'     Standard deviation of estimated slope of path from `m` to `y` \eqn{\left( \hat{\beta} \right)}.
#'   }
#'   \item{alphahat_sd}{
#'     Standard deviation of estimated slope of path from `x` to `m` \eqn{\left( \hat{\alpha} \right)}.
#'   }
#'   \item{alphahatbetahat_sd}{
#'     Standard deviation of estimated indirect effect of `x` on `y` through `m` \eqn{\left( \hat{\alpha} \hat{\beta} \right)}.
#'   }
#'   \item{taudothat_skew}{
#'     Skewness of estimated slope of path from `x` to `y` \eqn{\left( \hat{\dot{\tau}} \right)}.
#'   }
#'   \item{betahat_skew}{
#'     Skewness of estimated slope of path from `m` to `y` \eqn{\left( \hat{\beta} \right)}.
#'   }
#'   \item{alphahat_skew}{
#'     Skewness of estimated slope of path from `x` to `m` \eqn{\left( \hat{\alpha} \right)}.
#'   }
#'   \item{alphahatbetahat_skew}{
#'     Skewness of estimated indirect effect of `x` on `y` through `m` \eqn{\left( \hat{\alpha} \hat{\beta} \right)}.
#'   }
#'   \item{taudothat_kurt}{
#'     Excess kurtosis of estimated slope of path from `x` to `y` \eqn{\left( \hat{\dot{\tau}} \right)}.
#'   }
#'   \item{betahat_kurt}{
#'     Excess kurtosis of estimated slope of path from `m` to `y` \eqn{\left( \hat{\beta} \right)}.
#'   }
#'   \item{alphahat_kurt}{
#'     Excess kurtosis of estimated slope of path from `x` to `m` \eqn{\left( \hat{\alpha} \right)}.
#'   }
#'   \item{alphahatbetahat_kurt}{
#'     Excess kurtosis of estimated indirect effect of `x` on `y` through `m` \eqn{\left( \hat{\alpha} \hat{\beta} \right)}.
#'   }
#'   \item{taudothat_bias}{
#'     Bias of estimated slope of path from `x` to `y` \eqn{\left( \hat{\dot{\tau}} \right)}.
#'   }
#'   \item{betahat_bias}{
#'     Bias of estimated slope of path from `m` to `y` \eqn{\left( \hat{\beta} \right)}.
#'   }
#'   \item{alphahat_bias}{
#'     Bias of estimated slope of path from `x` to `m` \eqn{\left( \hat{\alpha} \right)}.
#'   }
#'   \item{alphahatbetahat_bias}{
#'     Bias of estimated indirect effect of `x` on `y` through `m` \eqn{\left( \hat{\alpha} \hat{\beta} \right)}.
#'   }
#'   \item{taudothat_mse}{
#'     Mean square error of estimated slope of path from `x` to `y` \eqn{\left( \hat{\dot{\tau}} \right)}.
#'   }
#'   \item{betahat_mse}{
#'     Mean square error of estimated slope of path from `m` to `y` \eqn{\left( \hat{\beta} \right)}.
#'   }
#'   \item{alphahat_mse}{
#'     Mean square error of estimated slope of path from `x` to `m` \eqn{\left( \hat{\alpha} \right)}.
#'   }
#'   \item{alphahatbetahat_mse}{
#'     Mean square error of estimated indirect effect of `x` on `y` through `m` \eqn{\left( \hat{\alpha} \hat{\beta} \right)}.
#'   }
#'   \item{taudothat_rmse}{
#'     Root mean square error of estimated slope of path from `x` to `y` \eqn{\left( \hat{\dot{\tau}} \right)}.
#'   }
#'   \item{betahat_rmse}{
#'     Root mean square error of estimated slope of path from `m` to `y` \eqn{\left( \hat{\beta} \right)}.
#'   }
#'   \item{alphahat_rmse}{
#'     Root mean square error of estimated slope of path from `x` to `m` \eqn{\left( \hat{\alpha} \right)}.
#'   }
#'   \item{alphahatbetahat_rmse}{
#'     Root mean square error of estimated indirect effect of `x` on `y` through `m` \eqn{\left( \hat{\alpha} \hat{\beta} \right)}.
#'   }
#'   \item{missing}{
#'     Type of missingness.
#'   }
#'   \item{std}{
#'     Standardized vs. unstandardize indirect effect.
#'   }
#'   \item{Method}{
#'     Method used. Fit in this case.
#'   }
#'   \item{n_label}{
#'     Sample size labels.
#'   }
#'   \item{alpha_label}{
#'     \eqn{\alpha} labels.
#'   }
#'   \item{beta_label}{
#'     \eqn{\beta} labels.
#'   }
#'   \item{taudot_label}{
#'     \eqn{\dot{\tau}} labels.
#'   }
#'   \item{theta_label}{
#'     \eqn{\theta} labels.
#'   }
#' }
#' @examples
#' data(results_mvn_fit.sem, package = "jeksterslabRmedsimple")
#' head(results_mvn_fit.sem)
#' str(results_mvn_fit.sem)
"results_mvn_fit.sem"
