#' Results: Simple Mediation Model - Vale and Maurelli (1983) - Skewness = 2, Kurtosis = 7 - Complete Data - Fit Ordinary Least Squares
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
#'   \item{deltayhat}{
#'     Mean of estimated intercept of `y` \eqn{\left( \hat{\delta}_y \right)}.
#'   }
#'   \item{taudothat}{
#'     Mean of estimated slope of path from `x` to `y` \eqn{\left( \hat{\dot{\tau}} \right)}.
#'   }
#'   \item{betahat}{
#'     Mean of estimated slope of path from `m` to `y` \eqn{\left( \hat{\beta} \right)}.
#'   }
#'   \item{deltamhat}{
#'     Mean of estimated intercept of `m` \eqn{\left( \hat{\delta}_{m} \right)}.
#'   }
#'   \item{alphahat}{
#'     Mean of estimated slope of path from `x` to `m` \eqn{\left( \hat{\alpha} \right)}.
#'   }
#'   \item{alphahatbetahat}{
#'     Mean of estimated indirect effect of `x` on `y` through `m` \eqn{\left( \hat{\alpha} \hat{\beta} \right)}.
#'   }
#'   \item{taudothatprime}{
#'     Mean of estimated standardized slope of path from `x` to `y` \eqn{\left( \hat{\dot{\tau}}^{\prime} \right)}.
#'   }
#'   \item{betahatprime}{
#'     Mean of estimated standardized slope of path from `m` to `y` \eqn{\left( \hat{\beta}^{\prime} \right)}.
#'   }
#'   \item{alphahatprime}{
#'     Mean of estimated standardized slope of path from `x` to `m` \eqn{\left( \hat{\alpha}^{\prime} \right)}.
#'   }
#'   \item{alphahatprimebetahatprime}{
#'     Mean of estimated standardized indirect effect of `x` on `y` through `m` \eqn{\left( \hat{\alpha}^{\prime} \hat{\beta}^{\prime} \right)}.
#'   }
#'   \item{sigma2xhat}{
#'     Mean of estimated variance of `x` \eqn{\left( \hat{\sigma}_{x}^{2} \right)}.
#'   }
#'   \item{sigma2hatepsilonmhat}{
#'     Mean of estimated error variance of `m` \eqn{\left( \hat{\sigma}_{\varepsilon_{m}}^{2} \right)}.
#'   }
#'   \item{sigma2hatepsilonyhat}{
#'     Mean of estimated error variance of `y` \eqn{\left( \hat{\sigma}_{\varepsilon_{y}}^{2} \right)}.
#'   }
#'   \item{muxhat}{
#'     Mean of estimated mean of `x` \eqn{\left( \hat{\mu}_x \right)}.
#'   }
#'   \item{sehatdeltayhat}{
#'     Mean of estimated standard error of \eqn{\hat{\delta}_{y}}.
#'   }
#'   \item{sehattaudothat}{
#'     Mean of estimated standard error of \eqn{\hat{\dot{\tau}}}.
#'   }
#'   \item{sehatbetahat}{
#'     Mean of estimated standard error of \eqn{\hat{\beta}}.
#'   }
#'   \item{sehatdeltamhat}{
#'     Mean of estimated standard error of \eqn{\hat{\delta}_{m}}.
#'   }
#'   \item{sehatalphahat}{
#'     Mean of estimated standard error of \eqn{\hat{\alpha}}.
#'   }
#'   \item{sehattaudothatprimetb}{
#'     Mean of estimated textbook standard error of \eqn{\hat{\dot{\tau}}^{\prime}}.
#'   }
#'   \item{sehatbetahatprimetb}{
#'     Mean of estimated textbook standard error of \eqn{\hat{\beta}^{\prime}}.
#'   }
#'   \item{sehatalphahatprimetb}{
#'     Mean of estimated textbook standard error of \eqn{\hat{\alpha}^{\prime}}.
#'   }
#'   \item{sehattaudothatprimedelta}{
#'     Mean of estimated delta method standard error of \eqn{\hat{\dot{\tau}}^{\prime}}.
#'   }
#'   \item{sehatbetahatprimedelta}{
#'     Mean of estimated delta method standard error of \eqn{\hat{\beta}^{\prime}}.
#'   }
#'   \item{sehatalphahatprimedelta}{
#'     Mean of estimated delta method standard error of \eqn{\hat{\alpha}^{\prime}}.
#'   }
#'   \item{theta}{
#'     Population parameter \eqn{\alpha \beta}.
#'   }
#'   \item{deltayhat_var}{
#'     Variance of estimated intercept of `y` \eqn{\left( \hat{\delta}_y \right)}.
#'   }
#'   \item{taudothat_var}{
#'     Variance of estimated slope of path from `x` to `y` \eqn{\left( \hat{\dot{\tau}} \right)}.
#'   }
#'   \item{betahat_var}{
#'     Variance of estimated slope of path from `m` to `y` \eqn{\left( \hat{\beta} \right)}.
#'   }
#'   \item{deltamhat_var}{
#'     Variance of estimated intercept of `m` \eqn{\left( \hat{\delta}_{m} \right)}.
#'   }
#'   \item{alphahat_var}{
#'     Variance of estimated slope of path from `x` to `m` \eqn{\left( \hat{\alpha} \right)}.
#'   }
#'   \item{alphahatbetahat_var}{
#'     Variance of estimated indirect effect of `x` on `y` through `m` \eqn{\left( \hat{\alpha} \hat{\beta} \right)}.
#'   }
#'   \item{taudothatprime_var}{
#'     Variance of estimated standardized slope of path from `x` to `y` \eqn{\left( \hat{\dot{\tau}}^{\prime} \right)}.
#'   }
#'   \item{betahatprime_var}{
#'     Variance of estimated standardized slope of path from `m` to `y` \eqn{\left( \hat{\beta}^{\prime} \right)}.
#'   }
#'   \item{alphahatprime_var}{
#'     Variance of estimated standardized slope of path from `x` to `m` \eqn{\left( \hat{\alpha}^{\prime} \right)}.
#'   }
#'   \item{alphahatprimebetahatprime_var}{
#'     Variance of estimated standardized indirect effect of `x` on `y` through `m` \eqn{\left( \hat{\alpha}^{\prime} \hat{\beta}^{\prime} \right)}.
#'   }
#'   \item{deltayhat_sd}{
#'     Standard deviation of estimated intercept of `y` \eqn{\left( \hat{\delta}_y \right)}.
#'   }
#'   \item{taudothat_sd}{
#'     Standard deviation of estimated slope of path from `x` to `y` \eqn{\left( \hat{\dot{\tau}} \right)}.
#'   }
#'   \item{betahat_sd}{
#'     Standard deviation of estimated slope of path from `m` to `y` \eqn{\left( \hat{\beta} \right)}.
#'   }
#'   \item{deltamhat_sd}{
#'     Standard deviation of estimated intercept of `m` \eqn{\left( \hat{\delta}_{m} \right)}.
#'   }
#'   \item{alphahat_sd}{
#'     Standard deviation of estimated slope of path from `x` to `m` \eqn{\left( \hat{\alpha} \right)}.
#'   }
#'   \item{alphahatbetahat_sd}{
#'     Standard deviation of estimated indirect effect of `x` on `y` through `m` \eqn{\left( \hat{\alpha} \hat{\beta} \right)}.
#'   }
#'   \item{taudothatprime_sd}{
#'     Standard deviation of estimated standardized slope of path from `x` to `y` \eqn{\left( \hat{\dot{\tau}}^{\prime} \right)}.
#'   }
#'   \item{betahatprime_sd}{
#'     Standard deviation of estimated standardized slope of path from `m` to `y` \eqn{\left( \hat{\beta}^{\prime} \right)}.
#'   }
#'   \item{alphahatprime_sd}{
#'     Standard deviation of estimated standardized slope of path from `x` to `m` \eqn{\left( \hat{\alpha}^{\prime} \right)}.
#'   }
#'   \item{alphahatprimebetahatprime_sd}{
#'     Standard deviation of estimated standardized indirect effect of `x` on `y` through `m` \eqn{\left( \hat{\alpha}^{\prime} \hat{\beta}^{\prime} \right)}.
#'   }
#'   \item{deltayhat_skew}{
#'     Skewness of estimated intercept of `y` \eqn{\left( \hat{\delta}_y \right)}.
#'   }
#'   \item{taudothat_skew}{
#'     Skewness of estimated slope of path from `x` to `y` \eqn{\left( \hat{\dot{\tau}} \right)}.
#'   }
#'   \item{betahat_skew}{
#'     Skewness of estimated slope of path from `m` to `y` \eqn{\left( \hat{\beta} \right)}.
#'   }
#'   \item{deltamhat_skew}{
#'     Skewness of estimated intercept of `m` \eqn{\left( \hat{\delta}_{m} \right)}.
#'   }
#'   \item{alphahat_skew}{
#'     Skewness of estimated slope of path from `x` to `m` \eqn{\left( \hat{\alpha} \right)}.
#'   }
#'   \item{alphahatbetahat_skew}{
#'     Skewness of estimated indirect effect of `x` on `y` through `m` \eqn{\left( \hat{\alpha} \hat{\beta} \right)}.
#'   }
#'   \item{taudothatprime_skew}{
#'     Skewness of estimated standardized slope of path from `x` to `y` \eqn{\left( \hat{\dot{\tau}}^{\prime} \right)}.
#'   }
#'   \item{betahatprime_skew}{
#'     Skewness of estimated standardized slope of path from `m` to `y` \eqn{\left( \hat{\beta}^{\prime} \right)}.
#'   }
#'   \item{alphahatprime_skew}{
#'     Skewness of estimated standardized slope of path from `x` to `m` \eqn{\left( \hat{\alpha}^{\prime} \right)}.
#'   }
#'   \item{alphahatprimebetahatprime_skew}{
#'     Skewness of estimated standardized indirect effect of `x` on `y` through `m` \eqn{\left( \hat{\alpha}^{\prime} \hat{\beta}^{\prime} \right)}.
#'   }
#'   \item{deltayhat_kurt}{
#'     Excess kurtosis of estimated intercept of `y` \eqn{\left( \hat{\delta}_y \right)}.
#'   }
#'   \item{taudothat_kurt}{
#'     Excess kurtosis of estimated slope of path from `x` to `y` \eqn{\left( \hat{\dot{\tau}} \right)}.
#'   }
#'   \item{betahat_kurt}{
#'     Excess kurtosis of estimated slope of path from `m` to `y` \eqn{\left( \hat{\beta} \right)}.
#'   }
#'   \item{deltamhat_kurt}{
#'     Excess kurtosis of estimated intercept of `m` \eqn{\left( \hat{\delta}_{m} \right)}.
#'   }
#'   \item{alphahat_kurt}{
#'     Excess kurtosis of estimated slope of path from `x` to `m` \eqn{\left( \hat{\alpha} \right)}.
#'   }
#'   \item{alphahatbetahat_kurt}{
#'     Excess kurtosis of estimated indirect effect of `x` on `y` through `m` \eqn{\left( \hat{\alpha} \hat{\beta} \right)}.
#'   }
#'   \item{taudothatprime_kurt}{
#'     Excess kurtosis of estimated standardized slope of path from `x` to `y` \eqn{\left( \hat{\dot{\tau}}^{\prime} \right)}.
#'   }
#'   \item{betahatprime_kurt}{
#'     Excess kurtosis of estimated standardized slope of path from `m` to `y` \eqn{\left( \hat{\beta}^{\prime} \right)}.
#'   }
#'   \item{alphahatprime_kurt}{
#'     Excess kurtosis of estimated standardized slope of path from `x` to `m` \eqn{\left( \hat{\alpha}^{\prime} \right)}.
#'   }
#'   \item{alphahatprimebetahatprime_kurt}{
#'     Excess kurtosis of estimated standardized indirect effect of `x` on `y` through `m` \eqn{\left( \hat{\alpha}^{\prime} \hat{\beta}^{\prime} \right)}.
#'   }
#'   \item{deltayhat_bias}{
#'     Bias of estimated intercept of `y` \eqn{\left( \hat{\delta}_y \right)}.
#'   }
#'   \item{taudothat_bias}{
#'     Bias of estimated slope of path from `x` to `y` \eqn{\left( \hat{\dot{\tau}} \right)}.
#'   }
#'   \item{betahat_bias}{
#'     Bias of estimated slope of path from `m` to `y` \eqn{\left( \hat{\beta} \right)}.
#'   }
#'   \item{deltamhat_bias}{
#'     Bias of estimated intercept of `m` \eqn{\left( \hat{\delta}_{m} \right)}.
#'   }
#'   \item{alphahat_bias}{
#'     Bias of estimated slope of path from `x` to `m` \eqn{\left( \hat{\alpha} \right)}.
#'   }
#'   \item{alphahatbetahat_bias}{
#'     Bias of estimated indirect effect of `x` on `y` through `m` \eqn{\left( \hat{\alpha} \hat{\beta} \right)}.
#'   }
#'   \item{taudothatprime_bias}{
#'     Bias of estimated standardized slope of path from `x` to `y` \eqn{\left( \hat{\dot{\tau}}^{\prime} \right)}.
#'   }
#'   \item{betahatprime_bias}{
#'     Bias of estimated standardized slope of path from `m` to `y` \eqn{\left( \hat{\beta}^{\prime} \right)}.
#'   }
#'   \item{alphahatprime_bias}{
#'     Bias of estimated standardized slope of path from `x` to `m` \eqn{\left( \hat{\alpha}^{\prime} \right)}.
#'   }
#'   \item{alphahatprimebetahatprime_bias}{
#'     Bias of estimated standardized indirect effect of `x` on `y` through `m` \eqn{\left( \hat{\alpha}^{\prime} \hat{\beta}^{\prime} \right)}.
#'   }
#'   \item{deltayhat_mse}{
#'     Mean square error of estimated intercept of `y` \eqn{\left( \hat{\delta}_y \right)}.
#'   }
#'   \item{taudothat_mse}{
#'     Mean square error of estimated slope of path from `x` to `y` \eqn{\left( \hat{\dot{\tau}} \right)}.
#'   }
#'   \item{betahat_mse}{
#'     Mean square error of estimated slope of path from `m` to `y` \eqn{\left( \hat{\beta} \right)}.
#'   }
#'   \item{deltamhat_mse}{
#'     Mean square error of estimated intercept of `m` \eqn{\left( \hat{\delta}_{m} \right)}.
#'   }
#'   \item{alphahat_mse}{
#'     Mean square error of estimated slope of path from `x` to `m` \eqn{\left( \hat{\alpha} \right)}.
#'   }
#'   \item{alphahatbetahat_mse}{
#'     Mean square error of estimated indirect effect of `x` on `y` through `m` \eqn{\left( \hat{\alpha} \hat{\beta} \right)}.
#'   }
#'   \item{taudothatprime_mse}{
#'     Mean square error of estimated standardized slope of path from `x` to `y` \eqn{\left( \hat{\dot{\tau}}^{\prime} \right)}.
#'   }
#'   \item{betahatprime_mse}{
#'     Mean square error of estimated standardized slope of path from `m` to `y` \eqn{\left( \hat{\beta}^{\prime} \right)}.
#'   }
#'   \item{alphahatprime_mse}{
#'     Mean square error of estimated standardized slope of path from `x` to `m` \eqn{\left( \hat{\alpha}^{\prime} \right)}.
#'   }
#'   \item{alphahatprimebetahatprime_mse}{
#'     Mean square error of estimated standardized indirect effect of `x` on `y` through `m` \eqn{\left( \hat{\alpha}^{\prime} \hat{\beta}^{\prime} \right)}.
#'   }
#'   \item{deltayhat_rmse}{
#'     Root mean square error of estimated intercept of `y` \eqn{\left( \hat{\delta}_y \right)}.
#'   }
#'   \item{taudothat_rmse}{
#'     Root mean square error of estimated slope of path from `x` to `y` \eqn{\left( \hat{\dot{\tau}} \right)}.
#'   }
#'   \item{betahat_rmse}{
#'     Root mean square error of estimated slope of path from `m` to `y` \eqn{\left( \hat{\beta} \right)}.
#'   }
#'   \item{deltamhat_rmse}{
#'     Root mean square error of estimated intercept of `m` \eqn{\left( \hat{\delta}_{m} \right)}.
#'   }
#'   \item{alphahat_rmse}{
#'     Root mean square error of estimated slope of path from `x` to `m` \eqn{\left( \hat{\alpha} \right)}.
#'   }
#'   \item{alphahatbetahat_rmse}{
#'     Root mean square error of estimated indirect effect of `x` on `y` through `m` \eqn{\left( \hat{\alpha} \hat{\beta} \right)}.
#'   }
#'   \item{taudothatprime_rmse}{
#'     Root mean square error of estimated standardized slope of path from `x` to `y` \eqn{\left( \hat{\dot{\tau}}^{\prime} \right)}.
#'   }
#'   \item{betahatprime_rmse}{
#'     Root mean square error of estimated standardized slope of path from `m` to `y` \eqn{\left( \hat{\beta}^{\prime} \right)}.
#'   }
#'   \item{alphahatprime_rmse}{
#'     Root mean square error of estimated standardized slope of path from `x` to `m` \eqn{\left( \hat{\alpha}^{\prime} \right)}.
#'   }
#'   \item{alphahatprimebetahatprime_rmse}{
#'     Root mean square error of estimated standardized indirect effect of `x` on `y` through `m` \eqn{\left( \hat{\alpha}^{\prime} \hat{\beta}^{\prime} \right)}.
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
#' data(results_vm_mod_fit.ols, package = "jeksterslabRmedsimple")
#' head(results_vm_mod_fit.ols)
#' str(results_vm_mod_fit.ols)
"results_vm_mod_fit.ols"
