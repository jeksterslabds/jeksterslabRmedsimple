#' Results: Fit Standardized Simple Mediation Model for Data Generated from a Multivariate Normal Distribution with Data Missing Completely at Random - Structural Equation Modeling
#'
#' @details The standardized simple mediation model is given by the following measurement model and regression model.
#'
#'   Measurement model
#'     \deqn{x_{\mathrm{latent}} = \lambda_x x}
#'     \deqn{m_{\mathrm{latent}} = \lambda_m m}
#'     \deqn{y_{\mathrm{latent}} = \lambda_y y}
#'
#'   Regression model
#'     \deqn{y_{\mathrm{latent}} = \dot{\tau}^{\prime} x_{\mathrm{latent}} + \beta^{\prime} m_{\mathrm{latent}} + \varepsilon_{y_{\mathrm{latent}}}}
#'     \deqn{m_{\mathrm{latent}} = \alpha^{\prime} x_{\mathrm{latent}} + \varepsilon_{m_{\mathrm{latent}}}}
#'
#'   - The measurement errors in the measurement model are fixed to `0`
#'   - The variance of \eqn{x_{\mathrm{latent}}} \eqn{\left( \sigma_{x_{\mathrm{latent}}}^{2} \right)} is fixed to `1`
#'   - The error variance \eqn{\varepsilon_{y_{\mathrm{latent}}}} \eqn{\left( \sigma_{\varepsilon_{y_{\mathrm{latent}}}}^{2} \right)}
#'     is constrained to \eqn{1 - \dot{\tau}^{\prime 2} - \beta^{\prime 2} -  2 \dot{\tau}^{\prime} \beta^{\prime} \alpha^{\prime}}
#'   - The error variance \eqn{\varepsilon_{m_{\mathrm{latent}}}} \eqn{\left( \sigma_{\varepsilon_{m_{\mathrm{latent}}}}^{2} \right)}
#'     is constrained to \eqn{1 - \alpha^{\prime 2}}
#'
#' @family results functions
#' @keywords results
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
#'   \item{lambdaxhat}{
#'     Mean of estimated factor loading `xlatent ~ x` \eqn{ \left( \lambda_x \right)}. Numerically equivalent to the standard deviation of `x`.
#'   }
#'   \item{lambdamhat}{
#'     Mean of estimated factor loading `mlatent ~ m` \eqn{ \left( \lambda_m \right)}. Numerically equivalent to the standard deviation of `m`.
#'   }
#'   \item{lambdayhat}{
#'     Mean of estimated factor loading `ylatent ~ y` \eqn{ \left( \lambda_y \right)}. Numerically equivalent to the standard deviation of `y`.
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
#'   \item{sigma2hatepsilonylatenthat}{
#'     Mean of estimated error variance of `y` \eqn{\left( \hat{\sigma}_{\varepsilon_{y_{\mathrm{latent}}}}^{2} \right)}.
#'   }
#'   \item{sigma2hatepsilonmlatenthat}{
#'     Mean of estimated error variance of `m` \eqn{\left( \hat{\sigma}_{\varepsilon_{m_{\mathrm{latent}}}}^{2} \right)}.
#'   }


#'   \item{muyhat}{
#'     Mean of estimated mean of `y` \eqn{\left( \hat{\mu}_y \right)}.
#'   }
#'   \item{mumhat}{
#'     Mean of estimated mean of `m` \eqn{\left( \hat{\mu}_m \right)}.
#'   }
#'   \item{muxhat}{
#'     Mean of estimated mean of `x` \eqn{\left( \hat{\mu}_x \right)}.
#'   }




#'   \item{alphahatprimebetahatprime}{
#'     Mean of estimated standardized indirect effect of `x` on `y` through `m` \eqn{\left( \hat{\alpha}^{\prime} \hat{\beta}^{\prime} \right)}.
#'   }
#'   \item{sehatlambdaxhat}{
#'     Mean of estimated standard error of estimated factor loading `xlatent ~ x` \eqn{ \left( \lambda_x \right)}.
#'   }
#'   \item{sehatlambdamhat}{
#'     Mean of estimated standard error of estimated factor loading `mlatent ~ m` \eqn{ \left( \lambda_m \right)}.
#'   }
#'   \item{sehatlambdayhat}{
#'     Mean of estimated standard error of estimated factor loading `ylatent ~ y` \eqn{ \left( \lambda_y \right)}.
#'   }
#'   \item{sehattaudothatprime}{
#'     Mean of estimated standard error of estimated standardized slope of path from `x` to `y` \eqn{\left( \hat{\dot{\tau}}^{\prime} \right)}.
#'   }
#'   \item{sehatbetahatprime}{
#'     Mean of estimated standard error of estimated standardized slope of path from `m` to `y` \eqn{\left( \hat{\beta}^{\prime} \right)}.
#'   }
#'   \item{sehatalphahatprime}{
#'     Mean of estimated standard error of estimated standardized slope of path from `x` to `m` \eqn{\left( \hat{\alpha}^{\prime} \right)}.
#'   }
#'   \item{sehatsigma2hatepsilonylatenthat}{
#'     Mean of estimated standard error of estimated error variance of `y` \eqn{\left( \hat{\sigma}_{\varepsilon_{y_{\mathrm{latent}}}}^{2} \right)}.
#'   }
#'   \item{sehatsigma2hatepsilonmlatenthat}{
#'     Mean of estimated standard error of estimated error variance of `m` \eqn{\left( \hat{\sigma}_{\varepsilon_{m_{\mathrm{latent}}}}^{2} \right)}.
#'   }
#'   \item{sehatmuyhat}{
#'     Mean of standard error of estimated mean of `y` \eqn{\left( \hat{\mu}_y \right)}.
#'   }
#'   \item{sehatmumhat}{
#'     Mean of standard error of estimated mean of `m` \eqn{\left( \hat{\mu}_m \right)}.
#'   }
#'   \item{sehatmuxhat}{
#'     Mean of standard error of estimated mean of `x` \eqn{\left( \hat{\mu}_x \right)}.
#'   }
#'   \item{theta}{
#'     Population parameter \eqn{\alpha^{\prime} \beta^{\prime}}.
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
#' data(results_mvn_mcar_std_fit.sem, package = "jeksterslabRmedsimple")
#' head(results_mvn_mcar_std_fit.sem)
#' str(results_mvn_mcar_std_fit.sem)
"results_mvn_mcar_std_fit.sem"
