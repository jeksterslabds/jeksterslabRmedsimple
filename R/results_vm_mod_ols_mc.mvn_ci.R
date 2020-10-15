#' Results: Simple Mediation Model - Vale and Maurelli (1983) - Skewness = 2, Kurtosis = 7 - Complete Data - Monte Carlo Method Confidence Intervals with Ordinary Least Squares Parameter Estimates and Standard Errors
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
#'   \item{simreps}{
#'     Monte Carlo replications.
#'   }
#'   \item{taudot}{
#'     Population slope of path from `x` to `y` \eqn{\left( \dot{\tau} \right)}
#'   }
#'   \item{beta}{
#'     Population slope of path from `m` to `y` \eqn{\left( \beta \right)}
#'   }
#'   \item{alpha}{
#'     Population slope of path from `x` to `m` \eqn{\left( \alpha \right)}
#'   }
#'   \item{alphabeta}{
#'     Population indirect effect of `x` on `y` through `m` \eqn{\left( \alpha \beta \right)}
#'   }
#'   \item{sigma2x}{
#'     Population variance of `x` \eqn{\left( \sigma_{x}^{2} \right)}
#'   }
#'   \item{sigma2epsilonm}{
#'     Population error variance of `m` \eqn{\left( \sigma_{\varepsilon_{m}}^{2} \right)}
#'   }
#'   \item{sigma2epsilony}{
#'     Population error variance of `y` \eqn{\left( \sigma_{\varepsilon_{y}}^{2} \right)}
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
#'   \item{est}{
#'     Mean of the estimate of the indirect effect \eqn{\left( \hat{\alpha} \hat{\beta} \right)}.
#'   }
#'   \item{se}{
#'     Mean of the estimate of standard error of the indirect effect \eqn{\left( \hat{\alpha} \hat{\beta} \right)}.
#'   }
#'   \item{reps}{
#'     Monte Carlo method of bootstrap replications.
#'   }
#'   \item{ci_0.05}{
#'     Mean of the lower limit confidence interval for the 99.9% confidence interval.
#'   }
#'   \item{ci_0.5}{
#'     Mean of the lower limit confidence interval for the 99% confidence interval.
#'   }
#'   \item{ci_2.5}{
#'     Mean of the lower limit confidence interval for the 95% confidence interval.
#'   }
#'   \item{ci_97.5}{
#'     Mean of the upper limit confidence interval for the 95% confidence interval.
#'   }
#'   \item{ci_99.5}{
#'     Mean of the upper limit confidence interval for the 99% confidence interval.
#'   }
#'   \item{ci_99.95}{
#'     Mean of the upper limit confidence interval for the 99.9% confidence interval.
#'   }
#'   \item{zero_hit_99.9}{
#'     Mean zero hit for the 99.9% confidence interval.
#'   }
#'   \item{zero_hit_99}{
#'     Mean zero hit for the 99% confidence interval.
#'   }
#'   \item{zero_hit_95}{
#'     Mean zero hit for the 95% confidence interval.
#'   }
#'   \item{len_99.9}{
#'     Mean confidence interval length for the 99.9% confidence interval.
#'   }
#'   \item{len_99}{
#'     Mean confidence interval length for the 99% confidence interval.
#'   }
#'   \item{len_95}{
#'     Mean confidence interval length for the 95% confidence interval.
#'   }
#'   \item{shape_99.9}{
#'     Mean confidence interval shape for the 99.9% confidence interval.
#'   }
#'   \item{shape_99}{
#'     Mean confidence interval shape for the 99% confidence interval.
#'   }
#'   \item{shape_95}{
#'     Mean confidence interval shape for the 95% confidence interval.
#'   }
#'   \item{theta_hit_99.9}{
#'     Mean theta hit for the 99.9% confidence interval.
#'   }
#'   \item{theta_hit_99}{
#'     Mean theta hit for the 99% confidence interval.
#'   }
#'   \item{theta_hit_95}{
#'     Mean theta hit for the 95% confidence interval.
#'   }
#'   \item{theta_miss_99.9}{
#'     Mean theta miss for the 99.9% confidence interval.
#'   }
#'   \item{theta_miss_99}{
#'     Mean theta miss for the 99% confidence interval.
#'   }
#'   \item{theta_miss_95}{
#'     Mean theta miss for the 95% confidence interval.
#'   }
#'   \item{theta}{
#'     Population parameter \eqn{\alpha \beta}.
#'   }
#'   \item{power_99.9}{
#'     Mean power for the 99.9% confidence interval.
#'   }
#'   \item{power_99}{
#'     Mean power for the 99% confidence interval.
#'   }
#'   \item{power_95}{
#'     Mean power for the 95% confidence interval.
#'   }
#'   \item{liberal_ll_99.9}{
#'     Lower limit of the liberal criteria for the 99.9% confidence interval.
#'   }
#'   \item{liberal_ul_99.9}{
#'     Upper limit of the liberal criteria for the 99.9% confidence interval.
#'   }
#'   \item{moderate_ll_99.9}{
#'     Lower limit of the moderate criteria for the 99.9% confidence interval.
#'   }
#'   \item{moderate_ul_99.9}{
#'     Upper limit of the moderate criteria for the 99.9% confidence interval.
#'   }
#'   \item{strict_ll_99.9}{
#'     Lower limit of the strict criteria for the 99.9% confidence interval.
#'   }
#'   \item{strict_ul_99.9}{
#'     Upper limit of the strict criteria for the 99.9% confidence interval.
#'   }
#'   \item{liberal_ll_99}{
#'     Lower limit of the liberal criteria for the 99% confidence interval.
#'   }
#'   \item{liberal_ul_99}{
#'     Upper limit of the liberal criteria for the 99% confidence interval.
#'   }
#'   \item{moderate_ll_99}{
#'     Lower limit of the moderate criteria for the 99% confidence interval.
#'   }
#'   \item{moderate_ul_99}{
#'     Upper limit of the moderate criteria for the 99% confidence interval.
#'   }
#'   \item{strict_ll_99}{
#'     Lower limit of the strict criteria for the 99% confidence interval.
#'   }
#'   \item{strict_ul_99}{
#'     Upper limit of the strict criteria for the 99% confidence interval.
#'   }
#'   \item{liberal_ll_95}{
#'     Lower limit of the liberal criteria for the 95% confidence interval.
#'   }
#'   \item{liberal_ul_95}{
#'     Upper limit of the liberal criteria for the 95% confidence interval.
#'   }
#'   \item{moderate_ll_95}{
#'     Lower limit of the moderate criteria for the 95% confidence interval.
#'   }
#'   \item{moderate_ul_95}{
#'     Upper limit of the moderate criteria for the 95% confidence interval.
#'   }
#'   \item{strict_ll_95}{
#'     Lower limit of the strict criteria for the 95% confidence interval.
#'   }
#'   \item{strict_ul_95}{
#'     Upper limit of the strict criteria for the 95% confidence interval.
#'   }
#'   \item{serlin_ll_95}{
#'     Lower limit of the Serlin criteria for the 95% confidence interval.
#'   }
#'   \item{serlin_ul_95}{
#'     Upper limit of the Serlin criteria for the 95% confidence interval.
#'   }
#'   \item{liberal_99.9}{
#'     Logical. 1 if miss rate is inside the liberal robustness criteria for 99.9% confidence interval.
#'   }
#'   \item{liberal_99}{
#'     Logical. 1 if miss rate is inside the liberal robustness criteria for 99% confidence interval.
#'   }
#'   \item{liberal_95}{
#'     Logical. 1 if miss rate is inside the liberal robustness criteria for 95% confidence interval.
#'   }
#'   \item{moderate_99.9}{
#'     Logical. 1 if miss rate is inside the moderate robustness criteria for 99.9% confidence interval.
#'   }
#'   \item{moderate_99}{
#'     Logical. 1 if miss rate is inside the moderate robustness criteria for 99% confidence interval.
#'   }
#'   \item{moderate_95}{
#'     Logical. 1 if miss rate is inside the moderate robustness criteria for 95% confidence interval.
#'   }
#'   \item{strict_99.9}{
#'     Logical. 1 if miss rate is inside the strict robustness criteria for 99.9% confidence interval.
#'   }
#'   \item{strict_99}{
#'     Logical. 1 if miss rate is inside the strict robustness criteria for 99% confidence interval.
#'   }
#'   \item{strict_95}{
#'     Logical. 1 if miss rate is inside the strict robustness criteria for 95% confidence interval.
#'   }
#'   \item{serlin_95}{
#'     Logical. 1 if miss rate is inside the Serlin robustness criteria for 95% confidence interval.
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
#' data(results_vm_mod_ols_mc.mvn_ci, package = "jeksterslabRmedsimple")
#' head(results_vm_mod_ols_mc.mvn_ci)
#' str(results_vm_mod_ols_mc.mvn_ci)
"results_vm_mod_ols_mc.mvn_ci"
