% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nbpc.fiml.R
\name{nbpc.fiml}
\alias{nbpc.fiml}
\title{Nonparametric Bootstrap Estimates with FIML of Indirect Effect in a Simple Mediation Model for Data Generated from a Multivariate Normal Distribution}
\usage{
nbpc.fiml(data, B = 5000L, taskid, mpluspath = "~/.mplus/mplus")
}
\arguments{
\item{data}{\code{n} by 3 matrix or data frame.
\code{data[, 1]} correspond to values for \code{x}.
\code{data[, 2]} correspond to values for \code{m}.
\code{data[, 3]} correspond to values for \code{y}.}

\item{B}{Integer.
Number of bootstrap samples.}

\item{taskid}{Numeric.
Task ID.}

\item{mpluspath}{Mplus path.}
}
\description{
Nonparametric Bootstrap Estimates with FIML of Indirect Effect in a Simple Mediation Model for Data Generated from a Multivariate Normal Distribution
}
\examples{
taskid <- 1
data <- mvn_dat(taskid = taskid)
nbpc.fiml(data = mvn_mcar_10_dat(data = data, taskid = taskid), taskid = taskid)
nbpc.fiml(data = mvn_mcar_20_dat(data = data, taskid = taskid), taskid = taskid)
nbpc.fiml(data = mvn_mcar_30_dat(data = data, taskid = taskid), taskid = taskid)
nbpc.fiml(data = mvn_mar_10_dat(data = data, taskid = taskid), taskid = taskid)
nbpc.fiml(data = mvn_mar_20_dat(data = data, taskid = taskid), taskid = taskid)
nbpc.fiml(data = mvn_mar_30_dat(data = data, taskid = taskid), taskid = taskid)
nbpc.fiml(data = mvn_mnar_10_dat(data = data, taskid = taskid), taskid = taskid)
nbpc.fiml(data = mvn_mnar_20_dat(data = data, taskid = taskid), taskid = taskid)
nbpc.fiml(data = mvn_mnar_30_dat(data = data, taskid = taskid), taskid = taskid)
}
\seealso{
Other nonparametric functions: 
\code{\link{beta_nb_bcaci_simulation}()},
\code{\link{beta_nb_bcaci_task}()},
\code{\link{beta_nb_bcci_simulation}()},
\code{\link{beta_nb_bcci_task}()},
\code{\link{beta_nb_pcci_simulation}()},
\code{\link{beta_nb_pcci_task}()},
\code{\link{beta_nb_simulation}()},
\code{\link{beta_nb_task}()},
\code{\link{beta_nb}()},
\code{\link{exp_nb_bcaci_simulation}()},
\code{\link{exp_nb_bcaci_task}()},
\code{\link{exp_nb_bcci_simulation}()},
\code{\link{exp_nb_bcci_task}()},
\code{\link{exp_nb_pcci_simulation}()},
\code{\link{exp_nb_pcci_task}()},
\code{\link{exp_nb_simulation}()},
\code{\link{exp_nb_task}()},
\code{\link{exp_nb}()},
\code{\link{mvn_mar_10_nb.del_bcci_simulation}()},
\code{\link{mvn_mar_10_nb.del_bcci_task}()},
\code{\link{mvn_mar_10_nb.del_pcci_simulation}()},
\code{\link{mvn_mar_10_nb.del_pcci_task}()},
\code{\link{mvn_mar_10_nb.del_simulation}()},
\code{\link{mvn_mar_10_nb.del_task}()},
\code{\link{mvn_mar_10_nb.del}()},
\code{\link{mvn_mar_10_nb.fiml_bcci_simulation}()},
\code{\link{mvn_mar_10_nb.fiml_pcci_simulation}()},
\code{\link{mvn_mar_10_nb_bcci_simulation}()},
\code{\link{mvn_mar_10_nb_bcci_task}()},
\code{\link{mvn_mar_10_nb_pcci_simulation}()},
\code{\link{mvn_mar_10_nb_pcci_task}()},
\code{\link{mvn_mar_10_nb_simulation}()},
\code{\link{mvn_mar_10_nb_task}()},
\code{\link{mvn_mar_10_nbbc.fiml_simulation}()},
\code{\link{mvn_mar_10_nbbc.fiml_task}()},
\code{\link{mvn_mar_10_nbpc.fiml_simulation}()},
\code{\link{mvn_mar_10_nbpc.fiml_task}()},
\code{\link{mvn_mar_10_nb}()},
\code{\link{mvn_mar_20_nb.del_bcci_simulation}()},
\code{\link{mvn_mar_20_nb.del_bcci_task}()},
\code{\link{mvn_mar_20_nb.del_pcci_simulation}()},
\code{\link{mvn_mar_20_nb.del_pcci_task}()},
\code{\link{mvn_mar_20_nb.del_simulation}()},
\code{\link{mvn_mar_20_nb.del_task}()},
\code{\link{mvn_mar_20_nb.del}()},
\code{\link{mvn_mar_20_nb.fiml_bcci_simulation}()},
\code{\link{mvn_mar_20_nb.fiml_pcci_simulation}()},
\code{\link{mvn_mar_20_nb_bcci_simulation}()},
\code{\link{mvn_mar_20_nb_bcci_task}()},
\code{\link{mvn_mar_20_nb_pcci_simulation}()},
\code{\link{mvn_mar_20_nb_pcci_task}()},
\code{\link{mvn_mar_20_nb_simulation}()},
\code{\link{mvn_mar_20_nb_task}()},
\code{\link{mvn_mar_20_nbbc.fiml_simulation}()},
\code{\link{mvn_mar_20_nbbc.fiml_task}()},
\code{\link{mvn_mar_20_nbpc.fiml_simulation}()},
\code{\link{mvn_mar_20_nbpc.fiml_task}()},
\code{\link{mvn_mar_20_nb}()},
\code{\link{mvn_mar_30_nb.del_bcci_simulation}()},
\code{\link{mvn_mar_30_nb.del_bcci_task}()},
\code{\link{mvn_mar_30_nb.del_pcci_simulation}()},
\code{\link{mvn_mar_30_nb.del_pcci_task}()},
\code{\link{mvn_mar_30_nb.del_simulation}()},
\code{\link{mvn_mar_30_nb.del_task}()},
\code{\link{mvn_mar_30_nb.del}()},
\code{\link{mvn_mar_30_nb.fiml_bcci_simulation}()},
\code{\link{mvn_mar_30_nb.fiml_pcci_simulation}()},
\code{\link{mvn_mar_30_nb_bcci_simulation}()},
\code{\link{mvn_mar_30_nb_bcci_task}()},
\code{\link{mvn_mar_30_nb_pcci_simulation}()},
\code{\link{mvn_mar_30_nb_pcci_task}()},
\code{\link{mvn_mar_30_nb_simulation}()},
\code{\link{mvn_mar_30_nb_task}()},
\code{\link{mvn_mar_30_nbbc.fiml_simulation}()},
\code{\link{mvn_mar_30_nbbc.fiml_task}()},
\code{\link{mvn_mar_30_nbpc.fiml_simulation}()},
\code{\link{mvn_mar_30_nbpc.fiml_task}()},
\code{\link{mvn_mar_30_nb}()},
\code{\link{mvn_mcar_10_nb.del_bcci_simulation}()},
\code{\link{mvn_mcar_10_nb.del_bcci_task}()},
\code{\link{mvn_mcar_10_nb.del_pcci_simulation}()},
\code{\link{mvn_mcar_10_nb.del_pcci_task}()},
\code{\link{mvn_mcar_10_nb.del_simulation}()},
\code{\link{mvn_mcar_10_nb.del_task}()},
\code{\link{mvn_mcar_10_nb.del}()},
\code{\link{mvn_mcar_10_nb.fiml_bcci_simulation}()},
\code{\link{mvn_mcar_10_nb.fiml_pcci_simulation}()},
\code{\link{mvn_mcar_10_nb_bcci_simulation}()},
\code{\link{mvn_mcar_10_nb_bcci_task}()},
\code{\link{mvn_mcar_10_nb_pcci_simulation}()},
\code{\link{mvn_mcar_10_nb_pcci_task}()},
\code{\link{mvn_mcar_10_nb_simulation}()},
\code{\link{mvn_mcar_10_nb_task}()},
\code{\link{mvn_mcar_10_nbbc.fiml_simulation}()},
\code{\link{mvn_mcar_10_nbbc.fiml_task}()},
\code{\link{mvn_mcar_10_nbpc.fiml_simulation}()},
\code{\link{mvn_mcar_10_nbpc.fiml_task}()},
\code{\link{mvn_mcar_10_nb}()},
\code{\link{mvn_mcar_20_nb.del_bcci_simulation}()},
\code{\link{mvn_mcar_20_nb.del_bcci_task}()},
\code{\link{mvn_mcar_20_nb.del_pcci_simulation}()},
\code{\link{mvn_mcar_20_nb.del_pcci_task}()},
\code{\link{mvn_mcar_20_nb.del_simulation}()},
\code{\link{mvn_mcar_20_nb.del_task}()},
\code{\link{mvn_mcar_20_nb.del}()},
\code{\link{mvn_mcar_20_nb.fiml_bcci_simulation}()},
\code{\link{mvn_mcar_20_nb.fiml_pcci_simulation}()},
\code{\link{mvn_mcar_20_nb_bcci_simulation}()},
\code{\link{mvn_mcar_20_nb_bcci_task}()},
\code{\link{mvn_mcar_20_nb_pcci_simulation}()},
\code{\link{mvn_mcar_20_nb_pcci_task}()},
\code{\link{mvn_mcar_20_nb_simulation}()},
\code{\link{mvn_mcar_20_nb_task}()},
\code{\link{mvn_mcar_20_nbbc.fiml_simulation}()},
\code{\link{mvn_mcar_20_nbbc.fiml_task}()},
\code{\link{mvn_mcar_20_nbpc.fiml_simulation}()},
\code{\link{mvn_mcar_20_nbpc.fiml_task}()},
\code{\link{mvn_mcar_20_nb}()},
\code{\link{mvn_mcar_30_nb.del_bcci_simulation}()},
\code{\link{mvn_mcar_30_nb.del_bcci_task}()},
\code{\link{mvn_mcar_30_nb.del_pcci_simulation}()},
\code{\link{mvn_mcar_30_nb.del_pcci_task}()},
\code{\link{mvn_mcar_30_nb.del_simulation}()},
\code{\link{mvn_mcar_30_nb.del_task}()},
\code{\link{mvn_mcar_30_nb.del}()},
\code{\link{mvn_mcar_30_nb.fiml_bcci_simulation}()},
\code{\link{mvn_mcar_30_nb.fiml_pcci_simulation}()},
\code{\link{mvn_mcar_30_nb_bcci_simulation}()},
\code{\link{mvn_mcar_30_nb_bcci_task}()},
\code{\link{mvn_mcar_30_nb_pcci_simulation}()},
\code{\link{mvn_mcar_30_nb_pcci_task}()},
\code{\link{mvn_mcar_30_nb_simulation}()},
\code{\link{mvn_mcar_30_nb_task}()},
\code{\link{mvn_mcar_30_nbbc.fiml_simulation}()},
\code{\link{mvn_mcar_30_nbbc.fiml_task}()},
\code{\link{mvn_mcar_30_nbpc.fiml_simulation}()},
\code{\link{mvn_mcar_30_nbpc.fiml_task}()},
\code{\link{mvn_mcar_30_nb}()},
\code{\link{mvn_mnar_10_nb.del_bcci_simulation}()},
\code{\link{mvn_mnar_10_nb.del_bcci_task}()},
\code{\link{mvn_mnar_10_nb.del_pcci_simulation}()},
\code{\link{mvn_mnar_10_nb.del_pcci_task}()},
\code{\link{mvn_mnar_10_nb.del_simulation}()},
\code{\link{mvn_mnar_10_nb.del_task}()},
\code{\link{mvn_mnar_10_nb.del}()},
\code{\link{mvn_mnar_10_nb.fiml_bcci_simulation}()},
\code{\link{mvn_mnar_10_nb.fiml_pcci_simulation}()},
\code{\link{mvn_mnar_10_nb_bcci_simulation}()},
\code{\link{mvn_mnar_10_nb_bcci_task}()},
\code{\link{mvn_mnar_10_nb_pcci_simulation}()},
\code{\link{mvn_mnar_10_nb_pcci_task}()},
\code{\link{mvn_mnar_10_nb_simulation}()},
\code{\link{mvn_mnar_10_nb_task}()},
\code{\link{mvn_mnar_10_nbbc.fiml_simulation}()},
\code{\link{mvn_mnar_10_nbbc.fiml_task}()},
\code{\link{mvn_mnar_10_nbpc.fiml_simulation}()},
\code{\link{mvn_mnar_10_nbpc.fiml_task}()},
\code{\link{mvn_mnar_10_nb}()},
\code{\link{mvn_mnar_20_nb.del_bcci_simulation}()},
\code{\link{mvn_mnar_20_nb.del_bcci_task}()},
\code{\link{mvn_mnar_20_nb.del_pcci_simulation}()},
\code{\link{mvn_mnar_20_nb.del_pcci_task}()},
\code{\link{mvn_mnar_20_nb.del_simulation}()},
\code{\link{mvn_mnar_20_nb.del_task}()},
\code{\link{mvn_mnar_20_nb.del}()},
\code{\link{mvn_mnar_20_nb.fiml_bcci_simulation}()},
\code{\link{mvn_mnar_20_nb.fiml_pcci_simulation}()},
\code{\link{mvn_mnar_20_nb_bcci_simulation}()},
\code{\link{mvn_mnar_20_nb_bcci_task}()},
\code{\link{mvn_mnar_20_nb_pcci_simulation}()},
\code{\link{mvn_mnar_20_nb_pcci_task}()},
\code{\link{mvn_mnar_20_nb_simulation}()},
\code{\link{mvn_mnar_20_nb_task}()},
\code{\link{mvn_mnar_20_nbbc.fiml_simulation}()},
\code{\link{mvn_mnar_20_nbbc.fiml_task}()},
\code{\link{mvn_mnar_20_nbpc.fiml_simulation}()},
\code{\link{mvn_mnar_20_nbpc.fiml_task}()},
\code{\link{mvn_mnar_20_nb}()},
\code{\link{mvn_mnar_30_nb.del_bcci_simulation}()},
\code{\link{mvn_mnar_30_nb.del_bcci_task}()},
\code{\link{mvn_mnar_30_nb.del_pcci_simulation}()},
\code{\link{mvn_mnar_30_nb.del_pcci_task}()},
\code{\link{mvn_mnar_30_nb.del_simulation}()},
\code{\link{mvn_mnar_30_nb.del_task}()},
\code{\link{mvn_mnar_30_nb.del}()},
\code{\link{mvn_mnar_30_nb.fiml_bcci_simulation}()},
\code{\link{mvn_mnar_30_nb.fiml_pcci_simulation}()},
\code{\link{mvn_mnar_30_nb_bcci_simulation}()},
\code{\link{mvn_mnar_30_nb_bcci_task}()},
\code{\link{mvn_mnar_30_nb_pcci_simulation}()},
\code{\link{mvn_mnar_30_nb_pcci_task}()},
\code{\link{mvn_mnar_30_nb_simulation}()},
\code{\link{mvn_mnar_30_nb_task}()},
\code{\link{mvn_mnar_30_nbbc.fiml_simulation}()},
\code{\link{mvn_mnar_30_nbbc.fiml_task}()},
\code{\link{mvn_mnar_30_nbpc.fiml_simulation}()},
\code{\link{mvn_mnar_30_nbpc.fiml_task}()},
\code{\link{mvn_mnar_30_nb}()},
\code{\link{mvn_nb_bcaci_simulation}()},
\code{\link{mvn_nb_bcaci_task}()},
\code{\link{mvn_nb_bcci_simulation}()},
\code{\link{mvn_nb_bcci_task}()},
\code{\link{mvn_nb_pcci_simulation}()},
\code{\link{mvn_nb_pcci_task}()},
\code{\link{mvn_nb_simulation}()},
\code{\link{mvn_nb_task}()},
\code{\link{mvn_nb}()},
\code{\link{mvn_std_nb_bcaci_simulation}()},
\code{\link{mvn_std_nb_bcaci_task}()},
\code{\link{mvn_std_nb_bcci_simulation}()},
\code{\link{mvn_std_nb_bcci_task}()},
\code{\link{mvn_std_nb_pcci_simulation}()},
\code{\link{mvn_std_nb_pcci_task}()},
\code{\link{mvn_std_nb_simulation}()},
\code{\link{mvn_std_nb_task}()},
\code{\link{mvn_std_nb}()},
\code{\link{nb.del}()},
\code{\link{nb.fiml}()},
\code{\link{nbbc.fiml}()},
\code{\link{nb}()},
\code{\link{vm_mod_nb_bcaci_simulation}()},
\code{\link{vm_mod_nb_bcaci_task}()},
\code{\link{vm_mod_nb_bcci_simulation}()},
\code{\link{vm_mod_nb_bcci_task}()},
\code{\link{vm_mod_nb_pcci_simulation}()},
\code{\link{vm_mod_nb_pcci_task}()},
\code{\link{vm_mod_nb_simulation}()},
\code{\link{vm_mod_nb_task}()},
\code{\link{vm_mod_nb}()},
\code{\link{vm_mod_std_nb_bcaci_simulation}()},
\code{\link{vm_mod_std_nb_bcaci_task}()},
\code{\link{vm_mod_std_nb_bcci_simulation}()},
\code{\link{vm_mod_std_nb_bcci_task}()},
\code{\link{vm_mod_std_nb_pcci_simulation}()},
\code{\link{vm_mod_std_nb_pcci_task}()},
\code{\link{vm_mod_std_nb_simulation}()},
\code{\link{vm_mod_std_nb_task}()},
\code{\link{vm_mod_std_nb}()},
\code{\link{vm_sev_nb_bcaci_simulation}()},
\code{\link{vm_sev_nb_bcaci_task}()},
\code{\link{vm_sev_nb_bcci_simulation}()},
\code{\link{vm_sev_nb_bcci_task}()},
\code{\link{vm_sev_nb_pcci_simulation}()},
\code{\link{vm_sev_nb_pcci_task}()},
\code{\link{vm_sev_nb_simulation}()},
\code{\link{vm_sev_nb_task}()},
\code{\link{vm_sev_nb}()},
\code{\link{vm_sev_std_nb_bcaci_simulation}()},
\code{\link{vm_sev_std_nb_bcaci_task}()},
\code{\link{vm_sev_std_nb_bcci_simulation}()},
\code{\link{vm_sev_std_nb_bcci_task}()},
\code{\link{vm_sev_std_nb_pcci_simulation}()},
\code{\link{vm_sev_std_nb_pcci_task}()},
\code{\link{vm_sev_std_nb_simulation}()},
\code{\link{vm_sev_std_nb_task}()},
\code{\link{vm_sev_std_nb}()}
}
\author{
Ivan Jacob Agaloos Pesigan
}
\concept{nonparametric functions}
\keyword{nb}
