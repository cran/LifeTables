\name{MLTobs}
\docType{data}
\alias{MLTobs}

\title{Background objects necessary for certain functions}
\description{ This data set contains 28 objects which are necessary for certain package functions and are described in detail below}

\usage{data(MLTobs)}
\format{28 objects: 
	\itemize{
\item{averages.smooth} A 48x5 matrix whose columns contain the five underlying family patterns. Rows 1:24 are the male pattern and rows 25:48 are the female patterns. Used in \code{\link{mortmod}}.
\item{class5} A vector of length 844 which contains the classification for the 844 HMD mortality rate schedules. Used in \code{\link{hmd.DA.mx}}.
\item{f.csd.weight} A function to determine the weight for the family-specific deviation. Used in \code{\link{mortmod}}.
\item{fitted.ms.pca.scores} A 48x10 matrix whose columns contain the first 10 component score vectors a SVD of the HMD mortality rate schedules. 
\item{flt.mx} A 24x844 matrix whose columns contain the 844 female mortality rate schedules in the Human Mortality Database
\item{flt.mx.info} A 844x28 matrix. The first four columns contain the Location ID, Location Name, Subgroup type and Period. The remaining columns contain the age-specific mortality rates contained in \code{flt.mx}.
\item{hi.devs} A 6x48 matrix whose first five rows contain the family-age-specific high deviations for when alpha is positive. The 6th row contains the age-specific high deviations from the entire HMD dataset. Used in \code{\link{mortmod}}.
\item{hmd.1m0.train.f} Training models for females when the input parameters include 1m0. Used in \code{\link{hmd.DA}}.
\item{hmd.1m0.train.m} Training models for males when the input parameters include 1m0. Used in \code{\link{hmd.DA}}.
\item{hmd.1m0a.train.f} Training models for females when the input parameters include 1m0 and 45q15. Used in \code{\link{hmd.DA}}.

\item{hmd.1m0a.train.m} Training models for males when the input parameters include 1m0 and 45q15. Used in \code{\link{hmd.DA}}.

\item{hmd.5m0.train.f} Training models for females when the input parameters include 5m0. Used in \code{\link{hmd.DA}}.

\item{hmd.5m0.train.m} Training models for males when the input parameters include 5m0. Used in \code{\link{hmd.DA}}.

\item{hmd.5m0a.train.f} Training models for females when the input parameters include 5m0 and 45q15. Used in \code{\link{hmd.DA}}.

\item{hmd.5m0a.train.m} Training models for males when the input parameters include 5m0 and 45q15. Used in \code{\link{hmd.DA}}.

\item{hmd.1q0.train.f} Training models for females when the input parameters include 1q0. Used in \code{\link{hmd.DA}}.

\item{hmd.1q0.train.m} Training models for males when the input parameters include 1q0. Used in \code{\link{hmd.DA}}.

\item{hmd.1q0a.train.f} Training models for females when the input parameters include 1q0 and 45q15. Used in \code{\link{hmd.DA}}.

\item{hmd.1q0a.train.m} Training models for males when the input parameters include 1q0 and 45q15. Used in \code{\link{hmd.DA}}.

\item{hmd.5q0.train.f} Training models for females when the input parameters include 5q0. Used in \code{\link{hmd.DA}}.

\item{hmd.5q0.train.m} Training models for males when the input parameters include 5q0. Used in \code{\link{hmd.DA}}.

\item{hmd.5q0a.train.f} Training models for females when the input parameters include 5q0 and 45q15. Used in \code{\link{hmd.DA}}.

\item{hmd.5q0a.train.m} Training models for males when the input parameters include 5q0 and 45q15. Used in \code{\link{hmd.DA}}.

\item{lo.devs} A 6x48 matrix whose first five rows contain the family-age-specific low deviations for when alpha is positive. The 6th row contains the age-specific low deviations from the entire HMD dataset. Used in \code{\link{mortmod}}.
\item{mlt.mx} A 24x844 matrix whose columns contain the 844 male mortality rate schedules in the Human Mortality Database
\item{mlt.mx.info} A 844x28 matrix. The first four columns contain the Location ID, Location Name, Subgroup type and Period. The remaining columns contain the age-specific mortality rates contained in \code{mlt.mx}.
\item{opt.alpha.f} A 25x5 matrix whose columns contain the female family-specifc alpha values to produce life tables with life expectancies at 25 levels from 30-90 in 2.5 year increments
\item{opt.alpha.m} A 25x5 matrix whose columns contain the male family-specifc alpha values to produce life tables with life expectancies at 25 levels from 30-90 in 2.5 year increments
\item{Patterns.final.coeffs} A 5x5 matrix whose columns contain the set of median coefficients for each family which are used to calculate the underlying family patterns
	}	
	}
	
\references{
S. Clark and D. Sharrow (2011) "Contemporary Model Life Tables for Developed Countries: An Application of Model-Based Clustering."  Working Paper No. 107, Center for Statistics and the Social Sciences, University of Washington, www.csss.washington.edu/Papers/

C. Fraley and A. E. Raftery (2002) "Model-based clustering, discriminant analysis, and density estimation." \emph{Journal of the American Statistical Association} \bold{97}:611-631.

C. Fraley and A. E. Raftery (2006) "MCLUST Version 3 for R: Normal Mixture Modeling and Model-Based Clustering", Technical Report no. 504, Department of Statistics, University of Washington

\emph{Human Mortality Database} University of California, Berkeley (USA), and Max Planck Institute for Demographic Research (Germany). Available at www.mortality.org or www.humanmortality.de (data downloaded on [November 2009]).
}

\keyword{datasets}
