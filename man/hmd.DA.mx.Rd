\name{hmd.DA.mx}
\alias{hmd.DA.mx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Model Life Table Discriminant Analysis
}
\description{
Training and testing steps to perform a discriminant analysis for a partial or complete human mortality rate schedules with the Human Mortality database as the training set. 
}
\usage{
hmd.DA.mx(data, sex="female")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
a partial or complete human mortality rate schedule to be classified. Must have contiguous age groups (with standard 5-year age intervals 0, 1-4, 5-9, 10-14...110+) and be in log scale. This can be a single schedule or many organized into a matrix with the columns representing the age groups.  
}
  \item{sex}{
"male" or "female". The sex for the schedule to be classified.
}
}
\details{
This function can only take contiguous age groups as arguments. Whatever age groups are used, the function uses that portion of the HMD schedules to generate the training models and then clasifies the test schedules. 
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{train}{The output from the function \code{\link{MclustDA}}, the parameters and other summary information for the model best fitting each class according to BIC.}
  \item{out.dens}{The output from the function \code{\link{predict}(MclustDA model name)}, A matrix in which the [i,j]th entry is the density for observation i in the model for class j.}
  \item{classification }{The classification for the test data}
}
\references{
S. Clark and D. Sharrow (2011) "Contemporary Model Life Tables for Developed Countries: An Application of Model-Based Clustering."  Working Paper No. 107, Center for Statistics and the Social Sciences, University of Washington, www.csss.washington.edu/Papers/

C. Fraley and A. E. Raftery (2002) "Model-based clustering, discriminant analysis, and density estimation." \emph{Journal of the American Statistical Association} \bold{97}:611-631.

C. Fraley, A. E. Raftery, T. B. Murphy, L. Scrucca (2012) "mclust Version 4 for R: Normal Mixture Modeling for Model-Based Clustering, Classification, and Density Estimation", Technical Report no. 597, Department of Statistics, University of Washington

\emph{Human Mortality Database} University of California, Berkeley (USA), and Max Planck Institute for Demographic Research (Germany). Available at www.mortality.org or www.humanmortality.de (data downloaded on [November 2009]).
}

\author{
David Sharrow
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{MclustDA}}, \code{\link{Mclust}}, \code{\link{MLTobs}}
}
\examples{
# some test data
data(MLTobs)
examp.data <- t(flt.mx[1:2,1:10])

# classify the test data
examp.out <- hmd.DA.mx(data=examp.data, sex="female")
examp.out$classification
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ cluster }
\keyword{ misc }% __ONLY ONE__ keyword per line
