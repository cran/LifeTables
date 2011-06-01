\name{hmd.DA}
\alias{hmd.DA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Discriminant analysis to determine life table family
}
\description{
Returns the results of a discriminant analysis to find the appropriate life table family based on either a single indicator of child mortality (1m0, 5m0, 1q0, 5q0) or a child indicator and adult mortality indicator (45q15)
}
\usage{
hmd.DA(x, child.mort=4, sex="female", adult.mort = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A numeric value for the child mortality indicator to be classified
}
  \item{sex}{"male" or "female" indicates the sex for the indicators
}
  \item{child.mort}{ An interger (1-4) to indicate which child mortality indicator is being supplied (1 - 1m0; 2 - 5m0; 3 - 1q0; 4 - 5q0)
}
  \item{adult.mort}{ The value for 45q15 (if no value is supplied, defaults to NULL and uses just the single child mortality measure to classify)
}
}
\details{
The training models based on the known classification of the training data can be found in the workspace \code{\link{MLTobs}} in the "data" subdirectory of this package.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{train}{The output from the function \code{\link{mclustDAtrain}}, the parameters and other summary information for the model best fitting each class according to BIC.}
  \item{test }{The output from the function \code{\link{mclustDAtest}}, A matrix in which the [i,j]th entry is the density for that test observation i in the model for class j.}
  \item{classification }{The classification for the test data}
}
\references{
S. Clark and D. Sharrow (2011) "Contemporary Model Life Tables for Developed Countries: An Application of Model-Based Clustering."  Working Paper No. 107, Center for Statistics and the Social Sciences, University of Washington, www.csss.washington.edu/Papers/

C. Fraley and A. E. Raftery (2002) "Model-based clustering, discriminant analysis, and density estimation." \emph{Journal of the American Statistical Association} \bold{97}:611-631.

C. Fraley and A. E. Raftery (2006) "MCLUST Version 3 for R: Normal Mixture Modeling and Model-Based Clustering", Technical Report no. 504, Department of Statistics, University of Washington

\emph{Human Mortality Database} University of California, Berkeley (USA), and Max Planck Institute for Demographic Research (Germany). Available at www.mortality.org or www.humanmortality.de (data downloaded on [November 2009]).
}

\author{
David Sharrow
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{mclustDAtest}}, \code{\link{mclustDAtrain}}, \code{\link{Mclust}}, \code{\link{MLTobs}}
}
\examples{

# classifying with 1m0
examp.out <- hmd.DA(x=.05, sex="male", child.mort=1)
examp.out$classification

# classifying with 5q0 and 45q15 
examp.out2 <- hmd.DA(x=.03, sex="male", child.mort=4, adult.mort=.18)
examp.out2$classification

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
\keyword{ misc }% __ONLY ONE__ keyword per line