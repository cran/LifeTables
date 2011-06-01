\name{lt.mx}
\alias{lt.mx}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Life tables from nmx
}
\description{
Builds a life table by using the mortality rate schedule to calculate the subsequent columns of the table
}
\usage{
lt.mx(nmx, sex="female", age=c(0,1,seq(5,110,5)), nax=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nmx}{
A vector of age-specific mortality rates
}
  \item{age}{
The start age of each age interval (will also be the start age for each interval of the subsequent life table)
}

  \item{sex}{
"male" or "female". The sex determines the nax value for the childhood age intervals using the Coale and Demeney West values when \code{nax=NULL}}

  \item{nax}{A set of nax values for the life table. Must have the same length as \code{nmx} and if no values of provided, this argument defaults to half the length of the age interval except for the first two childhood age groups.}
}


%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{

 \item{e0 }{Life expectancy at birth}
 \item{lt }{A life table with age intervals matching those of the nmx schedule on which the table is built and with columns for age, nax, nmx, nqx, npx, ndx, lx, nLx, Tx, and ex.}
 \item{lt.5q0}{The probability of death between birth and age 5}
 \item{lt.45q15}{The probability of death between age 15 and 60}
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
David Sharrow
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
# some data to build the table
data(MLTobs)
mx <- mlt.mx[,1]

# build the life table
lt.mx(nmx=mx, sex="male")
        }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
\keyword{ methods }% __ONLY ONE__ keyword per line
