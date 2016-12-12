\name{normCI}
\alias{normCI}
\title{ Confidence Intervals for Mean and Standard Deviation }
\description{
  This function can be used to compute confidence intervals for mean and
  standard deviation of a normal distribution.
}
\usage{
normCI(x, conf.level = 0.95, na.rm = TRUE)
}
\arguments{
  \item{x}{ vector of observations. }
  \item{conf.level}{ confidence level. }
  \item{na.rm}{ a logical value indicating whether NA values should be stripped before the computation proceeds. }
}
\details{
  The standard confidence intervals for mean and standard deviation are
  computed which can be found in many textbooks.
}
\value{
  A list with components
  \item{estimate}{ the estimated mean and sd. }
  \item{CI}{ confidence intervals for mean and sd. }
}
%\references{}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
%\seealso{}
\examples{
normCI(rnorm(100))
}
\keyword{univar}
