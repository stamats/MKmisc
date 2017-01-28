\name{rrCI}
\alias{rrCI}
\title{ Compute Approximate Confidence Interval for RR. }
\description{
  The function computes an approximate confidence interval for the
  relative risk (RR).
}
\usage{
rrCI(a, b, c, d, conf.level = 0.95)
}
\arguments{
  \item{a}{ integer: events in exposed group.}
  \item{b}{ integer: non-events in exposed group.}
  \item{c}{ integer: events in non-exposed group.}
  \item{d}{ integer: non-events in non-exposed group.}
  \item{conf.level}{ numeric: confidence level }
}
\details{
  The function computes an approximate confidence interval for the relative
  risk (RR) based on the normal approximation; see Jewell (2004).
}
\value{
  A list with class \code{"confint"} containing the following components:
  \item{estimate}{ the estimated relative risk. }
  \item{conf.int}{ a confidence interval for the relative risk. }
}
\references{
  Jewell, Nicholas P. (2004). Statistics for epidemiology. Chapman & Hall/CRC.

  Relative risk. (2016, November 4). In Wikipedia, The Free Encyclopedia.
  Retrieved 19:58, November 4, 2016,
  from \url{https://en.wikipedia.org/w/index.php?title=Relative_risk&oldid=747857409}
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\examples{
## See worked example in Wikipedia
rrCI(a = 15, b = 135, c = 100, d = 150)
rrCI(a = 75, b = 75, c = 100, d = 150)
}
\keyword{univar}
