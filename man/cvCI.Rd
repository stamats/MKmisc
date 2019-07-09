\name{cvCI}
\alias{cvCI}
\title{ Confidence Intervals for Coefficient of Variation }
\description{
  This function can be used to compute confidence intervals for the (classical)
  coefficient of variation.
}
\usage{
cvCI(x, conf.level = 0.95, method = "miller", na.rm = FALSE)
}
\arguments{
  \item{x}{ numeric vector. }
  \item{conf.level}{ confidence level }
  \item{method}{ character string specifing which method to use; see details. }
  \item{na.rm}{logical. Should missing values be removed?}
}
\details{
  For details about the confidence intervals we refer to Gulhar et al (2012) and
  Arachchige et al (2019).
}
\value{
  A list with class \code{"confint"} containing the following components:
  \item{estimate}{ the estimated coefficient of variation. }
  \item{conf.int}{ a confidence interval for the coefficient of variation. }
}
\references{
  C.N.P.G. Arachchige, L.A. Prendergast and R.G. Staudte (2019). Robust analogues
  to the Coefficient of Variation. https://arxiv.org/abs/1907.01110.

  M. Gulhar, G. Kibria, A. Albatineh, N.U. Ahmed (2012). A comparison of some
  confidence intervals for estimating the population coefficient of variation:
  a simulation study. \emph{Sort}, \bold{36}(1), 45-69.
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link{CV}}}
\examples{
x <- rnorm(100, mean = 10, sd = 2) # CV = 0.2
cvCI(x, method = "miller")
cvCI(x, method = "sharma")
cvCI(x, method = "curto")
cvCI(x, method = "mckay")
cvCI(x, method = "vangel")
cvCI(x, method = "panichkitkosolkul")
cvCI(x, method = "medmiller")
cvCI(x, method = "medmckay")
cvCI(x, method = "medvangel")
cvCI(x, method = "medcurto")
cvCI(x, method = "gulhar")
}
\keyword{univar}
