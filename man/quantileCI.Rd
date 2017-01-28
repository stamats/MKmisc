\name{quantileCI}
\alias{quantileCI}
\alias{medianCI}
\alias{madCI}
\title{ Confidence Intervals for Quantiles }
\description{
  These functions can be used to compute confidence intervals for quantiles
  (including median).
}
\usage{
quantileCI(x, prob = 0.5, conf.level = 0.95, method = "exact",
           minLength = FALSE, na.rm = FALSE)
medianCI(x, conf.level = 0.95, method = "exact",
         minLength = FALSE, na.rm = FALSE)
madCI(x, conf.level = 0.95, method = "exact", minLength = FALSE,
      na.rm = FALSE, constant = 1.4826)
}
\arguments{
  \item{x}{ numeric data vector }
  \item{prob}{ quantile }
  \item{conf.level}{ confidence level }
  \item{method}{ character string specifing which method to use; see details. }
  \item{minLength}{ logical, see details}
  \item{na.rm}{ logical, remove \code{NA} values. }
  \item{constant}{ scale factor (see \code{\link[stats]{mad}}). }
}
\details{
  The exact confidence interval (\code{method = "exact"}) is computed using binomial
  probabilities; see Section 6.8.1 in Sachs and Hedderich (2009). If the result is not
  unique, i.e. there is more than one interval with coverage proability closest to
  \code{conf.level}, then a matrix of confidence intervals is returned.
  If \code{minLength = TRUE}, an exact confidence interval with minimum length is
  returned.

  The asymptotic confidence interval (\code{method = "asymptotic"}) is based on the
  normal approximation of the binomial distribution; see Section 6.8.1 in Sachs and Hedderich (2009).
}
\value{
  A list with components
  \item{estimate}{ the sample quantile. }
  \item{CI}{ a confidence interval for the sample quantile. }
}
\references{
  L. Sachs and J. Hedderich (2009). Angewandte Statistik. Springer.
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\seealso{\code{\link[stats]{binom.test}}, \code{\link[Hmisc]{binconf}}}
\examples{
## To get a non-trivial exact confidence interval for the median
## one needs at least 6 observations
set.seed(123)
x <- rnorm(8)
## exact confidence interval not unique
medianCI(x)
madCI(x)

## minimum length exact confidence interval
medianCI(x, minLength = TRUE)
madCI(x, minLength = TRUE)

## asymptotic confidence interval
medianCI(x, method = "asymptotic")
madCI(x, method = "asymptotic")

## confidence interval for quantiles
quantileCI(x, prob = 0.4)
quantileCI(x, prob = 0.6)
}
\keyword{univar}
