\name{normDiffCI}
\alias{normDiffCI}
\title{ Confidence Intervals for Difference of Means }
\description{
  This function can be used to compute confidence intervals for difference
  of means assuming normal distributions.
}
\usage{
normDiffCI(x, y, conf.level = 0.95, paired = FALSE, var.equal = FALSE, na.rm = TRUE)
}
\arguments{
  \item{x}{ numeric vector of data values of group 1. }
  \item{y}{ numeric vector of data values of group 2. }
  \item{conf.level}{ confidence level. }
  \item{paired}{ a logical value indicating whether the two groups are paired. }
  \item{var.equal}{ a logical value indicating whether the variances of the two
  groups should be treated as equal. If FALSE, the Welch (Satterthwaite)
  approximation to the degrees of freedom is applied. }
  \item{na.rm}{ a logical value indicating whether NA values should be stripped before the computation proceeds. }
}
\details{
  The standard confidence intervals for the difference of means are
  computed that can be found in many textbooks, e.g. Chapter 4 in Altman et al. (2000).
}
\value{
  A list with class \code{"confint"} containing the following components:
  \item{estimate}{ the estimated mean and sd. }
  \item{conf.int}{ confidence interval(s) for mean and/or sd. }
}
\references{
  D. Altman, D. Machin, T. Bryant, M. Gardner (eds). Statistics with Confidence:
  Confidence Intervals and Statistical Guidelines, 2nd edition 2000.
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
%\seealso{}
\examples{
x <- rnorm(20)
y <- rnorm(20, mean = 0.5, sd = 2)
## Welch
normDiffCI(x, y)
## classical
normDiffCI(x, y, var.equal = TRUE)
## paired
normDiffCI(x, y, paired = TRUE)
## compare
normCI(x-y)
}
\keyword{univar}
