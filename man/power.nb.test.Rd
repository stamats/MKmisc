\name{power.nb.test}
\alias{power.nb.test}
\title{Power calculation for comparing two negative binomial rates}
\description{
 Compute sample size or power for comparing two negative binomial rates.
}
\usage{
power.nb.test(n = NULL, mu0, mu1, RR, duration = 1, theta, ssize.ratio = 1,
              sig.level = 0.05, power = NULL, alternative = c("two.sided", "one.sided"),
              approach = 3)
}
\arguments{
 \item{n}{Sample size for group 0 (control group).}
 \item{mu0}{expected rate of events per time unit for group 0}
 \item{mu1}{expected rate of events per time unit for group 1}
 \item{RR}{ratio of expected event rates: mu1/mu0}
 \item{duration}{(average) treatment duration}
 \item{theta}{theta parameter of negative binomial distribution; see \code{\link[MASS]{rnegbin}}}
 \item{ssize.ratio}{ratio of sample sizes: n/n1 where n1 is sample size of group 1}
 \item{sig.level}{Significance level (Type I error probability)}
 \item{power}{Power of test (1 minus Type II error probability)}
 \item{alternative}{one- or two-sided test}
 \item{approach}{1, 2, or 3; see Zhu and Lakkis (2014).}
}

\details{
  Exactly one of the parameters \code{n} and \code{power} must be passed as
  \code{NULL}, and that parameter is determined from the other.

  The computations are based on the formulas given in Zhu and Lakkis (2014).
  Please be careful, as we are using a slightly different parametrization
  (\code{theta} = 1/k).

  Zhu and Lakkis (2014) based on their simulation studies recommend to use
  their approach 2 or 3.
}
\value{
  Object of class \code{"power.htest"}, a list of the arguments
  (including the computed one) augmented with a \code{note} element.
}

\references{
  H. Zhu and H. Lakkis (2014). Sample size calculation for comparing two negative
  binomial rates. \emph{Statistics in Medicine}, \bold{33}:376-387.
}

\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}

%\note{}
\seealso{\code{\link[MASS]{rnegbin}}, \code{\link[MASS]{glm.nb}}}

\examples{
## examples from Table I in Zhu and Lakkis (2014)
## theta = 1/k, RR = rr, mu0 = r0, duration = mu_t
power.nb.test(mu0 = 0.8, RR = 0.85, theta = 1/0.4, duration = 0.75, power = 0.8, approach = 1)
power.nb.test(mu0 = 0.8, RR = 0.85, theta = 1/0.4, duration = 0.75, power = 0.8, approach = 2)
power.nb.test(mu0 = 0.8, RR = 0.85, theta = 1/0.4, duration = 0.75, power = 0.8, approach = 3)

power.nb.test(mu0 = 1.4, RR = 1.15, theta = 1/1.5, duration = 0.75, power = 0.8, approach = 1)
power.nb.test(mu0 = 1.4, RR = 1.15, theta = 1/1.5, duration = 0.75, power = 0.8, approach = 2)
power.nb.test(mu0 = 1.4, RR = 1.15, theta = 1/1.5, duration = 0.75, power = 0.8, approach = 3)


## examples from Table II in Zhu and Lakkis (2014) - seem to be total sample sizes
## can reproduce the results with mu_t = 1.0 (not 0.7!)
power.nb.test(mu0 = 2.0, RR = 0.5, theta = 1, duration = 1.0, ssize.ratio = 1,
              power = 0.8, approach = 1)
power.nb.test(mu0 = 2.0, RR = 0.5, theta = 1, duration = 1.0, ssize.ratio = 1,
              power = 0.8, approach = 2)
power.nb.test(mu0 = 2.0, RR = 0.5, theta = 1, duration = 1.0, ssize.ratio = 1,
              power = 0.8, approach = 3)

power.nb.test(mu0 = 10.0, RR = 1.5, theta = 1/5, duration = 1.0, ssize.ratio = 3/2,
              power = 0.8, approach = 1)
power.nb.test(mu0 = 10.0, RR = 1.5, theta = 1/5, duration = 1.0, ssize.ratio = 3/2,
              power = 0.8, approach = 2)
power.nb.test(mu0 = 10.0, RR = 1.5, theta = 1/5, duration = 1.0, ssize.ratio = 3/2,
              power = 0.8, approach = 3)


## examples from Table III in Zhu and Lakkis (2014)
power.nb.test(mu0 = 5.0, RR = 2.0, theta = 1/0.5, duration = 1, power = 0.8, approach = 1)
power.nb.test(mu0 = 5.0, RR = 2.0, theta = 1/0.5, duration = 1, power = 0.8, approach = 2)
power.nb.test(mu0 = 5.0, RR = 2.0, theta = 1/0.5, duration = 1, power = 0.8, approach = 3)


## examples from Table IV in Zhu and Lakkis (2014)
power.nb.test(mu0 = 5.9/3, RR = 0.4, theta = 0.49, duration = 3, power = 0.9, approach = 1)
power.nb.test(mu0 = 5.9/3, RR = 0.4, theta = 0.49, duration = 3, power = 0.9, approach = 2)
power.nb.test(mu0 = 5.9/3, RR = 0.4, theta = 0.49, duration = 3, power = 0.9, approach = 3)

power.nb.test(mu0 = 13/6, RR = 0.2, theta = 0.52, duration = 6, power = 0.9, approach = 1)
power.nb.test(mu0 = 13/6, RR = 0.2, theta = 0.52, duration = 6, power = 0.9, approach = 2)
power.nb.test(mu0 = 13/6, RR = 0.2, theta = 0.52, duration = 6, power = 0.9, approach = 3)


## see Section 5 of Zhu and Lakkis (2014)
power.nb.test(mu0 = 0.66, RR = 0.8, theta = 1/0.8, duration = 0.9, power = 0.9)
}
\keyword{htest}


