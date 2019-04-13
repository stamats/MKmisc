\name{power.hsu.t.test}
\alias{power.hsu.t.test}
\title{Power calculations for two sample Hsu t test}
\usage{
power.hsu.t.test(n = NULL, delta = NULL, sd1 = 1, sd2 = 1, sig.level = 0.05,
                   power = NULL, alternative = c("two.sided", "one.sided"),
                   strict = FALSE, tol = .Machine$double.eps^0.25)
}
\arguments{
 \item{n}{number of observations (per group)}
 \item{delta}{(expected) true difference in means}
 \item{sd1}{(expected) standard deviation of group 1}
 \item{sd2}{(expected) standard deviation of group 2}
 \item{sig.level}{significance level (Type I error probability)}
 \item{power}{power of test (1 minus Type II error probability)}
 \item{alternative}{one- or two-sided test.  Can be abbreviated.}
 \item{strict}{use strict interpretation in two-sided case}
 \item{tol}{numerical tolerance used in root finding, the default
    providing (at least) four significant digits.}
}
\description{
  Compute the power of the two-sample Hsu t test, or determine parameters
  to obtain a target power; see Section 7.4.4 in Hedderich and Sachs (2016),
}

\details{
  Exactly one of the parameters \code{n}, \code{delta}, \code{power},
  \code{sd1}, \code{sd2} and \code{sig.level} must be passed as \code{NULL},
  and that parameter is determined from the others. Notice that the last three
  have non-NULL defaults, so NULL must be explicitly passed if you want to
  compute them.

  If \code{strict = TRUE} is used, the power will include the probability of
  rejection in the opposite direction of the true effect, in the two-sided
  case. Without this the power will be half the significance level if the
  true difference is zero.
}
\value{
  Object of class \code{"power.htest"}, a list of the arguments
  (including the computed one) augmented with \code{method} and
  \code{note} elements.
}

\references{
  J. Hedderich, L. Sachs. \emph{Angewandte Statistik: Methodensammlung mit R}.
  Springer 2016.
}

\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}

\note{
  The function and its documentation was adapted from \code{power.t.test}
  implemented by Peter Dalgaard and based on previous work by Claus Ekstroem.

  \code{uniroot} is used to solve the power equation for unknowns, so
  you may see errors from it, notably about inability to bracket the
  root when invalid arguments are given.
}
\seealso{\code{\link{power.welch.t.test}}, \code{\link{power.t.test}},
\code{\link{t.test}}, \code{\link{uniroot}}}

\examples{
 ## more conservative than classical or Welch t-test
 power.hsu.t.test(n = 20, delta = 1)
 power.hsu.t.test(power = .90, delta = 1)
 power.hsu.t.test(power = .90, delta = 1, alternative = "one.sided")

 ## sd1 = 0.5, sd2 = 1
 power.welch.t.test(delta = 0.5, sd1 = 0.5, sd2 = 1, power = 0.9)
 power.hsu.t.test(delta = 0.5, sd1 = 0.5, sd2 = 1, power = 0.9)

 \donttest{
 ## empirical check
 M <- 10000
 ps <- numeric(M)
 for(i in seq_len(M)){
   x <- rnorm(55, mean = 0, sd = 0.5)
   y <- rnorm(55, mean = 0.5, sd = 1.0)
   ps[i] <- hsu.t.test(x, y)$p.value
 }
 ## empirical power
 sum(ps < 0.05)/M
 }
}
\keyword{htest}


