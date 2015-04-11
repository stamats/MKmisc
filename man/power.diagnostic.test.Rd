\name{power.diagnostic.test}
\alias{power.diagnostic.test}
\title{Power calculations for a diagnostic test}
\description{
 Compute sample size, power, delta, or significance level of a diagnostic test
 for an expected sensititivy or specificity.
}
\usage{
power.diagnostic.test(sens = NULL, spec = NULL,
                      n = NULL, delta = NULL, sig.level = 0.05,
                      power = NULL, prev = NULL, 
                      method = c("exact", "asymptotic"),
                      NMAX = 1e4)
}
\arguments{
 \item{sens}{Expected sensitivity; either \code{sens} or \code{spec} has to be specified.}
 \item{spec}{Expected specificity; either \code{sens} or \code{spec} has to be specified.}
 \item{n}{Number of cases if \code{sens} and number of controls if \code{spec} is given.}
 \item{delta}{\code{sens}-\code{delta} resp. \code{spec}-\code{delta} is used as lower
 confidence limit}
 \item{sig.level}{Significance level (Type I error probability)}
 \item{power}{Power of test (1 minus Type II error probability)}
 \item{prev}{Expected prevalence, if \code{NULL} prevalence is ignored which means \code{prev = 0.5}
 is assumed.}
 \item{method}{exact or asymptotic formula; default \code{"exact"}.}
 \item{NMAX}{Maximum sample size considered in case \code{method = "exact"}.}
}

\details{
  Either \code{sens} or \code{spec} has to be specified which leads to 
  computations for either cases or controls.
  
  Exactly one of the parameters \code{n}, \code{delta}, \code{sig.level}, 
  and \code{power} must be passed as \code{NULL}, and that parameter is determined 
  from the others. Notice that \code{sig.level} has a non-\code{NULL} default 
  so \code{NULL} must be explicitly passed if you want to compute it.

  The computations are based on the formulas given in the Appendix of 
  Flahault et al. (2005). Please be careful, in Equation (A1) the numerator
  should be squared, in equation (A2) and (A3) the second exponent should be
  n-i and not i.
  
  As noted in Chu and Cole (2007) power is not a monotonically increasing
  function in n but rather saw toothed (see also Chernick and Liu (2002)).
  Hence, in our calculations we use the more conservative approach II); 
  i.e., the minimum sample size \code{n} such that the actual power is 
  larger or equal \code{power} andsuch that for any sample size larger 
  than \code{n} it also holds that the actual power is larger or equal 
  \code{power}.
}
\value{
  Object of class \code{"power.htest"}, a list of the arguments
  (including the computed one) augmented with \code{method} and
  \code{note} elements.
}

\references{ 
  A. Flahault, M. Cadilhac, and G. Thomas (2005). Sample size calculation 
  should be performed for design accuracy in diagnostic test studies. 
  \emph{Journal of Clinical Epidemiology}, \bold{58}(8):859-862.

  H. Chu and S.R. Cole (2007). Sample size calculation using exact methods 
  in diagnostic test studies. 
  \emph{Journal of Clinical Epidemiology}, \bold{60}(11):1201-1202.
  
  M.R. Chernick amd C.Y. Liu (2002). The saw-toothed behavior of power versus 
  sample size and software solutions: single binomial proportion using 
  exact methods. \emph{Am Stat}, \bold{56}:149-155.
}

\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}

\note{
  \code{uniroot} is used to solve power equation for unknowns, so
  you may see errors from it, notably about inability to bracket the
  root when invalid arguments are given.
}
\seealso{\code{\link{uniroot}}}

\examples{
## see n2 on page 1202 of Chu and Cole (2007)
power.diagnostic.test(sens = 0.99, delta = 0.14, power = 0.95) # 40
power.diagnostic.test(sens = 0.99, delta = 0.13, power = 0.95) # 43
power.diagnostic.test(sens = 0.99, delta = 0.12, power = 0.95) # 47

power.diagnostic.test(sens = 0.98, delta = 0.13, power = 0.95) # 50
power.diagnostic.test(sens = 0.98, delta = 0.11, power = 0.95) # 58

## see page 1201 of Chu and Cole (2007)
power.diagnostic.test(sens = 0.95, delta = 0.1, n = 93) ## 0.957
power.diagnostic.test(sens = 0.95, delta = 0.1, n = 93, power = 0.95, 
                      sig.level = NULL) ## 0.0496
power.diagnostic.test(sens = 0.95, delta = 0.1, n = 102) ## 0.968
power.diagnostic.test(sens = 0.95, delta = 0.1, n = 102, power = 0.95, 
                      sig.level = NULL) ## 0.0471
## yields 102 not 93!
power.diagnostic.test(sens = 0.95, delta = 0.1, power = 0.95)
}
\keyword{htest}


