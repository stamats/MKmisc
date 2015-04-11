\name{HLgof.test}
\alias{HLgof.test}
\title{
  Hosmer-Lemeshow goodness of fit tests.
}
\description{
  The function computes Hosmer-Lemeshow goodness of fit tests
  for C and H statistic as well as the le Cessie-van Houwelingen-Copas-Hosmer 
  unweighted sum of squares test for global goodness of fit.
}
\usage{
HLgof.test(fit, obs, ngr = 10, X, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{ numeric vector with fitted probabilities. }
  \item{obs}{ numeric vector with observed values. }
  \item{ngr}{ number of groups for C and H statistic. }
  \item{X}{ covariate(s) for le Cessie-van Houwelingen-Copas-Hosmer
  global goodness of fit test. }
  \item{verbose}{ logical, print intermediate results. }
}
\details{
  Hosmer-Lemeshow goodness of fit tests are computed; see Lemeshow and Hosmer 
  (1982).
  
  If \code{X} is specified, the le Cessie-van Houwelingen-Copas-Hosmer 
  unweighted sum of squares test for global goodness of fit is additionally 
  determined; see Hosmer et al. (1997).
  A more general version of this test is implemented in function 
  \code{\link[rms]{residuals.lrm}} in package \pkg{rms}.
}
\value{
  A list of test results.
}
\references{
  S. Lemeshow and D.W. Hosmer (1982). A review of goodness of fit statistics
  for use in the development of logistic regression models. 
  \emph{American Journal of Epidemiology, \bold{115}(1), 92-106.}

  D.W. Hosmer, T. Hosmer, S. le Cessie, S. Lemeshow (1997). A comparison 
  of goodness-of-fit tests for the logistic regression model. 
  \emph{Statistics in Medicine}, \bold{16}, 965-980.
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\seealso{ \code{\link[rms]{residuals.lrm}} }
\examples{
set.seed(111)
x1 <- factor(sample(1:3, 50, replace = TRUE))
x2 <- rnorm(50)
obs <- sample(c(0,1), 50, replace = TRUE)
fit <- glm(obs ~ x1+x2, family = binomial)
HLgof.test(fit = fitted(fit), obs = obs)
HLgof.test(fit = fitted(fit), obs = obs, X = model.matrix(obs ~ x1+x2))
}
\keyword{univar}
