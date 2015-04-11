\name{predValues}
\alias{predValues}
\title{ Compute PPV and NPV. }
\description{
  The function computes the positive (PPV) and negative predictive value (NPV)
  given sensitivity, specificity and prevalence (pre-test probability).
}
\usage{
predValues(sens, spec, prev)
}
\arguments{
  \item{sens}{ numeric vector: sensitivities. }
  \item{spec}{ numeric vector: specificities. }
  \item{prev}{ numeric vector: prevalence.}
}
\details{
  The function computes the positive (PPV) and negative predictive value (NPV)
  given sensitivity, specificity and prevalence (pre-test probability).
  
  It's a simple application of the Bayes formula.
  
  One can also specify vectors of length larger than 1 for sensitivity and specificity.
}
\value{
  Vector or matrix with PPV and NPV.
}
%\references{}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\examples{
## Example: HIV test 
## 1. ELISA screening test (4th generation)
predValues(sens = 0.999, spec = 0.998, prev = 0.001)
## 2. Western-Plot confirmation test
predValues(sens = 0.998, spec = 0.999996, prev = 1/3)

## Example: connection between sensitivity, specificity and PPV
sens <- seq(0.6, 0.99, by = 0.01)
spec <- seq(0.6, 0.99, by = 0.01)
ppv <- function(sens, spec, pre) predValues(sens, spec, pre)[,1]
res <- outer(sens, spec, ppv, pre = 0.1)
image(sens, spec, res, col = terrain.colors(256), main = "PPV for prevalence = 10\%",
      xlim = c(0.59, 1), ylim = c(0.59, 1))
contour(sens, spec, res, add = TRUE)
}
\keyword{univar}
