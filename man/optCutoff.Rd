\name{optCutoff}
\alias{optCutoff}
\title{ Compute the Optimal Cutoff for Binary Classification }
\description{
  The function computes the optimal cutoff for various performance weasures for
  binary classification.
}
\usage{
optCutoff(pred, truth, namePos, perfMeasure = "Youden's J statistic",
          max = TRUE, parallel = FALSE, ncores, delta = 0.01)
}
\arguments{
  \item{pred}{ numeric values that shall be used for classification; e.g. probabilities
  to belong to the positive group. }
  \item{truth}{ true grouping vector or factor. }
  \item{namePos}{ value representing the positive group.}
  \item{perfMeasure}{ a performance measure computed by function \code{perfMeasure}.}
  \item{max}{logical value. Whether to maximize or minimize the performacne
  measure.}
  \item{parallel}{ logical value. If \code{TRUE} packages foreach and doParallel
  are used to parallelize the computations.}
  \item{ncores}{integer value, number of cores that shall be used to parallelize
  the computations.}
  \item{delta}{numeric value for setting up grid for optimization; start is 
  minimum of \code{pred-delta}, end is maximum of \code{pred+delta}.}
}
\details{
  The function is ablte to compute the optimal cutoff for various performance
  measures, all performance measures that are implemented in function
  \code{perfMeasures}.
}
\value{
  Optimal cutoff and value of the optimized performance measure based on a 
  simple grid search.
}
%\references{}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\examples{
## example from dataset infert
fit <- glm(case ~ spontaneous+induced, data = infert, family = binomial())
pred <- predict(fit, type = "response")
optCutoff(pred, truth = infert$case, namePos = 1)
}
\keyword{univar}
