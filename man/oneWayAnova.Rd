\name{oneWayAnova}
\alias{oneWayAnova}
\docType{data}
\title{ A function for Analysis of Variance }
\description{
  This function is a slight modification of function \code{\link[genefilter]{Anova}} of
  package \code{"genefilter"}.
}
\usage{
oneWayAnova(cov, na.rm = TRUE, var.equal = FALSE)}
\arguments{
  \item{cov}{ The covariate. It must have length equal to the number of
          columns of the array that the result of \code{oneWayAnova} will be 
          applied to. }
  \item{na.rm}{ a logical value indicating whether \code{NA} values should be
          stripped before the computation proceeds. }
  \item{var.equal}{ a logical variable indicating whether to treat the variances
          in the samples as equal.  If \code{TRUE}, then a simple F test for
          the equality of means in a one-way analysis of variance is
          performed.  If \code{FALSE}, an approximate method of Welch (1951)
          is used, which generalizes the commonly known 2-sample Welch
          test to the case of arbitrarily many samples. }
}
\details{
  The function returned by \code{oneWayAnova} uses \code{\link[stats]{oneway.test}} 
  to perform a one-way ANOVA, where \code{x} is the set of gene expressions. 
  The F statistic for an overall effect is computed and the corresponding
  p-value is returned.

  The function \code{\link[genefilter]{Anova}} instead compares the computed 
  p-value to a prespecified p-value and returns \code{TRUE}, if the computed p-value
  is smaller than the prespecified one.
}
\value{
  \code{oneWayAnova} returns a function with bindings for \code{cov} that will
  perform a one-way ANOVA.
     
  The covariate can be continuous, in which case the test is for a linear effect 
  for the covariate.
}
\references{
  R. Gentleman, V. Carey, W. Huber and F. Hahne (2006). 
  genefilter: methods for filtering genes from microarray experiments. 
  R package version 1.13.7.
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\note{
  A first version of this function appeared in package SLmisc.
}
\seealso{ \code{\link[stats]{oneway.test}}, \code{\link[genefilter]{Anova}} }
\examples{
set.seed(123)
af <- oneWayAnova(c(rep(1,5),rep(2,5)))
af(rnorm(10))
}
\keyword{htest}
\keyword{models}
