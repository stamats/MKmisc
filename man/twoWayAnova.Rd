\name{twoWayAnova}
\alias{twoWayAnova}
\docType{data}
\title{ A function for Analysis of Variance }
\description{
  This function is a slight modification of function \code{\link[genefilter]{Anova}} of
  package \code{"genefilter"}.
}
\usage{
twoWayAnova(cov1, cov2, interaction, na.rm = TRUE)}
\arguments{
  \item{cov1}{ The first covariate. It must have length equal to the number of
          columns of the array that the result of \code{twoWayAnova} will be 
          applied to. }
  \item{cov2}{ The second covariate. It must have length equal to the number of
          columns of the array that the result of \code{twoWayAnova} will be 
          applied to. }
  \item{interaction}{ logical, should interaction be considered }
  \item{na.rm}{ a logical value indicating whether 'NA' values should be
          stripped before the computation proceeds. }
}
\details{
  The function returned by \code{twoWayAnova} uses \code{\link[stats]{lm}} to fit 
  a linear model of the form \code{lm(x ~ cov1*cov2)}, where \code{x} is the set 
  of gene expressions. The F statistics for the main effects and the interaction are  
  computed and the corresponding p-values are returned.
}
\value{
  \code{twoWayAnova} returns a function with bindings for \code{cov1} and 
  \code{cov2}that will perform a two-way ANOVA.
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
\seealso{ \code{\link[genefilter]{Anova}} }
\examples{
set.seed(123)
af1 <- twoWayAnova(c(rep(1,6),rep(2,6)), rep(c(rep(1,3), rep(2,3)), 2))
af2 <- twoWayAnova(c(rep(1,6),rep(2,6)), rep(c(rep(1,3), rep(2,3)), 2), 
                   interaction = FALSE)
x <- matrix(rnorm(12*10), nrow = 10)
apply(x, 1, af1)
apply(x, 1, af2)
}
\keyword{htest}
\keyword{models}
