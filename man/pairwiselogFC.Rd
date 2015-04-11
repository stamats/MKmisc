\name{pairwise.logfc}
\alias{pairwise.logfc}
\title{ Compute pairwise log-fold changes }
\description{
  The function computes pairwise log-fold changes.
}
\usage{
pairwise.logfc(x, g, ave = mean, log = TRUE, base = 2, ...)
}
\arguments{
  \item{x}{ numeric vector. }
  \item{g}{ grouping vector or factor }
  \item{ave}{ function to compute the group averages. }
  \item{log}{ logical. Is the data logarithmic? }
  \item{base}{ If \code{log = TRUE}, the base which was used to compute
        the logarithms. }
  \item{\dots}{ optional arguments to \code{ave}. }
}
\details{
  The function computes pairwise log-fold changes between groups, where
  the group values are aggregated using the function which is 
  given by the argument \code{ave}.

  The implementation is in certain aspects analogously to \code{\link[stats]{pairwise.t.test}}.
}
\value{
  Vector with pairwise log-fold changes.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{ \code{\link[stats]{pairwise.t.test}} }
\examples{
set.seed(13)
x <- rnorm(100) ## assumed as log2-data
g <- factor(sample(1:4, 100, replace = TRUE))
levels(g) <- c("a", "b", "c", "d")
pairwise.logfc(x, g)

## some small checks
res <- by(x, list(g), mean)
res[[1]] - res[[2]] # a vs. b
res[[1]] - res[[3]] # a vs. c
res[[1]] - res[[4]] # a vs. d
res[[2]] - res[[3]] # b vs. c
res[[2]] - res[[4]] # b vs. d
res[[3]] - res[[4]] # c vs. d
}
\keyword{univar}
