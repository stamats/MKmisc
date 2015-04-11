\name{pairwise.fc}
\alias{pairwise.fc}
\title{ Compute pairwise fold changes }
\description{
  This function computes pairwise fold changes. It also works for 
  logarithmic data. 
}
\usage{
pairwise.fc(x, g, ave = mean, log = TRUE, base = 2, mod.fc = TRUE, ...)
}
\arguments{
  \item{x}{ numeric vector. }
  \item{g}{ grouping vector or factor }
  \item{ave}{ function to compute the group averages. }
  \item{log}{ logical. Is the data logarithmic? }
  \item{base}{ If \code{log = TRUE}, the base which was used to compute
        the logarithms. }
  \item{mod.fc}{ logical. Return modified fold changes? (see details) }
  \item{\dots}{ optional arguments to \code{ave}. }
}
\details{
  The function computes pairwise fold changes between groups, where
  the group values are aggregated using the function which is 
  given by the argument \code{ave}.

  The fold changes are returned in a slightly modified form if mod.fc = TRUE. 
  Fold changes \code{FC} which are smaller than \code{1} are reported as
  to \code{-1/FC}.

  The implementation is in certain aspects analogously to \code{\link[stats]{pairwise.t.test}}.
}
\value{
  Vector with pairwise fold changes.
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
pairwise.fc(x, g)

## some small checks
res <- by(x, list(g), mean)
2^(res[[1]] - res[[2]]) # a vs. b
-1/2^(res[[1]] - res[[3]]) # a vs. c
2^(res[[1]] - res[[4]]) # a vs. d
-1/2^(res[[2]] - res[[3]]) # b vs. c
-1/2^(res[[2]] - res[[4]]) # b vs. d
2^(res[[3]] - res[[4]]) # c vs. d
}
\keyword{univar}
