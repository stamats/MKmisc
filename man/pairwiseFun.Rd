\name{pairwise.fun}
\alias{pairwise.fun}
\title{ Compute pairwise values for a given function }
\description{
  The function computes pairwise values for a given function.
}
\usage{
pairwise.fun(x, g, fun, ...)
}
\arguments{
  \item{x}{ numeric vector. }
  \item{g}{ grouping vector or factor }
  \item{fun}{ some function where the first two arguments have to be
              numeric vectors for which the function computes some
              quantity; see example section below. }
  \item{\dots}{ additional arguments to fun. }
}
\details{
  The function computes pairwise values for a given function.

  The implementation is in certain aspects analogously to 
  \code{\link[stats]{pairwise.t.test}}.
}
\value{
  Vector with pairwise function values.
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\seealso{ \code{\link[stats]{pairwise.t.test}}, \code{\link{pairwise.fc}},
          \code{\link{pairwise.logfc}}, \code{\link{pairwise.auc}} }
\examples{
set.seed(13)
x <- rnorm(100)
g <- factor(sample(1:4, 100, replace = TRUE))
levels(g) <- c("a", "b", "c", "d")
pairwise.fun(x, g, fun = function(x, y) t.test(x,y)$p.value)
## in contrast to
pairwise.t.test(x, g, p.adjust.method = "none", pool.sd = FALSE)
}
\keyword{univar}
