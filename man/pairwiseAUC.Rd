\name{pairwise.auc}
\alias{pairwise.auc}
\title{ Compute pairwise AUCs }
\description{
  The function computes pairwise AUCs.
}
\usage{
pairwise.auc(x, g)
}
\arguments{
  \item{x}{ numeric vector. }
  \item{g}{ grouping vector or factor }
}
\details{
  The function computes pairwise areas under the receiver operating 
  characteristic curves (AUC under ROC curves) using function \code{\link{AUC}}.

  The implementation is in certain aspects analogously to 
  \code{\link[stats]{pairwise.t.test}}.
}
\value{
  Vector with pairwise AUCs.
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\seealso{ \code{\link{AUC}}, \code{\link[stats]{pairwise.t.test}} }
\examples{
set.seed(13)
x <- rnorm(100)
g <- factor(sample(1:4, 100, replace = TRUE))
levels(g) <- c("a", "b", "c", "d")
pairwise.auc(x, g)
}
\keyword{univar}
