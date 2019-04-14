\name{pairwise.mod.t.test}
\alias{pairwise.mod.t.test}
\title{Pairwise Moderated t-Tests}
\description{
  Performs pairwise moderated t-tests based on Bioconductor package limma.
}
\usage{
pairwise.mod.t.test(x, group, adjust.method = "BH", sort.by = "none")
}
\arguments{
  \item{x}{a (non-empty) numeric matrix of data values.}
  \item{group}{an optional factor representing the groups.}
  \item{adjust.method}{see \code{\link{p.adjust}}}
  \item{sort.by}{see \code{\link[limma]{toptable}}}, where \code{"logFC"}
  corresponds to difference in means.
}
\details{
  The function uses Bioconductor package limma to compute pairwise moderated
  t-tests. For more details we refer to \code{\link[limma]{ebayes}}.
}
\value{
  A data.frame with the results.
}
\references{
  B. Phipson, S. Lee, I.J. Majewski, W.S. Alexander, G.H. Smyth (2016). Robust
  hyperparameter estimation protects against hypervariable genes and improves
  power to detect differential expression. \emph{Annals of Applied Statistics}
  10(2), 946-963.
}
\seealso{\code{\link{oneway.test}}, \code{mod.t.test}}
\examples{
set.seed(123)
X <- rbind(matrix(rnorm(5*20), nrow = 5, ncol = 20),
           matrix(rnorm(5*20, mean = 1), nrow = 5, ncol = 20))
gr <- factor(c(rep("A1", 5), rep("B2", 5), rep("C3", 5), rep("D4", 5)))
mod.oneway.test(X, gr)
pairwise.mod.t.test(X, gr)
}
\keyword{htest}
