\name{mod.oneway.test}
\alias{mod.oneway.test}
\title{Moderated 1-Way ANOVA}
\description{
  Performs moderated 1-Way ANOVAs based on Bioconductor package limma.
}
\usage{
mod.oneway.test(x, group, adjust.method = "BH", sort.by = "none")
}
\arguments{
  \item{x}{a (non-empty) numeric matrix of data values.}
  \item{group}{an optional factor representing the groups.}
  \item{adjust.method}{see \code{\link{p.adjust}}}
  \item{sort.by}{see \code{\link[limma]{toptable}}}, where \code{"logFC"}
  corresponds to difference in means.
}
\details{
  The function uses Bioconductor package limma to compute moderated 1-way ANOVAs.
  For more details we refer to \code{\link[limma]{ebayes}}.
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

## Welch 1-Way ANOVA (not moderated)
ow.test <- function(x, g){
  res <- oneway.test(x ~ g)
  c(res$statistic, res$p.value)
}
ow.res <- t(apply(X, 1, ow.test, g = gr))
colnames(ow.res) <- c("F", "p.value")
ow.res
}
\keyword{htest}
