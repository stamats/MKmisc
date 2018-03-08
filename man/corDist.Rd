\name{corDist}
\alias{corDist}
\title{ Correlation Distance Matrix Computation }
\description{
  The function computes and returns the correlation and absolute correlation distance
  matrix computed by using the specified distance measure to compute the distances
  between the rows of a data matrix.
}
\usage{
corDist(x, method = "pearson", diag = FALSE, upper = FALSE, abs = FALSE,
        use = "pairwise.complete.obs", ...)
}
\arguments{
  \item{x}{ a numeric matrix or data frame }
  \item{method}{ the correlation distance measure to be used. This must be one of
          \code{"pearson"}, \code{"spearman"}, \code{"kandall"}, \code{"cosine"},
          \code{"mcd"} or \code{"ogk"}, respectively.
          Any unambiguous substring can be given. }
  \item{diag}{ logical value indicating whether the diagonal of the distance
          matrix should be printed by 'print.dist'. }
  \item{upper}{ logical value indicating whether the upper triangle of the
          distance matrix should be printed by 'print.dist'. }
  \item{abs}{ logical, compute absolute correlation distances }
  \item{use}{ character, correponds to argument \code{use} of function
          \code{\link[stats]{cor}} }
  \item{...}{ further arguments to functions \code{\link[robustbase]{covMcd}}
          or \code{\link[robustbase]{covOGK}}, respectively. }
}
\details{
  The function computes the Pearson, Spearman, Kendall or Cosine sample correlation
  and absolute correlation; confer Section 12.2.2 of Gentleman et al (2005). For more
  details about the arguments we refer to functions \code{\link[stats]{dist}} and
  \code{\link[stats]{cor}}.
  Moreover, the function computes the minimum covariance determinant or the
  orthogonalized Gnanadesikan-Kettenring estimator. For more details we refer to
  functions \code{\link[robustbase]{covMcd}} and \code{\link[robustbase]{covOGK}},
  respectively.
}
\value{
  \code{corDist} returns an object of class \code{"dist"}; cf. \code{\link[stats]{dist}}.
}
\references{
  Gentleman R. Ding B., Dudoit S. and Ibrahim J. (2005). Distance Measures in DNA
  Microarray Data Analysis. In: Gentleman R., Carey V.J., Huber W., Irizarry R.A. and
  Dudoit S. (editors) Bioinformatics and Computational Biology Solutions Using R and
  Bioconductor. Springer.

  P. J. Rousseeuw and A. M. Leroy (1987). Robust Regression and Outlier Detection. Wiley.

  P. J. Rousseeuw and K. van Driessen (1999) A fast algorithm for the minimum
  covariance determinant estimator. Technometrics 41, 212-223.

  Pison, G., Van Aelst, S., and Willems, G. (2002), Small Sample Corrections for
  LTS and MCD, Metrika, 55, 111-123.

  Maronna, R.A. and Zamar, R.H. (2002). Robust estimates of location and dispersion
  of high-dimensional datasets; Technometrics 44(4), 307-317.

  Gnanadesikan, R. and John R. Kettenring (1972). Robust estimates, residuals, and
  outlier detection with multiresponse data. Biometrics 28, 81-124.
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\note{
A first version of this function appeared in package SLmisc.
}
\examples{
## only a dummy example
M <- matrix(rnorm(1000), ncol = 20)
D <- corDist(M)
}
\keyword{multivariate}
