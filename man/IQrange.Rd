\name{IQrange}
\alias{IQrange}
\title{The Interquartile Range}
\usage{
IQrange(x, na.rm = FALSE, type = 7)
}
\description{computes interquartile range of the \code{x} values.}
\arguments{
  \item{x}{a numeric vector.}
  \item{na.rm}{logical. Should missing values be removed?}
  \item{type}{ an integer between 1 and 9 selecting one of nine quantile
          algorithms; for more details see \code{\link[stats]{quantile}}. }
}
\details{
  This function computes quartiles as \code{IQR(x) = quantile(x,3/4) - quantile(x,1/4)}.
  The function is identical to function \code{\link[stats]{IQR}}. It was added
  before the \code{type} argument was introduced to function \code{\link[stats]{IQR}}
  in 2010 (r53643, r53644).
  
  For normally \eqn{N(m,1)} distributed \eqn{X}, the expected value of
  \code{IQR(X)} is \code{2*qnorm(3/4) = 1.3490}, i.e., for a normal-consistent
  estimate of the standard deviation, use \code{IQR(x) / 1.349}.
}
\references{
  Tukey, J. W. (1977). \emph{Exploratory Data Analysis.} Reading: Addison-Wesley.
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\seealso{
 \code{\link[stats]{quantile}}, \code{\link[stats]{IQR}}.
}
\examples{
IQrange(rivers)

## identical to
IQR(rivers)

## but, e.g.
IQrange(rivers, type = 4)
IQrange(rivers, type = 5)
}
\keyword{univar}
\keyword{robust}
\keyword{distribution}

