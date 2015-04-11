\name{fiveNS}
\alias{fiveNS}
\title{ Five-Number Summaries }
\description{
  Function to compute five-number summaries (minimum, 1st quartile,
  median, 3rd quartile, maximum)
}
\usage{
fiveNS(x, na.rm = TRUE, type = 7)
}
\arguments{
  \item{x}{ numeric vector }
  \item{na.rm}{ logical; remove \code{NA} before the computations. }
  \item{type}{ an integer between 1 and 9 selecting one of nine quantile
          algorithms; for more details see \code{\link[stats]{quantile}}. }
}
\details{
  In contrast to \code{\link[stats]{fivenum}} the functions computes the
  first and third quartile using function \code{\link[stats]{quantile}}.
}
\value{
  A numeric vector of length 5 containing the summary information.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{ \code{\link[stats]{fivenum}}, \code{\link[stats]{quantile}}  }
\examples{
x <- rnorm(100)
fiveNS(x)
fiveNS(x, type = 2)
fivenum(x)
}
\keyword{univar}
\keyword{distribution}
