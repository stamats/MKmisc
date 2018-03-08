\name{madMatrix}
\alias{madMatrix}
\title{ Compute MAD between colums of a matrix or data.frame }
\description{
  Compute MAD between colums of a matrix or data.frame. Can be used to create
  a similarity matrix for a microarray experiment.
}
\usage{
madMatrix(x)
}
\arguments{
  \item{x}{ matrix or data.frame }
}
\details{
  This functions computes the so called similarity matrix (based on MAD)
  for a microarray experiment; cf. Buness et. al. (2004).
}
\value{
  matrix of MAD values between colums of \code{x}
}
\references{
  Andreas Buness, Wolfgang Huber, Klaus Steiner, Holger Sueltmann, and
  Annemarie Poustka. arrayMagic: two-colour cDNA microarray quality
  control and preprocessing. Bioinformatics Advance Access published on
  September 28, 2004. doi:10.1093/bioinformatics/bti052
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\note{
  A first version of this function appeared in package SLmisc.
}
\seealso{\code{plotMAD}}
\examples{
## only a dummy example
madMatrix(matrix(rnorm(1000), ncol = 10))
}
\keyword{univar}
