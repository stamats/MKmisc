\name{traceBack}
\alias{traceBack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to trace back
}
\description{
Function computes an optimal global or local alignment based on a trace back
matrix as provided by function \code{\link{stringDist}} or \code{\link{stringSim}}.
}
\usage{
traceBack(D, global = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{D}{object of class \code{"stringDist"}}
  \item{global}{logical,  global or local alignment}
}
\details{
Computes one possible optimal global or local alignment based on the trace back
matrix saved in an object of class \code{"stringDist"} or \code{"stringSim"}.
}
\value{matrix: pairwise global/local alignment}
\references{
  R. Merkl and S. Waack (2009). Bioinformatik Interaktiv. Wiley.
}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\note{
  The function is mainly for teaching purposes.

  For distances between strings and string alignments see Bioconductor package
  \pkg{Biostrings}.
}
\seealso{
\code{\link{stringDist}}
}
\examples{
x <- "GACGGATTATG"
y <- "GATCGGAATAG"

## Levenshtein distance
d <- stringDist(x, y)
## optimal global alignment
traceBack(d)

## Optimal global alignment score
d <- stringSim(x, y)
## optimal global alignment
traceBack(d)

## Optimal local alignment score
d <- stringSim(x, y, global = FALSE)
## optimal local alignment
traceBack(d, global = FALSE)
}
\keyword{univar}
