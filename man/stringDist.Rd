\name{stringDist}
\alias{stringDist}
\title{Function to compute distances between strings}
\description{
 The function can be used to compute distances between strings.
}
\usage{
stringDist(x, y, method = "levenshtein", mismatch = 1, gap = 1)
}
\arguments{
  \item{x}{character vector, first string}
  \item{y}{character vector, second string}
  \item{method}{character, name of the distance method. This must be 
        \code{"levenshtein"} or \code{"hamming"}. Default is the classical
        Levenshtein distance.}
  \item{mismatch}{numeric, distance value for a mismatch between symbols}
  \item{gap}{numeric, distance value for inserting a gap}
}
\details{
  The function computes the Hamming and the Levenshtein (edit) distance of two given strings
  (sequences). 
  
  In case of the Hamming distance the two strings must have the same length.
  
  In case of the Levenshtein (edit) distance a scoring and a trace-back matrix are computed
  and are saved as attributes \code{"ScoringMatrix"} and \code{"TraceBackMatrix"}. 
  The characters in the trace-back matrix reflect insertion of a gap in string \code{y} 
  (\code{d}: deletion), match (\code{m}), mismatch (\code{mm}), 
  and insertion of a gap in string \code{x} (\code{i}).
}
\value{
  \code{stringDist} returns an object of S3 class \code{"stringDist"} inherited 
  from class \code{"dist"}; cf. \code{\link[stats]{dist}}.
}
\references{
  R. Merkl and S. Waack (2009). Bioinformatik Interaktiv. Wiley.
}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de} }
\note{
  The function is mainly for teaching purposes.
  
  For distances between strings and string alignments see also Bioconductor package
  \pkg{Biostrings}.
}

\seealso{ \code{\link[stats]{dist}}, \code{\link{stringSim}} }
\examples{
x <- "GACGGATTATG"
y <- "GATCGGAATAG"
## Levenshtein distance
d <- stringDist(x, y)
d
attr(d, "ScoringMatrix")
attr(d, "TraceBackMatrix")

## Hamming distance
stringDist(x, y)
}
\keyword{univar}
