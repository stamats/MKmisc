\name{stringSim}
\alias{stringSim}
\title{Function to compute similarity scores between strings}
\description{
 The function can be used to compute similarity scores between strings.
}
\usage{
stringSim(x, y, global = TRUE, match = 1, mismatch = -1, gap = -1, minSim = 0)
}
\arguments{
  \item{x}{character vector, first string}
  \item{y}{character vector, second string}
  \item{global}{logical; global or local alignment}
  \item{match}{numeric, score for a match between symbols}
  \item{mismatch}{numeric, score for a mismatch between symbols}
  \item{gap}{numeric, penalty for inserting a gap}
  \item{minSim}{numeric, used as required minimum score in case of local alignments}
}
\details{
  The function computes optimal alignment scores for global (Needleman-Wunsch) 
  and local (Smith-Waterman) alignments with constant gap penalties. 
  
  Scoring and trace-back matrix are computed and saved in form of attributes 
  \code{"ScoringMatrix"} and \code{"TraceBackMatrix"}. 
  The characters in the trace-back matrix reflect insertion of a gap in 
  string \code{y} (\code{d}: deletion), match (\code{m}), mismatch (\code{mm}), 
  and insertion of a gap in string \code{x} (\code{i}). In addition \code{stop}
  indicates that the minimum similarity score has been reached.
}
\value{
  \code{stringSim} returns an object of S3 class \code{"stringSim"} inherited 
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

\seealso{ \code{\link[stats]{dist}}, \code{\link{stringDist}} }
\examples{
x <- "GACGGATTATG"
y <- "GATCGGAATAG"

## optimal global alignment score
d <- stringSim(x, y)
d
attr(d, "ScoringMatrix")
attr(d, "TraceBackMatrix")

## optimal local alignment score
d <- stringSim(x, y, global = FALSE)
d
attr(d, "ScoringMatrix")
attr(d, "TraceBackMatrix")
}
\keyword{univar}
