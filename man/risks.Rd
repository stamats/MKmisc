\name{risks}
\alias{risks}
\title{ Compute RR, OR, etc. }
\description{
  The function computes relative risk (RR), odds ration (OR), and several
  other risk measures; see details.
}
\usage{
risks(p0, p1)
}
\arguments{
  \item{p0}{ numeric vector of length 1: incidence of the outcome of interest in
             the nonexposed group.}
  \item{p1}{ numeric vector of length 1: incidence of the outcome of interest in
             the exposed group.}
}
\details{
  The function computes relative risk (RR), odds-ratio (OR), relative risk
  reduction (RRR) resp. relative risk increase (RRI), absolute risk reduction (ARR)
  resp. absolute risk increase (ARI), number needed to treat (NNT) resp.
  number needed to harm (NNH).
}
\value{
  Vector including several risk measures.
}
\references{
  See for instance:
  Relative risk. (2016, November 4). In Wikipedia, The Free Encyclopedia.
  Retrieved 19:58, November 4, 2016,
  from \url{https://en.wikipedia.org/w/index.php?title=Relative_risk&oldid=747857409}
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\examples{
## See worked example in Wikipedia
risks(p0 = 0.4, p1 = 0.1)
risks(p0 = 0.4, p1 = 0.5)
}
\keyword{univar}
