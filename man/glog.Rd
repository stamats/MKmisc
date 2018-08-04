\name{glog}
\alias{glog}
\alias{glog10}
\alias{glog2}
\title{ Compute Generalized Logarithm }
\description{
  The function computes the generalized logarithm which is more or less
  identical to the area hyperbolic sine; see details.
}
\usage{
glog(x, base = exp(1))
glog10(x)
glog2(x)
}
\arguments{
  \item{x}{ a numeric or complex vector.}
  \item{base}{ a positive or a positive or complex number: the base with
  respect to which logarithms are computed. Defaults to e=exp(1).}
}
\details{
  The function computes
  \deqn{\log(x + \sqrt{x^2 + 1}) - \log(2)}{log(x + sqrt(x^2 + 1)) - log(2)}
  where the first part corresponds to the area hyperbolic sine. Subtracting
  log(2) makes the function asymptotically identical to the logarithm.
}
\value{
  A vector of the same length as x containing the transformed values.
}
%\references{}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\examples{
curve(log, from = -3, to = 5)
curve(glog, from = -3, to = 5, add = TRUE, col = "orange")
legend("topleft", fill = c("black", "orange"), legend = c("log", "glog"))

curve(log10(x), from = -3, to = 5)
curve(glog10(x), from = -3, to = 5, add = TRUE, col = "orange")
legend("topleft", fill = c("black", "orange"), legend = c("log10", "glog10"))
}
\keyword{univar}
