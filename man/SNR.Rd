\name{SNR}
\alias{SNR}
\alias{medSNR}
\alias{iqrSNR}
\title{ Compute SNR }
\description{
  The functions compute SNR as well as two robust versions of the SNR.
}
\usage{
SNR(x, na.rm = FALSE)
}
\arguments{
  \item{x}{ numeric vector. }
  \item{na.rm}{logical. Should missing values be removed?}
}
\details{
  The functions compute the (classical) coefficient of variation as well as
  two robust variants.

  \code{medSNR} uses the (standardized) MAD instead of SD and median instead of mean.

  \code{iqrSNR} uses the (standardized) IQR instead of SD and median instead of mean.
}
\value{
  SNR value.
}
\references{
  C.N.P.G. Arachchige, L.A. Prendergast and R.G. Staudte. Robust analogues
  to the Coefficient of Variation. https://arxiv.org/abs/1907.01110.
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\examples{
## 5% outliers
out <- rbinom(100, prob = 0.05, size = 1)
sum(out)
x <- (1-out)*rnorm(100, mean = 10, sd = 2) + out*25
SNR(x)
medSNR(x)
iqrSNR(x)
}
\keyword{univar}
