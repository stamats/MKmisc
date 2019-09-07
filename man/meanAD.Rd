\name{meanAD}
\alias{meanAD}
\title{The Mean Absolute Deviation}
\usage{
meanAD(x, na.rm = FALSE, constant = sqrt(pi/2))
}
\description{Computes (standardized) mean absolute deviation.}
\arguments{
  \item{x}{a numeric vector.}
  \item{na.rm}{logical. Should missing values be removed?}
  \item{constant}{ standardizing contant; see details below. }
}
\details{
  The mean absolute deviation is a consistent estimator of 
  \eqn{\sqrt{2/\pi}\sigma}{sqrt(2/pi)sigma} for the standard deviation of
  a normal distribution. Under minor deviations of the normal distributions
  its asymptotic variance is smaller than that of the sample standard 
  deviation (Tukey (1960)).
  
  It works well under the assumption of symmetric, where mean and median
  coincide. Under the normal distribution it's about 18\% more efficient
  (asymptotic relative efficiency) than the median absolute deviation 
  (\code{(1/qnorm(0.75))/sqrt(pi/2)}) and about 12\% less efficient than the 
  sample standard deviation (Tukey (1960)).
}
\references{
  Tukey, J. W. (1960). A survey of sampling from contaminated distribution.
  In Olink, I., editor, \emph{Contributions to Probablity and Statistics. 
  Essays in Honor of H. Hotelling.}, pages 448-485. Stanford University Press.
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\seealso{
 \code{\link[stats]{sd}}, \code{\link[stats]{mad}}, \code{sIQR}.
}
\examples{
## right skewed data
## mean absolute deviation
meanAD(rivers)
## standardized IQR
sIQR(rivers)
## median absolute deviation
mad(rivers)
## sample standard deviation
sd(rivers)

## for normal data
x <- rnorm(100)
sd(x)
sIQR(x)
mad(x)
meanAD(x)

## Asymptotic relative efficiency for Tukey's symmetric gross-error model
## (1-eps)*Norm(mean, sd = sigma) + eps*Norm(mean, sd = 3*sigma)
eps <- seq(from = 0, to = 1, by = 0.001)
ARE <- function(eps){
  0.25*((3*(1+80*eps))/((1+8*eps)^2)-1)/(pi*(1+8*eps)/(2*(1+2*eps)^2)-1)
}
plot(eps, ARE(eps), type = "l", xlab = "Proportion of gross-errors",
     ylab = "Asymptotic relative efficiency", 
     main = "ARE of mean absolute deviation w.r.t. sample standard deviation")
abline(h = 1.0, col = "red")
text(x = 0.5, y = 1.5, "Mean absolute deviation is better", col = "red", 
    cex = 1, font = 1)
## lower bound of interval
uniroot(function(x){ ARE(x)-1 }, interval = c(0, 0.002))
## upper bound of interval
uniroot(function(x){ ARE(x)-1 }, interval = c(0.5, 0.55))
## worst case
optimize(ARE, interval = c(0,1), maximum = TRUE)
}
\keyword{univar}
\keyword{robust}
\keyword{distribution}

