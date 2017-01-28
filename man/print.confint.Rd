\name{print.confint}
\title{Print Method for Confidence Intervals}
\alias{print.confint}
\description{
  Printing objects of class \code{"confint"} by a simple \code{\link{print}} method.
}
\usage{
  \method{print}{confint}(x, digits = getOption("digits"), prefix = "\t", ...)
}
\arguments{
  \item{x}{object of class \code{"confint"}.}
  \item{digits}{number of significant digits to be used.}
  \item{prefix}{string, passed to \code{\link{strwrap}} for displaying
    the \code{method} component of the \code{mpe.test} object.}
  \item{\dots}{further arguments to be passed to or from methods.}
}
\details{
  A \code{confint} object is just a named list of confidence intervals
  and respective (point) estimates.
}
\value{
  the argument \code{x}, invisibly, as for all \code{\link{print}}
  methods.
}
\seealso{\code{\link[stats]{print.power.htest}}}
\examples{
x <- rnorm(20)
(CI <- normCI(x))
print(CI, digits = 3)
}
\keyword{univar}
