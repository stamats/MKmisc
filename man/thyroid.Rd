\name{thyroid}
\alias{thyroid}
\title{ Plot TSH, fT3 and fT4 with respect to reference range. }
\description{
  The function computes and plots TSH, fT3 and fT4 values with respect to
  the provided reference range.
}
\usage{
thyroid(TSH, fT3, fT4, TSHref, fT3ref, fT4ref)
}
\arguments{
  \item{TSH}{ numeric vector of length 1: measured TSH concentration.}
  \item{fT3}{ numeric vector of length 1: measured fT3 concentration.}
  \item{fT4}{ numeric vector of length 1: measured fT4 concentration.}
  \item{TSHref}{ numeric vector of length 2: reference range TSH.}
  \item{fT3ref}{ numeric vector of length 2: reference range fT3.}
  \item{fT4ref}{ numeric vector of length 2: reference range fT4.}
}
\details{
  A simple function that computes the relative values of the measured values
  with respect to the provided reference range and visualizes the values
  using a barplot. Relative values between 40\% and 60\% are marked as O.K..
}
\value{
  Invisible \code{data.frame} with the relative values.
}
%\references{}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\examples{
thyroid(TSH = 1.5, fT3 = 2.5, fT4 = 14, TSHref = c(0.2, 3.0),
        fT3ref = c(1.7, 4.2), fT4ref = c(7.6, 15.0))
}
\keyword{hplot}
