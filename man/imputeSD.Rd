\name{imputeSD}
\alias{imputeSD}
\title{ Impute Standard Deviations for Changes from Baseline }
\description{
  The function imputes standard deviations for changes from baseline
  adopting the approach describe in the Cochrane handbook, Section 16.1.3.2.
}
\usage{
imputeSD(SD1, SD2, SDchange)
}
\arguments{
  \item{SD1}{ numeric vector, baseline SD. }
  \item{SD2}{ numeric vector, follow-up SD.}
  \item{SDchange}{ numeric vector, SD for changes from baseline. }
}
\details{
  The function imputes standard deviations for changes from baseline
  adopting the approach describe in the Cochrane handbook, Section 16.1.3.2.

  1) Missing \code{SD1} are replaced by correspondig values of \code{SD2} and
     vice versa.

  2) Correlations for complete data (rows) are computed.

  3) Minimum, mean and maximum correlation (over rows) are computed.

  4) Missing values of SDchange are computed by the formula provided in
     the handbook. The minimum, mean and maximum correlation are used leading
     to maximal, mean and minimal SD values that may be used for imputation as
     well as a sensitivity analysis.
}
\value{
  \code{data.frame} with possibly imputed SD1 and SD2 values as well as the
  given SDchange values are returen. Moreover, the computed correlations as
  well as possible values for the imputation of SDchange are returned.
}
\references{
  Higgins JPT, Green S (editors). Cochrane Handbook for Systematic Reviews
  of Interventions Version 5.1.0 [updated March 2011]. The Cochrane
  Collaboration, 2011. Available from www.handbook.cochrane.org.
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\examples{
SD1 <- c(0.149, 0.022, 0.036, 0.085, 0.125, NA, 0.139, 0.124, 0.038)
SD2 <- c(NA, 0.039, 0.038, 0.087, 0.125, NA, 0.135, 0.126, 0.038)
SDchange <- c(NA, NA, NA, 0.026, 0.058, NA, NA, NA, NA)
imputeSD(SD1, SD2, SDchange)
}
\keyword{univar}
