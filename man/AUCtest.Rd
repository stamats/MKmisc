\name{AUC.test}
\alias{AUC.test}
\title{
  AUC-Test
}
\description{
  Performs tests for one and two AUCs.
}
\usage{
AUC.test(pred1, lab1, pred2, lab2, conf.level = 0.95, paired = FALSE)
}
\arguments{
  \item{pred1}{ numeric vector. }
  \item{lab1}{ grouping vector or factor for \code{pred1}. }
  \item{pred2}{ numeric vector. }
  \item{lab2}{grouping vector or factor for \code{pred2}. }
  \item{conf.level}{ confidence level of the interval. }
  \item{paired}{ not yet implemented. }
}
\details{
  If \code{pred2} and \code{lab2} are missing, the AUC for \code{pred1}
  and \code{lab1} is tested using the Wilcoxon signed rank test;
  see \code{\link[stats]{wilcox.test}}.
  
  If \code{pred1} and \code{lab1} as well as \code{pred2} and \code{lab2}
  are specified, the Hanley and McNeil test (cf. Hanley and McNeil (1982)) 
  is computed.
}
\value{
A list with AUC, SE and confidence interval as well as the corresponding
test result.
}
\references{ 
  J. A. Hanley and B. J. McNeil (1982). The meaning and use of the area under a receiver
  operating characteristic (ROC) curve. \emph{Radiology}, \bold{143}, 29-36.
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\seealso{
\code{\link[stats]{wilcox.test}}, \code{\link{AUC}}
}
\examples{
set.seed(13)
x <- rnorm(100) ## assumed as log2-data
g <- sample(1:2, 100, replace = TRUE)
AUC.test(x, g)
y <- rnorm(100) ## assumed as log2-data
h <- sample(1:2, 100, replace = TRUE)
AUC.test(x, g, y, h)
}
\keyword{univar}
