\name{AUC}
\alias{AUC}
\title{ Compute AUC }
\description{
  The function computes AUC.
}
\usage{
AUC(x, y, group, switchAUC = TRUE)
}
\arguments{
  \item{x}{ numeric vector. }
  \item{y}{ numeric vector. If missing, \code{group} has to be specified.}
  \item{group}{ grouping vector or factor. }
  \item{switchAUC}{logical value. Switch AUC; see Details section.}
}
\details{
  The function computes the area under the receiver operating 
  characteristic curve (AUC under ROC curve). 
  
  If \code{AUC < 0.5}, a warning is printed and \code{1-AUC} is returned. This 
  behaviour can be suppressed by using \code{switchAUC = FALSE}

  The implementation uses the connection of AUC to the Wilcoxon rank sum test;
  see Hanley and McNeil (1982).
}
\value{
  AUC value.
}
\references{ 
  J. A. Hanley and B. J. McNeil (1982). The meaning and use of the area under a receiver
  operating characteristic (ROC) curve. \emph{Radiology}, \bold{143}, 29-36.
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\examples{
set.seed(13)
x <- rnorm(100) ## assumed as log2-data
g <- sample(1:2, 100, replace = TRUE)
AUC(x, group = g)
## avoid switching AUC
AUC(x, group = g, switchAUC = FALSE)
}
\keyword{univar}
