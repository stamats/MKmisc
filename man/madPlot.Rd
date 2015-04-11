\name{madPlot}
\alias{madPlot}
\title{ Plot of similarity matrix based on MAD }
\description{
  Plot of similarity matrix based on MAD between microarrays.
}
\usage{
madPlot(x, new = FALSE, col, maxMAD = 3, labels = FALSE, 
        labcols = "black", title = "", protocol = FALSE, ...)
}
\arguments{
  \item{x}{ data or correlation matrix, respectively }
  \item{new}{ If \code{new=FALSE}, \code{x} must already be a matrix with MAD
              values. If \code{new=TRUE}, the MAD matrix for the columns of 
              \code{x} is computed and displayed in the image. }
  \item{col}{ colors palette for image. If missing, the \code{RdYlGn} palette
      of \code{RColorBrewer} is used. }
  \item{maxMAD}{ maximum MAD value displayed  }
  \item{labels}{ vector of character strings to be placed at the tickpoints,
          labels for the columns of \code{x}. }
  \item{labcols}{ colors to be used for the labels of the columns of \code{x}.
                  \code{labcols} can have either length 1, in which case all 
                  the labels are displayed using the same color, or the same 
                  length as \code{labels}, in which case a color is specified 
                  for the label of each column of \code{x}. }
  \item{title}{ character string, overall title for the plot. }
  \item{protocol}{ logical, display color bar without numbers }
  \item{\dots}{ graphical parameters may also be supplied as arguments to the
          function (see \code{\link[graphics]{par}}). For comparison purposes, 
          it is good to set \code{zlim=c(-1,1)}. }
}
\details{
  This functions generates the so called similarity matrix (based on MAD) for 
  a microarray experiment; cf. Buness et. al. (2004). The function is similar
  to \code{\link{corPlot}}.
}
%\value{
%}
\references{ 
  Sandrine Dudoit, Yee Hwa (Jean) Yang, Benjamin Milo Bolstad and with 
  contributions from Natalie Thorne, Ingrid Loennstedt and Jessica Mar.
  sma: Statistical Microarray Analysis.\cr
  http://www.stat.berkeley.edu/users/terry/zarray/Software/smacode.html
  
  Andreas Buness, Wolfgang Huber, Klaus Steiner, Holger Sueltmann, and
  Annemarie Poustka. arrayMagic: two-colour cDNA microarray quality
  control and preprocessing. Bioinformatics Advance Access published on
  September 28, 2004. doi:10.1093/bioinformatics/bti052
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\note{
  A first version of this function appeared in package SLmisc.
}
\seealso{\code{corPlot}}
\examples{
## only a dummy example
set.seed(13)
x <- matrix(rnorm(1000), ncol = 10)
x[1:20,5] <- x[1:20,5] + 10
madPlot(x, new = TRUE, maxMAD = 2.5)
## in contrast
corPlot(x, new = TRUE, minCor = -0.5)
}
\keyword{hplot}
