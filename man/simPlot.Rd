\name{simPlot}
\alias{simPlot}
\title{ Plot of a similarity matrix. }
\description{
  Plot of similarity matrix. 
}
\usage{
simPlot(x, col, minVal, labels = FALSE, lab.both.axes = FALSE, 
          labcols = "black", title = "", cex.title = 1.2, 
          protocol = FALSE, cex.axis = 0.8, 
          cex.axis.bar = 1, signifBar = 2, ...)
}
\arguments{
  \item{x}{ quadratic data matrix. }
  \item{col}{ colors palette for image. If missing, the \code{RdYlGn} palette
      of \code{RColorBrewer} is used. }
  \item{minVal}{ numeric, minimum value which is display by a color; used to adjust \code{col}}
  \item{labels}{ vector of character strings to be placed at the tickpoints,
          labels for the columns of \code{x}. }
  \item{lab.both.axes}{ logical, display labels on both axes }
  \item{labcols}{ colors to be used for the labels of the columns of \code{x}.
                  \code{labcols} can have either length 1, in which case all 
                  the labels are displayed using the same color, or the same 
                  length as \code{labels}, in which case a color is specified 
                  for the label of each column of \code{x}. }
  \item{title}{ character string, overall title for the plot. }
  \item{cex.title}{ A numerical value giving the amount by which plotting text
                and symbols should be magnified relative to the default;
                cf. \code{\link[graphics]{par}}, \code{cex.main}. }
  \item{protocol}{ logical, display color bar without numbers }
  \item{cex.axis}{ The magnification to be used for axis annotation relative to the 
                         current setting of 'cex'; cf. \code{\link[graphics]{par}}.}
  \item{cex.axis.bar}{ The magnification to be used for axis annotation of the color 
                               bar relative to the current setting of 'cex'; cf. 
                               \code{\link[graphics]{par}}.}
  \item{signifBar}{ integer indicating the precision to be used for the bar.}
  \item{\dots}{ graphical parameters may also be supplied as arguments to the
          function (see \code{\link[graphics]{par}}). For comparison purposes, 
          it is good to set \code{zlim=c(-1,1)}. }
}
\details{
  This functions generates a so called similarity matrix. 

  If \code{min(x)} is smaller than \code{minVal}, the colors in \code{col} are 
  adjusted such that the minimum value which is color coded is equal to \code{minVal}.
}
\value{
  \code{invisible()}
}
\references{ 
  Sandrine Dudoit, Yee Hwa (Jean) Yang, Benjamin Milo Bolstad and with 
  contributions from Natalie Thorne, Ingrid Loennstedt and Jessica Mar.
  sma: Statistical Microarray Analysis.\cr
  http://www.stat.berkeley.edu/users/terry/zarray/Software/smacode.html
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\note{
  The function is a slight modification of function \code{\link[MKmisc]{corPlot}} of package MKmisc.
}
\examples{
## only a dummy example
M <- matrix(rnorm(1000), ncol = 20)
colnames(M) <- paste("Sample", 1:20)
M.cor <- cor(M)

simPlot(M.cor, minVal = min(M.cor))
simPlot(M.cor, minVal = min(M.cor), lab.both.axes = TRUE)
simPlot(M.cor, minVal = min(M.cor), protocol = TRUE)
simPlot(M.cor, minVal = min(M.cor), signifBar = 1)
}
\keyword{univar}
