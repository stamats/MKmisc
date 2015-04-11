\name{heatmapCol}
\alias{heatmapCol}
\title{ Generate colors for heatmaps }
\description{
  This function modifies a given color vector as used for heatmaps.
}
\usage{
heatmapCol(data, col, lim, na.rm = TRUE)
}
\arguments{
  \item{data}{ matrix or data.frame; data which shall be displayed in a heatmap; ranging from negative to positive numbers. }
  \item{col}{ vector of colors used for heatmap. }
  \item{lim}{ constant colors are used for data below \code{-lim} resp. above \code{lim}. }
  \item{na.rm}{ logical; remove \code{NA} values. }
}
\details{
  Colors below and above a specified value are kept constant. In addition, the colors
  are symmetrizised.
}
\value{
  vector of colors
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\note{
  A first version of this function appeared in package SLmisc.
}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
data.plot <- matrix(rnorm(100*50, sd = 1), ncol = 50)
colnames(data.plot) <- paste("patient", 1:50)
rownames(data.plot) <- paste("gene", 1:100)
data.plot[1:70, 1:30] <- data.plot[1:70, 1:30] + 3
data.plot[71:100, 31:50] <- data.plot[71:100, 31:50] - 1.4
data.plot[1:70, 31:50] <- rnorm(1400, sd = 1.2)
data.plot[71:100, 1:30] <- rnorm(900, sd = 1.2)
nrcol <- 128

require(gplots)
require(RColorBrewer)
myCol <- rev(colorRampPalette(brewer.pal(10, "RdBu"))(nrcol))
heatmap.2(data.plot, col =  myCol, trace = "none", tracecol = "black")
farbe <- heatmapCol(data = data.plot, col = myCol, 
                    lim = min(abs(range(data.plot)))-1)
heatmap.2(data.plot, col = farbe, trace = "none", tracecol = "black")
}
\keyword{hplot}
