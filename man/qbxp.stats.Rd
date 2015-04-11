\name{qbxp.stats}
\alias{qbxp.stats}
\title{ Box Plot Statistics }
\description{
  This functions works identical to \code{\link[grDevices]{boxplot.stats}}.
  It is typically called by another function to gather the statistics 
  necessary for producing box plots, but may be invoked separately.
}
\usage{
qbxp.stats(x, coef = 1.5, do.conf = TRUE, do.out = TRUE, type = 7)
}
\arguments{
  \item{x}{ a numeric vector for which the boxplot will be constructed 
    (\code{\link{NA}}s and \code{\link{NaN}}s are allowed and omitted). }
  \item{coef}{ it determines how far the plot \sQuote{whiskers} extend out
    from the box. If \code{coef} is positive, the whiskers extend to the
    most extreme data point which is no more than \code{coef} times
    the length of the box away from the box. A value of zero causes
    the whiskers to extend to the data extremes (and no outliers be returned). }
  \item{do.conf}{ logical; if \code{FALSE}, the \code{conf} component 
    will be empty in the result. }
  \item{do.out}{ logical; if \code{FALSE}, \code{out} component will 
    be empty in the result. }
  \item{type}{ an integer between 1 and 9 selecting one of nine quantile
    algorithms; for more details see \code{\link[stats]{quantile}}. }
}
\details{
  The notches (if requested) extend to \code{+/-1.58 IQR/sqrt(n)}.
  This seems to be based on the same calculations as the formula with 1.57 in
  Chambers \emph{et al.} (1983, p. 62), given in McGill \emph{et al.}
  (1978, p. 16).  They are based on asymptotic normality of the median
  and roughly equal sample sizes for the two medians being compared, and
  are said to be rather insensitive to the underlying distributions of
  the samples.  The idea appears to be to give roughly a 95\% confidence
  interval for the difference in two medians.
}
\value{
  List with named components as follows:
  \item{stats}{a vector of length 5, containing the extreme of the
    lower whisker, the first quartile, the median, the third quartile
    and the extreme of the upper whisker.}
  \item{n}{the number of non-\code{NA} observations in the sample.}
  \item{conf}{the lower and upper extremes of the \sQuote{notch}
    (\code{if(do.conf)}). See the details.}
  \item{out}{the values of any data points which lie beyond the
    extremes of the whiskers (\code{if(do.out)}).}

  Note that \code{$stats} and \code{$conf} are sorted in \emph{in}creasing
  order, unlike S, and that \code{$n} and \code{$out} include any
  \code{+- Inf} values.
}
\references{ 
  Tukey, J. W. (1977) \emph{Exploratory Data Analysis.} Section 2C.

  McGill, R., Tukey, J. W. and Larsen, W. A. (1978) Variations of box
  plots. \emph{The American Statistician} \bold{32}, 12--16.

  Velleman, P. F. and Hoaglin, D. C. (1981) \emph{Applications, Basics
    and Computing of Exploratory Data Analysis.}  Duxbury Press.

  Emerson, J. D and Strenio, J. (1983). Boxplots and batch comparison.
  Chapter 3 of \emph{Understanding Robust and Exploratory Data
    Analysis}, eds. D. C. Hoaglin, F. Mosteller and J. W. Tukey.  Wiley.

  Chambers, J. M., Cleveland, W. S., Kleiner, B. and Tukey, P. A. (1983)
  \emph{Graphical Methods for Data Analysis.}  Wadsworth \& Brooks/Cole.
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{ \code{\link[stats]{quantile}}, \code{\link[grDevices]{boxplot.stats}} }
\examples{
## adapted example from boxplot.stats
x <- c(1:100, 1000)
(b1 <- qbxp.stats(x))
(b2 <- qbxp.stats(x, do.conf=FALSE, do.out=FALSE))
stopifnot(b1$stats == b2$stats) # do.out=F is still robust
qbxp.stats(x, coef = 3, do.conf=FALSE)
## no outlier treatment:
qbxp.stats(x, coef = 0)

qbxp.stats(c(x, NA)) # slight change : n is 101
(r <- qbxp.stats(c(x, -1:1/0)))
stopifnot(r$out == c(1000, -Inf, Inf))
}
\keyword{dplot}
