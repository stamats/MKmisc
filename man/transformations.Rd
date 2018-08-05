\name{transformations}
\alias{glog_trans}
\alias{glog10_trans}
\alias{glog2_trans}
\alias{scale_y_glog}
\alias{scale_x_glog}
\alias{scale_y_glog10}
\alias{scale_x_glog10}
\alias{scale_y_glog2}
\alias{scale_x_glog2}
\alias{neglog_breaks}
\alias{neglog_trans}
\alias{neglog10_trans}
\alias{neglog2_trans}
\alias{scale_y_neglog}
\alias{scale_x_neglog}
\alias{scale_y_neglog10}
\alias{scale_x_neglog10}
\alias{scale_y_neglog2}
\alias{scale_x_neglog2}
\title{ New Transformations for Use with ggplot2 Package}
\description{
  The functions generate new transformations for the generalized logarithm and
  the negative logarithm that can be used for transforming the axes in ggplot2
  plots.
}
\usage{
glog_trans(base = exp(1))
glog10_trans()
glog2_trans()
scale_y_glog(...)
scale_x_glog(...)
scale_y_glog10(...)
scale_x_glog10(...)
scale_y_glog2(...)
scale_x_glog2(...)
neglog_breaks(n = 5, base = 10)
neglog_trans(base = exp(1))
neglog10_trans()
neglog2_trans()
scale_y_neglog(...)
scale_x_neglog(...)
scale_y_neglog10(...)
scale_x_neglog10(...)
scale_y_neglog2(...)
scale_x_neglog2(...)
}
\arguments{
  \item{base}{ a positive or a positive or complex number: the base with
  respect to which generalized and negative logarithms are computed.
  Defaults to e=exp(1).}
  \item{\dots}{ Arguments passed on to scale_(x|y)_continuous.}
  \item{n}{ desired number of breaks.}
}
\details{
  The functions can be used to transform axes in ggplot2 plots. The implementation
  is analogous to e.g. \code{scale_y_log10}.

  The negative logarithm is for instance of use in case of p values (e.g.
  volcano plots),

  The functions were adapted from packages scales and ggplot2.
}
\value{
  A transformation.
}
\references{
  H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\seealso{\code{\link[ggplot2]{scale_continuous}}, \code{\link[scales]{log_trans}}}
\examples{
library(ggplot2)
data(mpg)
p1 <- ggplot(mpg, aes(displ, hwy)) + geom_point()
p1
p1 + scale_x_log10()
p1 + scale_x_glog10()
p1 + scale_y_log10()
p1 + scale_y_glog10()

## A volcano plot
x <- matrix(rnorm(1000, mean = 10), nrow = 10)
g1 <- rep("control", 10)
y1 <- matrix(rnorm(500, mean = 11.25), nrow = 10)
y2 <- matrix(rnorm(500, mean = 9.75), nrow = 10)
g2 <- rep("treatment", 10)
group <- factor(c(g1, g2))
Data <- rbind(x, cbind(y1, y2))
pvals <- apply(Data, 2, function(x, group) t.test(x ~ group)$p.value,
               group = group)
## compute log-fold change
logfc <- function(x, group){
  res <- tapply(x, group, mean)
  log2(res[1]/res[2])
}
lfcs <- apply(Data, 2, logfc, group = group)
ps <- data.frame(pvals = pvals, logfc = lfcs)
ggplot(ps, aes(x = logfc, y = pvals)) + geom_point() +
    geom_hline(yintercept = 0.05) + scale_y_neglog10() +
    geom_vline(xintercept = c(-0.1, 0.1)) + xlab("log-fold change") +
    ylab("-log10(p value)") + ggtitle("A Volcano Plot")
}
\keyword{hplot}
