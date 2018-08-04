\name{melt.long}
\alias{melt.long}
\title{ Transform data.frame to Long Form }
\description{
  The function transforms a given data.frame form wide to long form.
}
\usage{
melt.long(data, select, group)
}
\arguments{
  \item{data}{ data.frame that shall be transformed.}
  \item{select}{ optional integer vector to select a subset of the columns of \code{data}.}
  \item{group}{ optional vector to include an additional grouping in the output;
  for more details see examples below.}
}
\details{
  The function transforms a given data.frame form wide to long form. This is
  for example useful for plotting with ggplot2.
}
\value{
  data.frame in long form.
}
%\references{}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de} }
\examples{
library(ggplot2)
## some random data
test <- data.frame(x = rnorm(10), y = rnorm(10), z = rnorm(10))
test.long <- melt.long(test)
test.long
ggplot(test.long, aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = variable))
## introducing an additional grouping variable
group <- factor(rep(c("a","b"), each = 5))
test.long.gr <- melt.long(test, select = 1:2, group = group)
test.long.gr
ggplot(test.long.gr, aes(x = variable, y = value, fill = group)) +
  geom_boxplot()
}
\keyword{univar}
