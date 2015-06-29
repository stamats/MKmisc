\name{mi.t.test}
\alias{mi.t.test}
\alias{mi.t.test.default}
\title{Multiple Imputation Student's t-Test}
\description{
  Performs one and two sample t-tests on multiple imputed datasets.
}
\usage{
mi.t.test(miData, \dots)

\method{mi.t.test}{default}(miData, x, y = NULL,
        alternative = c("two.sided", "less", "greater"), mu = 0,
        paired = FALSE, var.equal = FALSE, conf.level = 0.95,
        subset = NULL, \dots)
}
\arguments{
  \item{miData}{list of multiple imputed datasets.}
  \item{x}{name of a variable that shall be tested.}
  \item{y}{an optional name of a variable that shall be tested (paired test)
  or a variable that shall be used to split into groups (unpaired test).}
  \item{alternative}{a character string specifying the alternative
    hypothesis, must be one of \code{"two.sided"} (default),
    \code{"greater"} or \code{"less"}.  You can specify just the initial
    letter.}
  \item{mu}{a number indicating the true value of the mean (or
    difference in means if you are performing a two sample test).}
  \item{paired}{a logical indicating whether you want a paired
    t-test.}
  \item{var.equal}{a logical variable indicating whether to treat the
    two variances as being equal. If \code{TRUE} then the pooled
    variance is used to estimate the variance otherwise the Welch
    (or Satterthwaite) approximation to the degrees of freedom is used.}
  \item{conf.level}{confidence level of the interval.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used.}
  \item{\dots}{further arguments to be passed to or from methods.}
}
\details{
  \code{alternative = "greater"} is the alternative that \code{x} has a
  larger mean than \code{y}.

  If \code{paired} is \code{TRUE} then both \code{x} and \code{y} must
  be specified and they must be the same length.  Missing values are
  not allowed as they should have been imputed.  If
  \code{var.equal} is \code{TRUE} then the pooled estimate of the
  variance is used.  By default, if \code{var.equal} is \code{FALSE}
  then the variance is estimated separately for both groups and the
  Welch modification to the degrees of freedom is used.

  We use the approach of Rubin (1987) in combination with the adjustment of
  Barnard and Rubin (1999).
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the value of the t-statistic.}
  \item{parameter}{the degrees of freedom for the t-statistic.}
  \item{p.value}{the p-value for the test.}
  \item{conf.int}{a confidence interval for the mean appropriate to the
    specified alternative hypothesis.}
  \item{estimate}{the estimated mean (one-sample test), difference in means
    (paired test), or estimated means (two-sample test) as well as the
    respective standard deviations.}
  \item{null.value}{the specified hypothesized value of the mean or mean
    difference depending on whether it was a one-sample test or a
    two-sample test.}
  \item{alternative}{a character string describing the alternative
    hypothesis.}
  \item{method}{a character string indicating what type of t-test was
    performed.}
  \item{data.name}{a character string giving the name(s) of the data.}
}
\references{
  Rubin, D. (1987). \emph{Multiple Imputation for Nonresponse in Surveys}.
  John Wiley \& Sons, New York.

  Barnard, J. and Rubin, D. (1999). Small-Sample Degrees of Freedom with
  Multiple Imputation. \emph{Biometrika}, \bold{86}(4), 948-955.
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\seealso{
  \code{\link[stats]{t.test}}
}
\examples{
## Generate some data
set.seed(123)
x <- rnorm(25, mean = 1)
x[sample(1:25, 5)] <- NA
y <- rnorm(20, mean = -1)
y[sample(1:20, 4)] <- NA
pair <- c(rnorm(25, mean = 1), rnorm(20, mean = -1))
g <- factor(c(rep("yes", 25), rep("no", 20)))
D <- data.frame(ID = 1:45, variable = c(x, y), pair = pair, group = g)

## Use Amelia to impute missing values
library(Amelia)
res <- amelia(D, m = 10, p2s = 0, idvars = "ID", noms = "group")

## Per protocol analysis (Welch two-sample t-test)
t.test(variable ~ group, data = D)
## Intention to treat analysis (Multiple Imputation Welch two-sample t-test)
mi.t.test(res$imputations, x = "variable", y = "group")

## Per protocol analysis (Two-sample t-test)
t.test(variable ~ group, data = D, var.equal = TRUE)
## Intention to treat analysis (Multiple Imputation two-sample t-test)
mi.t.test(res$imputations, x = "variable", y = "group", var.equal = TRUE)

## Specifying alternatives
mi.t.test(res$imputations, x = "variable", y = "group", alternative = "less")
mi.t.test(res$imputations, x = "variable", y = "group", alternative = "greater")

## One sample test
t.test(D$variable[D$group == "yes"])
mi.t.test(res$imputations, x = "variable", subset = D$group == "yes")
mi.t.test(res$imputations, x = "variable", mu = -1, subset = D$group == "yes",
          alternative = "less")
mi.t.test(res$imputations, x = "variable", mu = -1, subset = D$group == "yes",
          alternative = "greater")

## paired test
t.test(D$variable, D$pair, paired = TRUE)
mi.t.test(res$imputations, x = "variable", y = "pair", paired = TRUE)
}
\keyword{htest}
