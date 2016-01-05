\name{simCorVars}
\alias{simCorVars}
\title{ Simulate correlated variables. }
\description{
  The function simulates a pair of correlated variables.
}
\usage{
simCorVars(n, r, plot = TRUE)
}
\arguments{
  \item{n}{ integer: sample size. }
  \item{r}{ numeric: correlation.}
  \item{plot}{logical: generate scatter plot of the variables.}
}
\details{
  The function is mainly for teaching purposes and simulates \code{n} observations
  from a pair of normal distributed variables with correlation \code{r}.

  By specifying \code{plot = TRUE} a scatter plot of the data is generated.
}
\value{
  data.frame with entries \code{Var1} and \code{Var2}
}
%\references{}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\examples{
res <- simCorVars(n = 100, r = 0.8)
cor(res$Var1, res$Var2)
}
\keyword{univar}
