\name{repMeans}
\alias{repMeans}
\title{ Compute mean of replicated spots }
\description{
  Compute mean of replicated spots where additionally spot flags may incorporated.
}
\usage{
repMeans(x, flags, use.flags = NULL, ndups, spacing, method, ...)
}
\arguments{
  \item{x}{ matrix or data.frame of expression values }
  \item{flags}{ matrix or data.frame of spot flags; must have same dimension as \code{x} }
  \item{use.flags}{ should flags be included and in which way; cf. section details }
  \item{ndups}{ integer, number of replicates on chip. The number of rows of 
                \code{x} must be divisible by \code{ndups} }
  \item{spacing}{ the spacing between the rows of 'x' corresponding to 
                  replicated spots, \code{spacing = 1} for consecutive spots; 
                  cf. function \code{\link[limma]{unwrapdups}} in package 
                  \code{"limma"} }
  \item{method}{ function to aggregate the replicated spots. If missing, the mean is used. }
  \item{...}{ optional arguments to \code{method}. }
}
\details{
  The incorporation of spot flags is controlled via argument \code{use.flags}.

  \code{NULL}: flags are not used; minimum flag value of replicated 
               spots is returned

  \code{"max"}: only spots with flag value equal to the maximum flag value of 
                replicated spots are used

  \code{"median"}: only spots with flag values larger or equal to median of 
                   replicated spots are used

  \code{"mean"}: only spots with flag values larger or equal to mean of replicated 
                 spots are used  
}
\value{
  LIST with components
  \item{exprs}{mean of expression values}
  \item{flags}{flags for mean expression values}
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\note{
  A first version of this function appeared in package SLmisc.
}
\seealso{\code{\link[limma]{unwrapdups}}}
\examples{
## only a dummy example
M <- matrix(rnorm(1000), ncol = 10)
FL <- matrix(rpois(1000, lambda = 10), ncol = 10) # only for this example
res <- repMeans(x = M, flags = FL, use.flags = "max", ndups = 5, spacing = 20)
}
\keyword{univar}
