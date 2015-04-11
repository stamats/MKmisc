\name{ssize.pcc}
\alias{ssize.pcc}
\title{Sample Size Planning for Developing Classifiers Using High Dimensional Data}
\description{
  Calculate sample size for training set in developing classifiers using high
  dimensional data. The calculation is based on the probability of correct classification 
  (PCC).
}
\usage{
ssize.pcc(gamma, stdFC, prev = 0.5, nrFeatures, sigFeatures = 20, verbose = FALSE)
}
\arguments{
 \item{gamma}{tolerance between PCC(infty) and PCC(n).}
 \item{stdFC}{expected standardized fold-change; that is, expected fold-change devided
 by within class standard deviation.}
 \item{prev}{expected prevalence.}
 \item{nrFeatures}{number of features (variables) considered.}
 \item{sigFeatures}{number of significatn features; default (20) should be sufficient 
 for most if not all cases.}
 \item{verbose}{print intermediate results.}
}

\details{
  The computations are based the algorithm provided in Section~4.2 of 
  Dobbin and Simon (2007). Prevalence is incorporated by the simple rough
  approach given in Section~4.4 (ibid.).
  
  The results for prevalence equal to $50\%$ are identical to the numbers computed 
  by \url{http://linus.nci.nih.gov/brb/samplesize/samplesize4GE.html}. For 
  other prevalences the numbers differ and are larger for our implementation. 
}
\value{
  Object of class \code{"power.htest"}, a list of the arguments
  (including the computed one) augmented with \code{method} and
  \code{note} elements.
}

\references{ 
  K. Dobbin and R. Simon (2007). Sample size planning for developing classifiers 
  using high-dimensional DNA microarray data. 
  \emph{Biostatistics}, \bold{8}(1):101-117.
  
  K. Dobbin, Y. Zhao, R. Simon (2008). How Large a Training Set is Needed to 
  Develop a Classifier for Microarray Data?
  \emph{Clin Cancer Res.}, \bold{14}(1):108-114.
}

\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}

\note{
  \code{optimize} is used to solve equation (4.3) of Dobbin and Simon (2007), 
  so you may see errors from it.
}
\seealso{\code{\link{optimize}}}

\examples{
## see Table 2 of Dobbin et al. (2008)
g <- 0.1
fc <- 1.6
ssize.pcc(gamma = g, stdFC = fc, nrFeatures = 22000)

## see Table 3 of Dobbin et al. (2008)
g <- 0.05
fc <- 1.1
ssize.pcc(gamma = g, stdFC = fc, nrFeatures = 22000)
}
\keyword{htest}


