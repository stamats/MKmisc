## odds ratio (OR) to relative risk (RR)
or2rr <- function(or, p0, p1){
  if(any(or <= 0))
    stop("'or' has to be positive")
  if(!missing(p0)){
    if(p0 <= 0 | p0 >= 1)
      stop("'p0' has to be in (0,1)")
    if(length(p0) != 1)
      stop("'p0' has to be of length 1")
    names(or) <- NULL
    rr <- or/(1 - p0 + p0*or)
  }
  if(!missing(p1)){
    if(p1 <= 0 | p1 >= 1)
      stop("'p1' has to be in (0,1)")
    if(length(p1) != 1)
      stop("'p1' has to be of length 1")
    names(or) <- NULL
    rr <- or*(1-p1) + p1
  }
  rr
}
