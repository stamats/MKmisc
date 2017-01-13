risks <- function(p0, p1){
  if(p0 <= 0 | p0 >= 1)
    stop("'p0' has to be in (0,1)")
  if(length(p0) != 1)
    stop("'p0' has to be of length 1")
  if(p1 <= 0 | p1 >= 1)
    stop("'p1' has to be in (0,1)")
  if(length(p1) != 1)
    stop("'p1' has to be of length 1")
  RR <- p1/p0
  OR <- p1/(1-p1)/(p0/(1-p0))
  if(RR <= 1){
    ARR <- p0 - p1
    NNT <- 1/ARR
    RRR <- 1 - RR
    res <- c(p0 = p0, p1 = p1, RR = RR, OR = OR, RRR = RRR, ARR = ARR, NNT = NNT)
  }else{
    ARI <- p1 - p0
    NNH <- 1/ARI
    RRI <- RR-1
    res <- c(p0 = p0, p1 = p1, RR = RR, OR = OR, RRI = RRI, ARI = ARI, NNH = NNH)
  }
  return(res)
}
