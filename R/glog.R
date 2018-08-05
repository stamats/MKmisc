## compute "generalized log" = asinh
glog <- function(x, base = exp(1)){
  log(x + sqrt(x^2 + 1), base = base) - log(2, base = base)
}
glog10 <- function(x){ glog(x, base = 10) }
glog2 <- function(x){ glog(x, base = 2) }
inv.glog <- function(x, base = exp(1)){
  sinh(log(base)*x + log(2))
}
inv.glog10 <- function(x){ inv.glog(x, base = 10) }
inv.glog2 <- function(x){ inv.glog(x, base = 2) }
