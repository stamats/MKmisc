## compute "generalized log" = asinh
glog <- function(x, base = exp(1)){
  log(x + sqrt(x^2 + 1), base = base) - log(2, base = base)
}
glog10 <- function(x){ glog(x, base = 10) }
glog2 <- function(x){ glog(x, base = 2) }
