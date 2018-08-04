## compute "generalized log" = asinh
glog <- function(x, base = exp(1)){
  log(x + sqrt(x^2 + 1), base = base) - log(2, base = base)
}
