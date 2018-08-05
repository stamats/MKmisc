library(scales)
glog_trans <- function(base = exp(1)){
  trans_new("glog",
            transform = function(x) glog(x, base = base),
            inverse = function(x) inv.glog(x, base = base))
}
glog10_trans <- function(){ glog_trans(base = 10) }
glog2_trans <- function(){ glog_trans(base = 2) }

scale_y_glog <- function(...){
  scale_y_continuous(..., trans = glog_trans())
}
scale_x_glog <- function(...){
  scale_x_continuous(..., trans = glog_trans())
}
scale_y_glog10 <- function(...){
  scale_y_continuous(..., trans = glog10_trans())
}
scale_x_glog10 <- function(...){
  scale_x_continuous(..., trans = glog10_trans())
}
scale_y_glog2 <- function(...){
  scale_y_continuous(..., trans = glog2_trans())
}
scale_x_glog2 <- function(...){
  scale_x_continuous(..., trans = glog2_trans())
}

neglog_breaks <- function (n = 5, base = 10){
  function(x){
    rng <- rev(-log(range(x, na.rm = TRUE), base = base))
    min <- floor(rng[1])
    max <- ceiling(rng[2])
    if (max == min)
      return(base^min)
    by <- floor((max - min)/n) + 1
    base^(seq(min, max, by = by))
  }
}
neglog_trans <- function(base = exp(1)){
  trans_new("neglog",
            transform = function(x) -log(x, base),
            inverse = function(x) base^(-x),
            breaks = neglog_breaks(base = base),
            domain = c(1e-100, Inf))
}
neglog10_trans <- function(){ neglog_trans(base = 10) }
neglog2_trans <- function(){ neglog_trans(base = 2) }
scale_y_neglog <- function(...){
  scale_y_continuous(..., trans = neglog_trans())
}
scale_x_neglog <- function(...){
  scale_x_continuous(..., trans = neglog_trans())
}
scale_y_neglog10 <- function(...){
  scale_y_continuous(..., trans = neglog10_trans())
}
scale_x_neglog10 <- function(...){
  scale_x_continuous(..., trans = neglog10_trans())
}
scale_y_neglog2 <- function(...){
  scale_y_continuous(..., trans = neglog2_trans())
}
scale_x_neglog2 <- function(...){
  scale_x_continuous(..., trans = neglog2_trans())
}
