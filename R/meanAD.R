meanAD <- function(x, na.rm = FALSE, constant = sqrt(pi/2)){
  stopifnot(is.numeric(x))
  AM <- mean(x, na.rm = na.rm)
  constant*mean(abs(x-AM), na.rm = na.rm)
}
