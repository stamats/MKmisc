sIQR <- function(x, na.rm = FALSE, type = 7, constant = 2*qnorm(0.75)){
  IQR(x, na.rm = na.rm, type = type)/constant
}
