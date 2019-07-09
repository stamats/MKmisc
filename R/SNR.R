SNR <- function(x, na.rm = FALSE){
  stopifnot(is.numeric(x))
  if(any(x <= 0))
    stop("Your data must be positive!")
  mean(x, na.rm = na.rm)/sd(x, na.rm = na.rm)
}

medSNR <- function(x, na.rm = FALSE){
  stopifnot(is.numeric(x))
  if(any(x <= 0))
    stop("Your data must be positive!")
  median(x, na.rm = na.rm)/mad(x, na.rm = na.rm)
}

iqrSNR <- function(x, na.rm = FALSE){
  stopifnot(is.numeric(x))
  if(any(x <= 0))
    stop("Your data must be positive!")
  median(x, na.rm = na.rm)/sIQR(x, na.rm = na.rm)
}
