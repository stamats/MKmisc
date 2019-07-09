CV <- function(x, na.rm = FALSE){
  stopifnot(is.numeric(x))
  if(any(x <= 0))
    stop("Your data must be positive!")
  sd(x, na.rm = na.rm)/mean(x, na.rm = na.rm)
}

medCV <- function(x, na.rm = FALSE){
  stopifnot(is.numeric(x))
  if(any(x <= 0))
    stop("Your data must be positive!")
  mad(x, na.rm = na.rm)/median(x, na.rm = na.rm)
}

iqrCV <- function(x, na.rm = FALSE){
  stopifnot(is.numeric(x))
  if(any(x <= 0))
    stop("Your data must be positive!")
  sIQR(x, na.rm = na.rm)/median(x, na.rm = na.rm)
}
