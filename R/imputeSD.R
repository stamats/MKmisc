## Function to impute SD
## Cochrane Handbook (2011), Section 16.1.3.2
imputeSD <- function(SD1, SD2, SDchange){
  ## 0. Check of input
  if(any(SD1[!is.na(SD1)] <= 0)){
    stop("All non NA values of 'SD1' need to be positive")
  }
  if(any(SD2[!is.na(SD2)] <= 0)){
    stop("All non NA values of 'SD2' need to be positive")
  }
  if(any(SDchange[!is.na(SDchange)] <= 0)){
    stop("All non NA values of 'SDchange' need to be positive")
  }
  stopifnot(length(SD1) == length(SD2))
  stopifnot(length(SD1) == length(SDchange))

  ## 1. Missing SD1 values are replaced by SD2 values and vice versa
  SD1[is.na(SD1)] <- SD2[is.na(SD1)]
  SD2[is.na(SD2)] <- SD1[is.na(SD2)]

  ## 2. Correlations for complete data are computed
  corr <- (SD1^2 + SD2^2 - SDchange^2)/(2*SD1*SD2)

  ## 3. Minimum, mean and maximum correlation are computed
  corr.min <- min(corr, na.rm = TRUE)
  corr.mean <- mean(corr, na.rm = TRUE)
  corr.max <- max(corr, na.rm = TRUE)

  ## 4. Missing values of SDchange are imputed
  ind.na <- is.na(SDchange)
  SDchange.min <- sqrt(pmax(SD1^2 + SD2^2 - 2*corr.max*SD1*SD2, 0))
  SDchange.mean <- sqrt(pmax(SD1^2 + SD2^2 - 2*corr.mean*SD1*SD2, 0))
  SDchange.max <- sqrt(pmax(SD1^2 + SD2^2 - 2*corr.min*SD1*SD2, 0))

  ## 5. Return results
  data.frame(SD1 = SD1, SD2 = SD2, SDchange = SDchange, Cor=corr,
             SDchange.min, SDchange.mean, SDchange.max)
}
