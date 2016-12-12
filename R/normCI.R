## Confidence Intervals for normal mean and standard deviation
normCI <- function(x, conf.level = 0.95, na.rm = TRUE){
  if(!is.numeric(x))
    stop("'x' must be a numeric vector")
  if(length(conf.level) != 1)
    stop("'conf.level' has to be of length 1 (confidence level)")
  if(conf.level < 0.5 | conf.level > 1)
    stop("'conf.level' has to be in [0.5, 1]")

  m <- mean(x, na.rm = na.rm)
  s <- sd(x, na.rm = na.rm)
  if(na.rm) n <- length(x[!is.na(x)]) else n <- length(x)
  est <- c(m, s)
  names(est) <- c("mean", "sd")
  alpha <- 1 - conf.level
  z <- qnorm(1-alpha/2)
  CI.lower.mean <- m - z*s/sqrt(n)
  CI.upper.mean <- m + z*s/sqrt(n)
  CI.lower.sd <- sqrt(n-1)*s/sqrt(qchisq(1-alpha/2, df = n-1))
  CI.upper.sd <- sqrt(n-1)*s/sqrt(qchisq(alpha/2, df = n-1))

  CI <- rbind(c(CI.lower.mean, CI.upper.mean),
              c(CI.lower.sd, CI.upper.sd))
  rownames(CI) <- c("mean", "sd")
  colnames(CI) <- c(paste(alpha/2*100, "%"), paste((1-alpha/2)*100, "%"))
  attr(CI, "confidence level") <- conf.level

  return(list("estimate" = est, "CI" = CI))
}
