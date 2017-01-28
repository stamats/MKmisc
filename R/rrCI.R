rrCI <- function(a, b, c, d, conf.level = 0.95){
  x <- as.integer(c(a, b, c, d))
  stopifnot(all(!is.na(x)))
  stopifnot(all(x > 0))
  if(length(conf.level) != 1)
    stop("'conf.level' has to be of length 1 (confidence level)")
  if(conf.level < 0.5 | conf.level > 1)
    stop("'conf.level' has to be in [0.5, 1]")

  RR <- a/(a+b)/(c/(c+d))
  s <- sqrt(b/(a*(a+b)) + d/(c*(c+d)))

  alpha <- 1 - conf.level
  k <- qnorm(1-alpha/2)

  CI.lower <- exp(log(RR) - k*s)
  CI.upper <- exp(log(RR) + k*s)
  CI <- matrix(c(CI.lower, CI.upper), nrow = 1)
  rownames(CI) <- "relative risk"
  colnames(CI) <- c(paste(alpha/2*100, "%"), paste((1-alpha/2)*100, "%"))
  attr(CI, "conf.level") <- conf.level
  names(RR) <- "relative risk"

  return(structure(list("estimate" = RR, "conf.int" = CI,
                        "method" = "asymptotic confidence interval"),
                   class = "confint"))
}
