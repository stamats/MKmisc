## confidence interval for difference of means
normDiffCI <- function(x, y, conf.level = 0.95, paired = FALSE,
                       var.equal = FALSE, na.rm = TRUE){
  if(!is.numeric(x))
    stop("'x' must be a numeric vector")
  if(!is.numeric(y))
    stop("'y' must be a numeric vector")
  if(length(conf.level) != 1)
    stop("'conf.level' has to be of length 1 (confidence level)")
  if(conf.level < 0.5 | conf.level > 1)
    stop("'conf.level' has to be in [0.5, 1]")

  alpha <- 1 - conf.level
  if(paired){
    CIres <- normCI(x = x-y, conf.level = conf.level, na.rm = na.rm)
    d <- CIres$estimate[1]
    CI.lower <- CIres$conf.int[1,1]
    CI.upper <- CIres$conf.int[1,2]
    names(d) <- "mean of differences"
    method <- "Exact confidence interval (paired)"
  }else{
    mx <- mean(x, na.rm = na.rm)
    my <- mean(y, na.rm = na.rm)
    d <- mx - my
    if(na.rm) nx <- length(x[!is.na(x)]) else nx <- length(x)
    if(na.rm) ny <- length(y[!is.na(y)]) else ny <- length(y)
    vx <- var(x, na.rm = na.rm)
    vy <- var(y, na.rm = na.rm)
    if(var.equal){
      df <- nx + ny - 2
      s <- sqrt(((nx-1)*vx + (ny-1)*vy)/df)
      se <- s*sqrt(1/nx + 1/ny)
      method <- "Exact confidence interval (unpaired)"
    }else{
      sex <- sqrt(vx/nx)
      sey <- sqrt(vy/ny)
      se <- sqrt(sex^2 + sey^2)
      df <- se^4/(sex^4/(nx - 1) + sey^4/(ny - 1))
      method <- "Exact confidence interval (Welch, unpaired)"
    }
    t.alpha <- qt(1-alpha/2, df = df)
    ## confidence bounds
    CI.lower <- d - t.alpha*se
    CI.upper <- d + t.alpha*se
    names(d) <- "difference of means"
  }

  CI <- matrix(c(CI.lower, CI.upper), nrow = 1)
  if(paired){
    rownames(CI) <- "mean of differences"
  }else{
    rownames(CI) <- "difference of means"
  }
  colnames(CI) <- c(paste(alpha/2*100, "%"),
                    paste((1-alpha/2)*100, "%"))
  attr(CI, "conf.level") <- conf.level

  return(structure(list("estimate" = d, "conf.int" = CI,
                        method = method),
                   class = "confint"))
}
