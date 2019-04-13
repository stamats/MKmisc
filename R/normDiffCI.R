## confidence interval for difference of means
normDiffCI <- function(x, y, conf.level = 0.95, paired = FALSE,
                       method = "welch", na.rm = TRUE){
  if(!is.na(pmatch(method, "welch"))) method <- "welch"

  METHODS <- c("classical", "welch", "hsu")
  method <- pmatch(method, METHODS)

  if (is.na(method))
    stop("invalid method")
  if (method == -1)
    stop("ambiguous method")

  if(!is.numeric(x))
    stop("'x' must be a numeric vector")
  if(!is.numeric(y))
    stop("'y' must be a numeric vector")
  if(length(conf.level) != 1)
    stop("'conf.level' has to be of length 1 (confidence level)")
  if(conf.level < 0.5 | conf.level > 1)
    stop("'conf.level' has to be in [0.5, 1]")

  Infos <- NULL
  alpha <- 1 - conf.level
  if(paired){
    CIres <- normCI(x = x-y, conf.level = conf.level, na.rm = na.rm)
    d <- CIres$estimate[1]
    CI.lower <- CIres$conf.int[1,1]
    CI.upper <- CIres$conf.int[1,2]
    Infos <- CIres$Infos
    names(Infos) <- "SE of mean of differences"
    names(d) <- "mean of differences"
    method <- "Confidence interval (paired)"
  }else{
    mx <- mean(x, na.rm = na.rm)
    my <- mean(y, na.rm = na.rm)
    d <- mx - my
    if(na.rm) nx <- length(x[!is.na(x)]) else nx <- length(x)
    if(na.rm) ny <- length(y[!is.na(y)]) else ny <- length(y)
    vx <- var(x, na.rm = na.rm)
    vy <- var(y, na.rm = na.rm)
    if(method == 1){
      df <- nx + ny - 2
      s <- sqrt(((nx-1)*vx + (ny-1)*vy)/df)
      se <- s*sqrt(1/nx + 1/ny)
      method <- "Classical confidence interval (unpaired)"
    }
    if(method == 2){
      sex <- sqrt(vx/nx)
      sey <- sqrt(vy/ny)
      s <- sqrt(vx + vy)
      se <- sqrt(vx/nx + vy/ny)
      df <- se^4/(sex^4/(nx - 1) + sey^4/(ny - 1))
      method <- "Welch confidence interval (unpaired)"
    }
    if(method == 3){
      if(nx < 6 || ny < 6)
        warning("For method of Hsu the sample size per group should be > 5.")
      s <- sqrt(vx + vy)
      se <- sqrt(vx/nx + vy/ny)
      df <- min(nx, ny) - 1
      method <- "Hsu confidence interval (unpaired)"
    }
    t.alpha <- qt(1-alpha/2, df = df)
    ## confidence bounds
    CI.lower <- d - t.alpha*se
    CI.upper <- d + t.alpha*se
    names(d) <- "difference in means"
    Infos <- list(c(se, d/s), c(mx, sqrt(vx), my, sqrt(vy)))
    names(Infos[[1]]) <- c("SE of difference in means", "Cohen's d (SMD)")
    names(Infos[[2]]) <- c("mean of x", "SD of x", "mean of y", "SD of y")
  }

  CI <- matrix(c(CI.lower, CI.upper), nrow = 1)
  if(paired){
    rownames(CI) <- "mean of differences"
  }else{
    rownames(CI) <- "difference in means"
  }
  colnames(CI) <- c(paste(alpha/2*100, "%"),
                    paste((1-alpha/2)*100, "%"))
  attr(CI, "conf.level") <- conf.level

  return(structure(list("estimate" = d, "conf.int" = CI, "Infos" = Infos,
                        method = method),
                   class = "confint"))
}
