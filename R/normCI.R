## Confidence Intervals for normal mean and standard deviation
normCI <- function(x, mean = NULL, sd = NULL, conf.level = 0.95, na.rm = TRUE){
  if(!is.numeric(x))
    stop("'x' must be a numeric vector")
  if(!is.null(mean))
    if(length(mean) != 1) stop("'mean' has to be of length 1")
  if(!is.null(sd))
    if(length(sd) != 1) stop("'sd' has to be of length 1")
  if(!is.null(sd))
    if(sd <= 0) stop("'sd' has to be positive")
  if(length(conf.level) != 1)
    stop("'conf.level' has to be of length 1 (confidence level)")
  if(conf.level < 0.5 | conf.level > 1)
    stop("'conf.level' has to be in [0.5, 1]")

  Infos <- NULL
  if(is.null(mean)){
    m <- mean(x, na.rm = na.rm)
  }else{
    m <- mean
  }
  if(is.null(sd)){
    s <- sd(x, na.rm = na.rm)
  }else{
    s <- sd
  }
  if(na.rm) n <- length(x[!is.na(x)]) else n <- length(x)
  if(is.null(mean) & is.null(sd)){
    est <- c(m, s)
    names(est) <- c("mean", "sd")
  }else{
    if(is.null(mean)){
      est <- m
      names(est) <- "mean"
    }else{
      est <- s
      names(est) <- "sd"
    }
  }
  alpha <- 1 - conf.level
  if(is.null(sd)){
    k <- qt(1-alpha/2, df = n-1)
  }else{
    k <- qnorm(1-alpha/2)
  }
  if(is.null(mean)){
    sem <- s/sqrt(n)
    names(sem) <- "SE of mean"
    CI.lower.mean <- m - k*sem
    CI.upper.mean <- m + k*sem
    Infos <- sem
  }
  if(is.null(sd)){
    CI.lower.sd <- sqrt(n-1)*s/sqrt(qchisq(1-alpha/2, df = n-1))
    CI.upper.sd <- sqrt(n-1)*s/sqrt(qchisq(alpha/2, df = n-1))
  }
  if(is.null(mean) & is.null(sd)){
    CI <- rbind(c(CI.lower.mean, CI.upper.mean),
                c(CI.lower.sd, CI.upper.sd))
    rownames(CI) <- c("mean", "sd")
    colnames(CI) <- c(paste(alpha/2*100, "%"), paste((1-alpha/2)*100, "%"))
  }else{
    if(is.null(mean)){
      CI <- matrix(c(CI.lower.mean, CI.upper.mean), nrow = 1)
      rownames(CI) <- "mean"
      colnames(CI) <- c(paste(alpha/2*100, "%"), paste((1-alpha/2)*100, "%"))
    }else{
      CI <- matrix(c(CI.lower.sd, CI.upper.sd), nrow = 1)
      rownames(CI) <- "sd"
      colnames(CI) <- c(paste(alpha/2*100, "%"), paste((1-alpha/2)*100, "%"))
    }
  }
  attr(CI, "conf.level") <- conf.level

  return(structure(list("estimate" = est, "conf.int" = CI, "Infos" = Infos,
                        method = "Exact confidence interval(s)"),
                   class = "confint"))
}

print.confint <- function (x, digits = getOption("digits"), prefix = "\t", ...) {
  cat("\n")
  cat(strwrap(x$method, prefix = prefix), sep = "\n")
  cat("\n")
  out <- x$conf.int
  attr(out, "conf.level") <- NULL
  if (nrow(x$conf.int) > 1) {
    cat(format(100 * attr(x$conf.int, "conf.level")),
        " percent confidence intervals:\n", sep = "")
  }else{
    cat(format(100 * attr(x$conf.int, "conf.level")),
        " percent confidence interval:\n", sep = "")
  }
  print(out, digits = digits, ...)
  if (!is.null(x$estimate)) {
    cat("\n")
    if(length(x$estimate) == 1) cat("sample estimate:\n")
    if(length(x$estimate) > 1) cat("sample estimates:\n")
    print(x$estimate, digits = digits, ...)
  }
  if (!is.null(x$Infos)) {
    cat("\n")
    cat("additional information:\n")
    if(length(x$Infos) > 1){
      for(i in seq_len(length(x$Infos))){
        if(i > 1) cat("\n")
        print(x$Infos[[i]], digits = digits, ...)
      }
    }else{
      print(x$Infos, digits = digits, ...)
    }
  }
  cat("\n")
  invisible(x)
}
