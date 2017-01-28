## Confidence Intervals for quantiles
quantileCI <- function(x, prob = 0.5, conf.level = 0.95, method = "exact",
                       minLength = FALSE, na.rm = FALSE){
    if(!is.na(pmatch(method, "exact")))
        method <- "exact"

    METHODS <- c("exact", "asymptotic")
    method <- pmatch(method, METHODS)

    if(is.na(method))
        stop("invalid method")

    if(method == -1)
        stop("ambiguous method")

    if(length(x) <= 1)
        stop("'x' has to be of at least length 2")
    if(length(conf.level) != 1)
        stop("'conf.level' has to be of length 1 (confidence level)")
    if(conf.level < 0.5 | conf.level > 1)
        stop("'conf.level' has to be in [0.5, 1]")

    alpha <- 1 - conf.level
    z <- qnorm(1-alpha/2)
    if(na.rm) x <- x[!is.na(x)]
    n <- length(x)
    est <- quantile(x, prob = prob)
    xs <- sort(x)

    if(method == 1){ # exact
        CI.mat <- matrix(NA, ncol = 2, nrow = n-1)
        pcov.vec <- numeric(n-1)
        for(i in 1:(n-1)){
          for(j in (i+1):n){
            pcov <- pbinom(j-1, size = n, prob = prob)-pbinom(i-1, size = n, prob = prob)
            if(pcov > conf.level){
              pcov.vec[i] <- pcov
              CI.mat[i,] <- c(xs[i], xs[j])
              break
            }
          }
        }
        if(all(pcov.vec == 0)){
          CI <- matrix(c(xs[1], xs[n]), nrow = 1)
          attr(CI, "conf.level") <- 1
          alpha <- 0
          rownames(CI) <- rep(paste(100*prob, "% quantile"), nrow(CI))
          colnames(CI) <- c("lower", "upper")
        }else{
          CI.mat <- CI.mat[pcov.vec > 0,,drop = FALSE]
          pcov.vec <- pcov.vec[pcov.vec > 0]
          pcov.min <- min(pcov.vec)
          CI <- CI.mat[pcov.vec == pcov.min,,drop = FALSE]
          if(minLength){
            CI <- CI[which.min(diff(t(CI))),,drop = FALSE]
          }
          attr(CI, "conf.level") <- pcov.min
          rownames(CI) <- rep(paste(100*prob, "% quantile"), nrow(CI))
          colnames(CI) <- c("lower", "upper")
        }
    }
    if(method == 2){ # approx
        prob.sd <- sqrt(n*prob*(1-prob))
        k.lo <- max(1, floor(n*prob - z*prob.sd))
        k.up <- min(n, ceiling(n*prob + z*prob.sd))
        CI <- matrix(c(xs[k.lo], xs[k.up]), nrow = 1)
        attr(CI, "conf.level") <- conf.level
        rownames(CI) <- rep(paste(100*prob, "% quantile"), nrow(CI))
        colnames(CI) <- c(paste(alpha/2*100, "%"), paste((1-alpha/2)*100, "%"))
    }

    names(est) <- paste(100*prob, "% quantile")

    if(minLength){
      meth <- paste("minimum length", METHODS[method], "confidence interval")
    }else{
      meth <- paste(METHODS[method], "confidence interval")
    }

    return(structure(list("estimate" = est, "conf.int" = CI,
                          "method" = meth),
                     class = "confint"))
}

medianCI <- function(x, conf.level = 0.95, method = "exact", minLength = FALSE,
                     na.rm = FALSE){
    res <- quantileCI(x, prob = 0.5, conf.level = conf.level, method = method,
                      minLength = minLength, na.rm = na.rm)
    rownames(res$conf.int) <- rep("median", nrow(res$conf.int))
    names(res$estimate) <- "median"
    res
}

madCI <- function(x, conf.level = 0.95, method = "exact", minLength = FALSE,
                  na.rm = FALSE, constant = 1.4826){
  M <- median(x, na.rm = na.rm)
  res <- medianCI(constant*abs(x-M), conf.level = conf.level,
                  method = method, minLength = minLength, na.rm = na.rm)
  rownames(res$conf.int) <- rep("MAD", nrow(res$conf.int))
  names(res$estimate) <- "MAD"
  res
}
